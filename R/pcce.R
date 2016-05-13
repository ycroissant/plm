## Common Correlated Effects Pooled/MG estimators
  ## ref. Holly, Pesaran and Yamagata JoE 158 (2010)
  ## (also Kapetanios, Pesaran and Yamagata JoE 2010)
  ## CCEP and CCEMG together in the same SW framework
  ## based on generalized FEs

  ## this version 6: includes both defactored (cce) and raw (standard) residuals,
  ## leaving to a special residuals.pcce method the choice of which to retrieve

  ## NB the effect of including a trend is exactly the same as for
  ## including as.numeric(<timeindex>) in the model specification
  ## If the panel is unbalanced, though, then for some i the trend becomes
  ## (3,4,5, ...) instead of (1,2,3, ...); the difference is absorbed by
  ## the individual intercept, and *the group intercept* changes.

## needed for standalone operation:
#plm <- plm:::plm
#pdim <- plm:::pdim

#model.matrix.plm<-plm:::model.matrix.plm
#pmodel.response.plm<-plm:::pmodel.response.plm

#tss <- plm:::tss


pcce <- function (formula, data, subset, na.action,
                   model=c("mg", "p"),
                   #residuals = c("defactored", "standard"),
                   index = NULL, trend = FALSE, ...)
{
    ## Create a Formula object if necessary (from plm.R)
    if (!inherits(formula, "pFormula")) formula <- pFormula(formula)

    ## same as pggls but for effect, fixed at "individual" for compatibility
    ## ind for id, tind for time, k for K, coefnam for coef.names
    effect <- "individual"

    ## record call etc.
    model.name <- paste("cce", match.arg(model), sep="")
    data.name <- paste(deparse(substitute(data)))
    cl <- match.call()
    plm.model <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "na.action", "effect",
        "model", "index"), names(plm.model), 0)
    plm.model <- plm.model[c(1, m)]
    plm.model[[1]] <- as.name("plm")
    ## change the 'model' in call
    plm.model$model <- "pooling"
    ## evaluates the call, modified with model = "pooling", inside the
    ## parent frame resulting in the pooling model on formula, data
    plm.model <- eval(plm.model, parent.frame())
    index <- attr(model.frame(plm.model), "index")
    ## group index
    ind <- index[[1]]
    ## time index
    tind <- index[[2]]
    ## set dimension variables
    pdim <- pdim(plm.model)
    balanced <- pdim$balanced
    nt <- pdim$Tint$nt
    Ti <- pdim$Tint$Ti
    T. <- pdim$nT$T
    n <- pdim$nT$n
    N <- pdim$nT$N
    ## set index names
    time.names <- pdim$panel.names$time.names
    id.names <- pdim$panel.names$id.names
    coef.names <- names(coef(plm.model))
    ## number of coefficients
    k <- length(coef.names)

    ## model data
    X <- model.matrix(plm.model)
    y <- model.response(model.frame(plm.model))

  ## det. *minimum* group numerosity
  t<-min(tapply(X[,1],ind,length))

  ## check min. t numerosity
  ## NB it is also possible to allow estimation if there *is* one group
  ## with t large enough and average on coefficients removing NAs
  ## Here we choose the explicit way: let estimation fail if we lose df
  ## but a warning would do...
  if(t<(k+1)) stop("Insufficient number of time periods")

  ## one regression for each group i in 1..n
  ## and retrieve coefficients putting them into a matrix
  ## (might be unbalanced => t1!=t2 but we don't care as long
  ## as min(t)>k+1)

  ## subtract intercept from parms number and names
    if(attr(terms(plm.model), "intercept")) {
        k <- k-1
        coef.names <- coef.names[-1]
    }

  ## "pre-allocate" coefficients matrix for the n models
  tcoef<-matrix(NA,nrow=k,ncol=n)

  ## pre-allocate residuals lists for individual regressions
  ## (lists allow for unbalanced panels)
  cceres <- vector("list", n)
  stdres <- vector("list", n)

  ## CCE by-group estimation

    ## must put the intercept into the group-invariant part!!
    ## so first drop it from X
    if(attr(terms(plm.model), "intercept")) {
        X <- X[,-1, drop=FALSE]
    }

    ## group-invariant part, goes in Hhat
      ## between-periods transformation (take means over groups for each t)
      be<-function(x,index,na.rm=T) tapply(x,index,mean,na.rm=na.rm)
      Xm<-apply(X,2,FUN=be,index=tind)[tind, , drop=FALSE]
      ym<-apply(as.matrix(as.numeric(y)),2,FUN=be,index=tind)[tind]

      if(attr(terms(plm.model), "intercept")) {
              Hhat <- cbind(ym, Xm, 1)
          } else {
              Hhat <- cbind(ym, Xm)
          }

      ## prepare XMX, XMy arrays
      XMX <- array(dim=c(k,k,n))
      XMy <- array(dim=c(k,1,n))

      ## hence calc. beta_i anyway because of vcov

      ## for each x-sect. i=1..n estimate (over t) the CCE for every TS
      ## as in KPY, eq. 15
      unind<-unique(ind)
      for(i in 1:n) {
          tX <- X[ind==unind[i], , drop=FALSE]
          ty <- y[ind==unind[i]]
          tHhat <- Hhat[ind==unind[i], , drop=FALSE]

          ## if 'trend' then augment the xs-invariant component
          if(trend) tHhat <- cbind(tHhat, 1:(dim(tHhat)[[1]]))

          ## NB tHat, tMhat should be i-invariant
          tMhat <- diag(1, length(ty)) -
              tHhat %*% solve(crossprod(tHhat), t(tHhat))
          tXMX <- crossprod(tX, tMhat %*% tX)
          tXMy <- crossprod(tX, tMhat %*% ty)

          ## XMX_i, XMy_i
          XMX[,,i] <- tXMX
          XMy[,,i] <- tXMy

          ## single CCE coefficients
          tb <- ginv(tXMX) %*% tXMy  #solve(tXMX, tXMy)
          ## USED A GENERALIZED INVERSE HERE BECAUSE OF PBs WITH ECM SPECS
          ## Notice remark in Pesaran (2006, p.977, between (27) and (28))
          ## that XMX.i is invariant to the choice of a g-inverse for H'H
          tcoef[,i] <- tb

          ## cce (defactored) residuals as M_i(y_i - X_i * bCCEMG_i)
          cceres[[i]] <- tMhat %*% (ty - tX %*% tb)
          ## std. (raw) residuals as y_i - X_i * bCCEMG_i - a_i
          ta <- mean(ty-tX)
          stdres[[i]] <- ty - tX %*% tb - ta
        }

  ## module for making transformed data My, MX for vcovHC use
    ## (NB M is symmetric)
    ## Some redundancy because this might be moved to model.matrix.pcce

    ## initialize
    tX1 <- X[ind==unind[1], , drop=FALSE]
    ty1 <- y[ind==unind[1]]
    tHhat1 <- Hhat[ind==unind[1], , drop=FALSE]

    ## if 'trend' then augment the xs-invariant component
    if(trend) tHhat1 <- cbind(tHhat1, 1:(dim(tHhat)[[1]]))

    ## NB tHat, tMhat should be i-invariant (but beware of unbalanced)
    tMhat1 <- diag(1, length(ty1)) -
        tHhat1 %*% solve(crossprod(tHhat1), t(tHhat1))
    MX <- crossprod(tMhat1, tX1)
    My <- crossprod(tMhat1, ty1)
    for(i in 2:n) {
        tX <- X[ind==unind[i], , drop=FALSE]
        ty <- y[ind==unind[i]]
        tHhat <- Hhat[ind==unind[i], , drop=FALSE]

        ## if 'trend' then augment the xs-invariant component
        if(trend) tHhat <- cbind(tHhat, 1:(dim(tHhat)[[1]]))

        ## NB tHat, tMhat should be i-invariant
        tMhat <- diag(1, length(ty)) -
            tHhat %*% solve(crossprod(tHhat), t(tHhat))
        tMX <- crossprod(tMhat, tX)
        tMy <- crossprod(tMhat, ty)

        MX <- rbind(MX, tMX)
        My <- c(My, tMy)
    }

    ## checks
    ## MX<<-MX
    ## My<<-My

    ## ALT:
    ## MXa <<- kronecker(diag(n), tMhat1) %*% X
    ## Mya <<- kronecker(diag(n), tMhat1) %*% y
    ## very same result, less efficient

  ## end data module

    ## CCEMG coefs are averages across individual regressions
    ## (here: coefs of xs-variants only!)
    coefmg <- apply(tcoef, 1, mean)

    ## make matrix of cross-products of demeaned individual coefficients
    Rmat <- array(dim=c(k, k, n))

    ## make b_i - b_CCEMG
    demcoef <- tcoef - coefmg # coefmg gets recycled n times by column

    ## calc. coef and vcov according to model
    switch(match.arg(model), mg={
        ## assign beta CCEMG
        coef <- coefmg
        for(i in 1:n) Rmat[,,i] <-  outer(demcoef[,i], demcoef[,i])
        vcov <- 1/(n*(n-1)) * apply(Rmat, 1:2, sum)
    }, p={
        ## calc beta_CCEP
        sXMX <- apply(XMX, 1:2, sum)
        sXMy <- apply(XMy, 1:2, sum)
        coef <- solve(sXMX, sXMy)

        ## calc CCEP covariance:
        psi.star <- 1/N * sXMX

        for(i in 1:n) Rmat[,,i] <- XMX[,,i] %*%
            outer(demcoef[,i], demcoef[,i]) %*% XMX[,,i]
        ## summing over the n-dimension of the array we get the
        ## covariance matrix of coefs
        R.star <- 1/(n-1) * apply(Rmat, 1:2, sum) * 1/(t^2)

        Sigmap.star <- solve(psi.star, R.star) %*% solve(psi.star)
        vcov <- Sigmap.star/n

        ## calc CCEP residuals both defactored and raw
        for(i in 1:n) {
            ## must redo all this because needs b_CCEP, which is
            ## not known at by-groups step
            tX <- X[ind==unind[i], , drop=FALSE]
            ty <- y[ind==unind[i]]
            tHhat <- Hhat[ind==unind[i], , drop=FALSE]

            ## if 'trend' then augment the xs-invariant component
            if(trend) tHhat <- cbind(tHhat, 1:(dim(tHhat)[[1]]))

            ## NB tHat, tMhat should be i-invariant (but for the
            ## group size if unbalanced)
            tMhat <- diag(1, length(ty)) -
                tHhat %*% solve(crossprod(tHhat), t(tHhat))

            ## cce residuals as M_i(y_i - X_i * bCCEP)
            cceres[[i]] <- tMhat %*% (ty - tX %*% coef)
            ## std. (raw) residuals as y_i - X_i * bCCEMG_i - a_i
            ta <- mean(ty-tX)
            stdres[[i]] <- ty - tX %*% coef - ta
        }
    })

    ## calc. measures of fit according to model type
    switch(match.arg(model), mg={

        ## R2 as in HPY 2010: sigma2ccemg = average (over n) of variances
        ## of defactored residuals
        ## (for unbalanced panels, each variance is correctly normalized
        ## by group dimension T.i)
        ##
        ## If balanced, would simply be
        ## sum(unlist(cceres)^2)/(n*(T.-2*k-2))

        ## pre-allocate list for individual CCEMG residual variances
        sigma2cce.i <- vector("list", n)
        ## average variance of defactored residuals sigma2ccemg as in
        ## Holly, Pesaran and Yamagata, (3.14)
        for(i in 1:n) {
            sigma2cce.i[[i]] <- crossprod(cceres[[i]])*
                1/(length(cceres[[i]])-2*k-2)
        }
        sigma2cce <- 1/n*sum(unlist(sigma2cce.i))

    }, p={

        ## variance of defactored residuals sigma2ccep as in Holly,
        ## Pesaran and Yamagata, (3.15)
        sigma2cce <- 1/(n*(T.-k-2)-k)*
            sum(unlist(lapply(cceres, crossprod)))
        ## is the same as sum(unlist(cceres)^2)

    })

    ## calc. overall R2, CCEMG or CCEP depending on 'model'
    sigma2.i <- vector("list", n)
    for(i in 1:n) {
          ty <- y[ind==unind[i]]
          sigma2.i[[i]] <- sum((ty-mean(ty))^2)/(length(ty)-1)
      }
    sigma2y <- mean(unlist(sigma2.i))
    r2cce <- 1 - sigma2cce/sigma2y

    ## allow outputting different types of residuals
    stdres <- unlist(stdres)
    residuals <- unlist(cceres)

    ## add transformed data (for now a simple list)
    tr.model <- list(y=My, X=MX)
    ## so that if the model is ccepmod,
    ## > lm(ccepmod$tr.model[["y"]]~ccepmod$tr.model[["X"]]-1)
    ## reproduces the model results

    ## Final model object:
    ## code as in pggls, differences:
    ## - here there is no 'sigma'
    ## - there are two types of residuals
    ## - transformed data My, MX are included for vcovHC usage
    df.residual <- nrow(X) - ncol(X)
    fitted.values <- y - residuals
    names(coef) <- rownames(vcov) <- colnames(vcov) <- coef.names
    dimnames(tcoef) <- list(coef.names, id.names)
    pmodel <- attr(plm.model, "pmodel")
    pmodel$model.name <- model
    mgmod <- list(coefficients = coef, residuals = residuals,
                  stdres = stdres, tr.model=tr.model,
                  fitted.values = fitted.values, vcov = vcov,
                  df.residual = df.residual,
                  model = model.frame(plm.model), sigma=NULL,
                  indcoef = tcoef, r.squared=r2cce,
                  #cceres = as.vector(cceres),
                  #ccemgres = as.vector(ccemgres),
                  formula = formula, call = cl)
    mgmod <- structure(mgmod, pdim = pdim, pmodel = pmodel)
    class(mgmod) <- c("pcce", "panelmodel")
    mgmod
}


summary.pcce <- function(object,...){
  pmodel <- attr(object,"pmodel")
  std.err <- sqrt(diag(object$vcov))
  b <- object$coefficients
  z <- b/std.err
#  p <- 2*(1-pnorm(abs(z)))
  p <- 2*pnorm(abs(z),lower.tail=FALSE)
  CoefTable <- cbind(b,std.err,z,p)
  colnames(CoefTable) <- c("Estimate","Std. Error","z-value","Pr(>|z|)")
  object$CoefTable <- CoefTable
  y <- object$model[[1]]
  object$tss <- tss(y)
  object$ssr <- sum(residuals(object)^2)
  object$rsqr <- object$r.squared #1-object$ssr/object$tss
  class(object) <- c("summary.pcce")
  return(object)
}

print.summary.pcce <- function(x,digits=max(3, getOption("digits") - 2), width = getOption("width"),...){
  pmodel <- attr(x,"pmodel")
  pdim <- attr(x,"pdim")
  effect <- pmodel$effect
  formula <- pmodel$formula
  model.name <- pmodel$model.name
#  cat(paste(effect.pggls.list[effect]," ",sep=""))
#  cat(paste(model.pggls.list[model.name],"\n",sep=""))
  cat("Common Correlated Effects model")
  cat("\nCall:\n")
  print(x$call)
  cat("\n")
  print(pdim)
  cat("\nResiduals\n")
  print(summary(unlist(residuals(x))))
  cat("\nCoefficients\n")
  printCoefmat(x$CoefTable,digits=digits)
  cat(paste("Total Sum of Squares: ",signif(x$tss,digits),"\n",sep=""))
  cat(paste("Residual Sum of Squares: ",signif(x$ssr,digits),"\n",sep=""))
  cat(paste("HPY R-squared: ",signif(x$rsqr,digits),"\n",sep=""))
  invisible(x)
}

residuals.pcce <- function(object,
                           type = c("defactored", "standard"),
                           ...) {
    ## special resid() method for pcce: allows to extract either
    ## defactored residuals (default) or raw residuals
    defres <- pres(object)
    switch(match.arg(type),
           standard={
               stdres <- object$stdres
               ## add panel features taking from
               class(stdres) <- class(defres)
               attr(stdres, "index") <- attr(defres, "index")
               residuals <- stdres
                 },
           defactored={residuals <- defres}
           )
    return(residuals)
}


model.matrix.pcce <- function(object, ...) {
    object$tr.model$X
}


pmodel.response.pcce <- function(object, ...) {
    object$tr.model$y
}
