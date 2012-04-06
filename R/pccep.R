
  ## Common Correlated Effects Pooled estimator
  ## ref. Kapetanios, Pesaran and Yamagata JoE 2010
  ## this version 2: polished etc.

  ## NB the effect of including a trend is exactly the same as for
  ## including as.numeric(<timeindex>) in the model specification
  ## Yet it is cleaner unless some automatic treatment of group invariant
  ## variates is added for the CCE case (where else any group invariant
  ## becomes perfectly collinear with the ybar, Xbar and gives NAs in coefs.
  ## Moreover, if the panel is unbalanced then for some i the trend becomes
  ## (3,4,5, ...) instead of (1,2,3, ...); the difference is absorbed by
  ## the individual intercept, and *the group intercept* changes.

  ## TODO: see last point above: treatment of invariants

## needed for standalone operation:
#plm <- plm:::plm
#pdim <- plm:::pdim

#model.matrix.plm<-plm:::model.matrix.plm
#pmodel.response<-plm:::pmodel.response.plm


pccep <- function (formula, data, subset, na.action,
                   residuals = c("standard", "cce", "ccemg"),
                   index = NULL, trend = FALSE, ...)
{

    ## same as pggls but for effect, fixed at "individual" for compatibility
    ## ind for id, tind for time, k for K, coefnam for coef.names
    effect <- "individual"

    ## record call etc.
    model <- "ccep"
    model.name <- "ccep"  #match.arg(model)
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
  ## pre-allocate residuals matrix for individual regressions
  cceres <- matrix(NA, nrow=t, ncol=n)
  ccemgres <- matrix(NA, nrow=t, ncol=n)


  ## CCEP estimation

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

          ## single CCE coefficients (needed for vcov)
          tcoef[,i] <- solve(tXMX, tXMy)
        }

    ## calc beta_CCEP
    sXMX <- apply(XMX, 1:2, sum)
    sXMy <- apply(XMy, 1:2, sum)
    coef <- solve(sXMX, sXMy)

    ## TODO: adjust model formula etc. etc.
    ## (else breaks waldtest, update, ...)
    ## <-- maybe not necessary here, check

    ## CCEMG coefs are averages across individual regressions
    ## (here: coefs of xs-variants only!)
    coefmg <- apply(tcoef, 1, mean)

    ## calc CCEP covariance:
    psi.star <- 1/N * sXMX

    ## make matrix of cross-products of demeaned individual coefficients
    Rmat <- array(dim=c(k, k, n))
    demcoef <- tcoef - coefmg # coefmg gets recycled n times by column
    for(i in 1:n) Rmat[,,i] <- XMX[,,i] %*%
        outer(demcoef[,i], demcoef[,i]) %*% XMX[,,i]
    ## summing over the n-dimension of the array we get the
    ## covariance matrix of coefs
    R.star <- 1/(n-1) * apply(Rmat, 1:2, sum) * 1/(t^2)

    Sigmap.star <- solve(psi.star, R.star) %*% solve(psi.star)
    vcov <- Sigmap.star/n

    ## allow outputting different types of residuals
    switch(match.arg(residuals),
           standard={
               residuals <- as.vector(y) -
                   as.vector(crossprod(t(X), coef))
               }, cce={residuals <- as.vector(cceres)
               }, ccemg={
                   ## calc. CCEMG residuals as
                   ## u_CCEMG = y_it - beta_CCEMG * X_it
                   for(i in 1:n) {
                       tX<-X[ind==unind[i], , drop=FALSE]
                       ty<-y[ind==unind[i]]
                       ccemgres[,i] <- ty - tX %*% coefmg
                   }
                   residuals <- as.vector(ccemgres)
                   })

    ## code as in pggls, only difference is here there is no 'sigma'
    df.residual <- nrow(X) - ncol(X)
    fitted.values <- y - residuals
    names(coef) <- rownames(vcov) <- colnames(vcov) <- coef.names
    dimnames(tcoef) <- list(coef.names, 1:dim(tcoef)[[2]])
    pmodel <- attr(plm.model, "pmodel")
    pmodel$model.name <- model
    mgmod <- list(coefficients = coef, residuals = residuals,
                  fitted.values = fitted.values, vcov = vcov,
                  df.residual = df.residual,
                  model = model.frame(plm.model), sigma=NULL,
                  indcoef = tcoef,
                  #cceres = as.vector(cceres),
                  #ccemgres = as.vector(ccemgres),
                  call = cl)
    mgmod <- structure(mgmod, pdim = pdim, pmodel = pmodel)
    class(mgmod) <- c("pccep", "panelmodel")
    mgmod
}

## use summary and print.summary taken from pggls for now


summary.pccep <- function(object,...){
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
  object$rsqr <- 1-object$ssr/object$tss
  class(object) <- c("summary.pccep")
  return(object)
}

print.summary.pccep <- function(x,digits=max(3, getOption("digits") - 2), width = getOption("width"),...){
  pmodel <- attr(x,"pmodel")
  pdim <- attr(x,"pdim")
  effect <- pmodel$effect
  formula <- pmodel$formula
  model.name <- pmodel$model.name
#  cat(paste(effect.pggls.list[effect]," ",sep=""))
#  cat(paste(model.pggls.list[model.name],"\n",sep=""))
  cat("Common Correlated Effects Pooled model")
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
  cat(paste("Multiple R-squared: ",signif(x$rsqr,digits),"\n",sep=""))
  invisible(x)
}


