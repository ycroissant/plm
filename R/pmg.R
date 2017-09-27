#pmg <- function(formula,index,tindex=NULL,data=ls(),type=c("mg","cmg","dmg")) {
  ## Mean Group estimator
  ## ref. Coakley, Fuertes and Smith 2004
  ##
  ## This version 10:
  ##   added R2=1-var(resid)/var(y) as a measure of fit
  ## from version 9:
  ##   fixed residuals
  ##   output matrix of individual coefficients as 'indcoef' aptly named

  ## NB the effect of including a trend is exactly the same as for
  ## including as.numeric(<timeindex>) in the model specification
  ## Yet it is cleaner unless some automatic treatment of group invariant
  ## variates is added for the CCE case (where else any group invariant
  ## becomes perfectly collinear with the ybar, Xbar and gives NAs in coefs.
  ## Moreover, if the panel is unbalanced then for some i the trend becomes
  ## (3,4,5, ...) instead of (1,2,3, ...); the difference is absorbed by
  ## the individual intercept, and *the group intercept* changes.

  ## TODO: see last point above: treatment of invariants

  ## TODO: see how to estimate the intercept in cmg, dmg

  ## TODO: manage models without intercept in cmg, dmg

  ## TODO: output single coefs (see how the structure of pvcm is)

## needed for standalone operation:
#plm <- plm:::plm
#pdim <- plm:::pdim

#model.matrix.plm <- plm:::model.matrix.plm
#pmodel.response <- plm:::pmodel.response.plm


pmg <- function(formula, data, subset, na.action,
                model = c("mg","cmg","dmg"), index = NULL,
                trend = FALSE, ...)
{

    ## same as pggls but for effect, fixed at "individual" for compatibility
    ## ind for id, tind for time, k for K, coefnam for coef.names
    effect <- "individual"

    ## record call etc.
    model.name <- match.arg(model)
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
  t <- min(tapply(X[,1], ind, length))

  ## check min. t numerosity
  ## NB it is also possible to allow estimation if there *is* one group
  ## with t large enough and average on coefficients removing NAs
  ## Here we choose the explicit way: let estimation fail if we lose df
  ## but a warning would do...
  if(t < (k+1)) stop("Insufficient number of time periods")

  ## one regression for each group i in 1..n
  ## and retrieve coefficients putting them into a matrix
  ## (might be unbalanced => t1!=t2 but we don't care as long
  ## as min(t)>k+1)

  ## "pre-allocate" coefficients matrix for the n models
  kt <- if(trend) 1 else 0
  tcoef <- matrix(NA, nrow = k+kt, ncol = n)
  tres <- vector("list", n)

  switch(match.arg(model),
    mg={
      ## for each x-sect. i=1..n
      unind <- unique(ind)
      for(i in 1:n) {
        tX <- X[ind == unind[i],]
        ty <- y[ind == unind[i]]
        if(trend) tX <- cbind(tX, 1:(dim(tX)[[1]]))
        tfit <- lm.fit(tX, ty)
        tcoef[ , i] <- tfit$coefficients
        tres[[i]] <- tfit$residuals
      }
      ## 'trend' always comes last
      if(trend) coef.names <- c(coef.names, "trend")
      ## adjust k
      k <- length(coef.names)
      },
    cmg={

      ## between-periods transformation (take means over groups for each t)
      be <- function(x,index,na.rm=TRUE) tapply(x,index,mean,na.rm=na.rm)
      Xm <- apply(X,2,FUN=be,index=tind)[tind,]
      ym <- apply(as.matrix(as.numeric(y)),2,FUN=be,index=tind)[tind]

      augX <- cbind(X, ym, Xm[ ,-1])

      ## allow for extended coef vector
      tcoef0 <- matrix(NA, nrow = 2*k+kt, ncol = n)

      ## for each x-sect. i=1..n estimate (over t) an augmented model
      ## y_it = alfa_i + beta_i*X_it + c1_i*my_t + c2_i*mX_t + err_it
      unind <- unique(ind)
      for(i in 1:n) {
        taugX <- augX[ind == unind[i], ]
        ty <- y[ind == unind[i]]

        if(trend) taugX <- cbind(taugX, 1:(dim(taugX)[[1]]))

        tfit <- lm.fit(taugX, ty)
        tcoef0[ ,i] <- tfit$coefficients
        tres[[i]] <- tfit$residuals
      }
      tcoef <- tcoef0[1:k,]
      tcoef.bar <- tcoef0[-(1:k),]

      coef.names.bar <- c("y.bar", paste(coef.names[-1], ".bar", sep=""))

      ## 'trend' always comes last
      if(trend) coef.names.bar <- c(coef.names.bar, "trend")

      ## output complete coefs
      tcoef <- tcoef0
      coef.names <- c(coef.names, coef.names.bar)
      ## adjust k
      k <- length(coef.names)

      ## TODO: adjust model formula etc. etc. (else breaks waldtest, update, ...)

  },
    dmg={

      ## between-periods transformation (take means over group for each t)
      be <- function(x,index,na.rm=TRUE) tapply(x,index,mean,na.rm=na.rm)
      Xm <- apply(X,2,FUN=be,index=tind)[tind,]
      ym <- apply(as.matrix(as.numeric(y)),2,FUN=be,index=tind)[tind]
      ## ...but of course we do not demean the intercept!
      Xm[ ,1] <- 0

      demX <- X - Xm
      demy <- y - ym

      ## for each x-sect. i=1..n estimate (over t) a demeaned model
      ## (y_it-my_t) = alfa_i + beta_i*(X_it-mX_t) + err_it
      unind <- unique(ind)
      for(i in 1:n) {
        tdemX <- demX[ind == unind[i], ]
        tdemy <- demy[ind == unind[i]]
        if(trend) tdemX <- cbind(tdemX, 1:(dim(tdemX)[[1]]))
        tfit <- lm.fit(tdemX, tdemy)
        tcoef[,i] <- tfit$coefficients
        tres[[i]] <- tfit$residuals
      }
      ## 'trend' always comes last
      if(trend) coef.names <- c(coef.names, "trend")
      ## adjust k
      k <- length(coef.names)
  })


    ## coefs are averages across individual regressions
    coef <- rowMeans(tcoef) # == apply(tcoef, 1, mean)

    ## make matrix of cross-products of demeaned individual coefficients

    coefmat <- array(dim=c(k, k, n))
    demcoef <- tcoef - coef # gets recycled n times by column

    for(i in 1:n) coefmat[,,i] <- outer(demcoef[,i], demcoef[,i])
    ## summing over the n-dimension of the array we get the
    ## covariance matrix of coefs
    vcov <- apply(coefmat, 1:2, sum)/(n*(n-1))

    ######### na.omit=T in apply was the big problem!!

    ## code as in pggls, only difference is here there is no 'sigma'
    residuals <- unlist(tres)
    ##was: as.vector(y) - as.vector(crossprod(t(X), coef[1:(dim(X)[[2]])]))
    df.residual <- nrow(X) - ncol(X)
    fitted.values <- y - residuals

    ## R2 as 1-var(res)/var(y);
    ## originally (HPY 3.14) adjusted by *(T.-1)/(T.-2*k0-2)
    ## but here k has expanded to include ybar, Xbar, (trend)
    r2 <- 1-var(residuals)/var(y)*(T.-1)/(T.-k-1)

    names(coef) <- rownames(vcov) <- colnames(vcov) <- coef.names
    dimnames(tcoef) <- list(coef.names, id.names)
    pmodel <- attr(plm.model, "pmodel")
    pmodel$model.name <- model.name
    mgmod <- list(coefficients = coef, residuals = residuals,
                  fitted.values = fitted.values, vcov = vcov,
                  df.residual = df.residual, r.squared = r2,
                  model = model.frame(plm.model), sigma = NULL,
                  indcoef = tcoef, call = cl)
    mgmod <- structure(mgmod, pdim = pdim, pmodel = pmodel)
    class(mgmod) <- c("pmg", "panelmodel")
    mgmod
}


summary.pmg <- function(object,...){
  pmodel <- attr(object,"pmodel")
  std.err <- sqrt(diag(object$vcov))
  b <- object$coefficients
  z <- b/std.err
  p <- 2*pnorm(abs(z), lower.tail = FALSE)
  CoefTable <- cbind(b, std.err, z, p)
  colnames(CoefTable) <- c("Estimate", "Std. Error", "z-value", "Pr(>|z|)")
  object$CoefTable <- CoefTable
  y <- object$model[[1]]
  object$tss <- tss(y)
  object$ssr <- sum(residuals(object)^2)
  object$rsqr <- 1-object$ssr/object$tss
  class(object) <- c("summary.pmg")
  return(object)
}

print.summary.pmg <- function(x, digits = max(3, getOption("digits") - 2), width = getOption("width"),...){
  pmodel <- attr(x, "pmodel")
  pdim <- attr(x, "pdim")
  effect <- pmodel$effect
  formula <- pmodel$formula
  model.name <- pmodel$model.name
#  cat(paste(effect.pggls.list[effect]," ",sep=""))
#  cat(paste(model.pggls.list[model.name],"\n",sep=""))
  cat("Mean Groups model")
  cat("\nCall:\n")
  print(x$call)
  cat("\n")
  print(pdim)
  cat("\nResiduals:\n")
  print(summary(unlist(residuals(x))))
  cat("\nCoefficients:\n")
  printCoefmat(x$CoefTable,digits=digits)
  cat(paste("Total Sum of Squares: ", signif(x$tss,digits),"\n",sep=""))
  cat(paste("Residual Sum of Squares: ", signif(x$ssr,digits),"\n",sep=""))
  cat(paste("Multiple R-squared: ", signif(x$rsqr,digits),"\n",sep=""))
  invisible(x)
}

residuals.pmg <- function(object, ...) {
    return(pres(object))
}
