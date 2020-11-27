  ## Mean Group estimator
  ## ref. Coakley, Fuertes and Smith 2004
  ##
  ## This version 10:
  ##   added R2 = 1-var(resid)/var(y) as a measure of fit
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




#' Mean Groups (MG), Demeaned MG and CCE MG estimators
#' 
#' Mean Groups (MG), Demeaned MG (DMG) and Common Correlated Effects
#' MG (CCEMG) estimators for heterogeneous panel models, possibly with
#' common factors (CCEMG)
#' 
#' `pmg` is a function for the estimation of linear panel models with
#' heterogeneous coefficients by the Mean Groups estimator. `model =
#' "mg"` specifies the standard Mean Groups estimator, based on the
#' average of individual time series regressions. If `model = "dmg"`
#' the data are demeaned cross-sectionally, which is believed to
#' reduce the influence of common factors (and is akin to what is done
#' in homogeneous panels when `model = "within"` and `effect =
#' "time"`). Lastly, if `model = "cmg"` the CCEMG estimator is
#' employed: this latter is consistent under the hypothesis of
#' unobserved common factors and idiosyncratic factor loadings; it
#' works by augmenting the model by cross-sectional averages of the
#' dependent variable and regressors in order to account for the
#' common factors, and adding individual intercepts and possibly
#' trends.
#' 
#' @aliases pmg
#' @param formula a symbolic description of the model to be estimated,
#' @param object,x an object of class `pmg`,
#' @param data a `data.frame`,
#' @param subset see [lm()],
#' @param na.action see [lm()],
#' @param model one of `c("mg", "cmg", "dmg")`,
#' @param index the indexes, see [pdata.frame()],
#' @param trend logical specifying whether an individual-specific
#'     trend has to be included,
#' @param digits digits,
#' @param width the maximum length of the lines in the print output,
#' @param \dots further arguments.
#' @return An object of class `c("pmg", "panelmodel")` containing:
#'     \item{coefficients}{the vector of coefficients,}
#'     \item{residuals}{the vector of residuals,}
#'     \item{fitted.values}{the vector of fitted values,}
#'     \item{vcov}{the covariance matrix of the coefficients,}
#'     \item{df.residual}{degrees of freedom of the residuals,}
#'     \item{model}{a data.frame containing the variables used for the
#'     estimation,} \item{call}{the call,} \item{sigma}{always `NULL`,
#'     `sigma` is here only for compatibility reasons (to allow using
#'     the same `summary` and `print` methods as `pggls`),}
#'     \item{indcoef}{the matrix of individual coefficients from
#'     separate time series regressions.}
#' @export
#' @author Giovanni Millo
#' @references
#'
#' \insertRef{PESA:06}{plm}
#'
#' @keywords regression
#' @examples
#' data("Produc", package = "plm")
#' ## Mean Groups estimator
#' mgmod <- pmg(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc)
#' summary(mgmod)
#' 
#' ## demeaned Mean Groups
#' dmgmod <- pmg(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, 
#'              data = Produc, model = "dmg")
#' summary(dmgmod)
#' 
#' ## Common Correlated Effects Mean Groups
#' ccemgmod <- pmg(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, 
#'                 data = Produc, model = "cmg")
#' summary(ccemgmod) 
pmg <- function(formula, data, subset, na.action,
                model = c("mg", "cmg", "dmg"), index = NULL,
                trend = FALSE, ...)
{

    ## same as pggls but for effect, fixed at "individual" for compatibility
    ## ind for id, tind for time, k for K, coefnam for coef.names
    effect <- "individual"

    ## record call etc.
    model <- match.arg(model)
    model.name <- model
    data.name <- paste(deparse(substitute(data)))
    cl <- match.call()
    plm.model <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "na.action", "effect",
        "model", "index"), names(plm.model), 0)
    plm.model <- plm.model[c(1L, m)]
    plm.model[[1L]] <- as.name("plm")
    ## change the 'model' in call
    plm.model$model <- "pooling"
    ## evaluates the call, modified with model = "pooling", inside the
    ## parent frame resulting in the pooling model on formula, data
    plm.model <- eval(plm.model, parent.frame())
    index <- attr(model.frame(plm.model), "index")
    ind  <- index[[1L]] ## individual index
    tind <- index[[2L]] ## time index
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
  t <- min(Ti) # == min(tapply(X[ , 1], ind, length))

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
  kt <- if (trend) 1L else 0L
  tcoef <- matrix(data = NA_real_, nrow = k+kt, ncol = n)
  tres <- vector("list", n)

  switch(model,
    "mg" = {
      ## for each x-sect. i = 1..n
      unind <- unique(ind)
      for(i in 1:n) {
        tX <- X[ind == unind[i], ]
        ty <- y[ind == unind[i]]
        if(trend) tX <- cbind(tX, 1:(dim(tX)[[1L]]))
        tfit <- lm.fit(tX, ty)
        tcoef[ , i] <- tfit$coefficients
        tres[[i]]   <- tfit$residuals
      }
      ## 'trend' always comes last
      if(trend) coef.names <- c(coef.names, "trend")
      ## adjust k
      k <- length(coef.names)
      },
    
    "cmg" = {
      ## between-periods transformation (take means over groups for each t)
         #  be <- function(x, index, na.rm = TRUE) tapply(x, index, mean, na.rm = na.rm)
         #  Xm <- apply(X, 2, FUN = be, index = tind)[tind, , drop = FALSE]
         #  ym <- apply(as.matrix(as.numeric(y)), 2, FUN = be, index = tind)[tind]
      Xm <- Between(X, effect = "time", na.rm = TRUE)
      ym <- as.numeric(Between(y, effect = "time", na.rm = TRUE))
      
      augX <- cbind(X, ym, Xm[ , -1])

      ## allow for extended coef vector
      tcoef0 <- matrix(data = NA_real_, nrow = 2*k+kt, ncol = n)

      ## for each x-sect. i = 1..n estimate (over t) an augmented model
      ## y_it = alpha_i + beta_i*X_it + c1_i*my_t + c2_i*mX_t + err_it
      unind <- unique(ind)
      for(i in 1:n) {
        taugX <- augX[ind == unind[i], ]
        ty <- y[ind == unind[i]]

        if(trend) taugX <- cbind(taugX, 1:(dim(taugX)[[1L]]))

        tfit <- lm.fit(taugX, ty)
        tcoef0[ , i] <- tfit$coefficients
        tres[[i]]    <- tfit$residuals
      }
      tcoef <- tcoef0[1:k, ]
      tcoef.bar <- tcoef0[-(1:k), ]

      coef.names.bar <- c("y.bar", paste(coef.names[-1L], ".bar", sep=""))

      ## 'trend' always comes last
      if(trend) coef.names.bar <- c(coef.names.bar, "trend")

      ## output complete coefs
      tcoef <- tcoef0
      coef.names <- c(coef.names, coef.names.bar)
      ## adjust k
      k <- length(coef.names)

      ## TODO: adjust model formula etc. (else breaks waldtest, update, ...)
      },
    
    "dmg" = {
      ## old: between-periods transformation (take means over group for each t)
         ##  be <- function(x, index, na.rm = TRUE) tapply(x, index, mean, na.rm = na.rm)
         ##  Xm <- apply(X, 2, FUN = be, index = tind)[tind, , drop = FALSE]
         ##  ym <- apply(as.matrix(as.numeric(y)), 2, FUN = be, index = tind)[tind]
          # Xm <- Between(X, effect = "time", na.rm = TRUE)
          # ym <- as.numeric(Between(y, effect = "time", na.rm = TRUE))
          ## ...but of course we do not demean the intercept!
          # Xm[ , 1] <- 0
          # demX <- X - Xm
          # demy <- y - ym
      
      demX <- Within(X, effect = "time", na.rm = TRUE)
      demX[ , 1] <- 1 # put back intercept lost by within transformation
      demy <- as.numeric(Within(y, effect = "time", na.rm = TRUE))
      
      ## for each x-sect. i=1..n estimate (over t) a demeaned model
      ## (y_it-my_t) = alpha_i + beta_i*(X_it-mX_t) + err_it
      unind <- unique(ind)
      for (i in 1:n) {
        tdemX <- demX[ind == unind[i], ]
        tdemy <- demy[ind == unind[i]]
        if(trend) tdemX <- cbind(tdemX, 1:(dim(tdemX)[[1L]]))
        tfit <- lm.fit(tdemX, tdemy)
        tcoef[ , i] <- tfit$coefficients
        tres[[i]]   <- tfit$residuals
      }
      ## 'trend' always comes last
      if(trend) coef.names <- c(coef.names, "trend")
      ## adjust k
      k <- length(coef.names)
  })

    ## coefs are averages across individual regressions
    coef <- rowMeans(tcoef) # == apply(tcoef, 1, mean)

    ## make matrix of cross-products of demeaned individual coefficients
    coefmat <- array(data = NA_real_, dim = c(k, k, n))
    demcoef <- tcoef - coef # gets recycled n times by column

    for (i in 1:n) coefmat[ , , i] <- outer(demcoef[ , i], demcoef[ , i])
    ## summing over the n-dimension of the array we get the
    ## covariance matrix of coefs
    vcov <- apply(coefmat, 1:2, sum)/(n*(n-1))

    ######### na.omit = T in apply was the big problem!!

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
                  indcoef = tcoef, formula = formula,
                  call = cl)
    mgmod <- structure(mgmod, pdim = pdim, pmodel = pmodel)
    class(mgmod) <- c("pmg", "panelmodel")
    mgmod
}

#' @rdname pmg
#' @export
summary.pmg <- function(object, ...){
  pmodel <- attr(object, "pmodel")
  std.err <- sqrt(diag(object$vcov))
  b <- object$coefficients
  z <- b/std.err
  p <- 2*pnorm(abs(z), lower.tail = FALSE)
  CoefTable <- cbind(b, std.err, z, p)
  colnames(CoefTable) <- c("Estimate", "Std. Error", "z-value", "Pr(>|z|)")
  object$CoefTable <- CoefTable
  y <- object$model[[1L]]
  object$tss <- tss(y)
  object$ssr <- sum(residuals(object)^2)
  object$rsqr <- 1-object$ssr/object$tss
  class(object) <- c("summary.pmg")
  return(object)
}

#' @rdname pmg
#' @export
print.summary.pmg <- function(x, digits = max(3, getOption("digits") - 2), width = getOption("width"), ...){
  pmodel <- attr(x, "pmodel")
  pdim <- attr(x, "pdim")
#  formula <- pmodel$formula
  model.name <- pmodel$model.name
  cat(paste(model.pmg.list[model.name], "\n", sep=""))
  cat("\nCall:\n")
  print(x$call)
  cat("\n")
  print(pdim)
  cat("\nResiduals:\n")
  print(summary(unlist(residuals(x))))
  cat("\nCoefficients:\n")
  printCoefmat(x$CoefTable, digits = digits)
  cat(paste("Total Sum of Squares: ",    signif(x$tss, digits), "\n", sep=""))
  cat(paste("Residual Sum of Squares: ", signif(x$ssr, digits), "\n", sep=""))
  cat(paste("Multiple R-squared: ",      signif(x$rsqr, digits),"\n", sep=""))
  invisible(x)
}

#' @rdname pmg
#' @export
residuals.pmg <- function(object, ...) {
  return(pres(object))
}
