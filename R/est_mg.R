  ## Mean Group estimator
  ## ref. Coakley, Fuertes and Smith 2004
  ##
  ## This version 11:
  ##   uses optimised data split approach (via split())
  ## version 10:
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
#' heterogeneous coefficients by various Mean Groups estimators. Setting
#' argument `model = "mg"` specifies the standard Mean Groups estimator, based on the
#' average of individual time series regressions. If `model = "dmg"`
#' the data are demeaned cross-sectionally, which is believed to
#' reduce the influence of common factors (and is akin to what is done
#' in homogeneous panels when `model = "within"` and `effect = "time"`).
#' Lastly, if `model = "cmg"` the CCEMG estimator is
#' employed which is consistent under the hypothesis of
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
#' @param model one of `"mg"`, `"cmg"`, or `"dmg"`,
#' @param index the indexes, see [pdata.frame()],
#' @param trend logical specifying whether an individual-specific
#'     trend has to be included,
#' @param digits digits,
#' @param width the maximum length of the lines in the print output,
#' @param \dots further arguments.
#' 
#' @return An object of class `c("pmg", "panelmodel")` containing:
#'     \item{coefficients}{the vector of coefficients,}
#'     \item{residuals}{the vector of residuals,}
#'     \item{fitted.values}{the vector of fitted values,}
#'     \item{vcov}{the covariance matrix of the coefficients,}
#'     \item{df.residual}{degrees of freedom of the residuals,}
#'     \item{model}{a data.frame containing the variables used for the
#'                  estimation,}
#'     \item{r.squared}{numeric, the R squared,}
#'     \item{call}{the call,}
#'     \item{indcoef}{the matrix of individual coefficients from
#'                    separate time series regressions.}
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
                trend = FALSE, ...) {

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
    mf <- model.frame(plm.model)
    index <- unclass(attr(mf, "index")) # unclass for speed
    ind  <- index[[1L]] ## individual index
    ind.GRP <- collapse::GRP(ind) ## individual index as collapse-optimised GRP
    tind <- index[[2L]] ## time index
    ## set dimension variables
    pdim <- pdim(plm.model)
    Ti <- pdim$Tint$Ti
    T. <- pdim$nT$T
    n <- pdim$nT$n
    ## set index names
    time.names <- pdim$panel.names$time.names
    id.names   <- pdim$panel.names$id.names
    coef.names <- names(coef(plm.model))
    ## number of coefficients
    k <- length(coef.names)

    ## model data
    X <- model.matrix(plm.model)
    y <- model.response(mf)


  ## det. *minimum* group numerosity
  t <- min(Ti)

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

  ## "pre-allocate" coefficients matrix for the n models and list for residuals
  kt <- if(trend) 1L else 0L
  tcoef <- if(model == "cmg") {
         matrix(data = NA_real_, nrow = 2*k+kt, ncol = n) ## allow for extended coef vector
  } else matrix(data = NA_real_, nrow =   k+kt, ncol = n)
  tres <- vector("list", n)

  switch(model,
    "mg" = {
      # split X, y by individual and store in lists
      tX.list <- collapse::rsplit(X, ind.GRP, use.names = FALSE)
      ty.list <- collapse::gsplit(y, ind.GRP)
      
      ## for each x-sect. i = 1..n
      for(i in seq_len(n)) {
        tX <- tX.list[[i]]
        ty <- ty.list[[i]]
        if(trend) tX <- cbind(tX, seq_len(dim(tX)[[1L]]))
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
      Xm <- Between(X, effect = "time", na.rm = TRUE)
      ym <- as.numeric(Between(y, effect = "time", na.rm = TRUE))
      
      augX <- cbind(X, ym, Xm[ , -1L, drop = FALSE])

      taugX.list <- collapse::rsplit(augX, ind.GRP, use.names = FALSE)
      
      ty.list <- collapse::gsplit(y, ind.GRP)
      
      ## for each x-sect. i = 1..n estimate (over t) an augmented model
      ## y_it = alpha_i + beta_i*X_it + c1_i*my_t + c2_i*mX_t + err_it
      for(i in seq_len(n)) {
        taugX <- taugX.list[[i]]
        ty    <-    ty.list[[i]]
        if(trend) taugX <- cbind(taugX, seq_len(dim(taugX)[[1L]]))
        tfit <- lm.fit(taugX, ty)
        tcoef[ , i] <- tfit$coefficients
        tres[[i]]   <- tfit$residuals
      }

      ## add names of coefs for augmented x-sectional averages
      coef.names.bar <- c("y.bar", paste(coef.names[-1L], ".bar", sep=""))
      coef.names <- c(coef.names, coef.names.bar)
      ## 'trend' always comes last
      if(trend) coef.names <- c(coef.names, "trend")
      ## adjust k
      k <- length(coef.names)

      ## TODO: adjust model formula etc. (else breaks waldtest, update, ...)
      },
    
    "dmg" = {
      ##  time-demean
      demX <- Within(X, effect = "time", na.rm = TRUE)
      demX[ , 1L] <- 1 # put back intercept lost by within transformation
      demy <- as.numeric(Within(y, effect = "time", na.rm = TRUE))

      tdemX.list <- collapse::rsplit(demX, ind.GRP, use.names = FALSE)
      
      tdemy.list <- collapse::gsplit(demy, ind.GRP)
            
      ## for each x-sect. i=1..n estimate (over t) a demeaned model
      ## (y_it-my_t) = alpha_i + beta_i*(X_it-mX_t) + err_it
      for (i in seq_len(n)) {
        tdemX <- tdemX.list[[i]]
        tdemy <- tdemy.list[[i]]
        if(trend) tdemX <- cbind(tdemX, seq_len(dim(tdemX)[[1L]]))
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

    for (i in seq_len(n)) coefmat[ , , i] <- outer(demcoef[ , i], demcoef[ , i])
    ## summing over the n-dimension of the array we get the
    ## covariance matrix of coefs
    vcov <- rowSums(coefmat, dims = 2L) / (n*(n-1)) # == apply(coefmat, 1:2, sum) / (n*(n-1)) but rowSums(., dims = 2L)-construct is way faster

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
    pmodel <- list(model.name = model.name)
    mgmod <- list(coefficients  = coef,
                  residuals     = residuals,
                  fitted.values = fitted.values,
                  vcov          = vcov,
                  df.residual   = df.residual,
                  r.squared     = r2,
                  model         = mf,
                  indcoef       = tcoef,
                  formula       = formula,
                  call          = cl)
    mgmod <- structure(mgmod, pdim = pdim, pmodel = pmodel)
    class(mgmod) <- c("pmg", "panelmodel")
    mgmod
}

#' @rdname pmg
#' @export
summary.pmg <- function(object, ...){
  std.err <- sqrt(diag(object$vcov))
  b <- object$coefficients
  z <- b/std.err
  p <- 2*pnorm(abs(z), lower.tail = FALSE)
  CoefTable <- cbind(b, std.err, z, p)
  colnames(CoefTable) <- c("Estimate", "Std. Error", "z-value", "Pr(>|z|)")
  object$CoefTable <- CoefTable
  y <- object$model[[1L]]
  object$tss <- tss(y)
  object$ssr <- as.numeric(crossprod(residuals(object)))
  object$rsqr <- 1-object$ssr/object$tss
  class(object) <- c("summary.pmg")
  return(object)
}

#' @rdname pmg
#' @export
print.summary.pmg <- function(x, digits = max(3, getOption("digits") - 2),
                              width = getOption("width"), ...){
  pmodel <- attr(x, "pmodel")
  pdim   <- attr(x, "pdim")
  cat(paste(model.pmg.list[pmodel$model.name], "\n", sep=""))
  cat("\nCall:\n")
  print(x$call)
  cat("\n")
  print(pdim)
  cat("\nResiduals:\n")
  print(sumres(x))
  cat("\nCoefficients:\n")
  printCoefmat(x$CoefTable, digits = digits)
  cat(paste("Total Sum of Squares: ",    signif(x$tss,  digits), "\n", sep=""))
  cat(paste("Residual Sum of Squares: ", signif(x$ssr,  digits), "\n", sep=""))
  cat(paste("Multiple R-squared: ",      signif(x$rsqr, digits), "\n", sep=""))
  invisible(x)
}

#' @rdname pmg
#' @export
residuals.pmg <- function(object, ...) {
  pres(object)
}
