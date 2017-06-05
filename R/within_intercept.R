# Note: return value of within_intercept is related to return values of fixef.plm,
#       see tests/test_within_intercept.R

within_intercept.plm <- function(object, vcov = NULL, ...) {
  
  if (!inherits(object, "plm")) stop("input 'object' needs to be a \"within\" model estimated by plm()")
  model  <- describe(object, what = "model")
  effect <- describe(object, what = "effect")
  if (model != "within") stop("input 'object' needs to be a \"within\" model estimated by plm(..., model = \"within\", ...)")
  
  # vcov must be a function, because the auxiliary model estimated to get the
  # overall intercept next to its standard errors is different from
  # the FE model for which the intercept is estimated, e.g. dimensions
  # of vcov differ for FE and for auxiliary model.
  if (!is.null(vcov)) {
    if (is.matrix(vcov)) stop("for within_intercept, 'vcov' may not be of class 'matrix', it must be supplied as a function, e.g. vcov = function(x) vcovHC(x)")
    if (!is.function(vcov)) stop("for within_intercept, argument 'vcov' must be a function, e.g. vcov = function(x) vcovHC(x)")
  }
  
  index <- attr(object$model, which = "index")
  
  # Transformation to get the overall intercept is:
  # demean groupwise and add back grand mean of each variable, then run OLS
  mf      <- model.frame(object)
  withinY <- pmodel.response(object) # returns the response specific to the 'effect' of the est. FE model object
  meanY   <- mean(mf[ , 1])          # mean of original data's response
  transY  <- withinY + meanY
  
  withinM <- model.matrix(object) # returns the model.matrix specific to the 'effect' of the est. FE model object
  M <- model.matrix(pFormula(object$formula), data = mf) # model.matrix of original data
  M <- M[, colnames(M) %in% colnames(withinM)]           # just to be sure: should be same columns
  meansM <- colMeans(M)
  transM <- t(t(withinM) + meansM)
  
  # estimation by lm()
    # data <- data.frame(cbind(transY, transM))
    # auxreg <- lm(data)
    # summary(auxreg)

  # estimation by plm() - to apply robust vcov function if supplied
  data <- pdata.frame(data.frame(cbind(index, transY, transM)), drop.index = TRUE)
  form <- as.formula(paste0(names(data)[1], "~", paste(names(data)[-1], collapse = "+")))
  auxreg <- plm(form, data = data, model = "pooling")
  
  ## Two cases:
  ##  (1) in case of "normal" vcov, we need to adjust the vcov by the corrected degrees of freedom
  ##  (2) in case of robust vcov, which is supplied by a function, no adjustment to the robust vcov is necessary
  if (!is.function(vcov)) {
    # (1) degrees of freedom correction due to FE transformation for "normal" vcov [copied over from plm.fit]
    pdim <- pdim(index)
    card.fixef <- switch(effect,
                            "individual" = pdim$nT$n,
                            "time"       = pdim$nT$T,
                            "twoways"    = pdim$nT$n + pdim$nT$T - 1)
    
    df <- df.residual(auxreg) - card.fixef  + 1 # just for within_intercept: here we need '+1' to correct for the intercept
    vcov_new <- vcov(auxreg)
    vcov_new <- vcov_new * df.residual(auxreg) / df
  } else {
    # (2) robust vcov estimated by function supplied in vcov
    vcov_new <- vcov(auxreg)
  }
  
  auxreg$vcov <- vcov_new # plug in new vcov (adjusted "normal" vcov or robust vcov) in auxiliary model
  
  coef_se <- lmtest::coeftest(auxreg)
  intercept <- coef_se[1,1]
  attr(intercept, which = "se") <- coef_se[1,2]
  names(intercept) <- "(overall_intercept)"
  
  return(intercept)
} # END within_intercept.plm


# generic
within_intercept <- function(object, ...) {
  UseMethod("within_intercept")
}
