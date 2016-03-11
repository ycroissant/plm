# Note: within_intercept is related to fixef.plm


# TODO: vcov as an argument
# TODO: time, twoways?
within_intercept.plm <- function(x, ...) { 
  
  if(!inherits(x, "plm")) stop("input 'x' needs to be a \"within\" model estimated by plm()")
  model  <- describe(x, what = "model")
  effect <- describe(x, what = "effect")
  if(model != "within") stop("input 'x' needs to be a \"within\" model estimated by plm(..., model = \"within\", ...)")
  if(effect != "individual") stop("currently only for within models with 'effect = \"individual\"'")
  index <- index(x)
  
  # Transformation needed to get the overall intercept is:
  # demean groupwise and add back grand mean of each variable
  
  withinY <- pmodel.response(x)
  meanY <- mean(pmodel.response(x, model = "pooling")) # on original data
  transY <- withinY + meanY
  
  withinM <- model.matrix(x)
  M <- model.matrix(x, model = "pooling") # on original data
  M <- M[, colnames(M) %in% colnames(withinM)]
  meansM <- colMeans(M)
  transM <- t(t(withinM) + meansM)
  
  # estimation by lm()
    # data <- data.frame(cbind(transY, transM))
    # auxreg <- lm(data)
    # summary(auxreg)

  # estimation by plm()  
  data <- pdata.frame(data.frame(cbind(index, transY, transM)), drop.index = TRUE)
  form <- as.formula(paste0(names(data)[1], "~", paste(names(data)[-1], collapse = "+")))
  auxreg <- plm(form, data = data, model = "pooling")
  
  ## degrees of freedom correction due to FE transformation for vcov [copied over from plm.fit]
  pdim <- pdim(index)
  card.fixef <- switch(effect,
                          "individual" = pdim$nT$n,
                          "time"       = pdim$nT$T,
                          "twoways"    = pdim$nT$n + pdim$nT$T - 1
                          )
  df <- df.residual(auxreg) - card.fixef  + 1 # just for within_intercept: here we need '+1' to correct for the intercept
  vcov <- vcov(auxreg)
  vcov <- vcov * df.residual(auxreg) / df
  auxreg$vcov <- vcov

  s <- summary(auxreg) # TODO: vcov as an argument
  
  intercept <- s$coefficients["(Intercept)", 1] # estimate
  names(intercept) <- "(overall_intercept)"
  attr(intercept, which = "se") <- s$coefficients["(Intercept)", 2] # standard error as attribute
  
  return(intercept)
} # END within_intercept.plm


# generic
within_intercept <- function(x, ...) {
  UseMethod("within_intercept")
}
