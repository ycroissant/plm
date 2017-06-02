## calculate random effects from estimated model object
## (like fixef())

# TODO:
#       * unbalanced one-way case
#       * balanced two-way case
#       * documentation
#       * export
#       + unbalanced two-way case

ranef.plm <- function(x) {
  ## function is ok for one-way balanced cases (individual, time)
  
  model <- describe(x, "model")
  effect <- describe(x, "effect")
  
  balanced <- is.pbalanced(x)
  if (!balanced) stop("only implemented for balanced")
  
  if (model != "random") stop("only applicable to random effect models")
  if (effect == "twoways") stop("only for one way models")
  
  erc <- ercomp(x)
  theta <- unlist(erc["theta"])
  
  # res <- x$residuals # gives residuals of quasi-demeaned model
  res <- residuals_overall_exp.plm(x) # but need RE residuals of overall model
  
  if (!inherits(res, "pseries")) {
    # in development version 1.6-6, residuals() do not seem to return pseries anymore..?
    # residuals_overall_exp.plm() still returns pseries, but just make sure we have a pseries
    attr(res, "index") <- index(x$model)
    class(res) <- c("pseries", class(res))
  }
  
  # mean_res <- Between(res, effect = effect) # gives length = # obs
  mean_res <- between(res, effect = effect)  # gives length = # individuals
  
  raneffects <- (1 - (1 - theta)^2) * mean_res # balanced one-way (is symmetric for individual/time)
  
  return(raneffects)
}
