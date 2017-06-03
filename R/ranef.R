## calculate random effects from estimated model object
## (like fixef() for FE models)

# TODO:
#       * balanced two-way case
#       * unbalanced two-way case
#       * -> introduce argument 'effect' (see fixef how to best handle default value etc.)
#            (allowing to extract the effect not in the model seems overengineering for a first step)


## generic for ranef supplied by importing nlme::ranef via NAMESPACE

ranef.plm <- function(object, ...) {
  model <- describe(object, "model")
  effect <- describe(object, "effect")
  balanced <- is.pbalanced(object)
  
  if (model != "random") stop("only applicable to random effect models")
  if (effect == "twoways") stop("only for one-way models (individual or time)")
  
  # TODO: Are random effects for nested models and IV models calculated the same way?
  #       Be defensive here and error for such models.
  if (effect == "nested")  stop("nested random effect models are not supported")
  if (length(object$formula)[2] == 2) stop("IV models not supported")
  
  erc <- ercomp(object)
  theta <- unlist(erc["theta"])
  
  # res <- x$residuals                     # gives residuals of quasi-demeaned model
  res <- residuals_overall_exp.plm(object) # but need RE residuals of overall model
  
  if (!inherits(res, "pseries")) {
    # in development version 1.6-6, residuals() do not seem to return pseries anymore..?
    # residuals_overall_exp.plm() still returns pseries, but just make sure we have a pseries
    attr(res, "index") <- index(object$model)
    class(res) <- c("pseries", class(res))
  }
   
  # mean_res <- Between(res, effect = effect)  # has length = # observations
  mean_res <- between(res, effect = effect)    # need length = # individuals
  
  if (!balanced) {
    # in the (one-way) unbalanced case, ercomp$theta is full length (# obs)
    #  -> reduce to per id
    select <- switch(effect,
                     "individual" = !duplicated(index(object$model)[1]),
                     "time"       = !duplicated(index(object$model)[2]))
    theta <- theta[select]
  }
  
  # calculate random effects:
  # This formula works (at least) for:
  #  balanced one-way (is symmetric for individual/time)
  #  unbalanced one-way (symmetric) is also caught by this line as theta is reduced before
  raneffects <- (1 - (1 - theta)^2) * mean_res
  names(raneffects) <- names(mean_res)
  return(raneffects)
}
