## calculate random effects from estimated model object
## (like fixef() for FE models)

# TODO:
#      Check if the same procedure can be applied to
#       * unbalanced two-way case (for now: implemented the same way, but not entirely sure)
#       * random IV models
#       * nested random effect models


## generic for ranef supplied by importing nlme::ranef via NAMESPACE

ranef.plm <- function(object, effect = NULL, ...) {
  model <- describe(object, "model")
  obj.effect <- describe(object, "effect")
  balanced <- is.pbalanced(object)
  
  if (model != "random") stop("only applicable to random effect models")
  # TODO: Are random effects for nested models and IV models calculated the same way?
  #       Be defensive here and error for such models.
  if (obj.effect == "nested")  stop("nested random effect models are not supported (yet?)")
  if (length(object$formula)[2] == 2) stop("IV models not supported (yet?)")
  
  if (!is.null(effect) && !(effect %in% c("individual", "time"))) 
    stop("argument 'effect' must be NULL, \"individual\", or \"time\"")
  if (obj.effect != "twoways" && !is.null(effect) && effect != obj.effect) 
    stop(paste0("for one-way models, argument \"effect\" must be NULL or match the effect introduced in model estimation"))

  # default effect is the model's effect
  # for two-ways RE models: set default to effect = "individual"
  if (obj.effect == "twoways" && is.null(effect)) effect <- "individual"
  if (is.null(effect)) effect <- obj.effect
  
  erc <- ercomp(object)
  theta <- unlist(erc["theta"]) # extract theta, but depending on model/effect, it is adjusted later
  
  # res <- object$residuals                # gives residuals of quasi-demeaned model
  res <- residuals_overall_exp.plm(object) # but need RE residuals of overall model
  
  if (!inherits(res, "pseries")) {
    # just make sure we have a pseries for the following between() to work
    attr(res, "index") <- index(object$model)
    class(res) <- c("pseries", class(res))
  }
   
  # mean_res <- Between(res, effect = effect)  # has length == # observations
  mean_res <- between(res, effect = effect)    # but need length == # individuals
  
  if (obj.effect == "twoways" && balanced) {
    theta <- switch(effect,
                    "individual" = theta[1],
                    "time"       = theta[2])
  }
  if (obj.effect == "twoways" && !balanced) {
    theta <- erc[["theta"]][[ifelse(effect == "individual", "id", "time")]]
  }
  
  if (!balanced) {
    # in the unbalanced cases, ercomp[["theta"]] is full length (# obs)
    #  -> reduce to per id/time
    select <- switch(effect,
                     "individual" = !duplicated(index(object$model)[1]),
                     "time"       = !duplicated(index(object$model)[2]))
    theta <- theta[select]
  }
  
  # calculate random effects:
  # This formula works (at least) for:
  #  balanced one-way (is symmetric for individual/time)
  #  unbalanced one-way (symmetric) is also caught by this line as theta is reduced before
  #  balanced two-way case (symmetric)
  raneffects <- (1 - (1 - theta)^2) * mean_res
  names(raneffects) <- names(mean_res)
  return(raneffects)
}
