

ranef.plm <- function(x) {
  # x being a plm object, random effect model
  model <- describe(x, "model")
  effect <- describe(x, "effect")
  
  balanced <- is.pbalanced(x)
  if (!balanced) stop("only implemented for balanced")
  if (model != "random") stop("only applicable to random effect models")
  if (effect == "twoways") stop("only for one way models")
  if (effect != "individual") stop("only for individual models")
  
  erc <- ercomp(x)
  theta <- unlist(erc["theta"])
  
  #res <- x$residuals
  res <- residuals_overall_exp.plm(x)
  
  attr(res, "index") <- index(x$model)
  class(res) <- c("pseries", class(res))
  
  # mean_res <- plm:::Tapply(res, effect = "individual", mean) # gives length(obs)
  mean_res <- between(res) # length = # individuals
  
  raneffects <- (1 - (1 - theta)^2) * mean_res
  
  return(raneffects)
}


## 
# data("Grunfeld", package = "plm")
# 
# swar <- plm(inv ~ value + capital,
#             data = Grunfeld, model = "random", effect = "individual")
# 
# ranef.plm(swar)
