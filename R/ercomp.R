### ercomp(formula, data, random.method, effect)

ercomp <- function(object, ...){
  UseMethod("ercomp")
}

ercomp.plm <- function(object, ...){
  model <- describe(object, "model")
  if (model != "random") stop("ercomp only relevant for random models")
  object$ercomp
}

ercomp.formula <- function(object, data, 
                           effect = c('individual', 'time', 'twoways'),
                           method = c('swar', 'walhus', 'amemiya', 'nerlove', 'kinla'),
                           index = NULL, ...){
  
  # if the data argument is not a pdata.frame, create it using plm
  if (!inherits(data, "pdata.frame"))
    data <- plm(object, data, model = NA, index = index)

  # if formula is not a pFormula object, coerce it
  if (!inherits(object, "pFormula")) object <- pFormula(object)
  
  effect <- match.arg(effect)
  method <- match.arg(method)
  balanced <- pdim(data)$balanced
  result <- switch(method,
                   "swar"    = swar    (object, data, effect),
                   "walhus"  = walhus  (object, data, effect),
                   "amemiya" = amemiya (object, data, effect),
                   "nerlove" = nerlove (object, data, effect),
                   "kinla"   = kinla   (object, data, effect)
                   )
  result <- structure(result, class = "ercomp", balanced = balanced, effect = effect)
  result
  
}

swar <- function(formula, data, effect){
#  within <- plm.within(formula, data, effect = effect)
  within <- plm.fit(formula, data, model = "within", effect = effect)
  within$args <- list(effect = effect, random.method = "swar")
  data <- model.frame(within)
  pdim <- pdim(data)
  index <- attr(data, "index")
  balanced <- pdim$balanced
  sigma2 <- list()
  n <- pdim$nT$n
  T <- pdim$nT$T
  N <- pdim$nT$N
  if(effect != "twoways"){
    between <- plm.fit(formula, data, model = "between", effect = effect)
    if (effect == "individual"){
      arg.cond <- n
      arg.other <- T
      arg.cond.i <- pdim$Tint$nt
      arg.other.i <- pdim$Tint$Ti
      cond <- index[[1]]
    }
    else{
      arg.cond <- T
      arg.other <- n
      arg.cond.i <- pdim$Tint$Ti
      arg.other.i <- pdim$Tint$nt
      cond <- index[[2]]
    }
    Kb <- length(coef(between))
    Kw <- length(coef(within))
    if(balanced){
      sigma2$one <- arg.other * deviance(between) / df.residual(between)
      sigma2$idios <- deviance(within) / df.residual(within)
      sigma2$id <- (sigma2$one - sigma2$idios) / arg.other
      if (sigma2$id < 0)
        stop(paste("the estimated variance of the", effect, "effect is negative"))
      theta <- 1 - sqrt(sigma2$idios / sigma2$one)
      z <- list(sigma2 = sigma2, theta = theta)
    }
    else{
      cat("bon\n")
      X <- model.matrix(formula, data, rhs = 1, model = "pooling", effect = effect)
      X.m <- Tapply(X, cond, mean)
      X.sum <- apply(X, 2, tapply, cond, sum)
      X.m.X <- crossprod(X.m)
      X.sum.X <- crossprod(X.sum)
      X.m.X.eig <- eigen(X.m.X)
      if (any(abs(X.m.X.eig$values) < 1E-08)){
        cn <- which(abs(X.m.X.eig$values) < 1E-08)
        C <- X.m.X.eig$vectors[, - cn]
        OM <- diag(X.m.X.eig$values[- cn])
        X.m.Xi <- C %*% solve(OM) %*% t(C)
        X.sum.X <- crossprod(X.sum)
        tr <- sum(diag(X.m.Xi %*% X.sum.X))
      }
      else{
        tr <- sum(diag(solve(crossprod(X.m)) %*% crossprod(X.sum)))
      }
      sigma2$idios <- deviance(within)/(N-arg.cond-Kw)
      ssrbet <- sum(between$residuals^2 * arg.other.i)
      sigma2$id <- (ssrbet - (arg.cond - Kb) * sigma2$idios)/(N - tr)
      if (sigma2$id < 0)
        stop(paste("the estimated variance of the", effect, "effect is negative"))
      sigma2$one <- (arg.other.i * sigma2$id + sigma2$idios)
      theta <- 1-sqrt(sigma2$idios / (sigma2$idios + arg.other.i * sigma2$id))
      theta <- theta[as.character(cond)]
      z <- list(sigma2 = sigma2, theta = theta)
    }
  }
  else{
#    between.id <- plm.between(formula, data, "individual")
#    between.time <- plm.between(formula, data, "time")
    between.id <- plm.fit(formula, data, model = "between", effect = "individual")
    between.time <- plm.fit(formula, data, model = "between", effect = "time")
    if(balanced){
      theta <- list()
      n <- pdim$nT$n
      T <- pdim$nT$T
      sigma2$idios <- deviance(within)/((n-1)*(T-1)-length(coef(within)))
      lambda2 <- T*deviance(between.id)/df.residual(between.id)
      lambda3 <- n*deviance(between.time)/df.residual(between.time)
      lambda4 <- lambda2+lambda3-sigma2$idios
      sigma2$id <- (lambda2-sigma2$idios)/T
      sigma2$time <- (lambda3-sigma2$idios)/n
      if (sigma2$id < 0)
        warning("the estimated variance of the individual effect is negative")
      if (sigma2$time < 0)
        warning("the estimated variance of the time effect is negative")
      theta$id <- 1-sqrt(sigma2$idios/lambda2)
      theta$time <- 1-sqrt(sigma2$idios/lambda3)
      theta$total <- theta$id+theta$time+sqrt(sigma2$idios/lambda4)-1
      if (sigma2$time<0) theta$time <- theta$total <- sigma2$time <- 0
      z <- list(theta = theta, sigma2 = sigma2)
    }
    else{
      stop("twoway random effect model not implemented for unbalanced panels")
    }
  }
  z
}

walhus <- function(formula, data, effect){
#  pooling <- plm.pooling(formula, data)
  pooling <- plm.fit(formula, data, model = "pooling", effect = effect)
  data <- model.frame(pooling)
  index <- attr(data, "index")
  pdim <- pdim(data)
  balanced <- pdim$balanced
  respool <- resid(pooling)
  T <- pdim$nT$T
  n <- pdim$nT$n
  if(!balanced)
    stop("walhus not implemented for unbalanced panels\n")
  if(effect != "twoways"){
    if(effect=="individual"){
      condvar <- index[[1]]
      card.cond <- n
      card.other <- T
    }
    else{
      condvar <- index[[2]]
      card.cond <- T
      card.other <- n
    }
    one   = card.other * sum(tapply(respool, condvar, mean) ^ 2) / card.cond
    idios = sum((respool - tapply(respool, condvar, mean)[as.character(condvar)]) ^ 2) /
      (card.cond * (card.other - 1));
    sigma2 <- list(one = one,
                   idios = idios,
                   id    = (one - idios) / card.other
                   )
    if (sigma2$id < 0)
      stop(paste("the estimated variance of the", effect, "effect is negative"))
    theta <- 1-sqrt(idios/one)
    z <- list(theta = theta, sigma2 = sigma2)
  }
  else{
    index <- attr(data, "index")
    id <- index[[1]]
    time <- index[[2]]
    idios <- sum((respool-tapply(respool,id,mean)[as.character(id)]-
                  tapply(respool,time,mean)[as.character(time)])^2)/((n-1)*(T-1))
    lambda2 <- sum(tapply(respool,id,mean)^2)*T/(n-1)
    lambda3 <- sum(tapply(respool,time,mean)^2)*n/(T-1)
    lambda4 <- lambda2+lambda3-idios
    sigma2 <- list(
                   idios = idios,
                   id = (lambda2-idios)/T,
                   time = (lambda3-idios)/n
                   )
    theta <- list(
                  id = 1-sqrt(idios/lambda2),
                  time = 1-sqrt(idios/lambda3),
                  total = 1-sqrt(idios/lambda2)-sqrt(idios/lambda3)+sqrt(idios/lambda4)
                  )
    if (sigma2$id < 0)
      warning("the estimated variance of the individual effect is negative")
    if (sigma2$time < 0)
      warning("the estimated variance of the time effect is negative")
    if (sigma2$time<0) theta$time <- theta$total <- sigma2$time <- 0
    z <- list(theta=theta,sigma2=sigma2)
  }
  z
}

amemiya <- function(formula, data, effect){
#  within <- plm.within(formula, data, effect = effect)
  within <- plm.fit(formula, data, model = "within", effect = effect)
  within$args <- list(effect = effect, random.method = "amemiya")
  data <- model.frame(within)
  pdim <- pdim(data)
  balanced <- pdim$balanced
  T <- pdim$nT$T
  n <- pdim$nT$n
  K <- length(coef(within))
  index <- attr(data, "index")
  if(!balanced)
    stop("amemiya variance decomposition not implemented for unbalanced panels")
  if(effect != "twoways"){
    if (effect == "individual"){
      condvar <- index[[1]]
      card.cond <- n
      card.other <- T
    }
    else{
      condvar <- index[[2]]
      card.cond <- T
      card.other <- n
    }
    fe <- fixef(within, effect = effect)
    alpha <- mean(fe)
    uest <- resid(within)+fe[as.character(condvar)]-alpha
    # inutile : one <- T / n * sum(fixef(within, type="dmean") ^ 2) est OK 
    one <- card.other/card.cond*sum(tapply(uest,condvar,mean)^2)
    idios <- deviance(within)/(card.cond*(card.other-1)-K)
    sigma2 <- list(one = one,
                   idios = idios,
                   id = max((one-idios)/card.other,0))
    theta <- max(1-sqrt(idios/one),0)
    z <- list(theta = theta, sigma2 = sigma2)
  }
  else{
    X <- model.matrix(formula, data, rhs = 1, model = "pooling")[,-1,drop = FALSE]
    y <- pmodel.response(formula, data, model = "pooling")
    id <- index[[1]]
    time <- index[[2]]
    fe <- fixef(within)
    alpha <- mean(fe)
    uest <- as.vector(y-alpha-X%*%coef(within))
    idios <- sum((uest-tapply(uest,id,mean)[as.character(id)]-
                  tapply(uest,time,mean)[as.character(time)])^2)/((n-1)*(T-1))
    lambda2 <- sum(tapply(uest,id,mean)^2)*T/(n-1)
    lambda3 <- sum(tapply(uest,time,mean)^2)*n/(T-1)
    lambda4 <- lambda2+lambda3-idios
    sigma2 <- list(idios = idios,
                   id = (lambda2-idios)/T,
                   time = (lambda3-idios)/n
                   )
    theta <- list(id = 1-sqrt(idios/lambda2),
                  time = 1-sqrt(idios/lambda3),
                  total = 1-sqrt(idios/lambda2)-sqrt(idios/lambda3)+sqrt(idios/lambda4)
                  )
    if (sigma2$time<0) theta$time <- theta$total <- sigma2$time <- 0
    z <- list(theta = theta, sigma2 = sigma2)
  }
  z
}

nerlove <- function(formula, data, effect){
#  within <- plm.within(formula, data, effect)
  within <- plm.fit(formula, data, model = "within", effect = effect)
  within$args <- list(effect = effect, random.method = "nerlove")
  data <- model.frame(within)
  pdim <- pdim(data)
  balanced <- pdim$balanced
  n <- pdim$nT$n
  N <- pdim$nT$N
  if(effect != "twoways" && balanced){
    N <- pdim$nT$N
    if (effect == "individual"){
      arg.cond <- pdim$nT$n
      arg.other <- pdim$nT$T
    }
    else{
      arg.cond <- pdim$nT$T
      arg.other <- pdim$nT$n
    }
    idios <- deviance(within) / N 
#    s2id <- sum((fixef(within)-mean(fixef(within)))^2)/(arg.cond-1)
    s2id <- sum(fixef(within, type = "dmean") ^ 2)/(arg.cond - 1)
    one <- arg.other * s2id + idios
    sigma2 <- list(one = one,
                   idios = idios,
                   id    = s2id
                   )
    theta <- 1 - sqrt(idios / sigma2$one)
    z <- list(theta = theta, sigma2 = sigma2)
  }
  else stop("nerlove variance decomposition only implemented for balanced oneway panels")
  z
}

kinla <- function(formula, data, effect){
#  within <- plm.within(formula, data, effect)
  within <- plm.fit(formula, data, model = "within", effect = effect)
  within$args <- list(effect = effect, random.method = "kinla")
  data <- model.frame(within)
  pdim <- pdim(data)
  balanced <- pdim$balanced
  n <- pdim$nT$n
  N <- pdim$nT$N
  if(effect != "twoways" && balanced){
    N <- pdim$nT$N
    if (effect == "individual"){
      arg.cond <- pdim$nT$n
      arg.other <- pdim$nT$T
    }
    else{
      arg.cond <- pdim$nT$T
      arg.other <- pdim$nT$n
    }
    idios <- deviance(within) / (N - arg.cond)
#    s2id <- sum((fixef(within)-mean(fixef(within)))^2)/(arg.cond-1)
    s2id <- sum(fixef(within, type = "dmean") ^ 2)/(arg.cond - 1)
    one <- arg.other * s2id + idios
    sigma2 <- list(one = one,
                   idios = idios,
                   id    = s2id
                   )
    theta <- 1 - sqrt(idios / sigma2$one)
    z <- list(theta = theta, sigma2 = sigma2)
  }
  else stop("nerlove variance decomposition only implemented for balanced oneway panels")
  z
}

print.ercomp <- function(x, digits= max(3, getOption("digits") - 3), ...){
  effect <- attr(x, "effect")
  balanced <- attr(x, "balanced")
  sigma2 <- x$sigma2
  theta <- x$theta
  
  if (effect=="twoways"){
    sigma2 <- unlist(sigma2)
    sigma2Table <- cbind(var=sigma2,std.dev=sqrt(sigma2),share=sigma2/sum(sigma2))
    rownames(sigma2Table) <- c("idiosyncratic","individual","time")
  }
  else{
    sigma2 <- unlist(sigma2[c("idios", "id")])
    sigma2Table <- cbind(var=sigma2,std.dev=sqrt(sigma2),share=sigma2/sum(sigma2))
    rownames(sigma2Table) <- c("idiosyncratic",effect)
  }
  printCoefmat(sigma2Table,digits)

  if (!is.null(x$theta)){
    if (effect!="twoways"){
      if (balanced){
        cat(paste("theta: ",signif(x$theta,digits)," \n"))
      }
      else{
        cat("theta  : \n")
        print(summary(x$theta))
      }
    }
    else{
      if(balanced){
        cat(paste("theta  : ",signif(x$theta$id,digits)," (id) ",signif(x$theta$time,digits)," (time) ",signif(x$theta$total,digits)," (total)\n",sep=""))
      }
    }
  }
}

