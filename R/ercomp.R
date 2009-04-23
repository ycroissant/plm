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
                           method = c('swar', 'walhus', 'amemiya', 'nerlove'),
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
                   "swar"    = swar    (object, data,  effect),
                   "walhus"  = walhus  (object, data,  effect),
                   "amemiya" = amemiya (object, data,  effect),
                   "nerlove" = nerlove (object, data,  effect)
                   )
  result <- structure(result, class = "ercomp", balanced = balanced, effect = effect)
  result
  
}

swar <- function(formula, data, effect){
  pdim <- pdim(data)
  balanced <- pdim$balanced
  within <- plm.within(formula, data, effect)
  sigma2 <- list()
  n <- pdim$nT$n
  T <- pdim$nT$T
  N <- pdim$nT$N
  if(effect != "twoways"){
    between <- plm.between(formula, data, effect)
    if (effect == "individual"){
      arg.cond <- n
      arg.other <- T
      arg.cond.i <- pdim$Tint$nt
      arg.other.i <- pdim$Tint$Ti
      cond <- data[["(id)"]]
    }
    else{
      arg.cond <- T
      arg.other <- n
      arg.cond.i <- pdim$Tint$Ti
      arg.other.i <- pdim$Tint$nt
      cond <- data[["(time)"]]
    }
    Kb <- length(coef(between))
    Kw <- length(coef(within))
    if(balanced){
      sigma2$one <- arg.other*deviance(between)/df.residual(between)
      sigma2$idios <- deviance(within)/df.residual(within)
      sigma2$id <- (sigma2$one-sigma2$idios)/arg.other
      if (sigma2$id < 0) stop("the estimated variance of the individual effect is negative")
      theta <- 1-sqrt(sigma2$idios/sigma2$one)
      z <- list(sigma2 = sigma2, theta = theta)
    }
    else{
      X <- model.matrix(formula, data, part = "first", model = "pooling", effect = effect)
      X.m <- papply(X,mymean,cond)
      X.sum <- attr(papply(X,mysum,cond),"cm")
      X.m.X <- crossprod(X.m)
      X.sum.X <- crossprod(X.sum)
      X.m.X.eig <- eigen(X.m.X)
      if (any(abs(X.m.X.eig$values)<1E-12)){
        cn <- which(abs(X.m.X.eig$values)<1E-12)
        C <- X.m.X.eig$vectors[,-cn]
        OM <- diag(X.m.X.eig$values[-cn])
        X.m.Xi <- C%*%solve(OM)%*%t(C)
        X.sum.X <- crossprod(X.sum)
        tr <- sum(diag(X.m.Xi%*%X.sum.X))
      }
      else{
        tr <- sum(diag(solve(crossprod(X.m))%*%crossprod(X.sum)))
      }
      sigma2$idios <- deviance(within)/(N-arg.cond-Kw)
      ssrbet <- sum(between$residuals^2*arg.other.i)
      sigma2$id <- (ssrbet-(arg.cond-Kb)*sigma2$idios)/(N-tr)
      if (sigma2$id < 0) stop("the estimated variance of the individual effect is negative")
      sigma2$one <- (arg.other.i*sigma2$id+sigma2$idios)
      theta <- 1-sqrt(sigma2$idios/(sigma2$idios+arg.other.i*sigma2$id))
      theta <- theta[as.character(cond)]
      z <- list(sigma2 = sigma2, theta = theta)
    }
  }
  else{
    between.id <- plm.between(formula, data, "individual")
    between.time <- plm.between(formula, data, "time")
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
      theta$id <- 1-sqrt(sigma2$idios/lambda2)
      theta$time <- 1-sqrt(sigma2$idios/lambda3)
      theta$total <- theta$id+theta$time+sqrt(sigma2$idios/lambda4)-1
      if (sigma2$time<0) theta$time <- theta$total <- sigma2$time <- 0
      z <- list(theta=theta,sigma2=sigma2)
    }
    else{
      stop("twoway random effect model not implemented for unbalanced panels")
    }
  }
  z
}

walhus <- function(formula, data, effect){
  pdim <- pdim(data)
  balanced <- pdim$balanced
  pooling <- plm.pooling(formula, data)
  respool <- resid(pooling)
  T <- pdim$nT$T
  n <- pdim$nT$n
  if(!balanced) stop("walhus not implemented for unbalanced panels\n")
  if(effect != "twoways"){
    if(effect=="individual"){
      condvar <- data[["(id)"]]
      card.cond <- n
      card.other <- T
    }
    else{
      condvar <- data[["(time)"]]
      card.cond <- T
      card.other <- n
    }
    one   = card.other*sum(tapply(respool,condvar,mean)^2)/card.cond
    idios = sum((respool-tapply(respool,condvar,mean)[as.character(condvar)])^2)/
      (card.cond*(card.other-1))
    sigma2 <- list(one = one,
                   idios = idios,
                   id    = (one-idios)/card.other
                   )
    theta <- 1-sqrt(idios/one)
    z <- list(theta = theta, sigma2 = sigma2)
  }
  else{
    id <- data[["(id)"]]
    time <- data[["(time)"]]
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
    if (sigma2$time<0) theta$time <- theta$total <- sigma2$time <- 0    
    z <- list(theta=theta,sigma2=sigma2)
  }
  z
}

amemiya <- function(formula, data, effect){
  pdim <- pdim(data)
  balanced <- pdim$balanced
  within <- plm.within(formula, data, effect)
  T <- pdim$nT$T
  n <- pdim$nT$n
  K <- length(coef(within))
  if(!balanced) stop("amemiya variance decomposition not implemented for unbalanced panels")
  if(effect != "twoways"){
    if (effect=="individual"){
      condvar <- data[["(id)"]]
      card.cond <- n
      card.other <- T
    }
    else{
      condvar <- data[["(time)"]]
      card.cond <- T
      card.other <- n
    }
    fe <- fixef(within, effect = effect)
    alpha <- mean(fe)
    uest <- resid(within)+fe[as.character(condvar)]-alpha
    one <- card.other/card.cond*sum(tapply(uest,condvar,mean)^2)
    idios <- deviance(within)/(card.cond*(card.other-1)-K)
    sigma2 <- list(one = one,
                   idios = idios,
                   id = max((one-idios)/card.other,0))
    theta <- max(1-sqrt(idios/one),0)
    z <- list(theta = theta, sigma2 = sigma2)
  }
  else{
    X <- model.matrix(formula, data, part = "first", model = "pooling")[,-1,drop = FALSE]
    y <- pmodel.response(data, part = "first", model = "pooling")
    id <- data[["(id)"]]
    time <- data[["(time)"]]
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
  pdim <- pdim(data)
  balanced <- pdim$balanced
  within <- plm.within(formula, data, effect)
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
    idios <- deviance(within)/N
    s2id <- sum((fixef(within)-mean(fixef(within)))^2)/(arg.cond-1)
    sigma2 <- list(idios = idios,
                   id    = s2id,
                   one   = arg.other*s2id+idios
                   )
    theta <- 1-sqrt(idios/sigma2$one)
    z <- list(theta = theta, sigma2 = sigma2)
  }
  else stop("nerlove variance decomposition only implemented for balanced oneway panels")
  z
}

print.ercomp <- function(x, digits= max(3, getOption("digits") - 2)){
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
    if (balanced) sigma2 <- unlist(sigma2[-1]) else sigma2 <- unlist(sigma2[1:2])
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

