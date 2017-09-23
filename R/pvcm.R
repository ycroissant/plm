pvcm <- function(formula, data, subset ,na.action, effect = c("individual","time"),
                 model = c("within","random"), index = NULL, ...){

  effect <- match.arg(effect)
  model.name <- match.arg(model)
  data.name <- paste(deparse(substitute(data)))

  cl <- match.call(expand.dots = TRUE)
  mf <- match.call()
  mf[[1]] <- as.name("plm")
  mf$model <- NA
  data <- eval(mf, parent.frame())
  result <- switch(model.name,
                   "within" = pvcm.within(formula, data, effect),
                   "random" = pvcm.random(formula, data, effect)
  )
  class(result) <- c("pvcm", "panelmodel")
  result$call <- cl
  result$args <- list(model = model, effect = effect)
  result
}

pvcm.within <- function(formula, data, effect){

  index <- attr(data, "index")
  id <- index[[1]]
  time <- index[[2]]
  pdim <- pdim(data)

  if (effect == "time"){
    cond <- time
    other <- id
    card.cond <- pdim$nT$T
  }
  else{
    cond <- id
    other <- time
    card.cond <- pdim$nT$n
  }
  ml <- split(data, cond)
  nr <- sapply(ml, function(x) dim(x)[1]) > 0
  ml <- ml[nr]
  attr(ml, "index") <- index
  ols <- lapply(ml,
                function(x){
                  X <- model.matrix(formula, x)
                  if (nrow(X) <= ncol(X)) stop("insufficient number of observations")
#                  y <- model.response(x)
                  y <- x[[1]]
                  r <- lm(y ~ X - 1)
#                  r <- mylm(y,X)
                  nc <- colnames(model.frame(r)$X)
                  names(r$coefficients) <- nc
                  r
                })
  
  coef <- as.data.frame(t(sapply(ols, coefficients)))
  residuals <- unlist(lapply(ols, residuals))
  vcov <- lapply(ols, vcov)
  std <- as.data.frame(t(sapply(vcov, function(x) sqrt(diag(x)))))
  names(coef) <- names(std) <- colnames(coef)
  ssr <- sum(residuals^2)
  y <- unlist(lapply(ml, function(x) x[,1]))
  fitted.values <- y - residuals
  tss <- tss(y)
  df.residuals <- pdim$nT$N - card.cond * ncol(coef)
  nopool <- list(coefficients = coef, residuals = residuals, fitted.values = fitted.values,
                 vcov = vcov, df.residuals = df.residuals, model = data, std.error = std)
  nopool
}

pvcm.random <- function(formula, data, effect){
  interc <- has.intercept(formula)
  index <- attr(data, "index")
  id <- index[[1]]
  time <- index[[2]]
  pdim <- pdim(data)
  N <- nrow(data)
  if (effect == "time"){
    cond <- time
    other <- id
    card.cond <- pdim$nT$T
  }
  else{
    cond <- id
    other <- time
    card.cond <- pdim$nT$n
  }
  ml <- split(data, cond)
  nr <- sapply(ml, function(x) dim(x)[1]) > 0
  ml <- ml[nr]
  attr(ml, "index") <- index
  ols <- lapply(ml,
                function(x){
                  X <- model.matrix(formula, x)
                  if (nrow(X) <= ncol(X)) stop("insufficient number of observations")
#                  y <- model.response(x)
                  y <- x[[1]]
                  r <- lm(y ~ X - 1)
#                  r <- mylm(y, X)
                  nc <- colnames(model.frame(r)$X)
                  names(r$coefficients) <- nc
                  r
                }
                )

  # For some time series, some coefficients may be NA
  Ncoef <- which.max(sapply(ols, function(x) length(coef(x))))
  names.coef <- names(coef(ols[[Ncoef]]))
  
  coefm <- t(sapply(ols, function(x) coef(x)[names.coef]))
  coefNA <- is.na(coefm)
  colnames(coefm) <- names.coef
  coefm <- apply(coefm, 2, function(x){ x[is.na(x)] <- mean(x, na.rm = TRUE);x})
  coef <- lapply(seq_len(nrow(coefm)), function(i) coefm[i, ])
  K <- ncol(coefm) - has.intercept(formula)
  res <- lapply(ols, residuals)
  coefb <- apply(coefm, 2, function(x) mean(x, na.rm = TRUE))
  X <- lapply(ml, function(x) model.matrix(formula, x));print(X);stop()
  X <- lapply(ml, function(x) model.matrix(formula, x)[, colnames(coefm), drop = F])
#  y <- lapply(ml, model.response)
  y <- lapply(ml, function(x) x[[1]])
  xpxm1 <- lapply(X,
                  function(x){
                    solve(crossprod(x))
                  }
                  )
  bbp <- lapply(coef, function(x) x - coefb)
  D1 <- Reduce("+", lapply(bbp, function(x) crossprod(t(x))))/(card.cond-1)
  sigi <- lapply(res,function(x) sum(x^2)/(length(x)-K-interc))
  sigim <- lapply(sigi,function(x) x*matrix(1,K+interc,K+interc))
  s2xpxm1i <- mapply("*",xpxm1,sigim,SIMPLIFY=F)
  D2 <- matrix(apply(mapply("*",xpxm1,sigi),1,sum)/card.cond,ncol=K+interc)
  eig <- prod(eigen(D1-D2)$values>=0)
  if (eig) Delta <- D1-D2 else Delta <- D1  # si le Delta precedent nes pas semi defini positif
  Delta <- list(Delta)
  Wn <- mapply("+",s2xpxm1i,rep(Delta,card.cond),SIMPLIFY=F)
  Wnm <- sapply(Wn,solve)
  Wn <- lapply(Wn,solve)
  vcovb <- solve(matrix(apply(Wnm,1,sum),K+interc,K+interc))
  W <- lapply(Wn,function(x) vcovb%*%x)
  beta <- apply(mapply("%*%",W,coef),1,sum)
  df.residuals <- N-ncol(coefm)
  tss <- tss(unlist(y))
  haty <- oppl(X,beta,"%*%")
  residuals <- mapply("-",y,haty,SIMPLIFY=FALSE)
  ssr <- sum(unlist(residuals)^2)
  names.res <- lapply(residuals,rownames)
  residuals <- lapply(residuals,as.vector)
  zo <- function(x,y){names(x) <- y;x}
  residuals <- unlist(mapply(zo,residuals,names.res,SIMPLIFY=FALSE))
  y <- unlist(y)
  fitted.values <- y-residuals
  names(beta) <- rownames(vcovb) <- colnames(vcovb) <- colnames(coefm)
  swamy <- list(coefficients = beta, residuals = residuals, fitted.values = fitted.values,
                vcov = vcovb, df.residuals = df.residuals, model = data, Delta = Delta[[1]])
  swamy
}

### TODO: pvcm.random is defined two times in this file
pvcm.random <- function(formula, data, effect){

  interc <- has.intercept(formula)
  index <- index(data)
  id <- index[[1]]
  time <- index[[2]]
  pdim <- pdim(data)
  N <- nrow(data)
  if (effect == "time"){
    cond <- time
    other <- id
    card.cond <- pdim$nT$T
  }
  else{
    cond <- id
    other <- time
    card.cond <- pdim$nT$n
  }

  ml <- split(data, cond)
  nr <- sapply(ml, function(x) dim(x)[1]) > 0
  ml <- ml[nr]
  attr(ml, "index") <- index
  ols <- lapply(ml,
                function(x){
                  X <- model.matrix(formula, x)
                  if (nrow(X) <= ncol(X)) stop("insufficient number of observations")
#                  y <- model.response(x)
                  y <- x[[1]]
                  r <- lm(y ~ X - 1)
#                  r <- mylm(y, X)
                  nc <- colnames(model.frame(r)$X)
                  names(r$coefficients) <- nc
                  r
                })
  
  # matrix of coefficients
  coefm <- t(sapply(ols, coef))
  # number of covariates
  K <- ncol(coefm) - has.intercept(formula)
  # check for NA coefficients
  coefna <- is.na(coefm)
  # list of model matrices
  X <- lapply(ols, model.matrix)
  # same without the covariates with NA coefficients
  Xna <- lapply(seq_len(nrow(coefm)), function(i)X[[i]][, !coefna[i, ]])
  # list of model responses
  y <- lapply(ols, function(x) model.response(model.frame(x)))
  # compute a list of XpX^-1 matrices, with 0 for lines/columns with
  # NA coefficients
  xpxm1 <- lapply(seq_len(card.cond), function(i){
    z <- matrix(0, ncol(coefm), ncol(coefm),
                dimnames=list(colnames(coefm), colnames(coefm)))
    z[!coefna[i, ], !coefna[i, ]] <- solve(crossprod(X[[i]][!coefna[i, ], !coefna[i,]]))
    z
  })
  # compute the mean of the parameters
  coefb <- apply(coefm, 2, function(x) mean(x, na.rm = TRUE))
  # insert the mean values in place of NA coefficients
  coefm <- apply(coefm, 2, function(x){x[is.na(x)] <- mean(x, na.rm = TRUE);x})
  # compute the first part of the variance matrix
  D1 <- (t(coefm) - coefb) %*% t (t(coefm) - coefb) / (card.cond - 1)
  # compute the second part of the variance matrix
  sigi <- sapply(ols, function(x) deviance(x) / df.residual(x))
  D2 <- Reduce("+", lapply(seq_len(card.cond),
               function(i) sigi[i] * xpxm1[[i]])) / card.cond
  # if D1-D2 semi-definite positive, use it, otherwise use D1
  eig <- prod(eigen(D1 - D2)$values >= 0)
  if (eig){
    Delta <- D1 - D2
  }
  else{
#    print(eigen(D1-D2)$values)
#    cat("attention\n")
    Delta <- D1
  }
  # compute the Omega matrix for each individual
  Omegan <- lapply(seq_len(card.cond), function(i) sigi[i] * diag(nrow(X[[i]])) + X[[i]] %*% Delta %*% t(X[[i]]))
  # Compte X'Omega X et X'Omega y for each individual
  XyOmXy <- lapply(seq_len(card.cond), function(i){
    Xn <- X[[i]][, !coefna[i,]]
    yn <- y[[i]]
    XnXn <- matrix(0, ncol(coefm), ncol(coefm), dimnames=list(colnames(coefm), colnames(coefm)))
    Xnyn <- matrix(0, ncol(coefm), 1, dimnames=list(colnames(coefm), "y"))
    XnXn[!coefna[i,], !coefna[i,]] <- t(Xn) %*% solve(Omegan[[i]]) %*% Xn
    Xnyn[!coefna[i, ],] <- t(Xn) %*% solve(Omegan[[i]]) %*% yn
    list(XnXn = XnXn, Xnyn = Xnyn)
  })
  # Compute the coefficients
  XpXm1 <- solve(Reduce("+", lapply(XyOmXy, function(x) x$XnXn)))
  beta <- XpXm1 %*% Reduce("+", lapply(XyOmXy, function(x) x$Xnyn))
  
  if (TRUE){                                                    ### TODO: this statement is always TRUE...?!
    weightsn <- lapply(seq_len(card.cond),
                       function(i){
                         vcovn <- vcov(ols[[i]])
                         Deltan <- Delta[!coefna[i,], !coefna[i,]]
                         wn <- solve(vcovn + Deltan)
                         z <- matrix(0, ncol(coefm), ncol(coefm),
                                     dimnames = list(colnames(coefm), colnames(coefm)))
                         z[!coefna[i,], !coefna[i,]] <- wn
                         z
                       }
                       )
    V <- solve(Reduce("+", weightsn))
    weightsn <- lapply(weightsn, function(x) V %*% x)
    Beta <- Reduce("+", lapply(seq_len(card.cond), function(i) weightsn[[i]] %*% coefm[i, ]))
    XpXm1 <- V
  }
  
  y <- model.response(data)
  X <- model.matrix(formula, data)
  fit <- X %*% beta
  res <- y - fit
  df.residuals <- N - ncol(coefm)

  list(coefficients = beta, residuals = res, fitted.values = fit,
       vcov = XpXm1, df.residuals = df.residuals, model = data, Delta = Delta)
}


summary.pvcm <- function(object,...){
  model <- describe(object, "model")
  if (model == "random"){
    std.err <- sqrt(diag(vcov(object)))
    b <- object$coefficients
    z <- b/std.err
    p <- 2*pnorm(abs(z), lower.tail = FALSE)
    coef <- cbind(b, std.err, z, p)
    colnames(coef) <- c("Estimate", "Std. Error", "z-value", "Pr(>|z|)")
    object$coefficients <- coef
  }
  object$ssr <- deviance(object)
  object$tss <- tss(unlist(model.frame(object)))
  object$rsqr <- 1-object$ssr/object$tss
  class(object) <- c("summary.pvcm", "pvcm")
  return(object)
}

print.summary.pvcm <- function(x, digits = max(3, getOption("digits") - 2),
                               width = getOption("width"),...){
  effect <- describe(x, "effect")
  formula <- formula(x)
  model <- describe(x, "model")
  cat(paste(effect.pvcm.list[effect]," ",sep=""))
  cat(paste(model.pvcm.list[model],"\n",sep=""))
  cat("\nCall:\n")
  print(x$call)
  cat("\n")
  print(pdim(model.frame(x)))
  cat("\nResiduals:\n")
  print(summary(unlist(residuals(x))))
  if (model == "random"){
    cat("\nEstimated mean of the coefficients:\n")
    printCoefmat(x$coefficients, digits = digits)
    cat("\nEstimated variance of the coefficients:\n")
    print(x$Delta, digits = digits)
  }
  if (model == "within"){
    cat("\nCoefficients:\n")
    print(summary(x$coefficients))
  }
  cat("\n")
  cat(paste("Total Sum of Squares: ", signif(x$tss, digits), "\n", sep=""))
  cat(paste("Residual Sum of Squares: ", signif(x$ssr, digits), "\n", sep=""))
  cat(paste("Multiple R-Squared: ", signif(x$rsqr, digits), "\n", sep=""))
  invisible(x)
}
