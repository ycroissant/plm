pvcm <-  function(formula, data, subset ,na.action, effect = c("individual","time"),
                  model = c("within","random"), index = NULL, ...){

  effect <- match.arg(effect)
  model.name <- match.arg(model)
  data.name <- paste(deparse(substitute(data)))

  cl <- match.call(expand.dots = TRUE)
  mf <- match.call()
  mf[[1]] <- as.name("plm")
  mf$model <- NA
  data <- eval(mf,parent.frame())
  result <- switch(model.name,
                   "within" = pvcm.within(formula, data, effect),
                   "random" = pvcm.random(formula, data, effect)
                   )
  class(result) <- c("pvcm","panelmodel")
  result$call <- cl
  result$args <- list(model = model, effect = effect)
  result
}

pvcm.within <- function(formula, data, effect){

  index <- attr(data, "index")
  id <- index[[1]]
  time <- index[[2]]
  pdim <- pdim(data)

  if (effect=="time"){
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
  nr <- sapply(ml,function(x) dim(x)[1])>0
  ml <- ml[nr]
  attr(ml, "index") <- index
  ols <- lapply(ml,
                function(x){
                  X <- model.matrix(formula,x)
                  if (nrow(X) <= ncol(X)) stop("insufficient number of observations")
#                  y <- model.response(x)
                  y <- x[[1]]
                  r <- lm(y~X-1)
                  r <- mylm(y,X)
                  nc <- colnames(model.frame(r)$X)
                  names(r$coefficients) <- nc
                  r
                }
                )


  coef <- as.data.frame(t(sapply(ols,coefficients)))
  residuals <- unlist(lapply(ols,residuals))
  vcov <- lapply(ols,vcov)
  std <- as.data.frame(t(sapply(vcov,function(x) sqrt(diag(x)))))
  names(coef) <- names(std) <- colnames(coef)
  ssr <- sum(residuals^2)
  y <- unlist(lapply(ml,function(x) x[,1]))
  fitted.values <- y-residuals
  tss <- tss(y)
  df.residuals <- pdim$nT$N-card.cond*ncol(coef)
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
  
  if (effect=="time"){
    cond <- time
    other <- id
    card.cond <- pdim$nT$T
  }
  else{
    cond <- id
    other <- time
    card.cond <- pdim$nT$n
  }
  ml <- split(data,cond)
  nr <- sapply(ml,function(x) dim(x)[1])>0
  ml <- ml[nr]
  attr(ml, "index") <- index
  ols <- lapply(ml,
                function(x){
                  X <- model.matrix(formula,x)
                  if (nrow(X) <= ncol(X)) stop("insufficient number of observations")
#                  y <- model.response(x)
                  y <- x[[1]]
                  r <- lm(y~X-1)
                  r <- mylm(y,X)
                  nc <- colnames(model.frame(r)$X)
                  names(r$coefficients) <- nc
                  r
                }
                )

  coefm <- t(sapply(ols,coefficients))
  coef <- lapply(ols,coefficients)

  K <- ncol(coefm) - has.intercept(formula)
  res <- lapply(ols, residuals)
  coefb <- apply(coefm, 2, mean)

  X <- lapply(ml, function(x) model.matrix(formula, x)[,colnames(coefm), drop = F])
#  y <- lapply(ml, model.response)
  y <- lapply(ml, function(x) x[[1]])
  xpxm1 <- lapply(X,
                  function(x){
                    solve(crossprod(x))
                  }
                  )
  bbp <- lapply(coef, function(x) x - coefb)
  D1 <- suml(lapply(bbp, function(x) crossprod(t(x))))/(card.cond-1)
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

summary.pvcm <- function(object,...){
  model <- describe(object, "model")
  if (model == "random"){
    std.err <- sqrt(diag(vcov(object)))
    b <- object$coefficients
    z <- b/std.err
    p <- 2*(1-pnorm(abs(z)))
    coef <- cbind(b,std.err,z,p)
    colnames(coef) <- c("Estimate","Std. Error","z-value","Pr(>|z|)")
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
  cat(paste("Total Sum of Squares: ",signif(x$tss,digits),"\n",sep=""))
  cat(paste("Residual Sum of Squares: ",signif(x$ssr,digits),"\n",sep=""))
  cat(paste("Multiple R-Squared: ",signif(x$rsqr,digits),"\n",sep=""))
  invisible(x)
}
