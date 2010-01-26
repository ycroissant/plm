coef.pgmm <- function(object,...){
  model <- ifelse(is.null(object$call$model), "onestep", object$call$model)
  if(model=="onestep"){
    coefficients <- object$coefficients
  }
  else{
    coefficients <- object$coefficients[[2]]
  }
  coefficients
}

summary.pgmm <- function(object,robust=FALSE,...){
  model <- ifelse(is.null(object$call$model), "onestep", object$call$model)
  transformation <- ifelse(is.null(object$call$transformation), "d", object$call$transformation)
  if (robust){
    vv <- vcovHC(object)
  }
  else{
    vv <- vcov(object)
  }
  rowsel <- object$K$K+object$K$Ky
  std.err <- sqrt(diag(vv))
  b <- coef(object)
  z <- b/std.err
  p <- 2 * pnorm(abs(z),lower.tail=FALSE)
  CoefTable <- cbind(b,std.err,z,p)
  colnames(CoefTable) <- c("Estimate","Std. Error","z-value","Pr(>|z|)")
  object$CoefTable <- CoefTable[1:rowsel,,drop=FALSE]
  object$sargan <- sargan(object)
  object$m1 <- mtest(object,1,vv)
  object$m2 <- mtest(object,2,vv)
  object$wald.coef <- wald(object,"param",vv)
  if (describe(object, "effect") == "twoways") object$wald.td <- wald(object,"time",vv)
  class(object) <- "summary.pgmm"
  object
}

print.summary.pgmm <- function(x,digits=max(3, getOption("digits") - 2), width = getOption("width"),...){
  model <- ifelse(is.null(x$call$model), "onestep", x$call$model)
  transformation <- ifelse(is.null(x$call$transformation), "d", x$call$transformation)
  effect <- ifelse(is.null(x$call$effect), "individual", "twoways")
  pdim <- attr(x,"pdim")
  formula <- x$call$formula

  cat(paste(effect.pgmm.list[effect]," ",sep=""))
  cat(paste(model.pgmm.list[model],"\n",sep=""))
  cat("\nCall:\n")
  print(x$call)
  cat("\n")
  print(pdim)
  ntot <- apply(sapply(x$model,dim),1,sum)[1]
  cat("\nNumber of Observations Used: ",ntot,"\n")
  
  cat("\nResiduals\n")
  print(summary(unlist(residuals(x))))
  cat("\nCoefficients\n")
  printCoefmat(x$CoefTable,digits=digits)

  cat("\nSargan Test: ",names(x$sargan$statistic),
      "(",x$sargan$parameter,") = ",x$sargan$statistic,
      " (p.value=",format.pval(x$sargan$p.value,digits=digits),")\n",sep="")

  cat("Autocorrelation test (1): ",names(x$m1$statistic),
      " = ",x$m1$statistic,
      " (p.value=",format.pval(x$m1$p.value,digits=digits),")\n",sep="")
  
  cat("Autocorrelation test (2): ",names(x$m2$statistic),
      " = ",x$m2$statistic,
      " (p.value=",format.pval(x$m2$p.value,digits=digits),")\n",sep="")
  cat("Wald test for coefficients: ",names(x$wald.coef$statistic),
      "(",x$wald.coef$parameter,") = ",x$wald.coef$statistic,
      " (p.value=",format.pval(x$wald.coef$p.value,digits=digits),")\n",sep="")
  
  
  if (describe(x, "effect") == "twoways"){
    cat("Wald test for time dummies: ",names(x$wald.td$statistic),
        "(",x$wald.td$parameter,") = ",x$wald.td$statistic,
        " (p.value=",format.pval(x$wald.td$p.value,digits=digits),")\n",sep="")
  }
  invisible(x)
}



sargan <- function(object){
  model <- ifelse(is.null(object$call$model), "onestep", object$call$model)
  transformation <- ifelse(is.null(object$call$transformation), "d", object$call$transformation)
  Ktot <- object$K$K+object$K$Ky+object$K$Kt
  if (transformation=="ld"){
#    WS <- mapply(bdiag,object$W[[1]],object$W[[2]])
    resS <- lapply(object$residuals,function(x) c(diff(x),x))
    z <- suml(mapply(function(x,y) t(x)%*%y,object$W,resS,SIMPLIFY=FALSE))
    p <- ncol(object$W[[1]])
  }
  else{
    z <- suml(mapply(function(x,y) t(x)%*%y,object$W,object$residuals,SIMPLIFY=FALSE))
    p <- ncol(object$W[[1]])
  }
  if (model == "onestep"){
    A <- object$A1
  }
  else{
    A <- object$A2
  }
  stat <- as.numeric(crossprod(z,t(crossprod(z,A))))
  parameter <- p-Ktot
  names(parameter) <- "df"
  names(stat) <- "chisq"
  method <- "Sargan test"
#  pval <- 1-pchisq(stat,df=parameter)
  pval <- pchisq(stat, df = parameter, lower.tail = FALSE)
  sargan <- list(statistic = stat,
                 p.value = pval,
                 parameter = parameter,
                 method = "Sargan Test")
  class(sargan) <- "htest"
  sargan
}

wald <- function(x, param = "coef", vcov = NULL){
  myvcov <- vcov
  if (is.null(vcov)){
    vv <- vcov(x)
  } 
  else if (is.function(vcov)){
    vv <- myvcov(x)
  }
  else{
    vv <- myvcov
  }
  model <- ifelse(is.null(x$call$model), "onestep", x$call$model)
  transformation <- ifelse(is.null(x$call$transformation), "d", x$call$transformation)
  if (model == "onestep"){
    coefficients <- x$coefficients
  }
  else{
    coefficients <- x$coefficients[[2]]
  }
  Ktot <- length(coefficients)
  if (param == "time"){
    start <- switch(transformation,
                    "d"=(Ktot-x$K$Kt+1),
                    "ld"=(Ktot-x$K$Kt+2)
                    )
    end <- Ktot
  }
  else{
    end <- Ktot-x$K$Kt
    start <- 1
  }
  coef <- coefficients[start:end]
  vv <- vv[start:end,start:end]
  stat <- t(coef)%*%solve(vv)%*%coef
  names(stat) <- "chisq"
  parameter <- length(coef)
  pval <- pchisq(stat, df = parameter, lower.tail = FALSE)
  wald <- list(statistic = stat,
               p.value = pval,
               parameter = parameter,
               method = "Wald test")
  class(wald) <- "htest"
  wald
}

mtest <- function(object,order=1,vcov=NULL){
  myvcov <- vcov
  if (is.null(vcov)){
    vv <- vcov(object)
  }
  else if (is.function(vcov)){
    vv <- myvcov(object)
  }
  else{
    vv <- myvcov
  }
  model <- ifelse(is.null(object$call$model), "onestep", object$call$model)
  transformation <- ifelse(is.null(object$call$transformation), "d", object$call$transformation)
  time.names <- attr(object,"pdim")$panel.names$time.names
  time.lost <- object$time.lost
  time.names <- time.names[-(1:time.lost)]
  resid <- object$residuals
  if (transformation=="ld"){
    resid <- lapply(resid,diff)
    X <- lapply(object$model,function(x) rbind(diff(x[,-1]),x[,-1]))
    W <- object$W
    A2 <- object$A2
  }
  else{
    X <- lapply(object$model,function(x) x[,-1])
    W <- object$W
  }
  if (model == "onestep"){
    A <- object$A1
  }
  else{
    A <- object$A2
    
  }
  Eb <- lapply(resid,function(x){
    z <- rep(0,length(time.names))
    names(z) <- time.names
    z[names(x)] <- x
    return(z)
  })
  El <- lapply(resid,function(x){
    nx <- names(x)
    z <- c(rep(0,order),x[1:(length(x)-order)])
    names(z) <- nx
    z
  })
  Elb <- lapply(El,function(x){
    z <- rep(0,length(time.names))
    names(z) <- time.names
    z[names(x)] <- x
    z
  })

  if(transformation=="ld"){
    Eb <- lapply(Eb,function(x) c(x,rep(0,length(x)+1)))
    El <- lapply(El,function(x) c(x,rep(0,length(x)+1)))
    Elb <- lapply(Elb,function(x) c(x,rep(0,length(x)+1)))
    resid <- lapply(resid,function(x) c(x,rep(0,length(x)+1)))
  }
  EVE <- suml(mapply(function(x,y) t(y)%*%x%*%t(x)%*%y,Eb,Elb,SIMPLIFY=FALSE))
  EX <- suml(mapply(crossprod,El,X,SIMPLIFY=FALSE))
  XZ <- suml(mapply(crossprod,W,X,SIMPLIFY=FALSE))
  ZVE <- suml(mapply(function(x,y,z) t(x)%*%y%*%t(y)%*%z,W,resid,El,SIMPLIFY=FALSE))

  denom <- EVE-2*EX%*%vcov(object)%*%t(XZ)%*%A%*%ZVE+EX%*%vv%*%t(EX)
  num <- suml(mapply(crossprod,Eb,Elb,SIMPLIFY=FALSE))
  stat <- num/sqrt(denom)
  names(stat) <- "normal"
  pval <- pnorm(abs(stat),lower.tail=FALSE)
  mtest <- list(statistic = stat,
                p.value = pval,
                method = paste("Autocorrelation test of degree",order))
  class(mtest) <- "htest"
  mtest
}

