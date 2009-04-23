terms.panelmodel <- function(x,...){
  terms(formula(x))
}

vcov.panelmodel <- function(object,...){
  object$vcov
}

fitted.panelmodel <- function(object,...){
  object$fitted.values 
}

residuals.panelmodel <- function(object,...){
  object$residuals
}

df.residual.panelmodel <- function(object,...){
  object$df.residual
}

coef.panelmodel <- function(object,...){
  object$coefficients
}

print.panelmodel <- function(x,digits=max(3, getOption("digits") - 2), width = getOption("width"),...){
  cat("\nModel Formula: ")
  print(formula(x))
  cat("\nCoefficients:\n")
  print(coef(x),digits=digits)
  cat("\n")
  invisible(x)
}

print.form <- function(x,length.line){
  x <- deparse(x,width.cutoff=length.line)
  n <- length(x)
  cat(paste(x[1],"\n",sep=""))
  if (n>1){
    for (i in 2:n){
      cat(paste(x[i],"\n",sep=""))
    }
  }
}

print.theta <- function(x,digits){
  effect <- describe(x, "effect")
  pdim <- pdim(model.frame(x))
  if (effect!="twoways"){
    if (pdim$balanced){
      cat(paste("theta: ",signif(x$theta,digits)," \n"))
    }
    else{
      cat("theta  : \n")
      print(summary(x$theta))
    }
  }
  else{
    if(pdim$balanced){
      cat(paste("theta  : ",signif(x$theta$id,digits)," (id) ",signif(x$theta$time,digits)," (time) ",signif(x$theta$total,digits)," (total)\n",sep=""))
    }
  }
}

