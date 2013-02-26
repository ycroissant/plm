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

# Almost the same as the default method except that update.formula is
# replaced by update, so that the Formula method is used to update the
# formula
update.panelmodel <- function (object, formula., ..., evaluate = TRUE){
  if (is.null(call <- object$call)) # was: getCall(object))) 
    stop("need an object with call component")
  extras <- match.call(expand.dots = FALSE)$...
  if (!missing(formula.)){
    call$formula <- update(formula(object), formula.)
  }
  if (length(extras)) {
    existing <- !is.na(match(names(extras), names(call)))
    for (a in names(extras)[existing]) call[[a]] <- extras[[a]]
    if (any(!existing)) {
      call <- c(as.list(call), extras[!existing])
      call <- as.call(call)
    }
  }
  if (evaluate) 
    eval(call, parent.frame())
  else call
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

