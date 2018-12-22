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

