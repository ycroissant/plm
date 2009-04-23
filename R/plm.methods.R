summary.plm <- function(object,...){
  object$fstatistic <- Ftest(object, test = "F")
  # construct the table of coefficients
  std.err <- sqrt(diag(vcov(object)))
  b <- coefficients(object)
  z <- b/std.err
  p <- 2*pnorm(abs(z),lower.tail=FALSE)
  object$coefficients <- cbind("Estimate"   = b,
                               "Std. Error" = std.err,
                               "t-value"    = z,
                               "Pr(>|t|)"   = p)
  class(object) <- c("summary.plm", "plm", "panelmodel")
  object
}

print.summary.plm <- function(x,digits= max(3, getOption("digits") - 2),
                              width=getOption("width"),...){
  formula <- formula(x)
  has.instruments <- (length(formula) == 2)
  effect <- describe(x, "effect")
  model <- describe(x, "model")
  cat(paste(effect.plm.list[effect]," ",sep=""))
  cat(paste(model.plm.list[model]," Model",sep=""))

  if (model=="random"){
    ercomp <- describe(x, "ercomp")
    cat(paste(" \n   (",
              random.method.list[ercomp],
              "'s transformation)\n",
              sep=""))
  }
  else{
    cat("\n")
  }
  if (has.instruments){
    ivar <- describe(x, "ivar")
    if (model != "ht"){
      cat(paste("Instrumental variable estimation\n   (",
                inst.method.list[ivar],
                "'s transformation)\n",
                sep=""))
    }
  }
  cat("\nCall:\n")
  print(x$call)

  if (model == "ht"){
#    cat("\nTime-Varying Variables: ")
    names.xv <- paste(x$varlist$xv,collapse=",")
    names.nv <- paste(x$varlist$nv,collapse=",")
    names.xc <- paste(x$varlist$xc,collapse=",")
    names.nc <- paste(x$varlist$nc,collapse=",")
    cat(paste("\nT.V. exo  : ",names.xv,"\n",sep=""))
    cat(paste("T.V. endo : ",names.nv,"\n",sep=""))
#    cat("Time-Invariant Variables: ")
    cat(paste("T.I. exo  : ",names.xc,"\n",sep=""))
    cat(paste("T.I. endo : ",names.nc,"\n",sep=""))

  }
  cat("\n")
  pdim <- pdim(x)
  print(pdim)
  if (model %in% c("random", "ht")){
    cat("\nEffects:\n")
    print(x$ercomp)
  }
  cat("\nResiduals :\n")
  save.digits <- unlist(options(digits = digits))
  on.exit(options(digits = save.digits))
  print(sumres(x))
  
  cat("\nCoefficients :\n")
  printCoefmat(coef(x), digits = digits)
  cat("\n")
  cat(paste("Total Sum of Squares:    ",signif(tss(x),digits),"\n",sep=""))
  cat(paste("Residual Sum of Squares: ",signif(deviance(x),digits),"\n",sep=""))
#  cat(paste("Multiple R-Squared:      ",signif(x$rsq,digits),"\n",sep=""))
  fstat <- x$fstatistic
  if (names(fstat$statistic) == "F"){
    cat(paste("F-statistic: ",signif(fstat$statistic),
              " on ",fstat$parameter["df1"]," and ",fstat$parameter["df2"],
              " DF, p-value: ",format.pval(fstat$p.value,digits=digits),"\n",sep=""))
  }
  else{
    cat(paste("Chisq: ",signif(fstat$statistic),
              " on ",fstat$parameter,
              " DF, p-value: ",format.pval(fstat$p.value,digits=digits),"\n",sep=""))
    
  }
  invisible(x)
}

fitted.plm <- function(object, ...){
  model <- describe(object, "model")
  effect <- describe(object, "effect")
  X <- model.matrix(object, model = "pooling")
  beta <- coef(object)
  if (model == "within"){
    if (has.intercept(object)) X <- X[,-1]
    if (effect != "time") id <- model.frame(object)[,"(id)"]
    if (effect != "individual") time <- model.frame(object)[,"(time)"]
    fe <- switch(effect,
                 individual = fixef(object, effect = "individual")[as.character(id)],
                 time = fixef(object, effect="time")[as.character(time)],
                 twoways = fixef(object, effect = "individual")[as.character(id)]+
                           fixef(object, effect="time")[as.character(time)])
    fv <- as.numeric(crossprod(t(X),beta))+fe
  }
  else{
    fv <- as.numeric(crossprod(t(X),beta))
  }
  fv
#  model.response(model.frame(object))
}

predict.plm <- function(object, newdata = NULL, ...){
  tt <- terms(object)
  if (is.null(newdata)){
    result <- fitted(object, ...)
  }
  else{
    Terms <- delete.response(tt)
    m <- model.frame(Terms, newdata)
    X <- model.matrix(Terms, m)
    beta <- coef(object)
    result <- as.numeric(crossprod(beta, t(X)))
  }
  result
}

deviance.panelmodel <- function(object, ...){
  sum(residuals(object)^2)
} 

tss <- function(x, ...){
  UseMethod("tss")
}

tss.default <- function(x){
  n <- length(x)
  sum(x^2)-n*mean(x)^2
}

tss.plm <- function(x, model = NULL){
  if (is.null(model))
    model <- describe(x, "model")
  effect <- describe(x, "effect")
  if (model == "ht") model = "pooling"
  if (model == "random") theta <- x$ercomp$theta else theta <- NULL
  tss(pmodel.response(x, model = model, effect = effect, theta = theta))
}


r.squared <- function(object, ...){
  UseMethod("r.squared")
}

formula.plm <- function(x, ...){
  x$formula
}

# describe function: to extract the characteristics of the plm model
describe <- function(x, what = c('model', 'effect', 'ercomp', 'ivar')){
  cl <- x$call
  what <- match.arg(what)
  switch(what,
         model  = ifelse(!is.null(cl$model), cl$model, "within"),
         effect = ifelse(!is.null(cl$effect), cl$effect, "individual"),
         ercomp = ifelse(!is.null(cl$ercomp), cl$ercomp, "swar"),
         ivar   = ifelse(!is.null(cl$ivar), cl$ivar, "bvk")
         )
}
         
  
  
