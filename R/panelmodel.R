# panelmodel and plm methods :

## panelmodel methods :
# - terms
# - vcov
# - fitted
# - residuals
# - df.residuals
# - coef
# - print
# - update
# - deviance

## plm methods :
# - summary
# - print.summary
# - predict
# - formula
# - plot
# - residuals
# - fitted


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

print.panelmodel <- function(x,digits=max(3, getOption("digits") - 2),
                             width = getOption("width"),...){
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

deviance.panelmodel <- function(object, model = NULL, ...){
  if (is.null(model)) as.numeric(crossprod(resid(object)))
  else as.numeric(crossprod(residuals(object, model = model)))
}



# summary.plm creates a specific summary.plm object that is derived
# from the associated plm object
summary.plm <- function(object, vcov = NULL, ...){
    
    vcov_arg <- vcov
    
    object$fstatistic <- pwaldtest(object, test = "F", vcov = vcov_arg)
    model <- describe(object, "model")
    effect <- describe(object, "effect")
    object$r.squared <- c(rsq  = r.squared(object),
                          adjrsq = r.squared(object, dfcor = TRUE))
    # construct the table of coefficients
    if (!is.null(vcov_arg)) {
        if (is.matrix(vcov_arg))   rvcov <- vcov_arg
        if (is.function(vcov_arg)) rvcov <- vcov_arg(object)
        std.err <- sqrt(diag(rvcov))
    } else {
        std.err <- sqrt(diag(stats::vcov(object)))
    }
    b <- coefficients(object)
    z <- b / std.err
    p <- 2 * pt(abs(z), df = object$df.residual, lower.tail = FALSE)
    
    # construct the object of class summary.plm
    object$coefficients <- cbind("Estimate"   = b,
                                 "Std. Error" = std.err,
                                 "t-value"    = z,
                                 "Pr(>|t|)"   = p)
    
    ## add some info to summary.plm object 
    # robust vcov (next to "normal" vcov)
    if (!is.null(vcov_arg)) {
        object$rvcov <- rvcov
        rvcov.name <- paste0(deparse(substitute(vcov)))
        attr(object$rvcov, which = "rvcov.name") <- rvcov.name 
    }
    
    # mimics summary.lm's 'df' component
    # 1st entry: no. coefs (w/o aliased coefs); 2nd: residual df; 3rd no. coefs /w aliased coefs
    # NB: do not use length(object$coefficients) for 3rd entry!
    object$df <- c(length(b), object$df.residual, length(object$aliased))
    
    class(object) <- c("summary.plm", "plm", "panelmodel")
    object
}

print.summary.plm <- function(x, digits = max(3, getOption("digits") - 2),
                              width=getOption("width"), subset = NULL, ...){
  formula <- formula(x)
  has.instruments <- (length(formula)[2] == 2)
  effect <- describe(x, "effect")
  model  <- describe(x, "model")
  if (model != "pooling") { cat(paste(effect.plm.list[effect]," ",sep="")) }
  cat(paste(model.plm.list[model]," Model",sep=""))
  
  if (model == "random"){
    ercomp <- describe(x, "random.method")
    cat(paste(" \n   (",
              random.method.list[ercomp],
              "'s transformation)\n",
              sep=""))
  }
  else{
    cat("\n")
  }
  
  if (has.instruments){
    ivar <- describe(x, "inst.method")
    cat(paste("Instrumental variable estimation\n   (",
              inst.method.list[ivar],
              "'s transformation)\n",
              sep=""))
  }
  
  if (!is.null(x$rvcov)) {
      cat("\nNote: Coefficient variance-covariance matrix supplied: ",
          attr(x$rvcov, which = "rvcov.name"), "\n", sep = "")
  }
  
  cat("\nCall:\n")
  print(x$call)
  cat("\n")
  pdim <- pdim(x)
  print(pdim)
  if (model %in% c("fd", "between")) {
    # print this extra info, b/c model.frames of FD and between models
    # have original (undifferenced/"un-between-ed") obs/rows of the data
    cat(paste0("Observations used in estimation: ", nobs(x), "\n"))}

  if (model == "random"){
    cat("\nEffects:\n")
    print(x$ercomp)
  }
  cat("\nResiduals:\n")
  df <- x$df
  rdf <- df[2L]
  if (rdf > 5L) {
    save.digits <- unlist(options(digits = digits))
    on.exit(options(digits = save.digits))
    print(sumres(x))
  } else if (rdf > 0L) print(residuals(x), digits = digits)
  if (rdf == 0L) { # estimation is a perfect fit
   cat("ALL", x$df[1L], "residuals are 0: no residual degrees of freedom!")
   cat("\n")
  }
  
  if (any(x$aliased, na.rm = TRUE)) {
    # na.rm = TRUE because currently, RE tw unbalanced models might have NAs?
    naliased <- sum(x$aliased, na.rm = TRUE)
    cat("\nCoefficients: (", naliased, " dropped because of singularities)\n", sep = "")
  } else cat("\nCoefficients:\n")
  
  if (is.null(subset)) printCoefmat(coef(x), digits = digits)
  else printCoefmat(coef(x)[subset, , drop = FALSE], digits = digits)
  cat("\n")
  cat(paste("Total Sum of Squares:    ", signif(tss(x),      digits), "\n", sep = ""))
  cat(paste("Residual Sum of Squares: ", signif(deviance(x), digits), "\n", sep = ""))
  cat(paste("R-Squared:      ", signif(x$r.squared[1], digits),       "\n", sep = ""))
  cat(paste("Adj. R-Squared: ", signif(x$r.squared[2], digits),       "\n", sep = ""))
  fstat <- x$fstatistic
  if (names(fstat$statistic) == "F"){
    cat(paste("F-statistic: ",signif(fstat$statistic),
              " on ",fstat$parameter["df1"]," and ",fstat$parameter["df2"],
              " DF, p-value: ",format.pval(fstat$p.value,digits=digits), "\n", sep=""))
  }
  else{
    cat(paste("Chisq: ",signif(fstat$statistic),
              " on ",fstat$parameter,
              " DF, p-value: ",format.pval(fstat$p.value,digits=digits), "\n", sep=""))
    
  }
  invisible(x)
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

formula.plm <- function(x, ...){
  x$formula
}

plot.plm <- function(x, dx = 0.2, N = NULL, seed = 1,
                     within = TRUE, pooling = TRUE,
                     between = FALSE, random = FALSE, ...){
    set.seed(seed)# 8 est bien pour beertax
    subs <- ! is.null(N)
    x <- update(x, model = "within")
    mco <- update(x, model = "pooling")
    if (random) re <- update(x, model = "random")
    if (between) be <- update(x, model = "between")
    pdim <- pdim(x)
    n <- pdim$nT$n
    if (! subs) N <- n
    ids <- unique(index(x, "id"))
    if (subs) ids <- ids[sample(1:length(ids), N, replace = FALSE)]
    sel <- index(x, "id") %in% ids
    T <- pdim$nT$T
    cols <- rainbow(N)
    pts <- sample(1:25, N, replace = TRUE)
    thex <- as.numeric(model.matrix(x, model = "pooling")[sel, 2])
    they <- as.numeric(pmodel.response(x, model = "pooling")[sel])
    plot(thex, they, col = rep(cols, each = T),
         pch = rep(pts, each = T), ann = FALSE, las = 1)
    idsel <- as.numeric(index(x, "id")[sel])
    meanx <- tapply(thex, idsel, mean)
    meany <- tapply(they, idsel, mean)
    points(meanx, meany, pch = 19, col = cols, cex = 1.5)
    if (within){
        beta <- coef(x)
        alphas <- meany - meanx * beta
        dx <- dx * (max(thex) - min(thex))
        for (i in 1:N){
            xmin <- meanx[i] - dx
            xmax <- meanx[i] + dx
            ymin <- alphas[i] + beta * xmin
            ymax <- alphas[i] + beta * xmax
            lines(c(xmin, xmax), c(ymin, ymax), col = cols[i])
        }
    }
    if(random) abline(coef(re)[1], coef(re)[2], lty = "dotted")
    if(pooling) abline(coef(mco), lty = "dashed")
    if(between) abline(coef(be), lty = "dotdash")
    # where to put the legends, depends on the sign of the OLS slope
    modploted <- c(random, pooling, between, within)
    if (sum(modploted)){
        poslegend <- ifelse(beta > 0, "topleft", "topright")
        ltylegend <- c("dotted", "dashed", "dotdash", "solid")[modploted]
        leglegend <- c("random", "pooling", "between", "within")[modploted]
        legend(poslegend, lty = ltylegend, legend = leglegend)
    }
}

residuals.plm <- function(object, model = NULL, effect = NULL,  ...){
    if (is.null(model) & is.null(effect)){
        model <- describe(object, "model")
        res <- object$residuals
    }
    else{
        cl <- match.call(expand.dots = FALSE)
        cl[[1]] <- as.name("fitted.plm")
        bX <- eval(cl, parent.frame())
        if (is.null(model)) model <- describe(object, "model")
        if (is.null(effect)) effect <- describe(object, "effect")
        y <- pmodel.response(object, model = model, effect = effect)
        res <- y - bX
    }
    res <- if (model %in% c("between", "fd")) {
      # these models "compress" the data, thus an index does not make sense here
      # -> do not return pseries but plain numeric
      res
    } else {
      structure(res, index = index(object), class = union("pseries", class(res)))
    }
    return(res)
}

fitted.plm <- function(object, model = NULL, effect = NULL, ...){
    fittedmodel <- describe(object, "model")
    if (is.null(model)) model <- fittedmodel
    if (is.null(effect)) effect <- describe(object, "effect")
    if (fittedmodel == "random") theta <- ercomp(object)$theta else theta <- NULL
    X <- model.matrix(object, model = "pooling")
    y <- pmodel.response(object, model = "pooling", effect = effect)
    beta <- coef(object)
    comonpars <- intersect(names(beta), colnames(X))
    bX <- as.numeric(crossprod(t(X[, comonpars, drop = FALSE]), beta[comonpars]))
    bX <- structure(bX, index = index(object), class = union("pseries", class(bX)))
    if (fittedmodel == "within"){
        intercept <- mean(y - bX)
        bX <- bX + intercept
    }
    ptransform(bX, model = model, effect = effect, theta = theta)
}
