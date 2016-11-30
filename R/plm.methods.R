
# summary.plm creates a specific summary.plm object that is derived from the associated plm object
summary.plm <- function(object, vcov = NULL, ..., .vcov = NULL){

  ## deprecation notice on arg ".vcov" introduced Nov 2016: remove arg ."vcov" some time in the future
  
  vcov_arg <- vcov
  
  ### set correct vcov in case deprecated arg .vcov is not null to support the deprecated .vcov arg for a while
    depri_.vcov <- paste0("Use of argument \".vcov\" (notice leading dot) is deprecated.", 
                        " Please change your code to use argument \"vcov\", because \".vcov\" will be removed in the future.")
    depri_vcov.vcov <- paste0("Arguments \"vcov\" and \".vcov\" specified (not null), continuing with \"vcov\". ", depri_.vcov)
    if (!is.null(vcov) && !is.null(.vcov)) {
      warning(depri_vcov.vcov)
        vcov_arg <- vcov
    } else {
      if (!is.null(.vcov)) {
        warning(depri_.vcov)
        vcov_arg <- .vcov
      }
    }
  ### END set correct vcov in case deprecated arg .vcov is not null
  
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
      
      ## set correct rvcov.name depending on arg used for vcov (as long as we support .vcov)
      if (is.null(vcov)) {
        rvcov.name <- paste0(deparse(substitute(.vcov)))
      } else {
        rvcov.name <- paste0(deparse(substitute(vcov)))
      }
      
      attr(object$rvcov, which = "rvcov.name") <- rvcov.name 
    }
    
    # mimics summary.lm's 'df' component
    # 1st entry: no. coefs (w/o aliased coefs); 2nd: residual df; 3rd no. coefs /w aliased coefs
    object$df <- c(length(b), object$df.residual, length(object$aliased)) # NB: do not use length(object$coefficients) here
    
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
    cat("\nNote: Coefficient variance-covariance matrix supplied: ", attr(x$rvcov, which = "rvcov.name"), "\n", sep = "")
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
  cat("\nResiduals :\n")
  save.digits <- unlist(options(digits = digits))
  on.exit(options(digits = save.digits))
  print(sumres(x))
  
  cat("\nCoefficients :\n")
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

fitted.plm <- function(object, model = NULL, ...){
  # there are two 'models' used ; the fitted model and the
  # transformation used for the fitted values
  fittedmodel <- describe(object, "model")
  if (is.null(model)) model <- fittedmodel
  effect <- describe(object, "effect")
  X <- model.matrix(object, model = model)
  y <- pmodel.response(object, model = model)
  beta <- coef(object)
  # Kevin Tappe 2016-01-09 : perfect correlation of some columns of
  # the within model.matrix
  # NB: Could this make use of plmobject$aliased to simplify and save the lm estimation?
  if (ncol(X) != length(beta)){
      result <- lm(y ~ X - 1)
      X <- X[, ! is.na(coef(result))]
  }
  if (model == "within" & fittedmodel != "within"){
    Xw <- model.matrix(object, model = "within", effect = effect)
    varwith <- colnames(Xw)
    beta <- beta[varwith]
  }
  
  # Test if all coefficients could be estimated by plm
  # [plm silently drops non-estimatable coefficients [v1.5-13]]
  # With this test, we provide an additional warning message to
  # users to enhance the error message from failing crossprod later in the code
  # which relies on non-dropped coefficients; see also testfile tests/test_fitted.plm.R
  # This test could be computationally/space expensive due to creation of model.matrix.
  # if (!setequal(names(object$coefficients), colnames(model.matrix(object)))) {
  #    warning("Coefficients of estimated model do not match variables in its specified model.matrix.
  #           This is likely due to non-estimatable coefficients (compare object$formula with object$coefficients).")
  # }
  
  if (fittedmodel == "within"){
    if (model == "pooling"){
      if (has.intercept(object)) X <- X[,-1]
      index <- attr(model.frame(object), "index")
      if (effect != "time") id <- index[[1]]
      if (effect != "individual") time <- index[[2]]
      fe <- switch(effect,
                   individual = fixef(object, effect = "individual")[as.character(id)],
                   time = fixef(object, effect="time")[as.character(time)],
                   twoways = fixef(object, effect = "individual")[as.character(id)] +
                             fixef(object, effect = "time")[as.character(time)])
      fv <- as.numeric(crossprod(t(X), beta)) + fe
    }
    if (model == "between"){
      alpha <- mean(y) - crossprod(apply(X[, -1], 2, mean), beta)
      beta <- c(alpha, beta)
      fv <- as.numeric(crossprod(t(X), beta))
    }
    if (model == "within"){
      fv <- as.numeric(crossprod(t(X), beta))
    }
  }
  else{
    fv <- as.numeric(crossprod(t(X), beta))
  }
  structure(fv, index =  index(object), class = "pseries")
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

deviance.panelmodel <- function(object, model = NULL, ...){
  if (is.null(model)) sum(resid(object)^2)
  else sum(residuals(object, model = model)^2)
} 

tss <- function(x, ...){
  UseMethod("tss")
}

tss.default <- function(x){
  var(x)*(length(x)-1)
}

tss.plm <- function(x, model = NULL){
    if (is.null(model)) model <- describe(x, "model")
    effect <- describe(x, "effect")
    if (model == "ht") model = "pooling"
    if (model == "random") theta <- x$ercomp$theta else theta <- NULL
    tss(pmodel.response(x, model = model, effect = effect, theta = theta))
}

r.squared <- function(object, model = NULL,
                      type = c('cor', 'rss', 'ess'), dfcor = FALSE){
    if (is.null(model)) model <- describe(object, "model")
    effect <- describe(object, "effect")
    type <- match.arg(type)
    if (type == 'cor'){
        y <- pmodel.response(object, model = model, effect = effect)
        haty <- fitted(object, model = model, effect = effect)
        R2 <- cor(y, haty)^2
    }
    if (type == 'rss'){
        R2 <- 1 - deviance(object, model = model) / tss(object, model = model)
    }
    if (type == 'ess'){
        haty <- fitted(object, model = model)
        mhaty <- mean(haty)
        ess <- sum( (haty - mhaty)^2)
        R2 <- ess / tss(object, model = model)
    }
    # Kevin Tappe 2015-10-19, the computation of the adjusted R2 was wrong
    if (dfcor) R2 <- 1 - (1 - R2) * (length(resid(object)) - 1) / df.residual(object)
    R2
}


residuals.plm <- function(object, model = NULL, effect = NULL, ...){
    fittedmodel <- describe(object, "model")
    if (is.null(effect)) effect <- describe(object, "effect")
    if (is.null(model)) res <- object$residuals
    else{
        beta <- coef(object)
        X <- model.matrix(object, model = model, effect = effect)
        cstX <- attr(model.matrix(object, model = "within", effect = effect), "constant")
        cstX <- union(cstX, names(which(object$alias)))
        X <- X[, ! (colnames(X) %in% cstX), drop = FALSE]
        y <- pmodel.response(object, model = model, effect = effect)
        if (model == "within" & fittedmodel != "within"){
            if (names(beta)[1] == "(Intercept)") beta <- beta[-1]
        }
        if (model != "within" & fittedmodel == "within"){
            if (colnames(X)[1] == "(Intercept)"){
                alpha <- mean(y) - crossprod(apply(X[, -1, drop = FALSE], 2, mean), beta)
                beta <- c("(Intercept)" = alpha, beta)
            }
        }
        # !YC! QDF : on a Between estimation with time dummies, coefs
       # on years dummies disapears
        comonpars <- intersect(colnames(X), names(beta))
        res <- y - as.numeric(crossprod(t(X[, comonpars, drop = FALSE]), beta[comonpars]))
#        res <- y - as.numeric(crossprod(t(X), beta))
    }
    structure(res, index =  index(object), class = c("pseries", class(res)))
}

formula.plm <- function(x, ...){
  x$formula
}

# describe function: extract characteristics of plm model
describe <- function(x,
                     what = c('model', 'effect', 'random.method',
                              'inst.method', 'transformation')){
  what <- match.arg(what)
  cl <- x$args
##   if (is.name(cl$effect)) cl$effect <- eval(cl$effect, parent.frame())
##   if (is.name(cl$model)) cl$model <- eval(cl$model, parent.frame())
##   if (is.name(cl$random.method)) cl$random.method <- eval(cl$random.method, parent.frame())
##   if (is.name(cl$inst.method)) cl$inst.method <- eval(cl$inst.method, parent.frame())
  switch(what,
         "model"          = ifelse(!is.null(cl$model), cl$model, "within"),
         "effect"         = ifelse(!is.null(cl$effect), cl$effect, "individual"),
         "random.method"  = ifelse(!is.null(cl$random.method),
                                 cl$random.method, "swar"),
         "inst.method"    = ifelse(!is.null(cl$inst.method),
                                 cl$inst.method, "bvk"),
         "transformation" = ifelse(!is.null(cl$transformation),
                                 cl$transformation, "d")
         )
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
#    plot(thex, they, col = rep(cols, each = T), pch = rep(pts, each = T), ann = FALSE, axes = FALSE)
#    axis(side = 1)
#    axis(side = 2, las = 1)
    plot(thex, they, col = rep(cols, each = T), pch = rep(pts, each = T), ann = FALSE, las = 1)
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
    # where to put the legends, depends on the sign of the ols slope
    modploted <- c(random, pooling, between, within)
    if (sum(modploted)){
        poslegend <- ifelse(beta > 0, "topleft", "topright")
        ltylegend <- c("dotted", "dashed", "dotdash", "solid")[modploted]
        leglegend <- c("random", "pooling", "between", "within")[modploted]
        legend(poslegend, lty = ltylegend, legend = leglegend)
    }
}


