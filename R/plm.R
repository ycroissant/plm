plm <-  function(formula, data, subset, na.action,
                 effect=c('individual','time','twoways'),
                 model = c('within','random','ht','between','pooling','fd'),
                 random.method = c('swar','walhus','amemiya','nerlove'),
                 inst.method = c('bvk','baltagi'),
                 index = NULL,
                 ...){
  dots <- list(...)
  # check and match the arguments
  effect <- match.arg(effect)
  if (!any(is.na(model))) model <- match.arg(model)
  random.method <- match.arg(random.method)
  inst.method <- match.arg(inst.method)

  if (!is.na(model) && model == "ht"){
    ht <- match.call(expand.dots=FALSE)
    m <- match(c("formula", "data", "subset", "na.action", "index"),names(ht),0)
    ht <- ht[c(1,m)]
    ht[[1]] <- as.name("pht")
    ht <- eval(ht, parent.frame())
    return(ht)
  }

  # for backward compatibility, allows the instrument argument
  if (!is.null(dots$instruments)){
    as.Formula(formula, dots$instruments)
    deprec.instruments <- paste("the use of the instruments argument is deprecated,",
                                "use two-part formulas instead")
    warning(deprec.instruments)
  }

  # Check whether data is a pdata.frame and if not create it
  if (inherits(data, "pdata.frame") && !is.null(index))
    warning("the index argument is ignored because data is a pdata.frame")
  if (!inherits(data, "pdata.frame")) data <- pdata.frame(data, index)
  # Create a Formula object if necessary
  if (!inherits(formula, "pFormula")) formula <- pFormula(formula)

  # in case of 2part formula, check whether the second part should be
  # updated, e.g. y~x1+x2+x3|.-x2+z becomes y~x1+x2+x3|x1+x3+z

  # length(formula)[2] because the length is now a vector of length 2
  if (length(formula)[2] == 2) formula <- expand.formula(formula)

  # eval the model.frame
  cl <- match.call()
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "na.action"), names(mf), 0)
  mf <- mf[c(1, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1]] <- as.name("model.frame")
  mf$formula <- formula
  mf$data <- data
  # eval in parent.frame() doesn't work
  #  data <- eval(mf, sys.frame(which = nframe)) 
  data <- eval(mf, parent.frame())
  # return the model.frame or estimate the model
  if (is.na(model)){
    attr(data, "formula") <- formula
    return(data)
  }
  args <- list(model = model, effect = effect,
               random.method = random.method, inst.method = inst.method)
  result <- plm.fit(formula, data, model, effect, random.method, inst.method)
  result$call <- cl
  result$args <- args
  result
}

plm.fit <- function(formula, data, model, effect, random.method, inst.method){
  # if a random effect model is estimated, compute the error components
  if (model == "random"){
    pdim <- pdim(data)
    is.balanced <- pdim$balanced
    estec <- ercomp(formula, data, effect, method = random.method)
    sigma2 <- estec$sigma2
    theta <- estec$theta
    index <- attr(data, "index")
    if (effect == "individual") cond <- index[[1]]
    if (effect == "time") cond <- index[[2]]
    if (! is.balanced) sig2one <- sigma2$one[as.character(cond)] else sig2one <- sigma2$one
    if (length(formula)[2] == 2 && effect == "twoways")
      stop("Instrumental variable random effect estimation not implemented for two-ways panels")
  }
  # extract the model.matrix and the model.response
  X <- model.matrix(formula, data, rhs = 1, model = model, effect = effect, theta = theta)
  if (ncol(X) == 0)
      stop("empty model")
  y <- pmodel.response(formula, data, model = model, effect = effect, theta = theta)
  ## extract the matrix of instruments if necessary
  if (length(formula)[2] == 2){
    W <- model.matrix(formula, data, rhs = 2, model = model, effect = effect, theta = theta)
    if (model == "random"){
      X <- X / sqrt(sigma2$idios)
      y <- y / sqrt(sigma2$idios)
      Wb <- model.matrix(formula, data, rhs = 2, model = "Between", effect = effect)
      Ww <- model.matrix(formula, data, rhs = 2, model = "within", effect = effect)
      if (ncol(Wb) < ncol(X)) stop("Insufficient number of instruments\n")
      if(inst.method == "baltagi") W <- cbind(Ww, Wb)
      if (! is.balanced) sig2one <- sigma2$one[as.character(cond)] else sig2one <- sigma2$one
      if(inst.method == "bvk"){
        # to construct the W matrix, we must take into account the fact that 
        # ncol(Wb) >= ncol(Ws)
        W <- Wb / sqrt(sig2one)
        W[, colnames(Ww)] <- W[, colnames(Ww)]+ Ww / sqrt(sigma2$idios)
      }
    }
    if (ncol(W) < ncol(X)) stop("insufficient number of instruments")
  }
  else W <- NULL
  # compute the estimation
  result <- mylm(y, X, W)
  # in case of a within estimation, correct the degrees of freedom
  df <- df.residual(result)
  vcov <- result$vcov

  if (model == "within"){
    pdim <- pdim(data)
    card.fixef <- switch(effect,
                         "individual" = pdim$nT$n,
                         "time"       = pdim$nT$T,
                         "twoways"    = pdim$nT$n + pdim$nT$T - 1
                         )
    df <- df.residual(result) - card.fixef
    vcov <- result$vcov * df.residual(result) /df
  }
  result <- list(coefficients = coef(result),
                 vcov         = vcov,
                 residuals    = resid(result),
                 df.residual  = df,
                 formula      = formula,
                 model        = data)
  if (model == "random") result$ercomp <- estec
  result$args <- list(model = model, effect = effect)
  class(result) <- c("plm", "panelmodel")
  result
}    

mylm <- function(y, X, W = NULL){
  names.X <- colnames(X)
  if (is.null(W))
      result <- lm(y ~ X - 1)
  else
      result <- twosls(y, X, W)
  if (any(is.na(coef(result)))){
    na.coef <- is.na(coef(result))
    X <- X[, !na.coef, drop = FALSE]
    if (is.null(W)) result <- lm(y ~ X - 1)
    else result <- twosls(y, X, W)
  }
  result$vcov <- vcov(result)
  names(result$coefficients) <- colnames(result$vcov) <-
    rownames(result$vcov) <- colnames(X)
  result
}
