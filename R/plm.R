starX <- function(formula, data, model, rhs = 1, effect){
    apdim <- pdim(data)
    amatrix <- model.matrix(formula, data, model, effect, rhs)
    T <- length(unique(index(data, 2)))
    N <- length(unique(index(data, 1)))
    if (apdim$balanced){
        result <- Reduce("cbind",
                        lapply(seq_len(ncol(amatrix)),
                               function(x)
                               matrix(amatrix[, x], ncol = T, byrow = TRUE)[rep(1:N, each = T), ]))
    }
    else{
        Ti <- apdim$Tint$Ti
        result <- lapply(seq_len(ncol(amatrix)), function(x)
                     structure(amatrix[, x], index = index(data), class = c("pseries", class(amatrix[, x]))))
        result <- Reduce("cbind", lapply(result, as.matrix))
        result <- result[rep(1:N, times = Ti), ]
        result[is.na(result)] <- 0
    }
    result
}   

plm <-  function(formula, data, subset, na.action,
                 effect=c('individual','time','twoways'),
                 model = c('within','random','ht','between','pooling','fd'),
                 random.method = c('swar','walhus','amemiya','nerlove', 'kinla'),
                 random.dfcor = NULL,
                 inst.method = c('bvk','baltagi', 'am', 'bmc'),
                 restrict.matrix = NULL,
                 restrict.rhs = NULL,
                 index = NULL,
                 ...){

  nframe <- length(sys.calls())
  is.a.list <- class(formula)[1] == "list"
  
  if (is.a.list){
    plmlist <- match.call(expand.dots = FALSE)
    plmlist[[1]] <- as.name("plm.list")
#    plmlist <- eval(plmlist, parent.frame())
    plmlist <- eval(plmlist, sys.frame(which = nframe))

    return(plmlist)
  }

  dots <- list(...)
  # check and match the arguments
  effect <- match.arg(effect)
  if (!any(is.na(model))) model <- match.arg(model)
  random.method <- match.arg(random.method)
  inst.method <- match.arg(inst.method)

  # deprecated, pht is no longer maintained
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
  orig_rownames <- row.names(data)
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
  
  # preserve original row.names for data [also fancy rownames]; so functions like
  # pmodel.response(), model.frame(), model.matrix(), residuals() return the original row.names
  row.names(data) <- orig_rownames
  # return the model.frame or estimate the model
  if (is.na(model)){
    attr(data, "formula") <- formula
    return(data)
  }
  args <- list(model = model, effect = effect,
               random.method = random.method, random.dfcor = random.dfcor,
               inst.method = inst.method)
  result <- plm.fit(formula, data, model, effect, random.method, random.dfcor, inst.method)
  result$call <- cl
  result$args <- args
  result
}

plm.fit <- function(formula, data, model, effect, random.method, random.dfcor, inst.method){
  # if a random effect model is estimated, compute the error components
  if (model == "random"){
    pdim <- pdim(data)
    is.balanced <- pdim$balanced
    estec <- ercomp(formula, data, effect, method = random.method, dfcor = random.dfcor)
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
  if (ncol(X) == 0) stop("empty model")
  y <- pmodel.response(formula, data, model = model, effect = effect, theta = theta)
  ## extract the matrix of instruments if necessary
  if (length(formula)[2] > 1){
      if (length(formula)[2] == 2) W <- model.matrix(formula, data, rhs = 2, model = model, effect = effect, theta = theta)
      else W <- model.matrix(formula, data, rhs = c(2, 3), model = model, effect = effect, theta = theta)
      if (model == "random" && inst.method != "bvk"){
          X <- X / sqrt(sigma2$idios)
          y <- y / sqrt(sigma2$idios)
          W1 <- model.matrix(formula, data, rhs = 2, model = "within", effect = effect, theta = theta)
          B1 <- model.matrix(formula, data, rhs = 2, model = "Between", effect = effect, theta = theta)
          StarW1 <- starX(formula, data, rhs = 2, model = "within", effect = effect)
          if (length(formula)[2] == 3){
              W2 <- model.matrix(formula, data, rhs = 3, model = "within", effect = effect, theta = theta)
              StarW2 <- starX(formula, data, rhs = 3, model = "within", effect = effect)
          }
          else W2 <- StarW2 <- NULL
          if (inst.method == "baltagi") W <- cbind(W1, W2, B1)
          if (inst.method == "am") W <- cbind(W1, W2, B1, StarW1)
          if (inst.method == "bmc") W <- cbind(W1, W2, B1, StarW1, StarW2)
          # quick and dirty trick to remove columns of 0
          zerovars <- apply(W, 2, function(x) max(abs(x), na.rm = TRUE)) < 1E-5
          W <- W[, !zerovars]
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
    vcov <- result$vcov * df.residual(result) / df
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
  result$X <- X
  result$y <- y
  result$W <- W
  names(result$coefficients) <- colnames(result$vcov) <-
    rownames(result$vcov) <- colnames(X)
  result
}

