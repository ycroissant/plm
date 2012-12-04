plm.list <- function(formula, data, subset, na.action,
                     effect=c('individual','time','twoways'),
                     model = c('within','random','ht','between','pooling','fd'),
                     random.method = c('swar','walhus','amemiya','nerlove', 'kinla'),
                     inst.method = c('bvk','baltagi'),
                     index = NULL, theta = NULL,
                     ...){

  sysplm <- match.call(expand.dots = FALSE)
  plm.models <- function(sysplm, amodel, ...){
    # run plm for each equations of the list, store the results in a
    # list
    formulas <- sysplm$formula
    L <- length(formulas) - 1
    models <- vector(mode = "list", length = L)
    for (l in 2:(L+1)){
      aformula <- as.formula(formulas[[l]])
      sysplm$formula <- aformula
      sysplm[[1]] <- as.name("plm")
      sysplm$model <- amodel
      models[[l-1]] <- eval(sysplm, parent.frame())
    }
    models
  }
  models <- plm.models(sysplm, amodel = model, random.method = "kinla")
  Instruments <- sapply(models, function(x) length(formula(x))[2]) > 1

  # Get the residuals and compute the consistent estimation of the
  # covariance matrix of the residuals
  if (model == "random"){
    resid.pooling <- Reduce("cbind", lapply(models, function(x) resid(x, model = "pooling")))
    id <- index(models[[1]])[[1]]
    T <- pdim(models[[1]])$nT$T
    N <- pdim(models[[1]])$nT$n
    .fixef <- apply(resid.pooling, 2, tapply, id, mean)
    resid.within <- resid.pooling - .fixef[as.character(id),]
    Omega.nu <- crossprod(resid.within)/(N * (T - 1))
    Omega.eta <- crossprod(.fixef) / (N - 1)
    Omega.1 <- Omega.nu + T * Omega.eta
    phi <- 1 - sqrt(diag(Omega.nu)/diag(Omega.1))
    XW <- lapply(models, function(x) model.matrix(x, model = "within"))
    XB <- lapply(models, function(x) model.matrix(x, model = "Between"))
    yW <- lapply(models, function(x) pmodel.response(x, model = "within"))
    yB <- lapply(models, function(x) pmodel.response(x, model = "Between"))
    if (Instruments[1]){
      WW <- lapply(models,
                   function(x){
                     if (length(formula(x))[2] == 3) rhss = c(2, 3) else rhss = 2
                     model.matrix(formula(x), model.frame(x), rhs = rhss, model = "within")
                   }
                   )
      WB <- lapply(models, function(x) model.matrix(formula(x), model.frame(x), rhs = 2, model = "Between"))
    }
    else WW <- WB <- NULL
  }
  else{
    .resid <- Reduce("cbind", lapply(models, resid))
    Omega <- crossprod(.resid) / nrow(.resid)
    X <- lapply(models, model.matrix)
    y <- lapply(models, pmodel.response)
    if (Instruments[1])
      W <- lapply(models,
                  function(x){
                    if (length(formula(x))[2] == 3) rhss = c(2, 3) else rhss = 2
                    model.matrix(formula(x), model.frame(x), rhs = rhss)
                  }
                  )
    else W <- NULL
  }
  # Extract the model matrix and the response and transform them in
  # order to get clean errors
  BIG <- function(X, y, W, Omega){
    S <- chol(Omega)
    N <- length(y[[1]])
    if (!is.null(W)) BIGW <- c()
    BIGX <- c()
    BIGy <- c()
    L <- nrow(S)
    for (l in 1:L){
      rowBIGy <- rep(0, N)
      rowBIGX <- c()
      if (!is.null(W)) rowBIGW <- c()
      for (m in 1:L){
        rowBIGX <- cbind(rowBIGX, t(solve(S))[l, m] * X[[m]])
        if (!is.null(W)) rowBIGW <- cbind(rowBIGW, t(S)[l, m] * W[[m]])
        rowBIGy <- rowBIGy + t(solve(S))[l, m] * y[[m]]
      }
      BIGX <- rbind(BIGX, rowBIGX)
      if (!is.null(W)) BIGW <- rbind(BIGW, rowBIGW)
      BIGy <- c(BIGy, rowBIGy)
    }
    if (!is.null(W)) return(list(X = BIGX, y = BIGy, W = BIGW))
    else return(list(X = BIGX, y = BIGy))
  }
  if (model == "random"){
    coefnames <- lapply(XB, colnames)
    BIGW <- BIG(XW, yW, WW, Omega.nu)
    BIGB <- BIG(XB, yB, WB, Omega.1)
    y <- BIGW$y + BIGB$y
    X <- BIGB$X
    X[, colnames(BIGW$X)] <- X[, colnames(BIGW$X)] + BIGW$X
    m <- mylm(y, X, cbind(BIGW$W, BIGB$W))
  }
  else{
    coefnames <- lapply(X, colnames)
    BIGT <- BIG(X, y, W, Omega)
    m <- with(BIGT, mylm(y, X, W))
  }
  m$coefnames <- coefnames
  m$call <- sysplm
  class(m) <- c("plm.list", "lm")
  return(m)
}


summary.plm.list <- function(object, ...){
  class(object) <- class(object)[-1]
  formulas <- eval(object$call$formula)
  eqnames <- names(formulas)
  L <- length(object$coefnames)
  Ks <- c(0, cumsum(sapply(object$coefnames, length)))
  result <- vector(mode = "list", length = L)
  CoefNames <- coef(summary(object))
  for (l in 1:L){
    result[[l]] <- CoefNames[(Ks[l] + 1):Ks[l + 1] , ]
  }
  names(result) <- eqnames
  class(result) <- "summary.plm.list"
  result
}

print.summary.plm.list <- function(x, ...){
  for (l in 1:length(x)){
    cat(paste("\n * ", names(x)[l], "\n", sep = ""))
    print(x[[l]])
  }
  invisible(x)
}
          
