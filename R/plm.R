plm <-  function(formula, data, subset, na.action,
                 effect=c('individual','time','twoways'),
                 model = c('within','random','ht','between','pooling','fd'),
                 random.method = c('swar','walhus','amemiya','nerlove'),
                 inst.method = c('bvk','baltagi'),
                 index = NULL,
                 ...){

  dots <- list(...)
  if (!is.null(dots$instruments)){
    formula <- as.formula(paste(deparse(formula),"|",deparse(dots$instruments[[2]])))
    deprec.instruments <- paste("the use of the instruments argument is deprecated,",
                                "use two-part formulas instead")
    warning(deprec.instruments)
  }

  # check and match the arguments
  effect <- match.arg(effect)
  if (!any(is.na(model))) model <- match.arg(model)
  random.method <- match.arg(random.method)
  inst.method <- match.arg(inst.method)
  
  # gestion des index (a revoir en relation avec pdata.frame)
  data <- plm.data(data, index)
  data[[1]] <- factor(data[[1]])
  data[[2]] <- factor(data[[2]])
  index <- names(data)[1:2]
  names(index) <- c("id","time")
  new.data.name <- "mydata"
  attr(data,"indexes") <- as.list(index)
  nframe <- length(sys.calls())
  assign(new.data.name, data, env = sys.frame(which = nframe))
#  assign(new.data.name, data, env = parent.frame())
  ##    the logic of that is that from now we know the name of the data
  ##    and will later on attach it to every variable it contains so that
  ##    the transformation functions (diff, within ...) can be used
  for (i in 1:length(mydata)){
    attr(mydata[[i]], "data") <- new.data.name
    attr(mydata[[i]], "class") <- c("pserie", attr(mydata[[i]], "class"))
  }
  
  # Create a Formula object, with index as extra attributes
  formula <- pFormula(formula, extra = index)
  # in case of 2part formula, check whether the second part should be
  # updated, e.g. y~x1+x2+x3|.-x2+z becomes y~x1+x2+x3|x1+x3+z
  if (length(formula) == 2) formula <- expand.formula(formula)
  # eval the model.frame
  cl <- match.call()
  mf <- match.call(expand.dots=FALSE)
  m <- match(c("formula", "data", "subset", "na.action"),names(mf),0)
  mf <- mf[c(1,m)]
  mf$drop.unused.levels <- TRUE
  mf$data <- as.name(new.data.name)
  mf[[1]] <- as.name("model.frame")
  mf$formula <- formula
#  mf$formula <- do.call("pFormula", list(mf$formula, extra = index))
  mf$include.extra <- TRUE
  data <- eval(mf,sys.frame(which = nframe))
#  data <- eval(mf, parent.frame())
  
  class(data) <- c("pdata.frame", "data.frame")
  if (is.na(model)) attr(data, "formula") <- formula
  if (!is.na(model)){
    result <- switch(model,
                     "within"  = plm.within (formula, data, effect),
                     "between" = plm.between(formula, data, effect),
                     "pooling" = plm.pooling(formula, data),
                     "random"  = plm.random (formula, data, effect, random.method, inst.method),
                     "ht"      = plm.ht     (formula, data),
                     "fd"      = plm.fd     (formula, data)
                     )
    result$call <- cl
    
  }
  else{
    result <- data
  }
  result
}

plm.within <- function(formula, data, effect){
  pdim <- pdim(data)
  X <- model.matrix(formula, data, part = "first", model = "within", effect = effect)
  if (ncol(X) == 0) stop("empty model: no time-varying variable")
  print(dim(X))
  y <- pmodel.response(data, part = "first", model = "within", effect = effect)
  if (length(formula) == 2){
    W <- model.matrix(formula, data, part = "second", model = "within", effect = effect)
    if (ncol(W) < ncol(X)) stop("insufficient number of instruments")
  }
  else W <- NULL
  result <- mylm(y, X, W)
  card.fixef <- switch(effect,
                       "individual" = pdim$nT$n,
                       "time"       = pdim$nT$T,
                       "twoways"    = pdim$nT$n + pdim$nT$T - 1
                       )
  df <- df.residual(result) - card.fixef
  result <- list(coefficients = coef(result),
                 vcov         = result$vcov*df.residual(result)/df,
                 residuals    = resid(result),
                 df.residual  = df,
                 formula      = formula,
                 model        = data)
  class(result) <- c("plm", "panelmodel")
  result
}    
    
plm.between <- function(formula, data, effect){
  X <- model.matrix(formula, data, part = "first", model = "between", effect = effect)
  y <- pmodel.response(data, model="between", effect = effect)
  names.X <- colnames(X)
  if (length(formula) == 2){
    W <- model.matrix(formula, data, part = "second",model = "between", effect = effect)
    if (ncol(W) < ncol(X)) stop("insufficient number of instruments")
  }
  else W <-  NULL
  result <- mylm(y, X, W)
  result <- list(coefficients = coef(result),
                 vcov         = result$vcov,
                 residuals    = resid(result),
                 df.residual  = df.residual(result),
                 formula      = formula,
                 model        = data)
  class(result) <- c("plm", "panelmodel")
  result
}    

plm.pooling <- function(formula, data){
  X <- model.matrix(formula, data, part = "first", model = "pooling")
  y <- pmodel.response(data, model = "pooling")
  if (length(formula) == 2){
    W <- model.matrix(formula, data, part = "second", model = "pooling")
    if (ncol(W) < ncol(X)) stop("insufficient number of instruments")
  }
  else W <- NULL
  result <- mylm(y, X, W)
  result <- list(coefficients = coef(result),
                 vcov         = result$vcov,
                 residuals    = resid(result),
                 df.residual  = df.residual(result),
                 formula      = formula,
                 model        = data)
  class(result) <- c("plm","panelmodel")
  result  
}

plm.fd <- function(formula, data){
  X <- model.matrix(formula, data, part = "first", model = "fd");
  y <- pmodel.response(data, model = "fd")
  N <- nrow(X)
  if (length(formula) == 2){
    W <- model.matrix(formula, data, part = "second", model = "fd")
    if (ncol(W) < ncol(X)) stop("insufficient number of instruments")
  }
  else W <- NULL
  result <- mylm(y, X, W)
  result <- list(coefficients = coef(result),
                 vcov         = result$vcov,
                 residuals    = resid(result),
                 df.residual  = df.residual(result),
                 formula      = formula,
                 model        = data)

  class(result) <- c("plm","panelmodel")
  result  
}

plm.random <- function(formula, data, effect, random.method, ivar){
  has.instruments <- length(formula) == 2
  pdim <- pdim(data)
  balanced <- pdim$balanced
  estec <- ercomp(formula, data, effect, method = random.method)
  sigma2 <- estec$sigma2
  theta <- estec$theta
  if (effect == "individual") cond <- data[["(id)"]]
  if (effect == "time") cond <- data[["(time)"]]
  if (effect == "twoways"){
    if (has.instruments) stop("Instrumental variable random effect
               estimation not implemented for two-ways panels")
    X <- model.matrix(formula, data, model = "random", effect = effect, theta = theta)
    y <- pmodel.response(data, model = "random", effect = effect, theta = theta) 
    result <- mylm(y, X, NULL)
  }
  else{
    if (length(theta) > 1) theta <- theta[as.character(cond)]
    X <- model.matrix(formula, data, model = "random", effect = effect, theta = theta)
    y <- pmodel.response(data, model = "random", effect = effect, theta = theta)
    if (!has.instruments){
      result <- mylm(y, X, NULL)
    }
    else{
      X <- X/sqrt(sigma2$idios)
      y <- y/sqrt(sigma2$idios)
      Wb <- model.matrix(formula, data, part = "second", model = "Between", effect = effect)
      Ww <- model.matrix(formula, data, part = "second", model = "within", effect = effect)
      if (ncol(Wb) < ncol(X)) stop("Insufficient number of instruments\n")
      if(ivar == "baltagi") W <- cbind(Ww,Wb)
      if (! balanced) sig2one <- sigma2$one[as.character(cond)] else sig2one <- sigma2$one
      if(ivar == "bvk"){
        # to construct the W matrix, we must take into account the fact that 
        # ncol(Wb) >= ncol(Ws)
        W <- Wb/sqrt(sig2one)
        W[,colnames(Ww)] <- W[,colnames(Ww)]+Ww/sqrt(sigma2$idios)
      }
      if (ncol(W) < ncol(X)) stop("insufficient number of instruments")
      result <- mylm(y, X, W)
    }
  }
  result <- list(coefficients = coef(result),
                 vcov         = result$vcov,
                 residuals    = resid(result),
                 df.residual  = df.residual(result),
                 formula      = formula, 
                 model        = data,
                 ercomp       = estec)
  class(result) <- c("plm","panelmodel")
  result
}

plm.ht <- function(formula, data){
  # estimate the within model without instrument

  old.formula <- formula
  formula <- pFormula(formula(formula))
  within <- plm.within(formula, data, effect = "individual")
  fixef <- fixef(within)

  formula <- old.formula
  id <- data[["(id)"]]
  time <- data[["(time)"]]
  pdim <- pdim(data)
  balanced <- pdim$balanced
  T <- pdim$nT$T
  n <- pdim$nT$n
  N <- pdim$nT$N
  Ti <- pdim$Tint$Ti

  # get the typology of the variables
  X <- model.matrix(formula, data, model = "within")
  W <- model.matrix(formula, data, model = "within", part = "second")

  exo.all <- colnames(W)
  tot.all <- colnames(X)
  tot.cst <- attr(X,"timeconst")
  tot.var <- tot.all[!(tot.cst %in% tot.all)]

  exo.cst <- attr(W,"timeconst")
  exo.var <- exo.all[!(exo.all %in% exo.cst)]
  end.cst <- tot.cst[!(tot.cst %in% exo.cst)]
  end.var <- tot.var[!(tot.var %in% exo.var)]
  if (length(end.cst) > length(exo.var)){
    stop(" The number of endogenous time-invariant variables is greater
           than the number of exogenous time varying variables\n")
    }
  
  X <- model.matrix(formula, data, model = "pooling")
  if (length(exo.var) > 0) XV <- X[ , exo.var, drop = FALSE] else XV <- NULL
  if (length(end.var) > 0) NV <- X[ , end.var, drop = FALSE] else NV <- NULL
  if (length(exo.cst) > 0) XC <- X[ , exo.cst, drop = FALSE] else XC <- NULL
  if (length(end.cst) > 0) NC <- X[ , end.cst, drop = FALSE] else NC <- NULL

  sigma2 <- list()
  sigma2$one <- 0
  sigma2$idios <- deviance(within)/(N-n)
  if (length(tot.cst) !=0 ){
    zo <- twosls(fixef[as.character(id)],cbind(1,XC,NC),cbind(1,XC,XV))
  }
  else{
    zo <- lm(fixef~1)
  }
  ssr <- deviance(zo)/N

  if(balanced){
    sigma2$id <- ssr-sigma2$idios/T
    theta <- 1-sqrt(sigma2$idios/(sigma2$idios+T*sigma2$id))
  }
  else{
    sigma2$id <- ssr-sigma2$idios/T
    theta <- 1-sqrt(sigma2$idios/(sigma2$idios+Ti*sigma2$id))
    theta <- theta[as.character(id)]
  }
  estec <- structure(list(sigma2 = sigma2, theta = theta),
                     class = "ercomp",
                     balanced = balanced,
                     effect = "individual")
  y <- pmodel.response(data, model = "random", theta = theta)
  X <- model.matrix(formula, data, model = "random", theta = theta)
  within.inst <- model.matrix(formula, data, model = "within")
  between.inst <- model.matrix(formula, data, model = "Between",
                               part = "second")[, exo.var, drop = FALSE] 
  W <- cbind(within.inst, XC, between.inst)
  result <- twosls(y,X,W)
  K <- length(data)
  ve <- lev2var(data[,-c(K-1,K)])
  varlist <- list(xv = ve[exo.var],
                  nv = ve[end.var],
                  xc = ve[exo.cst[exo.cst != "(Intercept)"]],
                  nc = ve[end.cst]
                  )
  varlist <- lapply(varlist, function(x){ names(x) <- NULL; x})

  result <- list(coefficients = coef(result),
                 vcov         = vcov(result),
                 residuals    = resid(result),
                 df.residual  = df.residual(result),
                 formula      = formula, 
                 model        = data,
                 varlist      = varlist,
                 ercomp       = estec)
  names(result$coefficients) <- colnames(result$vcov) <-
    rownames(result$vcov) <- colnames(X)
  class(result) <- c("plm","panelmodel")
  result
}

mylm <- function(y, X, W = NULL){
  names.X <- colnames(X)
  if (is.null(W)) result <- lm(y ~ X - 1)
  else result <- twosls(y, X, W)
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
