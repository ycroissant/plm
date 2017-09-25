## some deprecated functions

# plm.data() is now deprecated (since February 2017).
# It is kept due to backward compatibility, especially for package 'systemfit'.
# While plm.data() was a 'full function' once, it now is now using pdata.frame()
# and re-works the properties of the "plm.dim" objects original created by the
# 'full' plm.data() function.
# The 'full' plm.data() function is kept non-exported as plm.data_depr_orig
# due to reference and testing (see tests/test_plm.data.R)

plm.data <- function(x, indexes = NULL) {

  .Deprecated(new = "pdata.frame", msg = "'plm.data' is deprecated, use 'pdata.frame'",
              old = "plm.data")

  # the class "plm.dim" (which plm.data creates) deviates from class "pdata.frame":
  #    * always contains the indexes (in first two columns (id, time))
  #    * does not have fancy rownames
  #    * always coerces strings to factors
  #    * does not have index attribute
  #    * leaves in constant columns (albeit the 'full' implementation printed a msg about dropping those ...)
  #
  #  -> call pdata.frame accordingly and adjust afterwards
  orig_col_order <- colnames(x)
  
  x <- pdata.frame(x, index = indexes,
                      drop.index = FALSE,
                      row.names = FALSE,
                      stringsAsFactors = TRUE,
                      replace.non.finite = TRUE,
                      drop.NA.series = TRUE,
                      drop.const.series = FALSE)

  # determine position and names of index vars in pdata.frame
  pos_indexes <- pos.index(x)
  names_indexes <- names(pos_indexes) # cannot take from arg 'indexes' as it could be only the index for id
  
  # the class "plm.dim" does not have the index attribute -> remove
  attr(x, "index") <- NULL
  # remove class 'pdata.frame' to prevent any dispatching of special methods on object x
  class(x) <- setdiff(class(x), "pdata.frame")
  
  # class "plm.dim" always has indexes in first two columns (id, time)
  # while "pdata.frame" leaves the index variables at it's place (if not dropped at all with drop.index = T)
  x <- x[ , c(names_indexes, setdiff(orig_col_order, names_indexes))]
  
  # set class
  class(x) <- c("plm.dim", "data.frame")
  return(x)
}


### convert data to plm format
### Author:
### Amendments by Ott Toomet

plm.data_depr_orig <- function(x, indexes = NULL){
  ## this is the original old full plm.data() function kept as reference for testing purposes (non-exported)
  if (is.null(indexes)){
    id <- NULL
    time <- NULL
  }
  if (length(indexes) == 1){
    id <- indexes
    time <- NULL
  }
  if (length(indexes) == 2){
    id <- indexes[1]
    time <- indexes[2]
  }
  if (is.null(id) & is.null(time)){
    id.name <- names(x)[1]
    time.name <- names(x)[2]
  }
  else{
    id.name <- id
    time.name <- time
  }
  data.name <- paste(deparse(substitute(x)))
  # coerce character vectors to factors
  x.char <- names(x)[sapply(x,is.character)]
  for (i in x.char){
    x[[i]] <- factor(x[[i]])
  }

  # replace Inf by NA
  for (i in names(x)) x[[i]][!is.finite(x[[i]])] <- NA

  # check and remove complete NA series
  na.check <- sapply(x,function(x) sum(!is.na(x))==0)
  na.serie <- names(x)[na.check]
  if (length(na.serie)>0){
    if (length(na.serie)==1){
      cat(paste("series",na.serie,"is NA and has been removed\n"))
    }
    else{
      cat(paste("series",paste(na.serie,collapse=", "),"are NA and have been removed\n"))
    }
  }
  x <- x[,!na.check]
  ## Which columns are constants?
  cst.check <- sapply(x, function(x) all(x[!is.na(x)] == (x[!is.na(x)])[1]))
                           # any NA-component equal to the first non-NA component
  cst.serie <- names(x)[cst.check]
  if (length(cst.serie)>0){
    if (length(cst.serie)==1){
      cat(paste("series",cst.serie,"is constant and has been removed\n"))
    }
    else{
      cat(paste("series",paste(cst.serie,collapse=", "),"are constants and have been removed\n"))
    }
  }
#  x <- x[,!cst.check]
  if(is.numeric(id.name)){
    if(!is.null(time.name)){warning("The time argument will be ignored\n")}
    N <- nrow(x)
    if( (N%%id.name)!=0){
      stop("unbalanced panel, the id variable should be indicated\n")
    }
    else{
      T <- N%/%id.name
      n <- N%/%T
      time <- rep(1:T,n)
      id <- rep(seq(1:n),rep(T,n))
      id.name <- "id"
      time.name <- "time"
      x[[id.name]] <- id <- as.factor(id)
      x[[time.name]] <- time <- as.factor(time)
    }
  }
  else{
    if (!id.name %in% names(x))
      stop(paste("variable ",id.name," does not exist",sep="")
           )
    if (is.factor(x[[id.name]])){
      id <- x[[id.name]] <- x[[id.name]][drop=T]
    }
    else{
      id <- x[[id.name]] <- as.factor(x[[id.name]])
    }

    if (is.null(time.name)){
      Ti <- table(id)
      n <- length(Ti)
      time <- c()
      for (i in 1:n){
        time <- c(time,1:Ti[i])
      }
      time.name <- "time"
      time <- x[[time.name]] <- time <- as.factor(time)
    }
    else{
    if (!time.name %in% names(x))
      stop(paste("variable ",time.name," does not exist",sep="")
           )
      if (is.factor(x[[time.name]])){
        time <- x[[time.name]] <- x[[time.name]][drop=T]
      }
      else{
        time <- x[[time.name]] <- as.factor(x[[time.name]])
      }
    }
  }
  x <- x[order(id,time),]
  indexes <- list(id=id.name,time=time.name)
  class(indexes) <- "indexes"
  var.names <- names(x)
  for (i in names(x)){
    if(is.factor(x[[i]])){
      if (length(unique(x[[i]])) < length(levels(x[[i]]))){
        x[[i]] <- x[[i]][,drop=TRUE]
      }
    }
  }
  posindexes <- match(c(id.name,time.name),names(x))
  x <- data.frame(x[posindexes],x[-posindexes])
  attr(x,"class") <- c("plm.dim","data.frame")
  x
}


data2plm.data <- function(data, indexes = NULL){
  data <- plm.data(data, indexes)
  id.name <- names(data)[1]
  time.name <- names(data)[2]
  list(data = data, id.name = id.name, time.name = time.name)
}

pht <-  function(formula, data, subset, na.action, model = c("ht", "am", "bms"), index = NULL, ...){

  cl <- match.call(expand.dots = TRUE)
  mf <- match.call()
  
  if (length(model) == 1 && model == "bmc") {
    # accept "bmc" (a long-standing typo) for Breusch-Mizon-Schmidt due to backward compatibility
    model <- "bms"
    warning("Use of model = \"bmc\" discouraged, set to \"bms\" for Breusch-Mizon-Schmidt instrumental variable transformation")
  }
  model <- match.arg(model)
  # compute the model.frame using plm with model = NA
  mf[[1]] <- as.name("plm")
  mf$model <- NA
  data <- eval(mf, parent.frame())
  # estimate the within model without instrument and extract the fixed
  # effects
    formula <- pFormula(formula)
  if (length(formula)[2] == 1) stop("a list of exogenous variables should be provided")
  else formula <- expand.formula(formula)
  mf$model = "within"
    mf$formula <- formula(formula, rhs = 1)
    within <- eval(mf, parent.frame())
  fixef <- fixef(within)
  id <- index(data, "id")
  time <- index(data, "time")
  pdim <- pdim(data)
  balanced <- pdim$balanced
  T <- pdim$nT$T
  n <- pdim$nT$n
  N <- pdim$nT$N
  Ti <- pdim$Tint$Ti
  # get the typology of the variables
  X <- model.matrix(formula, data, rhs = 1, model = "within")
  W <- model.matrix(formula, data, rhs = 2, model = "within")
  exo.all <- colnames(W)
  all.all <- colnames(X)
  edo.all <- all.all[!(all.all %in% exo.all)]
  all.cst <- attr(X, "constant")
  exo.cst <- attr(W, "constant")
  exo.var <- exo.all[!(exo.all %in% exo.cst)]
  edo.cst <- all.cst[!(all.cst %in% exo.cst)]
  edo.var <- edo.all[!(edo.all %in% edo.cst)]
  if (length(edo.cst) > length(exo.var)){
    stop(" The number of endogenous time-invariant variables is greater
           than the number of exogenous time varying variables\n")
    }
  
  X <- model.matrix(formula, data, model = "pooling", rhs = 1, lhs = 1)
  if (length(exo.var) > 0) XV <- X[ , exo.var, drop = FALSE] else XV <- NULL
  if (length(edo.var) > 0) NV <- X[ , edo.var, drop = FALSE] else NV <- NULL
  if (length(exo.cst) > 0) XC <- X[ , exo.cst, drop = FALSE] else XC <- NULL
  if (length(edo.cst) > 0) NC <- X[ , edo.cst, drop = FALSE] else NC <- NULL

  if (length(all.cst) !=0 )
    zo <- twosls(fixef[as.character(id)], cbind(XC, NC), cbind(XC, XV), TRUE)
  else zo <- lm(fixef ~ 1)

  sigma2 <- list()
  sigma2$one <- 0
  sigma2$idios <- deviance(within)/ (N - n)
  sigma2$one <- deviance(zo) / n
  
  if(balanced){
    sigma2$id <- (sigma2$one - sigma2$idios)/ T
    theta <- 1 - sqrt(sigma2$idios / sigma2$one)
  }
  else{
    # for unbalanced data, the harmonic mean of the Ti's is used ; why ??
    barT <- n / sum(1 / Ti)
    sigma2$id <- (sigma2$one - sigma2$idios) / barT
    theta <- 1 - sqrt(sigma2$idios / (sigma2$idios + Ti * sigma2$id))
    theta <- theta[as.character(id)]
  }
  estec <- structure(list(sigma2 = sigma2, theta = theta),
                     class = "ercomp",
                     balanced = balanced,
                     effect = "individual")
  y <- pmodel.response(data, model = "random", theta = theta)
  X <- model.matrix(formula, data, model = "random", theta = theta)
  within.inst <- model.matrix(formula, data, model = "within")

  if (model == "ht"){
    between.inst <- model.matrix(formula, data, model = "Between",
                                 rhs = 2)[, exo.var, drop = FALSE]
    W <- cbind(within.inst, XC, between.inst)
  }
  if (model == "am"){
    Vx <- model.matrix(formula, data, model = "pooling",
                       rhs = 2)[, exo.var, drop = FALSE]
    if (balanced){
      # Plus rapide mais pas robuste au non cylindre
      Vxstar <- Reduce("cbind",
                       lapply(seq_len(ncol(Vx)),
                              function(x)
                              matrix(Vx[, x], ncol = T, byrow = TRUE)[rep(1:n, each = T), ]))
    }
    else{
      Xs <- lapply(seq_len(ncol(Vx)), function(x)
                   structure(Vx[, x], index = index(data), class = c("pseries", class(Vx[, x]))))
      Vx2 <- Reduce("cbind", lapply(Xs, as.matrix))
      Vxstar <- Vx2[rep(1:n, times = Ti), ]
      Vxstar[is.na(Vxstar)] <- 0
    }
    W <- cbind(within.inst, XC, Vxstar)
  }
  if (model == "bms"){
    between.inst <- model.matrix(formula, data, model = "Between",
                                 rhs = 2)[, exo.var, drop = FALSE]
    Vx <- within.inst
    if (balanced){
      # Plus rapide mais pas robuste au non cylindre
      Vxstar <- Reduce("cbind",
                       lapply(seq_len(ncol(Vx)),
                              function(x)
                              matrix(Vx[, x], ncol = T, byrow = TRUE)[rep(1:n, each = T), ]))
    }
    else{
      Xs <- lapply(seq_len(ncol(Vx)), function(x)
                   structure(Vx[, x], index = index(data), class = c("pseries", class(Vx[, x]))))
      Vx2 <- Reduce("cbind", lapply(Xs, as.matrix))
      Vxstar <- Vx2[rep(1:n, times = Ti), ]
      Vxstar[is.na(Vxstar)] <- 0
    }
    W <- cbind(within.inst, XC, between.inst, Vxstar)
  }
    
  result <- twosls(y, X, W)
  K <- length(data)
  ve <- lev2var(data)
  varlist <- list(xv = unique(ve[exo.var]),
                  nv = unique(ve[edo.var]),
                  xc = unique(ve[exo.cst[exo.cst != "(Intercept)"]]),
                  nc = unique(ve[edo.cst])
                  )
  varlist <- lapply(varlist, function(x){ names(x) <- NULL; x})
  result <- list(coefficients = coef(result),
                 vcov         = vcov(result),
                 residuals    = resid(result),
                 df.residual  = df.residual(result),
                 formula      = formula, 
                 model        = data,
                 varlist      = varlist,
                 ercomp       = estec,
                 call         = cl,
                 args         = list(model = "ht"))
  names(result$coefficients) <- colnames(result$vcov) <-
    rownames(result$vcov) <- colnames(X)
  class(result) <- c("pht", "plm", "panelmodel")
  result
}

summary.pht <- function(object, ...){
  object$fstatistic <- pwaldtest(object, test = "F")
  # construct the table of coefficients
  std.err <- sqrt(diag(vcov(object)))
  b <- coefficients(object)
  z <- b/std.err
  p <- 2*pnorm(abs(z), lower.tail = FALSE)
  object$coefficients <- cbind("Estimate"   = b,
                               "Std. Error" = std.err,
                               "z-value"    = z,
                               "Pr(>|z|)"   = p)
  class(object) <- c("summary.pht", "pht", "plm", "panelmodel")
  object
}

print.summary.pht <- function(x, digits = max(3, getOption("digits") - 2),
                              width = getOption("width"), subset = NULL, ...){
  formula <- formula(x)
  has.instruments <- (length(formula)[2] == 2)
  effect <- describe(x, "effect")
  model <- describe(x, "model")
  cat(paste(effect.plm.list[effect]," ",sep=""))
  cat(paste(model.plm.list[model]," Model",sep=""))
  cat("\nCall:\n")
  print(x$call)

  #    cat("\nTime-Varying Variables: ")
  names.xv <- paste(x$varlist$xv,collapse=", ")
  names.nv <- paste(x$varlist$nv,collapse=", ")
  names.xc <- paste(x$varlist$xc,collapse=", ")
  names.nc <- paste(x$varlist$nc,collapse=", ")
  cat(paste("\nT.V. exo  : ",names.xv,"\n", sep = ""))
  cat(paste("T.V. endo : ", names.nv,"\n",sep = ""))
  #    cat("Time-Invariant Variables: ")
  cat(paste("T.I. exo  : ", names.xc, "\n", sep= ""))
  cat(paste("T.I. endo : ", names.nc, "\n", sep= ""))
  cat("\n")
  pdim <- pdim(x)
  print(pdim)
  cat("\nEffects:\n")
  print(x$ercomp)
  cat("\nResiduals:\n")
  save.digits <- unlist(options(digits = digits))
  on.exit(options(digits = save.digits))
  print(sumres(x))
  
  cat("\nCoefficients:\n")
  if (is.null(subset)) printCoefmat(coef(x), digits = digits)
  else printCoefmat(coef(x)[subset, , drop = FALSE], digits = digits)
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

dynformula <- function(formula, lag.form = NULL, diff.form = NULL, log.form = NULL){

  # for backward compatibility, accept a list argument and coerce it
  # to a vector
  if (!is.null(diff.form) && !is.list(diff.form)) diff.form <- as.list(diff.form)
  if (!is.null(log.form) && !is.list(log.form)) log.form <- as.list(log.form)

  
  # exo / endog are the names of the variable
  # has.int has.resp  TRUE if the formula has an intercept and a response
  # K is the number of exogenous variables
  exo <- attr(terms(formula),"term.labels")
  has.int <- attr(terms(formula),"intercept") == 1
  if(length(formula)==3){
    endog <- deparse(formula[[2]])
    has.resp <- TRUE
  }
  else{
    endog <- NULL
    has.resp <- FALSE
  }
  K <- length(exo)

  # use the create.list function to create the lists with the relevant
  # default values
  lag.form <- create.list(lag.form, K, has.int, has.resp, endog, exo, 0)
  diff.form <- unlist(create.list(diff.form, K, has.int, has.resp, endog, exo, FALSE))
  log.form <- unlist(create.list(log.form, K, has.int, has.resp, endog, exo, FALSE))

  structure(formula, class = c("dynformula","formula"), lag = lag.form,
            diff = diff.form, log = log.form, var = c(endog,exo))
}


formula.dynformula <- function(x, ...){
  log.form <- attr(x, "log")
  lag.form <- attr(x, "lag")
  diff.form <- attr(x, "diff")
  has.resp <- length(x) == 3
  exo <- attr(x, "var")
  if (has.resp){
    endog <- exo[1]
    exo <- exo[-1]
  }
  has.int <- attr(terms(x), "intercept") == 1
  chexo <- c()
  if (has.resp){
    if (log.form[1]) endog <- paste("log(",endog,")",sep="")
    if (diff.form[1]) endog <- paste("diff(",endog,")",sep="")
    if (length(lag.form[[1]]) == 1 && lag.form[[1]]!=0) lag.form[[1]] <- c(1,lag.form[[1]])
    if (!(length(lag.form[[1]]) == 1 && lag.form[[1]]==0))
      chexo <- c(chexo,write.lags(endog,lag.form[[1]],diff.form[1]))
  }
  for (i in exo){
    lag.formi <- lag.form[[i]]
    diff.formi <- diff.form[i]
    if (log.form[[i]]) i <- paste("log(",i,")",sep="")
    chexo <- c(chexo,write.lags(i,lag.formi,diff.formi))
  }
  chexo <- paste(chexo,collapse="+")
  if (has.resp){
    formod <- as.formula(paste(endog,"~",chexo,sep=""))
  }
  else{
    formod <- as.formula(paste("~",chexo,sep=""))
  }
  if (!has.int) formod <- update(formod,.~.-1)
  formod
}

create.list <- function(alist, K, has.int, has.resp, endog, exo, default){
  # if alist is NULL, create a list of 0
  if (is.null(alist)) alist <- rep(list(default), K+has.resp)
  # if alist is note a list, coerce it
  if (!is.list(alist)) alist <- list(alist)

  if (!is.null(names(alist))){
  # case where (at least) some elements are named
    nam <- names(alist) # vector of names of elements
    oalist <- alist  # copy of the alist provided
    notnullname <- nam[nam != ""]
    if (any (nam == "")){
    # case where one element is unnamed, and therefore is the default
      unnamed <- which(nam == "")
      if (length(unnamed) > 1) stop("Only one unnamed element is adminited\n")
      default <- alist[[unnamed]]
    }
    else{
    # case where there are no unnamed elements, the default is 0
      default <- default
    }
    alist <- rep(list(default), K+has.resp)
    names(alist) <- c(endog, exo)
    alist[notnullname] <- oalist[notnullname]
  }
  else{
  # case where there are no names, in this case the relevant length is
  # whether 1 or K+1
    if (length(alist) == 1) alist <- rep(alist, c(K+has.resp))
    else if (!length(alist) %in% c(K+has.resp)) stop("irrelevant length for alist\n")
  }
  names(alist) <- c(endog,exo)
  alist
}

write.lags <- function(name,lags,diff){
  lags <- switch(length(lags),
                 "1"=c(0,lags),
                 "2"=sort(lags),
                 stop("lags should be of length 1 or 2\n")
                 )
  lag.string <- ifelse(diff,"diff","lag")
  chlag <- c()
  if (lags[2]!=0){
    lags <- lags[1]:lags[2]
    for (i in lags){
      if (i==0){
        if (diff) chlag <- c(chlag,paste("diff(",name,")")) else chlag <- c(chlag,name)
      }
      else{
        ichar <- paste(i)
        chlag <- c(chlag,paste(lag.string,"(",name,",",i,")",sep=""))
      }
    }
    ret <- paste(chlag,collapse="+")
  }
  else{
    if (diff) chlag <- paste("diff(",name,")") else chlag <- name
    ret <- chlag
  }
  ret
}   


print.dynformula <- function(x,...){
  print(formula(x), ...)
}
