pht <-  function(formula, data, subset, na.action, index = NULL, ...){
  data.name <- paste(deparse(substitute(data)))
  cl <- match.call(expand.dots = TRUE)
  mf <- match.call()
  # compute the model.frame using plm with model = NA
  mf[[1]] <- as.name("plm")
  mf$model <- NA
  data <- eval(mf,parent.frame())
  # estimate the within model without instrument and extract the fixed
  # effects
  old.formula <- formula
  formula <- pFormula(formula)
  formula <- formula(formula, rhs = 1, lhs = 1)
  mf$model = "within"
  mf$formula <- formula
  within <- eval(mf, parent.frame())
  fixef <- fixef(within)
  formula <- old.formula
  id <- index(data, "id")
  time <- index(data, "time")
  pdim <- pdim(data)
  balanced <- pdim$balanced
  T <- pdim$nT$T
  n <- pdim$nT$n
  N <- pdim$nT$N
  Ti <- pdim$Tint$Ti
  
  # get the typology of the variables
  formula <- pFormula(old.formula)
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
    zo <- twosls(fixef[as.character(id)], cbind(XC,NC), cbind(XC,XV), TRUE)
  else zo <- lm(fixef~1)

  sigma2 <- list()
  sigma2$one <- 0
  sigma2$idios <- deviance(within)/(N-n)
  sigma2$one <- deviance(zo) / n
  
  if(balanced){
    sigma2$id <- (sigma2$one - sigma2$idios)/ T
    theta <- 1 - sqrt(sigma2$idios / sigma2$one)
  }
  else{
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
  between.inst <- model.matrix(formula, data, model = "Between",
                               rhs = 2)[, exo.var, drop = FALSE] 
  W <- cbind(within.inst, XC, between.inst)
  result <- twosls(y,X,W)
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

summary.pht <- function(object,...){
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
  cat("\nResiduals :\n")
  save.digits <- unlist(options(digits = digits))
  on.exit(options(digits = save.digits))
  print(sumres(x))
  
  cat("\nCoefficients :\n")
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
