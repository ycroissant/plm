plm.list <- function(formula, data, subset, na.action,
                     effect = c("individual", "time", "twoways"),
                     model = c("within", "random", "ht", "between", "pooling", "fd"),
                     random.method = NULL, #c("swar", "walhus", "amemiya", "nerlove", "ht"),
                     inst.method = c("bvk", "baltagi"),
                     restrict.matrix = NULL,
                     restrict.rhs = NULL,
                     index = NULL,
                     ...){
  sysplm <- match.call(expand.dots = FALSE)
  if (!inherits(data, "pdata.frame")){
    odataname <- substitute(data)
    data <- pdata.frame(data, index)
    sysplm$data <- data
  }

  names.eq <- names(formula)
  # run plm for each equation of the list, store the results in a
  # list
  plm.models <- function(sysplm, amodel, ...){
    formulas <- sysplm$formula
    L <- length(formulas) - 1
    models <- vector(mode = "list", length = L)
    for (l in 2:(L+1)){
      aformula <- formulas[[l]]
      if (is.name(aformula)) aformula <- eval(aformula, parent.frame())
      else aformula <- as.formula(formulas[[l]])
      sysplm$formula <- aformula
      sysplm[[1L]] <- as.name("plm")
      sysplm$model <- amodel
      # a new pb, plm on every equation fails because of the restrict.matrix argument
      sysplm$restrict.matrix <- NULL
      models[[l-1]] <- eval(sysplm, parent.frame())
    }
    models
  }

  # Extract the model matrix and the response and transform them in
  # order to get iid errors using a furnished matrix of covariance of
  # the raw errors
  BIG <- function(X, y, W, Omega){
    S <- chol(Omega)
    N <- length(y[[1L]])
    if (!is.null(W)) BIGW <- c()
    BIGX <- c()
    BIGy <- c()
    L <- nrow(S)
    for (l in seq_len(L)){
      rowBIGy <- rep(0, N)
      rowBIGX <- c()
      if (!is.null(W)) rowBIGW <- c()
      for (m in seq_len(L)){
        rowBIGX <- cbind(rowBIGX, t(solve(S))[l, m] * X[[m]])
        if (!is.null(W)) rowBIGW <- cbind(rowBIGW, t(S)[l, m] * W[[m]])
        rowBIGy <- rowBIGy + t(solve(S))[l, m] * y[[m]]
      }
      BIGX <- rbind(BIGX, rowBIGX)
      if (!is.null(W)) BIGW <- rbind(BIGW, rowBIGW)
      BIGy <- c(BIGy, rowBIGy)
    }
    if (!is.null(W)) return(structure(list(X = BIGX, y = BIGy, W = BIGW), class = "BIG"))
    else return(structure(list(X = BIGX, y = BIGy), class = "BIG"))
  }

  # take a list of unconstrained models and a restriction matrix and
  # return a list containing the coefficients, the vcov and the
  # residuals of the constrained model ; qad version which deals with
  # lists of plm models or with models fitted by mylm (which have X, y
  # and W slots)
  systemlm <- function(object, restrict.matrix, restrict.rhs){
    if (inherits(object, "list")){
      Ucoef <- Reduce("c", lapply(object, coef))
      Uvcov <- Reduce("bdiag", lapply(object, vcov))
      X <- Reduce("bdiag", lapply(object, model.matrix))
      y <- Reduce("c", lapply(object, pmodel.response))
    }
    else{
      Ucoef <- coef(object)
      Uvcov <- vcov(object)
      X <- object$X
      y <- object$y
    }
    if (!is.null(restrict.matrix)){
      R <- restrict.matrix
      if (is.null(restrict.rhs)) restrict.rhs <- rep(0, nrow(restrict.matrix))
      XpXm1 <- solve(crossprod(X))
      Q <- XpXm1 %*% t(R) %*% solve(R %*% XpXm1 %*% t(R))
      Ccoef <- as.numeric(Ucoef - Q %*% (R %*% Ucoef - restrict.rhs))
      names(Ccoef) <- names(Ucoef)
      Cvcov <- Uvcov - Q %*% R %*% Uvcov
      Cresid <- y - X %*% Ccoef
      structure(list(coefficients = Ccoef, vcov = Cvcov, residuals = Cresid), class = "basiclm")
    }
    else{
      .resid <- Reduce("c", lapply(object, resid))
      structure(list(coefficients = Ucoef, vcov = Uvcov, residuals = .resid), class = "basiclm")
    }
  }
  models <- plm.models(sysplm, amodel = model, random.method = "kinla") #TODO NB: "kinla" does not seem to be supported anymore...
  L <- length(models)
  sys <- systemlm(models, restrict.matrix = restrict.matrix, restrict.rhs = restrict.rhs)
  Instruments <- sapply(models, function(x) length(formula(x))[2L]) > 1L

  # Get the residuals and compute the consistent estimation of the
  # covariance matrix of the residuals : Note that if there are
  # restrictions, the "restricted" residuals are used ; for random
  # effect models, two covariance matrices must be computed
  if (model == "random"){
    resid.pooling <- Reduce("cbind", lapply(models, function(x) resid(x, model = "pooling")))
    id <- index(models[[1L]])[[1L]]
    pdim <- pdim(models[[1L]])
    T <- pdim$nT$T
    N <- pdim$nT$n
    .fixef <- apply(resid.pooling, 2, tapply, id, mean)
    resid.within <- resid.pooling - .fixef[as.character(id),]
    Omega.nu <- crossprod(resid.within)/(N * (T - 1))
    Omega.eta <- crossprod(.fixef) / (N - 1)
    colnames(Omega.nu) <- rownames(Omega.nu) <- colnames(Omega.eta) <- rownames(Omega.eta) <- names.eq
    Omega.1 <- Omega.nu + T * Omega.eta
    Omega <- list(id = Omega.eta, idios = Omega.nu)
    phi <- 1 - sqrt(diag(Omega.nu)/diag(Omega.1))
    XW <- lapply(models, function(x) model.matrix(x, model = "within"))
    intercepts <- lapply(models, has.intercept)
    XB <- lapply(models, function(x) model.matrix(x, model = "Between"))
    yW <- lapply(models, function(x) pmodel.response(x, model = "within"))
    yB <- lapply(models, function(x) pmodel.response(x, model = "Between"))
    if (Instruments[1L]){
      WW <- lapply(models,
                   function(x){
                     if (length(formula(x))[2L] == 3L) rhss = c(2, 3) else rhss = 2
                     model.matrix(model.frame(x), rhs = rhss, model = "within")
                   }
                   )
      WB <- lapply(models, function(x) model.matrix(model.frame(x), rhs = 2, model = "Between"))
    }
    else WW <- WB <- NULL
    coefnames <- lapply(XB, colnames)
    BIGW <- BIG(XW, yW, WW, Omega.nu)
    BIGB <- BIG(XB, yB, WB, Omega.1)
    y <- BIGW$y + BIGB$y
    X <- BIGB$X
    # Attention, pb lorsque noms de colonnes duppliques !!
    #    X[, colnames(BIGW$X)] <- X[, colnames(BIGW$X)] + BIGW$X
    # version provisoire : emplacement des constantes
    intercepts <- c(1, cumsum(sapply(XB, ncol))[-length(XB)]+1)
    X[ , - intercepts] <- X[ , - intercepts] + BIGW$X
    m <- mylm(y, X, cbind(BIGW$W, BIGB$W))
  }
  else{
    .resid <- matrix(sys$residuals, ncol = length(models))  
    Omega <- crossprod(.resid) / nrow(.resid)
    colnames(Omega) <- rownames(Omega) <- names.eq
    X <- lapply(models, model.matrix)
    y <- lapply(models, pmodel.response)
    if (Instruments[1L])
      W <- lapply(models,
                  function(x){
                    if (length(formula(x))[2L] == 3L) rhss = c(2, 3) else rhss = 2
                    model.matrix(model.frame(x), rhs = rhss)
                  }
                  )
    else W <- NULL
    coefnames <- lapply(X, colnames)
    BIGT <- BIG(X, y, W, Omega)
    X <- BIGT$X
    m <- with(BIGT, mylm(y, X, W))
  }
  if (!is.null(restrict.matrix)){
    m <- systemlm(m, restrict.matrix = restrict.matrix, restrict.rhs = restrict.rhs)
  }
  m$model <- data
  m$coefnames <- coefnames
  m$df.residual <- length(resid(m)) - length(coef(m))
  m$vcovsys <- Omega
  m$formula <- formula
  sysplm$data <- odataname
  m$call <- sysplm
  args <- list(model = model, effect = effect, random.method = random.method)
  m$args <- args
  class(m) <- c("plm.list", "plm", "panelmodel", "lm")
  return(m)
}

#' @rdname summary.plm
#' @export
summary.plm.list <- function(object, ...){
  class(object) <- setdiff(class(object), "plm.list")
  formulas <- eval(object$call$formula)
  eqnames <- names(formulas)
  L <- length(object$coefnames)
  Ks <- c(0, cumsum(sapply(object$coefnames, length)))
  models <- vector(mode = "list", length = L)
  if (is.null(object$vcov)){
    coefTable <- coef(summary(object))
  }
  else{
    std.err <- sqrt(diag(object$vcov))
    b <- coefficients(object)
    z <- b / std.err
    p <- 2 * pt(abs(z), df = object$df.residual, lower.tail = FALSE)
    coefTable <- cbind("Estimate"   = b,
                       "Std. Error" = std.err,
                       "t-value"    = z,
                       "Pr(>|t|)"   = p)
  }
  for (l in seq_len(L)){
      models[[l]] <- coefTable[(Ks[l] + 1):Ks[l + 1] , ]
  }
  names(models) <- eqnames
  object$models <- models
  object$coefficients <- coefTable
  class(object) <- c("summary.plm.list", class(object))
  object
}


#' @rdname summary.plm
#' @export
coef.summary.plm.list <- function(object, eq = NULL, ...){
  if (is.null(eq)) object$coefficients
  else object$models[[eq]]
}

#' @rdname summary.plm
#' @export
print.summary.plm.list <- function(x, digits = max(3, getOption("digits") - 2),
                                   width = getOption("width"), ...){
  effect <- describe(x, "effect")
  model <- describe(x, "model")
  cat(paste(effect.plm.list[effect]," ",sep=""))
  cat(paste(model.plm.list[model]," Model",sep=""))
  if (model=="random"){
    ercomp <- describe(x, "random.method")
    cat(paste(" \n   (",
              random.method.list[ercomp],
              "'s transformation)\n",
              sep=""))
  }
  else{
    cat("\n")
  }
  cat("Call:\n")
  print(x$call)
  cat("\n")
  print(pdim(x))
  cat("\nEffects:\n\n")
  cat("  Estimated standard deviations of the error\n")
  if (model == "random"){
    sd <- rbind(id = sqrt(diag(x$vcovsys$id)),
                idios = sqrt(diag(x$vcovsys$idios)))
    print(sd, digits = digits)
    cat("\n")
    cat("  Estimated correlation matrix of the individual effects\n")
    corid <- x$vcovsys$id / tcrossprod(sd[1L, ])
    corid[upper.tri(corid)] <- NA
    print(corid, digits = digits, na.print = ".")
    cat("\n")
    cat("  Estimated correlation matrix of the idiosyncratic effects\n")
    coridios <- x$vcovsys$idios / tcrossprod(sd[2L, ])
    coridios[upper.tri(coridios)] <- NA
    print(coridios, digits = digits, na.print = ".")
  }
  else{
    sd <- sqrt(diag(x$vcovsys))
    print(sd, digits = digits)
    cat("\n")
    cat("\nEstimated correlation matrix of the errors\n")
    corer <- x$vcovsys / tcrossprod(sd)
    corer[upper.tri(corer)] <- NA
    print(corer, digits = digits, na.print = ".")
    cat("\n")
  }
  for (l in seq_along(x$models)){
    cat(paste("\n - ", names(x$models)[l], "\n", sep = ""))
    printCoefmat(x$models[[l]], digits = digits)
  }
  invisible(x)
}

#' @rdname plm
#' @export
print.plm.list <- function(x, digits = max(3, getOption("digits") - 2), width = getOption("width"),...){
  cat("\nModel Formulas:\n")
  for (l in seq_along(formula(x))){
    cat(paste(names(formula(x))[l], "  : ", deparse(formula(x)[[l]]), "\n", sep = ""))
  }
  cat("\nCoefficients:\n")
  print(coef(x),digits = digits)
  cat("\n")
  invisible(x)
}
