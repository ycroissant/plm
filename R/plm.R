starX <- function(formula, data, model, rhs = 1, effect){
    apdim <- pdim(data)
    amatrix <- model.matrix(formula, data, model, effect, rhs)
    T <- length(unique(index(data, 2)))
    N <- length(unique(index(data, 1)))
    if (apdim$balanced){
        result <- Reduce("cbind",
                        lapply(seq_len(ncol(amatrix)),
                               function(x)
                               matrix(amatrix[, x], 
                                      ncol = T, byrow = TRUE)[rep(1:N, each = T), ]))
    }
    else{
        Ti <- apdim$Tint$Ti
        result <- lapply(seq_len(ncol(amatrix)), function(x)
                     structure(amatrix[, x], index = index(data), 
                               class = c("pseries", class(amatrix[, x]))))
        result <- Reduce("cbind", lapply(result, as.matrix))
        result <- result[rep(1:N, times = Ti), ]
        result[is.na(result)] <- 0
    }
    result
}   

plm <- function(formula, data, subset, weights, na.action,
                effect = c("individual", "time", "twoways", "nested"),
                model = c("within", "random", "ht", "between", "pooling", "fd"),
                random.method = NULL,
                random.models = NULL,
                random.dfcor = NULL,
                inst.method = c("bvk", "baltagi", "am", "bmc"),
                restrict.matrix = NULL,
                restrict.rhs = NULL,
                index = NULL,
                ...){
    # if the first argument is a list (of formulas), then call plmlist and exit
    if (is.list(formula)){
        plmlist <- match.call(expand.dots = FALSE)
        plmlist[[1]] <- as.name("plm.list")
        # eval in nframe and not the usual parent.frame(), relevant?
        nframe <- length(sys.calls())
        plmlist <- eval(plmlist, sys.frame(which = nframe))
        return(plmlist)
    }
  
    if ((!is.null(restrict.matrix) || !is.null(restrict.rhs)) && !is.list(formula)) {
      stop(paste0("arguments 'restrict.matrix' and 'restrict.rhs' not yet implemented ",
                  "for single equations"))
    }
    
    dots <- list(...)
    # check and match the arguments
    effect <- match.arg(effect)
    # note that model can be NA, in this case the model.frame is
    # returned
    if (! anyNA(model)) model <- match.arg(model)
    if (! anyNA(model) & effect == "nested") model <- "random"
    inst.method <- match.arg(inst.method)
  
    # input checks for FD model: give informative error messages as
    # described in footnote in vignette
    if (! is.na(model) && model == "fd") {
        if (effect == "time") stop(paste("effect = \"time\" for first-difference model", 
                                         "meaningless because cross-sections do not", 
                                         "generally have a natural ordering"))
        if (effect == "twoways") stop(paste("effect = \"twoways\" is not defined",  
                                            "for first-difference models"))
    }

    # deprecated section:
    # pht is no longer maintained
    if (! is.na(model) && model == "ht"){
        ht <- match.call(expand.dots=FALSE)
        m <- match(c("formula", "data", "subset", "na.action", "index"), names(ht), 0)
        ht <- ht[c(1,m)]
        ht[[1]] <- as.name("pht")
        ht <- eval(ht, parent.frame())
        return(ht)
    }
    
    # the use of the instrument argument is deprecated, use 2-part Formulas instead
    if (!is.null(dots$instruments)){
        formula <- as.Formula(formula, dots$instruments)
        deprec.instruments <- paste("the use of the instruments argument is deprecated,",
                                    "use two-part formulas instead")
        warning(deprec.instruments)
    }
    
    # check whether data and formula are pdata.frame and pFormula and if not
    # coerce them and if not create it
#YC    orig_rownames <- row.names(data)
    if (! inherits(data, "pdata.frame")) data <- pdata.frame(data, index)
    if (! inherits(formula, "pFormula")) formula <- pFormula(formula)
    
    # in case of 2-part formula, check whether the second part should
    # be updated, e.g. y ~ x1 + x2 + x3 | . - x2 + z becomes 
    # y ~ x1 + x2 + x3 | x1 + x3 + z
    # use length(formula)[2] because the length is now a vector of length 2
    if (length(formula)[2] == 2) formula <- expand.formula(formula)

    # eval the model.frame
    cl <- match.call()
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "weights", "na.action"), names(mf), 0)
    mf <- mf[c(1, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1]] <- as.name("model.frame")
    # use the pFormula and pdata.frame which were created if necessary (and not
    # the original formula / data)
    mf$formula <- formula
    mf$data <- data # data is a pdata.frame
    data <- eval(mf, parent.frame())
    
    # preserve original row.names for data [also fancy rownames]; so functions
    # like pmodel.response(), model.frame(), model.matrix(), residuals() return
    # the original row.names eval(mf, parent.frame()) returns row.names as
    # character vector containing"row_number" with incomplete observations
    # dropped
#YC    row.names(data) <- orig_rownames[as.numeric(row.names(data))]

    # return the model.frame or estimate the model
    if (is.na(model)){
        attr(data, "formula") <- formula
        return(data)
    }
    
    # note that the model.frame has as attributes the pFormula and the index
    # data.frame
    args <- list(model = model, effect = effect,
                 random.method = random.method,
                 random.models = random.models,
                 random.dfcor = random.dfcor,
                 inst.method = inst.method)
    result <- plm.fit(formula, data, model, effect, random.method, 
                      random.models, random.dfcor, inst.method)
    result$call <- cl
    result$args <- args
    result
}

plm.fit <- function(formula, data, model, effect, random.method, 
                    random.models, random.dfcor, inst.method){
  
    # check for 0 cases like in stats::lm.fit (e.g. due to NA dropping) 
    if (nrow(data) == 0L) stop("0 (non-NA) cases")
  
    # if a random effect model is estimated, compute the error components
    if (model == "random"){
        is.balanced <- is.pbalanced(data)
        estec <- ercomp(formula, data, effect, method = random.method, 
                        models = random.models, dfcor = random.dfcor)
        sigma2 <- estec$sigma2
        theta <- estec$theta
        index <- attr(data, "index")
        if (length(formula)[2] == 2 && effect == "twoways")
            stop(paste("Instrumental variable random effect estimation", 
                       "not implemented for two-ways panels"))
    }

    # For all models except the unbalanced twoways random model, the
    # estimator is obtained as a linear regression on transformed data
    if (! (model == "random" & effect == "twoways" && ! is.balanced)){
        # extract the model.matrix and the model.response actually, this can be
        # done by providing model.matrix and pmodel.response's methods
        # to pdata.frames
        X <- model.matrix(formula, data, rhs = 1, model = model, 
                          effect = effect, theta = theta)
        y <- pmodel.response(formula, data, model = model, 
                             effect = effect, theta = theta)
        if (ncol(X) == 0) stop("empty model")
        # call w the weights (use 1 if no weights are specified)
        w <- as.vector(model.weights(data))
        if (is.null(w)) w <- 1
        # weight accordingly the response and the covariates
        X <- X * sqrt(w)
        y <- y * sqrt(w)
        # extract the matrix of instruments if necessary (means here that we
        # have a multi-parts formula)
        if (length(formula)[2] > 1){
            if (length(formula)[2] == 2){
                W <- model.matrix(formula, data, rhs = 2, 
                                  model = model, effect = effect, 
                                  theta = theta)
            }
            else{
                W <- model.matrix(formula, data, rhs = c(2, 3), model = model, 
                                      effect = effect, theta = theta)
            }
            if (model == "within"){
                if (! is.null(W)){
                    cst.W <- match(attr(W, "constant"), colnames(W))
                    if (length(cst.W) > 0) W <- W[, - cst.W, drop = FALSE]
                }
                if (! is.null(X)){
                    cst.X <- match(attr(X, "constant"), colnames(X))
                    if (length(cst.X) > 0) X <- X[, - cst.X, drop = FALSE]
                }
            }
            if (model == "random" && inst.method != "bvk"){
                # the bvk estimator seems to have disappeared
                X <- X / sqrt(sigma2["idios"])
                y <- y / sqrt(sigma2["idios"])
                W1 <- model.matrix(formula, data, rhs = 2, model = "within", 
                                   effect = effect, theta = theta)
                B1 <- model.matrix(formula, data, rhs = 2, model = "Between", 
                                   effect = effect, theta = theta)
                
                if (inst.method %in% c("am", "bmc")) 
                    StarW1 <- starX(formula, data, rhs = 2, model = "within", 
                                    effect = effect)
                if (length(formula)[2] == 3){
                    W2 <- model.matrix(formula, data, rhs = 3, model = "within", 
                                           effect = effect, theta = theta)
                    if (inst.method == "bmc")
                        StarW2 <- starX(formula, data, rhs = 3, model = "within", 
                                        effect = effect)
                }
                else W2 <- StarW2 <- NULL
                if (inst.method == "baltagi") W <- sqrt(w) * cbind(W1, W2, B1)
                if (inst.method == "am")  W <- sqrt(w) * cbind(W1, W2, B1, StarW1)
                if (inst.method == "bmc") W <- sqrt(w) * cbind(W1, W2, B1, StarW1, StarW2)
                zerovars <- apply(W, 2, function(x) max(abs(x), na.rm = TRUE)) < 1E-5
                W <- W[, !zerovars, drop = FALSE]
            }
            if (ncol(W) < ncol(X)) stop("insufficient number of instruments")
        }
        else W <- NULL # no instruments
        
        # compute the estimation
        result <- mylm(y, X, W)
        df <- df.residual(result)
        vcov <- result$vcov
        aliased <- result$aliased
        
        # in case of a within estimation, correct the degrees of freedom
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
    }
    else{ # random twoways unbalanced:
        pdim <- pdim(data)
        TS <- pdim$nT$T
        theta <- estec$theta$id
        phi2mu <- estec$sigma2["time"] / estec$sigma2["idios"]
        Dmu <- model.matrix( ~ factor(index(data)[[2]]) - 1)
        Dmu <- Dmu - theta * Between(Dmu, index(data)[[1]])
        X <- model.matrix(   formula, data, rhs = 1, model = "random", 
                             effect = "individual", theta = theta)
        y <- pmodel.response(formula, data, model = "random", 
                             effect = "individual", theta = theta)
        P <- solve(diag(TS) + phi2mu * crossprod(Dmu))
        XPX <- crossprod(X)    - phi2mu * crossprod(X, Dmu) %*% P %*% crossprod(Dmu, X)
        XPy <- crossprod(X, y) - phi2mu * crossprod(X, Dmu) %*% P %*% crossprod(Dmu, y)
        gamma <- solve(XPX, XPy)[, , drop = TRUE]
        e <- pmodel.response(formula, data, model = "pooling") -
             as.numeric(model.matrix(formula, data, rhs = 1, model = "pooling") %*% gamma)
        
        result <- list(coefficients = gamma,
                       vcov         = solve(XPX),
                       formula      = formula,
                       model        = data,
                       ercomp       = estec,
                       df.residual  = nrow(X) - ncol(X),
                       residuals    = e)
        # TODO: find a way to determine aliased coefs for tw RE unbalanced: compare 'gamma' to names of model matrix? 
        # make 'aliased' the right length, so that summary.plm(model)$df[3] contains correct value (length(aliased))
        aliased <- rep(NA, length(gamma))
    }
    result$assign <- attr(X, "assign")
    result$contrasts <- attr(X, "contrasts")
    result$args <- list(model = model, effect = effect)
    result$aliased <- aliased
    class(result) <- c("plm", "panelmodel")
    result
}

mylm <- function(y, X, W = NULL){
  names.X <- colnames(X)
  if (is.null(W))
      result <- lm(y ~ X - 1)
  else
      result <- twosls(y, X, W)
  
  na.coef <- is.na(coef(result))
  if (any(na.coef)){
    ## for debug purpose:
    # warning("Coefficient(s) '", paste((names.X)[na.coef], collapse = ", "), 
    #"' could not be estimated and is (are) dropped.")
    X <- X[, ! na.coef, drop = FALSE]
    if (dim(X)[2] == 0L) stop("estimation not possible: all coefficients omitted from estimation due to aliasing")
    if (is.null(W)) result <- lm(y ~ X - 1)
    else result <- twosls(y, X, W)
  }
  result$vcov <- vcov(result)
  result$X <- X
  result$y <- y
  result$W <- W
  # aliased is an element of summary.lm-objects:
  # since plm drops aliased coefs, store this info in plm object
  result$aliased <- na.coef
  names(result$aliased) <- names.X
  names(result$coefficients) <- colnames(result$vcov) <- 
    rownames(result$vcov) <- colnames(X)
  result
}


plm.list <- function(formula, data, subset, na.action,
                     effect = c("individual", "time", "twoways"),
                     model = c("within", "random", "ht", "between", "pooling", "fd"),
                     random.method = c("swar", "walhus", "amemiya", "nerlove", "ht"),
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
  # run plm for each equations of the list, store the results in a
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
      sysplm[[1]] <- as.name("plm")
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
    if (!is.null(W)) return(structure(list(X = BIGX, y = BIGy, W = BIGW), class = "BIG"))
    else return(structure(list(X = BIGX, y = BIGy), class = "BIG"))
  }

  # take a list of unconstrained models and a restriction matrix and
  # return a list containing the coefficients, the vcov and the
  # residuals of the constrained model ; qad version which deals with
  # lists of plm models or with models fitted by mylm (which have X, y
  # and W slots)
  systemlm <- function(object, restrict.matrix, restrict.rhs){
    if (class(object) == "list"){
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
      structure(list(coefficents = Ucoef, vcov = Uvcov, residuals = .resid), class = "basiclm")
    }
  }
  models <- plm.models(sysplm, amodel = model, random.method = "kinla") #TODO NB: "kinla" does not seem to be supported anymore...
  L <- length(models)
  sys <- systemlm(models, restrict.matrix = restrict.matrix, restrict.rhs = restrict.rhs)
  Instruments <- sapply(models, function(x) length(formula(x))[2]) > 1

  # Get the residuals and compute the consistent estimation of the
  # covariance matrix of the residuals : Note that if there are
  # restrictions, the "restricted" residuals are used ; for random
  # effect models, two covariance matrices must be computed
  if (model == "random"){
    resid.pooling <- Reduce("cbind", lapply(models, function(x) resid(x, model = "pooling")))
    id <- index(models[[1]])[[1]]
    T <- pdim(models[[1]])$nT$T
    N <- pdim(models[[1]])$nT$n
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
    coefnames <- lapply(XB, colnames)
    BIGW <- BIG(XW, yW, WW, Omega.nu)
    BIGB <- BIG(XB, yB, WB, Omega.1)
    y <- BIGW$y + BIGB$y
    X <- BIGB$X
    # Attention, pb lorsque noms de colonnes duppliques !!
    #    X[, colnames(BIGW$X)] <- X[, colnames(BIGW$X)] + BIGW$X
    # version provisoire : emplacement des constantes
    intercepts <- c(1, cumsum(sapply(XB, ncol))[-length(XB)]+1)
    X[, - intercepts] <- X[, - intercepts] + BIGW$X
    m <- mylm(y, X, cbind(BIGW$W, BIGB$W))
  }
  else{
    .resid <- matrix(sys$residuals, ncol = length(models))  
    Omega <- crossprod(.resid) / nrow(.resid)
    colnames(Omega) <- rownames(Omega) <- names.eq
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
    p <- 2 * pt(abs(z), df = object$df.residual, lower.tail=FALSE)
    coefTable <- cbind("Estimate"   = b,
                       "Std. Error" = std.err,
                       "t-value"    = z,
                       "Pr(>|t|)"   = p)
  }
  for (l in 1:L){
      models[[l]] <- coefTable[(Ks[l] + 1):Ks[l + 1] , ]
  }
  names(models) <- eqnames
  object$models <- models
  object$coefficients <- coefTable
  class(object) <- c("summary.plm.list", class(object))
  object
}

coef.summary.plm.list <- function(object, eq = NULL, ...){
  if (is.null(eq)) object$coefficients
  else object$models[[eq]]
}

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
    corid <- x$vcovsys$id / tcrossprod(sd[1, ])
    corid[upper.tri(corid)] <- NA
    print(corid, digits = digits, na.print = ".")
    cat("\n")
    cat("  Estimated correlation matrix of the idiosyncratic effects\n")
    coridios <- x$vcovsys$idios / tcrossprod(sd[2, ])
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
  for (l in 1:length(x$models)){
    cat(paste("\n - ", names(x$models)[l], "\n", sep = ""))
    printCoefmat(x$models[[l]], digits = digits)
  }
  invisible(x)
}
          
print.plm.list <- function(x, digits = max(3, getOption("digits") - 2), width = getOption("width"),...){
  cat("\nModel Formulas:\n")
  for (l in 1:length(formula(x))){
    cat(paste(names(formula(x))[l], "  : ", deparse(formula(x)[[l]]), "\n", sep = ""))
  }
  cat("\nCoefficients:\n")
  print(coef(x),digits = digits)
  cat("\n")
  invisible(x)
}


# summary.plm creates a specific summary.plm object that is derived from the associated plm object
summary.plm <- function(object, vcov = NULL, ..., .vcov = NULL){

  ## deprecation notice on arg ".vcov" introduced Nov 2016: remove arg ".vcov" some time in the future
  
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
    ## NB: there is another instance of a switch for arg ".vcov" below concerning rvcov.name
  ### END set correct vcov in case deprecated arg ".vcov" is not null
  
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
      
      ## set correct rvcov.name depending on arg used for vcov (as long as we support the deprecated arg ".vcov")
      if (is.null(vcov)) {
        rvcov.name <- paste0(deparse(substitute(.vcov)))
      } else {
        rvcov.name <- paste0(deparse(substitute(vcov)))
      }
      
      attr(object$rvcov, which = "rvcov.name") <- rvcov.name 
    }
    
    # mimics summary.lm's 'df' component
    # 1st entry: no. coefs (w/o aliased coefs); 2nd: residual df; 3rd no. coefs /w aliased coefs
    object$df <- c(length(b), object$df.residual, length(object$aliased)) # NB: do not use length(object$coefficients) for 3rd entry!
    
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
  cat("\nResiduals:\n")
  save.digits <- unlist(options(digits = digits))
  on.exit(options(digits = save.digits))
  print(sumres(x))
  
  if (any(x$aliased, na.rm = TRUE)) {
    # na.rm = TRUE because currently, RE tw unbalanced models set aliased simply to NA
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

fitted_exp.plm <- function(x, ...) { #### experimental, non-exported function
# fitted_exp.plm: gives the fitted values of all types of plm models by substracting the overall 
#                 residuals from the untransformed response variable; does not have
#                 a model argument so it is not as versatile as 'fitted.plm' below.
# see also test file tests/test_residuals_overall_fitted_exp.R
  model <- describe(x, "model")
  res <- residuals_overall_exp.plm(x)
  
  # For "between" and "fd" models, the number of fitted values is not equal to the 
  # number of original observations. Thus, model.frame cannot be used but rather
  # pmodel.response because it has the right length. However, pmodel.response
  # shall not be used for the other models because we want the untransformed data.
  if (model %in% c("between", "fd")) {
    y <- pmodel.response(x)
  } else {
    y <- model.frame(x)[ , 1]
  }
  return(y - res) # TODO: shall the result be class pseries? currently, 
                  #       pmodel.response (used for between and fd model) does not set 'pseries' class
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
      X <- X[, ! is.na(coef(result)), drop = FALSE]
  }
  if (model == "within" & fittedmodel != "within"){
    Xw <- model.matrix(object, model = "within", effect = effect)
    varwith <- colnames(Xw)
    beta <- beta[varwith]
  }
  
  # Test if all coefficients could be estimated by plm
  # [plm silently drops non-estimable coefficients [v1.5-13]]
  # With this test, we provide an additional warning message to
  # users to enhance the error message from failing crossprod later in the code
  # which relies on non-dropped coefficients; see also testfile tests/test_fitted.plm.R
  # This test could be computationally/space expensive due to creation of model.matrix.
  # if (!setequal(names(object$coefficients), colnames(model.matrix(object)))) {
  #    warning("Coefficients of estimated model do not match variables in its specified model.matrix.
  #           This is likely due to non-estimable coefficients (compare object$formula with object$coefficients).")
  # }
  
  if (fittedmodel == "within"){
    if (model == "pooling"){
      if (has.intercept(object)) X <- X[ , -1, drop = FALSE]
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
      alpha <- mean(y) - as.numeric(crossprod(colMeans(X[ , -1, drop = FALSE]), beta))
      beta <- c(alpha, beta)
      fv <- as.numeric(crossprod(t(X), beta))
    }
    if (model == "within"){
      fv <- as.numeric(crossprod(t(X), beta))
    }
  }
  else{
      # QDF just in case check the conformabilty of beta and X cols,
      # useful for FD censored/truncated models
      comonpars <- union(colnames(X), na.omit(names(beta)))
      fv <- as.numeric(crossprod(t(X[, comonpars, drop = FALSE]), beta[comonpars]))
  }
  structure(fv, index = index(object), class = "pseries")
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
  var(x) * (length(x) - 1)
}

tss.plm <- function(x, model = NULL){
    if (is.null(model)) model <- describe(x, "model")
    effect <- describe(x, "effect")
    if (model == "ht") model = "pooling"
    if (model == "random") theta <- x$ercomp$theta else theta <- NULL
    tss(pmodel.response(x, model = model, effect = effect, theta = theta))
}

r.squared <- function(object, model = NULL,
                      type = c("cor", "rss", "ess"), dfcor = FALSE){
    if (is.null(model)) model <- describe(object, "model")
    effect <- describe(object, "effect")
    type <- match.arg(type)
    if (type == "cor"){
        y <- pmodel.response(object, model = model, effect = effect)
        haty <- fitted(object, model = model, effect = effect)
        R2 <- cor(y, haty)^2
    }
    if (type == "rss"){
        R2 <- 1 - deviance(object, model = model) / tss(object, model = model)
    }
    if (type == "ess"){
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
    if (is.null(model)){
        res <- object$residuals
    }
    else{
        beta <- coef(object)
        X <- model.matrix(object, model = model, effect = effect)
        cstX <- attr(model.matrix(object, model = "within", effect = effect), "constant")
        cstX <- union(cstX, names(which(object$aliased)))
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
    structure(res, index = index(object), class = c("pseries", class(res)))
}

residuals.plm <- function(object, model = NULL, effect = NULL, ...){
    fittedmodel <- describe(object, "model")
    if (is.null(effect)) effect <- describe(object, "effect")
    if (is.null(model)){
        res <- object$residuals
    }
    else{
        beta <- coef(object)
        X <- model.matrix(object, model = model, effect = effect)
        y <- pmodel.response(object, model = model, effect = effect)
        aliases <- object$aliased
        # beta has an intercept, X not
#        if (has.intercept(object)[1] & model == "within"){
        # doesn't work the intercept is detected in the formula
        if (names(coef(object))[1] == "(Intercept)" & model == "within"){
            beta <- beta[-1]
            aliases <- aliases[-1]
        }
        if (model != "within" & fittedmodel == "within"){
#        if (! has.intercept(object)[1] & model != "within"){
            XM <- apply(X[, -1, drop = FALSE], 2, mean)
            comonpars <- intersect(names(XM), names(beta))
            alpha <- mean(y) - sum(XM[comonpars] * beta[comonpars])
            beta <- c("(Intercept)" = alpha, beta)
            aliases <- c("(Intercept)" = FALSE, aliases)
        }
#        cstXW <- attr(model.matrix(object, model = "within", effect = effect), "constant")
#        X <- X[, ! (colnames(X) %in% cstX), drop = FALSE]
        # !YC! QDF : on a Between estimation with time dummies, coefs
       # on years dummies disapears
        comonpars <- intersect(colnames(X), names(beta))
        res <- y - as.numeric(crossprod(t(X[, comonpars, drop = FALSE]), beta[comonpars]))
#        res <- y - as.numeric(crossprod(t(X), beta))
    }
    structure(res, index = index(object), class = c("pseries", class(res)))
}

## residuals.plm <- function(object, model = NULL, effect = NULL, ...){
##     fittedmodel <- describe(object, "model")
##     if (is.null(effect)) effect <- describe(object, "effect")
##     if (is.null(model)){
##         res <- object$residuals
##     }
##     else{
##         beta <- coef(object)
##         X <- model.matrix(object, model = model, effect = effect)
##         ## cstX <- attr(model.matrix(object, model = "within", effect = effect), "constant")
##         ## cstX <- union(cstX, names(which(object$aliased)))
##         ## X <- X[, ! (colnames(X) %in% cstX), drop = FALSE]
##         y <- pmodel.response(object, model = model, effect = effect)
##         if (model == "within" & fittedmodel != "within"){
##             if (names(beta)[1] == "(Intercept)") beta <- beta[-1]
##         }
##         if (model != "within" & fittedmodel == "within"){
##             if (colnames(X)[1] == "(Intercept)"){
## #                alpha <- mean(y) - crossprod(apply(X[, -1, drop = FALSE], 2, mean), beta)
##                 XM <- apply(X, 2, mean)[-1]
##                 comonpars <- intersect(names(X), names(beta))
##                 alpha <- mean(y) - crossprod(XM[comonpars], beta[comonpars])
##                 beta <- c("(Intercept)" = alpha, beta)
##             }
##         }
##         # !YC! QDF : on a Between estimation with time dummies, coefs
##        # on years dummies disapears
##         comonpars <- intersect(colnames(X), names(beta))
##         res <- y - as.numeric(crossprod(t(X[, comonpars, drop = FALSE]), beta[comonpars]))
## #        res <- y - as.numeric(crossprod(t(X), beta))
##     }
##     structure(res, index = index(object), class = c("pseries", class(res)))
## }



residuals_overall_exp.plm <- function(x, ...) { #### experimental, non-exported function
# residuals_overall.plm: gives the residuals of the "overall"/outer model for all types of plm models.
# In the future, this could be integrated with residuals.plm by some argument, e.g. overall = FALSE (default).
# see also test file tests/test_residuals_overall_fitted_exp.R
  
  # no na.action eval yet
  
  model <- describe(x, "model")
  
  if (model == "ht") stop("model \"ht\" not (yet?) supported")
  
  # for all effects of within models: residuals of (quasi-)demeaned (inner) model
  # are also the residuals of the "overall" model
  if (model == "random") {
    # get untransformed data to calculate overall residuals
    X <- model.matrix(x, model = "pooling") 
    y <- pmodel.response(x, model = "pooling")
    # take care of any aliased coefficients:
    # they are not in x$coefficients but assoc. variables are still present in model.matrix
    if (any(x$aliased, na.rm = TRUE)) { # na.rm = TRUE because currently, RE tw unbalanced models set aliased simply to NA
      X <- X[ , !x$aliased, drop = FALSE]
    }
    
    est <- as.numeric(tcrossprod(coef(x), X))
    res <- y - est
    names(res) <- rownames(X)

    # make residuals a pseries
    res <- structure(res, index = index(x), class = c("pseries", class(res)))
      
  } else { # all plm models except random (also also except ht)
    res <- residuals(x)
  }
  return(res)
}

residuals_overall_e_exp <- function(object) { ### experimental non-exported function
  ## residuals of "overall" RE model minus random effects (=e_it)
  ## e.g.: two-way model: residual_overall_it = random_component_individual_i + random_component_time_t + e_it
  model <- describe(object, "model")
  if (model != "random") stop("only for random effect models")
  obj.eff <- describe(object, "effect")
  res_ov <- residuals_overall_exp.plm(object)
  if (obj.eff == "twoways") {
    res_ov_e <- res_ov - ranef(object, "individual")[index(object, "id")] - ranef(object, "time")[index(object, "time")]
  } else {
    res_ov_e <- res_ov - ranef(object)[index(object, ifelse(obj.eff == "individual", "id", "time"))]
  }
  names(res_ov_e) <- names(res_ov)
  return(res_ov_e)
}


formula.plm <- function(x, ...){
  x$formula
}

# describe function: extract characteristics of plm model
describe <- function(x,
                     what = c("model", "effect", "random.method",
                              "inst.method", "transformation")){
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



