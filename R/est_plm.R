starX <- function(formula, data, model, rhs = 1, effect){
    apdim <- pdim(data)
    amatrix <- model.matrix(data, model, effect, rhs)
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

mylm <- function(y, X, W = NULL){
  names.X <- colnames(X)
  if (is.null(W)) result <- lm(y ~ X - 1)
  else result <- twosls(y, X, W)
  na.coef <- is.na(coef(result))
  if (any(na.coef)){
    ## for debug purpose:
    # warning("Coefficient(s) '", paste((names.X)[na.coef], collapse = ", "), 
    #"' could not be estimated and is (are) dropped.")
      X <- X[, ! na.coef, drop = FALSE]
      if (dim(X)[2] == 0L) stop(paste("estimation not possible: all coefficients",
                                      "omitted from estimation due to aliasing"))
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

plm <- function(formula, data, subset, weights, na.action,
                effect = c("individual", "time", "twoways", "nested"),
                model = c("within", "random", "ht", "between", "pooling", "fd"),
                random.method = NULL,
                random.models = NULL,
                random.dfcor = NULL,
                inst.method = c("bvk", "baltagi", "am", "bms"),
                restrict.matrix = NULL,
                restrict.rhs = NULL,
                index = NULL,
                ...){

    if (is.list(formula)){
        # if the first argument is a list (of formulas), then call plmlist and exit
        plmlist <- match.call(expand.dots = FALSE)
        plmlist[[1]] <- as.name("plm.list")
        # eval in nframe and not the usual parent.frame(), relevant?
        nframe <- length(sys.calls())
        plmlist <- eval(plmlist, sys.frame(which = nframe))
        return(plmlist)
    }

    if ((! is.null(restrict.matrix) || ! is.null(restrict.rhs)) && ! is.list(formula)) {
        stop(paste0("arguments 'restrict.matrix' and 'restrict.rhs' cannot yet be used ",
                    "for single equations"))
    }
    dots <- list(...)
    
    # check and match the effect and model arguments
    effect <- match.arg(effect)
    # note that model can be NA, in this case the model.frame is returned
    if (! anyNA(model)) model <- ifelse(effect == "nested",
                                        "random", match.arg(model))


    # input checks for FD model: give informative error messages as
    # described in footnote in vignette
    if (! is.na(model) && model == "fd") {
        if (effect == "time") stop(paste("effect = \"time\" for first-difference model",
                                         "meaningless because cross-sections do not",
                                         "generally have a natural ordering"))
        if (effect == "twoways") stop(paste("effect = \"twoways\" is not defined",
                                            "for first-difference models"))
    }
    
    # Deprecated section
    
    # accept "bmc" (a long-standing typo) for Breusch-Mizon-Schmidt
    # due to backward compatibility
    if (length(inst.method) == 1 && inst.method == "bmc") {
      inst.method <- "bms"
        warning(paste("Use of inst.method = \"bmc\" discouraged, set to \"bms\"",
                      "for Breusch-Mizon-Schmidt instrumental variable transformation"))
    }
    inst.method <- match.arg(inst.method)
  
    
    # pht is no longer maintained
    if (! is.na(model) && model == "ht"){
        ht <- match.call(expand.dots = FALSE)
        m <- match(c("formula", "data", "subset", "na.action", "index"), names(ht), 0)
        ht <- ht[c(1, m)]
        ht[[1]] <- as.name("pht")
        ht <- eval(ht, parent.frame())
        return(ht)
    }
    
    # the use of the instrument argument is deprecated, use 2-part Formulas instead
    if (! is.null(dots$instruments)){
        formula <- as.Formula(formula, dots$instruments)
        deprec.instruments <- paste("the use of the instruments argument is deprecated,",
                                    "use two-part formulas instead")
        warning(deprec.instruments)
    }
    
    # check whether data and formula are pdata.frame and pFormula and if not
    # coerce them
    orig_rownames <- row.names(data)
    if (! inherits(data, "pdata.frame")) data <- pdata.frame(data, index)
    if (! inherits(formula, "Formula")) formula <- as.Formula(formula)
    # in case of 2-part formula, check whether the second part should
    # be updated, e.g. y ~ x1 + x2 + x3 | . - x2 + z becomes 
    # y ~ x1 + x2 + x3 | x1 + x3 + z
    # use length(formula)[2] because the length is now a vector of length 2
#    if (length(formula)[2] == 2) formula <- expand.formula(formula)
    # eval the model.frame
    cl <- match.call()
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("data", "formula", "subset", "weights", "na.action"), names(mf), 0)
    mf <- mf[c(1, m)]
    names(mf)[2:3] <- c("formula", "data")
    mf$drop.unused.levels <- TRUE
    mf[[1]] <- as.name("model.frame")
    # use the pFormula and pdata.frame which were created if necessary (and not
    # the original formula / data)
    mf$formula <- data
    mf$data <- formula
    data <- eval(mf, parent.frame())

    # preserve original row.names for data [also fancy rownames]; so functions
    # like pmodel.response(), model.frame(), model.matrix(), residuals() return
    # the original row.names eval(mf, parent.frame()) returns row.names as
    # character vector containing the "row_number" with incomplete observations
    # dropped
    row.names(data) <- orig_rownames[as.numeric(row.names(data))]

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
    result <- plm.fit(data, model, effect, random.method,
                      random.models, random.dfcor, inst.method)
    result$call <- cl
    result$args <- args
    result
}

plm.fit <- function(data, model, effect, random.method, 
                    random.models, random.dfcor, inst.method){
    formula <- attr(data, "formula")
    # check for 0 cases like in stats::lm.fit (e.g. due to NA dropping) 
    if (nrow(data) == 0L) stop("0 (non-NA) cases")

    # if a random effect model is estimated, compute the error components
    if (model == "random"){
        is.balanced <- is.pbalanced(data)
        estec <- ercomp(data, effect, method = random.method,
                        models = random.models, dfcor = random.dfcor)        
        sigma2 <- estec$sigma2
        theta <- estec$theta
        if (length(formula)[2] == 2 && effect == "twoways")
            stop(paste("Instrumental variable random effect estimation",
                       "not implemented for two-ways panels"))
    }
    else theta <- NULL

    # For all models except the unbalanced twoways random model, the
    # estimator is obtained as a linear regression on transformed data
    if (! (model == "random" & effect == "twoways" && ! is.balanced)){
        # extract the model.matrix and the model.response actually, this can be
        # done by providing model.matrix and pmodel.response's methods
        # to pdata.frames
        X <- model.matrix(data, rhs = 1, model = model, 
                          effect = effect, theta = theta, cstcovar.rm = "all")
        y <- pmodel.response(data, model = model, 
                             effect = effect, theta = theta)
        if (ncol(X) == 0) stop("empty model")
        
        w <- model.weights(data)
        if (! is.null(w)){
            if (! is.numeric(w)) stop("'weights' must be a numeric vector")
            X <- X * sqrt(w)
            y <- y * sqrt(w)
        }
        else w <- 1
        
        # extract the matrix of instruments if necessary (means here that we
        # have a multi-parts formula)
        if (length(formula)[2] > 1){
            if (length(formula)[2] == 2){
                W <- model.matrix(data, rhs = 2,
                                  model = model, effect = effect,
                                  theta = theta, cstcovar.rm = "all")
            }
            else{
                W <- model.matrix(data, rhs = c(2, 3), model = model,
                                      effect = effect, theta = theta, cstcovar.rm = "all")
            }
            if (model == "random" && inst.method != "bvk"){
                # the bvk estimator seems to have disappeared
                X <- X / sqrt(sigma2["idios"])
                y <- y / sqrt(sigma2["idios"])
                W1 <- model.matrix(data, rhs = 2, model = "within",
                                   effect = effect, theta = theta, cstcovar.rm = "all")
                B1 <- model.matrix(data, rhs = 2, model = "Between",
                                   effect = effect, theta = theta, cstcovar.rm = "all")
                
                if (inst.method %in% c("am", "bms")) 
                    StarW1 <- starX(formula, data, rhs = 2, model = "within",
                                    effect = effect)
                if (length(formula)[2] == 3){
                    W2 <- model.matrix(data, rhs = 3, model = "within",
                                           effect = effect, theta = theta, cstcovar.rm = "all")
                    if (inst.method == "bms")
                        StarW2 <- starX(formula, data, rhs = 3, model = "within",
                                        effect = effect)
                }
                else W2 <- StarW2 <- NULL
                if (inst.method == "baltagi") W <- sqrt(w) * cbind(W1, W2, B1)
                if (inst.method == "am")  W <- sqrt(w) * cbind(W1, W2, B1, StarW1)
                if (inst.method == "bms") W <- sqrt(w) * cbind(W1, W2, B1, StarW1, StarW2)
            }
            if (ncol(W) < ncol(X)) stop("insufficient number of instruments")
        }
        else W <- NULL
        
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
                       weights      = w,
                       df.residual  = df,
                       formula      = formula,  #fForm
                       model        = data)
        if (is.null(model.weights(data))) result$weights <- NULL
        if (model == "random") result$ercomp <- estec
    }
    else{
        # random twoways unbalanced:
        pdim <- pdim(data)
        TS <- pdim$nT$T
        theta <- estec$theta$id
        phi2mu <- estec$sigma2["time"] / estec$sigma2["idios"]
        Dmu <- model.matrix( ~ factor(index(data)[[2]]) - 1)
        attr(Dmu, "index") <- index(data)
        Dmu <- Dmu - theta * Between(Dmu, "individual")
        X <- model.matrix(data, rhs = 1, model = "random", 
                          effect = "individual", theta = theta)
        y <- pmodel.response(data, model = "random", 
                             effect = "individual", theta = theta)
        P <- solve(diag(TS) + phi2mu * crossprod(Dmu))
        XPX <- crossprod(X)    - phi2mu * crossprod(X, Dmu) %*% P %*% crossprod(Dmu, X)
        XPy <- crossprod(X, y) - phi2mu * crossprod(X, Dmu) %*% P %*% crossprod(Dmu, y)
        gamma <- solve(XPX, XPy)[, , drop = TRUE]

        # residuals 'e' are not the residuals of a quasi-demeaned
        # model but of the 'outer' model
        e <- pmodel.response(data, model = "pooling", effect = effect) -
            as.numeric(model.matrix(data, rhs = 1, model = "pooling") %*% gamma)
        result <- list(coefficients = gamma,
                       vcov         = solve(XPX),
                       formula      = formula, #fForm
                       model        = data,
                       ercomp       = estec,
                       df.residual  = nrow(X) - ncol(X),
                       residuals    = e)
        
        # derive 'aliased' information (this is based on the assumption that
        # estimation fails anyway if singularities are present).
        aliased <- is.na(gamma)
    }
    result$assign <- attr(X, "assign")
    result$contrasts <- attr(X, "contrasts")
    result$args <- list(model = model, effect = effect)
    result$aliased <- aliased
    class(result) <- c("plm", "panelmodel")
    result
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
    if (model == "ht") model <- "pooling"
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
        ess <- as.numeric(crossprod((haty - mhaty)))
        R2 <- ess / tss(object, model = model)
    }
    # Kevin Tappe 2015-10-19, the computation of the adjusted R2 was wrong
    if (dfcor) R2 <- 1 - (1 - R2) * (length(resid(object)) - 1) / df.residual(object)
    R2
}



# describe function: extract characteristics of plm model
describe <- function(x,
                     what = c("model", "effect", "random.method",
                              "inst.method", "transformation")){
  what <- match.arg(what)
  cl <- x$args
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

