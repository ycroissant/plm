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

plm <-  function(formula, data, subset, weights, na.action,
                 effect=c('individual','time','twoways'),
                 model = c('within','random','ht','between','pooling','fd'),
                 random.method = NULL,
                 random.models = NULL,
                 random.dfcor = NULL,
                 inst.method = c('bvk', 'baltagi', 'am', 'bmc'),
                 restrict.matrix = NULL,
                 restrict.rhs = NULL,
                 index = NULL,
                 ...){    
    # if the first argument is a list (of formulas), then call plmlist and exit
    if (is.list(formula)){
        plmlist <- match.call(expand.dots = FALSE)
        plmlist[[1]] <- as.name("plm.list")
        # eval in nframe and not the usual parent.frame(), relevant ?
        nframe <- length(sys.calls())
        plmlist <- eval(plmlist, sys.frame(which = nframe))
        return(plmlist)
    }
    
    dots <- list(...)
    # check and match the arguments
    effect <- match.arg(effect)
    # note that model can be NA, in this case the model.frame is
    # returned
    if (! any(is.na(model))) model <- match.arg(model)
    inst.method <- match.arg(inst.method)
  
    # input checks for FD model give informative error messages as
    # described in footnote in vignette
    if (! is.na(model) && model == "fd") {
        if (effect == "time") stop("effect = \"time\" for first-difference model meaningless because cross-sections do not generally have a natural ordering")
        if (effect == "twoways") stop("effect = \"twoways\" is not defined for first-difference models")
    }

    # deprecated section :
    # pht is no longer maintained
    if (! is.na(model) && model == "ht"){
        ht <- match.call(expand.dots=FALSE)
        m <- match(c("formula", "data", "subset", "na.action", "index"), names(ht), 0)
        ht <- ht[c(1,m)]
        ht[[1]] <- as.name("pht")
        ht <- eval(ht, parent.frame())
        return(ht)
    }
    # the use of the instrument argument is deprecated, use Formulas instead
    if (!is.null(dots$instruments)){
        as.Formula(formula, dots$instruments)
        deprec.instruments <- paste("the use of the instruments argument is deprecated,",
                                    "use two-part formulas instead")
        warning(deprec.instruments)
    }
    # check whether data and formula are pdta.frame and Formula and if not
    # coerce them and if not create it
    orig_rownames <- row.names(data)
    if (! inherits(data, "pdata.frame")) data <- pdata.frame(data, index)
    if (! inherits(formula, "pFormula")) formula <- pFormula(formula)
    # in case of 2part formula, check whether the second part should
    # be updated, e.g. y ~ x1 + x2 + x3 | . - x2 + z becomes y ~ x1 +
    # x2 + x3 | x1 + x3 + z length(formula)[2] because the length is
    # now a vector of length 2
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
    mf$data <- data
    data <- eval(mf, parent.frame())
    # preserve original row.names for data [also fancy rownames]; so functions
    # like pmodel.response(), model.frame(), model.matrix(), residuals() return
    # the original row.names eval(mf, parent.frame()) returns row.names as
    # character vector containing"row_number" with incomplete observations
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
    result <- plm.fit(formula, data, model, effect, random.method, 
                      random.models, random.dfcor, inst.method)
    result$call <- cl
    result$args <- args
    result
}

plm.fit <- function(formula, data, model, effect, random.method, random.models, random.dfcor, inst.method){
    # if a random effect model is estimated, compute the error components
    if (model == "random"){
        is.balanced <- is.pbalanced(data)
        estec <- ercomp(formula, data, effect, method = random.method, models = random.models, dfcor = random.dfcor)
        sigma2 <- estec$sigma2
        theta <- estec$theta
        index <- attr(data, "index")
        if (length(formula)[2] == 2 && effect == "twoways")
            stop("Instrumental variable random effect estimation not implemented for two-ways panels")
    }
    # For all models except the unbalanced twoways random model, the
    # estimator is obtained as a linear regression on transformed data
    if (! (model == "random" & effect == "twoways" && ! is.balanced)){
        # extract the model.matrix and the model.response actualy, this can be
        # actualy done by providing model.matrix and pmodel.response's methods
        # to pdata.frames
        X <- model.matrix(formula, data, rhs = 1, model = model, effect = effect, theta = theta)
        if (ncol(X) == 0) stop("empty model")
        y <- pmodel.response(formula, data, model = model, effect = effect, theta = theta)
        # call w the weights (use 1 if no weights are specified)
        w <- as.vector(model.weights(data))
        if (is.null(w)) w <- 1
        # weight accordingly the response and the covariates
        X <- X * sqrt(w)
        y <- y * sqrt(w)
        # extract the matrix of instruments if necessary (means here that we
        # have a multi-parts formula)
        if (length(formula)[2] > 1){
            if (length(formula)[2] == 2) W <- model.matrix(formula, data, rhs = 2, model = model, effect = effect, theta = theta)
            else W <- model.matrix(formula, data, rhs = c(2, 3), model = model, effect = effect, theta = theta)
            if (model == "random" && inst.method != "bvk"){
                # the bvk estimator seems to have desapeared
                X <- X / sqrt(sigma2$idios)
                y <- y / sqrt(sigma2$idios)
                W1 <- model.matrix(formula, data, rhs = 2, model = "within", effect = effect, theta = theta)
                B1 <- model.matrix(formula, data, rhs = 2, model = "Between", effect = effect, theta = theta)
                if (inst.method %in% c("am", "bmc")) 
                  StarW1 <- starX(formula, data, rhs = 2, model = "within", effect = effect)
                if (length(formula)[2] == 3){
                  W2 <- model.matrix(formula, data, rhs = 3, model = "within", effect = effect, theta = theta)
                  if (inst.method == "bmc")
                      StarW2 <- starX(formula, data, rhs = 3, model = "within", effect = effect)
                }
                if (inst.method == "baltagi") W <- sqrt(w) * cbind(W1, W2, B1)
                if (inst.method == "am") W <- sqrt(w) * cbind(W1, W2, B1, StarW1)
                if (inst.method == "bmc") W <- sqrt(w) * cbind(W1, W2, B1, StarW1, StarW2)
                # quick and dirty trick to remove columns of 0
                zerovars <- apply(W, 2, function(x) max(abs(x), na.rm = TRUE)) < 1E-5
                W <- W[, !zerovars]
            }
            if (ncol(W) < ncol(X)) stop("insufficient number of instruments")
        }
        else W <- NULL
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
    else{
        pdim <- pdim(data)
        TS <- pdim$nT$T
        theta <- estec$theta$id
        phi2mu <- estec$sigma2["time"] / estec$sigma2["idios"]        
        Dmu <- model.matrix( ~ factor(index(data)[[2]]) - 1)
        Dmu <- Dmu - theta * Between(Dmu, index(data)[[1]])
        X <- model.matrix(   formula, data, rhs = 1, model = "random", effect = "individual", theta = theta)
        y <- pmodel.response(formula, data,          model = "random", effect = "individual", theta = theta)
        P <- solve(diag(TS) + phi2mu * crossprod(Dmu))
        XPX <- crossprod(X)    - phi2mu * crossprod(X, Dmu) %*% P %*% crossprod(Dmu, X)
        XPy <- crossprod(X, y) - phi2mu * crossprod(X, Dmu) %*% P %*% crossprod(Dmu, y)
        gamma <- solve(XPX, XPy)[, , drop = TRUE]
        e <- pmodel.response(formula, data, model = "pooling") -
            as.numeric(model.matrix(formula, data, rhs = 1, model = "pooling") %*% gamma)
        result <- list(coefficients = gamma,
                       vcov = solve(XPX),
                       formula = formula,
                       model = data,
                       ercomp = estec,
                       df.residual = nrow(X) - ncol(X),
                       residuals = e)
        aliased <- NA
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
    # warning("Coefficient(s) '", paste((names.X)[na.coef], collapse = ", "), "' could not be estimated and is (are) dropped.")
    X <- X[, !na.coef, drop = FALSE]
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

