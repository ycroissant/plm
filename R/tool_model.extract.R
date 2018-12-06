# model.frame method for pdata.frame ; the formula argument must be a
# pdata.frame and the data argument must be a formula, which is quite
# hesoteric, but consistent with the argument list of
# model.frame.Formula which is latter called.

model.frame.pdata.frame <- function(formula, data = NULL, ...,
                                    lhs = NULL, rhs = NULL, dot = "separate"){
    pdata <- formula
    formula <- as.Formula(data)
    if (is.null(rhs)) rhs <- 1:(length(formula)[2])
    if (is.null(lhs)) lhs <- ifelse(length(formula)[1] > 0, 1, 0)
    index <- attr(pdata, "index")
    mf <- model.frame(formula, as.data.frame(pdata), ...,
                      lhs = lhs, rhs = rhs, dot = dot)
    index <- index[as.numeric(rownames(mf)), ]
    index <- droplevels(index)
    class(index) <- c("pindex", "data.frame")
    structure(mf,
              index = index,
              formula = formula,
              class = c("pdata.frame", class(mf)))
}

formula.pdata.frame <- function(x, ...){
    if (is.null(attr(x, "terms")))
        stop("formula expect a model.frame and not an ordinary pdata.frame")
    attr(x, "formula")
}
    

model.matrix.plm <- function(object, ...){
    dots <- list(...)
    model <- ifelse(is.null(dots$model), describe(object, "model"), dots$model)
    effect <- ifelse(is.null(dots$effect), describe(object, "effect"), dots$effect)
    rhs <- ifelse(is.null(dots$rhs), 1, dots$rhs)
    cstcovar.rm <- dots$cstcovar.rm
    formula <- formula(object)
    data <- model.frame(object)
    if (model != "random"){
        model.matrix(data, model = model, effect = effect,
                     rhs = rhs, cstcovar.rm = cstcovar.rm)
    }
    else{
        theta <- ercomp(object)$theta
        model.matrix(data, model = model, effect = effect,
                     theta = theta, rhs = rhs, cstcovar.rm = cstcovar.rm)
    }
}

pmodel.response <- function(object, ...) {
    UseMethod("pmodel.response")
}

pmodel.response.plm <- function(object, ...){
    y <- model.response(model.frame(object))
    dots <- list(...)
    if (is.null(dots$model)) model <- describe(object, "model") else model <- dots$model
    if (is.null(dots$effect))
        effect <- describe(object, "effect") else effect <- dots$effect
    if (is.null(dots$theta)){
        if (describe(object, "model") == "random")
            theta <- ercomp(object)$theta else theta <- NULL
    }
    else theta <- dots$theta
    ptransform(y, model = model, effect = effect, theta = theta)
}

pmodel.response.data.frame <- function(object, ...){
    dots <- list(...)
    if (is.null(attr(object, "terms"))) stop("not a model.frame")
    model <- ifelse(is.null(dots$model), "pooling", dots$model)
    effect <- ifelse(is.null(dots$effect), "individual", dots$effect)
    if (is.null(dots$theta)) theta <- NULL else theta <- dots$theta
#    theta <- ifelse(is.null(dots$theta), NULL, dots$theta)
    y <- model.response(object)
    ptransform(y, model = model, effect = effect, theta = theta)
}

# deprecated
pmodel.response.formula <- function(object, data, ...){
    dots <- list(...)
    if (is.null(data)) stop("the data argument is mandatory")
    if (! inherits(data, "pdata.frame")) stop("the data argument must be a pdata.frame")
    if (is.null(attr(data, "terms"))) data <- model.frame(data, object)
    model <- dots$model
    effect <- dots$effect
    theta <- dots$theta
    if (is.null(model)) model <- "pooling"
    if (is.null(effect)) effect <- "individual"
    if (model == "random" & is.null(theta)) stop("the theta argument is mandatory")
    y <- model.response(data)
    ptransform(y, model = model, effect = effect, theta = theta)
}

ptransform <- function(x, model = NULL, effect = NULL, theta = NULL, ...){
    if (model == "pooling") return(x)
    if (effect == "twoways" & model %in% c("between", "fd"))
        stop("twoways effect only relevant for within, random and pooling models")
    if (model == "within") x <- Within(x, effect)
    if (model == "between") x <- between(x, effect)
    if (model == "Between") x <- Between(x, effect)
    if (model == "fd") x <- pdiff(x, "individual")
    if (model == "random"){
        if (is.null(theta)) stop("a theta argument should be provided")
        if (effect %in% c("time", "individual")) x <- x - theta * Between(x, effect)
        if (effect == "nested") x <- x - theta$id * Between(x, "individual") -
                                    theta$gp * Between(x, "group")
        if (effect == "twoways" & is.pbalanced(x))
            x <- x - theta$id * Between(x, "individual") -
                theta$time * Between(x, "time") + theta$total * mean(x)
    }
    
    res <- if (model %in% c("between", "fd")) {
    # these models "compress" the data, thus an index does not make sense here -> no pseries
               x
           } else {
               structure(x, index = index(x), class = union("pseries", class(x)))
           }
    return(res)
}

####

Mean <- function(x) matrix(.colMeans(x, nrow(x), ncol(x)),
                           nrow(x), ncol(x), byrow = TRUE)

model.matrix.pdata.frame <- function(object, 
                                     model = c("pooling", "within", "Between", "Sum",
                                               "between", "mean", "random", "fd"),
                                     effect = c("individual", "time", "twoways", "nested"),
                                     rhs = 1,
                                     theta = NULL,
                                     cstcovar.rm = NULL,
                                     ...){
    if (is.null(attr(object, "terms")))
        stop("model.matrix expect a model.frame and not an ordinary pdata.frame")
    model <- match.arg(model)
    effect <- match.arg(effect)
    formula <- attr(object, "formula")
    data <- object
    has.intercept <- has.intercept(formula, rhs = rhs)
    # relevant defaults for cstcovar.rm
    if (is.null(cstcovar.rm)) cstcovar.rm <- ifelse(model == "within", "intercept", "none")
    balanced <- is.pbalanced(data)
    X <- model.matrix(as.Formula(formula), data = data, rhs = rhs, dot = "previous", ...)
    # check for infinite or NA values and exit if there are some
    if(any(! is.finite(X)))
        stop(paste("model matrix or response contains non-finite",
                   "values (NA/NaN/Inf/-Inf)"))
    X.assi <- attr(X, "assign")
    X.contr <- attr(X, "contrasts")
    X.contr <- X.contr[ ! sapply(X.contr, is.null) ]
    index <- index(data)
    if (anyNA(index[[1]])) stop("NA in the individual index variable")
    attr(X, "index") <- index
    if (effect == "twoways" & model %in% c("between", "fd"))
        stop("twoways effect only relevant for within, random and pooling models")
    if (model == "within") X <- Within(X, effect)
    if (model == "Sum") X <- Sum(X, effect)
    if (model == "Between") X <- Between(X, effect)
    if (model == "between") X <- between(X, effect)
    if (model == "mean") X <- Mean(X)
    if (model == "fd") X <- pdiff(X, effect = "individual",
                                  has.intercept = has.intercept)
    if (model == "random"){
        if (is.null(theta)) stop("a theta argument should be provided")
        if (effect %in% c("time", "individual")) X <- X - theta * Between(X, effect)
        if (effect == "nested") X <- X - theta$id * Between(X, "individual") -
                                    theta$gp * Between(X, "group")
        if (effect == "twoways" & balanced)
            X <- X - theta$id * Between(X, "individual") -
                theta$time * Between(X, "time") + theta$total * Mean(X)
    }

    if (cstcovar.rm == "intercept"){
        posintercept <- match("(Intercept)", colnames(X))
        if (! is.na(posintercept)) X <- X[, - posintercept, drop = FALSE]
    }
    if (cstcovar.rm %in% c("covariates", "all")){
        cols <- apply(X, 2, is.constant)
        cstcol <- names(cols)[cols]
        posintercept <- match("(Intercept)", cstcol)
        cstintercept <- ifelse(is.na(posintercept), FALSE, TRUE)
        zeroint <- ifelse(cstintercept &&
                          max(X[, posintercept]) < sqrt(.Machine$double.eps),
                          TRUE, FALSE)
        if (length(cstcol) > 0){
            if (cstcovar.rm == "covariates" | ! zeroint) cstcol <- cstcol[- posintercept]
            if (length(cstcol) > 0){
                X <- X[, - match(cstcol, colnames(X)), drop = FALSE]
                attr(X, "constant") <- cstcol
            }
        }
    }
    structure(X, assign = X.assi, contrasts = X.contr, index = index)
}
