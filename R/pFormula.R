## pFormula:
## methods : formula, model.frame, model.matrix, pmodel.response

pFormula <- function(object) {
    stopifnot(inherits(object, "formula"))
    if (!inherits(object, "Formula")){
        object <- Formula(object)
    }
    class(object) <- union("pFormula", class(object))
    object
}

as.Formula.pFormula <- function(x, ...){
    class(x) <- setdiff(class(x), "pFormula")
    x
}

model.frame.pFormula <- function(formula, data, ..., lhs = NULL, rhs = NULL){
    if (is.null(rhs)) rhs <- 1:(length(formula)[2])
    if (is.null(lhs)) lhs <- ifelse(length(formula)[1] > 0, 1, 0)
    index <- attr(data, "index")
    mf <- model.frame(as.Formula(formula), as.data.frame(data), ..., rhs = rhs)
    index <- index[as.numeric(rownames(mf)), ]
    index <- droplevels(index)
    class(index) <- c("pindex", "data.frame")
    structure(mf,
              index = index,
              class = c("pdata.frame", class(mf)))
}

model.matrix.plm <- function(object, ...){
    dots <- list(...)
    model <- ifelse(is.null(dots$model), describe(object, "model"), dots$model)
    effect <- ifelse(is.null(dots$effect), describe(object, "effect"), dots$effect)
    rhs <- ifelse(is.null(dots$rhs), 1, dots$rhs)
    cstcovar.rm <- ifelse(is.null(dots$cstcovar.rm), "none", dots$cstcovar.rm)
    formula <- formula(object)
    data <- model.frame(object)
    if (model != "random"){
        model.matrix(formula, data, model = model, effect = effect, rhs = rhs, cstcovar.rm = cstcovar.rm)
    }
    else{
        theta <- ercomp(object)$theta
        model.matrix(formula, data, model = model, effect = effect, theta = theta, rhs = rhs, cstcovar.rm = cstcovar.rm)
    }
}

pmodel.response <- function(object, ...) {
    UseMethod("pmodel.response")
}

pmodel.response.plm <- function(object, ...){
    y <- model.response(model.frame(object))
    dots <- list(...)
    if (is.null(dots$model)) model <- describe(object, "model") else model <- dots$model
    if (is.null(dots$effect)) effect <- describe(object, "effect") else effect <- dots$effect
    if (is.null(dots$theta)){
        if (describe(object, "model") == "random") theta <- ercomp(object)$theta else theta <- NULL
    }
    else theta <- dots$theta
    ptransform(y, model = model, effect = effect, theta = theta)
}

pmodel.response.data.frame <- function(object, ...){
    dots <- list(...)
    if (is.null(attr(object, "terms"))) stop("not a model.frame")
    if (is.null(dots$model)) model <- "pooling" else model <- dots$model#stop("the model argument is mandatory")
    if (is.null(dots$effect)) effect <- "individual" else effect <- dots$effect#stop("the effect argument is mandatory")
    if (is.null(dots$theta)) theta <- NULL else theta <- dots$theta
    y <- model.response(object)
    ptransform(y, model = model, effect = effect, theta = theta)
}

pmodel.response.formula <- function(object, ...){
    dots <- list(...)
    data <- dots$data
    if (is.null(data)) stop("the data argument is mandatory")
    if (is.null(attr(data, "terms"))) data <- model.frame(pFormula(object), data)
    model <- dots$model
    effect <- dots$effect
    theta <- dots$theta
    if (is.null(model)) model <- "pooling"#stop("the model argument is mandatory")
    if (is.null(effect)) effect <- "individual"#stop("the effect argument is mandatory")
    if (model == "random" & is.null(theta)) stop("the theta argument is mandatory")
    y <- model.response(data)
    ptransform(y, model = model, effect = effect, theta = theta)
}

ptransform <- function(x, model = NULL, effect = NULL, theta = NULL, ...){
    if (model == "pooling") return(x)
    if (effect == "twoways" & model %in% c("between", "fd"))
        stop("twoways effect only relevant for within, random and pooling models")
#    if (effect == "individual") theindex <- index(x)[[1]] else theindex <- index(x)[[2]]
    if (model == "within") x <- Within(x, effect)
    if (model == "between") x <- between(x, effect)
    if (model == "Between") x <- Between(x, effect)
#    if (model == "fd") x <- pdiff(x, theindex)
    if (model == "fd") x <- pdiff(x, "individual")
    if (model == "random"){
        if (is.null(theta)) stop("a theta argument should be provided")
        if (effect %in% c("time", "individual")) x <- x - theta * Between(x, effect)
        if (effect == "nested") x <- x - theta$id * Between(x, "individual") - theta$gp * Between(x, "group")
        if (effect == "twoways" & is.pbalanced(x))
            x <- x - theta$id * Between(x, "individual") - theta$time * Between(x, "time") + theta$total * mean(x)
    }
    structure(x, index = index(x), class = union("pseries", class(x)))
}

####

Mean <- function(x) matrix(.colMeans(x, nrow(x), ncol(x)),
                           nrow(x), ncol(x), byrow = TRUE)

model.matrix.pFormula <- function(object, data,
                                  model = c("pooling", "within", "Between", "Sum",
                                            "between", "mean", "random", "fd"),
                                  effect = c("individual", "time", "twoways", "nested"),
                                  rhs = 1,
                                  theta = NULL,
                                  cstcovar.rm = c("none", "intercept", "covariates", "all"),
                                  ...){
    
    model <- match.arg(model)
    effect <- match.arg(effect)
    formula <- object  
    has.intercept <- has.intercept(formula, rhs = rhs)
    cstcovar.rm <- match.arg(cstcovar.rm)
    balanced <- is.pbalanced(data)
    # check if inputted data is a model.frame, if not convert it to
    # model.frame (important for NA handling of the original data when
    # model.matrix.pFormula is called directly) As there is no own
    # class for a model.frame, check if the 'terms' attribute is
    # present (this mimics what lm does to detect a model.frame)    
    if (is.null(attr(data, "terms"))) data <- model.frame.pFormula(pFormula(formula), data)  
    # this goes to Formula::model.matrix.Formula:
    X <- model.matrix(as.Formula(formula), rhs = rhs, data = data, ...)
    X.assi <- attr(X, "assign")
    X.contr <- attr(X, "contrasts")
    X.contr <- X.contr[ ! sapply(X.contr, is.null) ]
    index <- index(data)
    if (anyNA(index[[1]])) stop("NA in the individual index variable")
    attr(X, "index") <- index
    if (effect == "twoways" & model %in% c("between", "fd"))
        stop("twoways effect only relevant for within, random and pooling models")
#    if (effect == "individual") cond <- index[[1]] else cond <- index[[2]]
    if (model == "within") X <- Within(X, effect)
    if (model == "Sum") X <- Sum(X, effect)
    if (model == "Between") X <- Between(X, effect)
    if (model == "between") X <- between(X, effect)
    if (model == "mean") X <- Mean(X)
#    if (model == "fd") X <- pdiff(X, cond, effect = effect, has.intercept = has.intercept)
    if (model == "fd") X <- pdiff(X, effect = "individual")
    if (model == "random"){
        if (is.null(theta)) stop("a theta argument should be provided")
        if (effect %in% c("time", "individual")) X <- X - theta * Between(X, effect)
        if (effect == "nested") X <- X - theta$id * Between(X, "individual") - theta$gp * Between(X, "group")
        if (effect == "twoways" & balanced)
            X <- X - theta$id * Between(X, "individual") - theta$time * Between(X, "time") + theta$total * Mean(X)
    }
    if (cstcovar.rm == "intercept"){
        posintercept <- match("(Intercept)", colnames(X))
        if (! is.na(posintercept)){
            X <- X[, - posintercept]
            attr(X, "constant") <- "(Intercept)"
        }
    }
    if (cstcovar.rm %in% c("covariates", "all")){
        sds <- apply(X, 2, sd)
        cstcol <- names(sds)[sds < 1E-07]
        posintercept <- match("(Intercept)", cstcol)
        if (! is.na(posintercept)) zeroint <- ifelse(max(abs(X[, "(Intercept)"])) < 1E-07, TRUE, FALSE)        
        if (length(cstcol) > 0){
            if (cstcovar.rm == "covariates" | (! is.na(posintercept) && ! zeroint)){
                posintercept <- match("(Intercept)", cstcol)
                if (! is.na(posintercept)){
                    cstcol <- cstcol[- posintercept]
                }
            }
            if (length(cstcol > 0)){
                X <- X[, - match(cstcol, colnames(X)), drop = FALSE]
                attr(X, "constant") <- cstcol
            }
        }
    }
    structure(X, assign = X.assi, contrasts = X.contr, index = index)
}
