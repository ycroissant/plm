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

model.matrix.pFormula <- function(object, data,
                                  model = c("pooling", "within", "Between", "Sum",
                                            "between", "mean", "random", "fd"),
                                  effect = c("individual", "time", "twoways", "nested"),
                                  rhs = 1,
                                  theta = NULL,
                                  rm.cst = FALSE, ...){
    model <- match.arg(model)
    effect <- match.arg(effect)
    formula <- object
    
    has.intercept <- has.intercept(formula, rhs = rhs)
    
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
    index <- attr(data, "index")
    id <- index[[1]]
    if (anyNA(id)) stop("NA in the individual index variable")
    time <- index[[2]]
    if (length(index) == 3) group <- index[[3]]
    balanced <- is.pbalanced(data)
#    if (has.intercept && model == "within") X <- X[ , -1, drop = FALSE]
    if (effect %in% c("individual", "time")){
        if (effect == "individual") cond <- id
        if (effect == "time") cond <- time
        #!YC! rm.null FALSE or TRUE ?
        result <- switch(model,
                         "within"  = Within(X, cond, rm.null = rm.cst),
                         "Sum"     = Sum(X, cond),
                         "Between" = Between(X, cond),
                         "between" = between(X, cond),
                         "pooling" = X,
                         "mean"    = matrix(.colMeans(X, nrow(X), ncol(X)), nrow(X), ncol(X), byrow = TRUE),
                         "random"  = X - theta * Between(X, cond),
                         "fd"      = pdiff(X, cond, effect = effect, has.intercept = has.intercept)
                         )
    }
    if (effect == "twoways"){
        if (balanced){ # two-ways balanced
            result <- switch(model,
                             "within"  = X - Between(X, id) - Between(X, time) +
                                 matrix(.colMeans(X, nrow(X), ncol(X)), nrow(X), ncol(X), byrow = TRUE),
                             "random"  = X - theta$id * Between(X,id) - theta$time * Between(X, time) +
                                 theta$total * matrix(.colMeans(X, nrow(X), ncol(X)), nrow(X), ncol(X), byrow = TRUE),
                             "pooling" = X,
                             # place case "mean" here also?  catch
                             # everything else (twoways balanced) and
                             # give error message
                             stop(paste0("in model.matrix.pFormula: no model.matrix for model =\"", model, "\" and effect = \"", effect, "\" meaningful or implemented"))
                             )
            # very QDF ! remove the column of one for two-ways balanced within (no call to the Within function)
            if (model == "within" & rm.cst == TRUE & ncol(result) > 1) result <- result[, -1]
        }
        else{ # two-ways unbalanced
            result <- switch(model,
                             "within"  = { # Wansbeek/Kapteyn (1989), Journal of Econometrics, 41, pp. 341-361 (2.12)
                                 if ("(Intercept)" %in% colnames(X))
                                     X <- X[, - match("(Intercept)", colnames(X)), drop = FALSE]
                                 Dmu <- model.matrix(~ time - 1)
                                 W1 <- Within(X, id, rm.null = FALSE)
                                 WDmu <- Within(Dmu, id)
                                 W2 <- fitted(lm.fit(WDmu, X))
                                 result <- W1 - W2
                                 if (! rm.cst) result <- cbind("(Intercept)"= 1, result)
                                 result
                             },
                             "pooling" = X,
                             "random" = X, # !YC! this value is irrelevant, just in order to compute the summary
                             stop(paste0("in model.matrix.pFormula: no model.matrix for model=\"", model, "\" and effect=\"", effect, "\" meaningful or implemented"))
                             )
        }
    }
    if (effect == "nested"){
        if (model == "pooling"){
            result <- X
        }
        else{
            result <- X - theta$id * Between(X, id) - theta$gp * Between(X, group)
        }
    }

    attr(result, "assign") <- X.assi
    attr(result, "contrasts") <- X.contr
    result
}


## pmodel.response.data.frame <- function(object,
##                                        model = c("pooling", "within", "Between",
##                                                  "between", "mean", "random", "fd"),
##                                        effect = c("individual", "time", "twoways", "nested"),
##                                        lhs = NULL,
##                                        theta = NULL, ...){
##     data <- object
##     model <- match.arg(model)
##     formula <- formula(paste("~ ", names(data)[[1]], " - 1", sep = ""))
##     y <- model.matrix(pFormula(formula), data = data,
##                       model = model, effect = effect,
##                       lhs = lhs, theta = theta, ...)
##     namesy <- rownames(y)
##     y <- as.numeric(y)
##     names(y) <- namesy
##     y
## }

## pmodel.response.pFormula <- function(object, data,
##                                      model = c("pooling", "within", "Between",
##                                                "between", "mean", "random", "fd"),
##                                      effect = c("individual", "time", "twoways", "nested"),
##                                      lhs = NULL,
##                                      theta = NULL, ...){
##     formula <- pFormula(object) # was: formula <- object
##     model <- match.arg(model)
  
##     # check if inputted data is already a model.frame, if not convert it to model.frame
##     # (important for NA handling of the original data when pmodel.response is called directly)
##     # As there is no own class for a model.frame, check if the 'terms' attribute
##     # is present (this mimics what lm does to detect a model.frame)
##     if (is.null(attr(data, "terms"))) {
##         data <- model.frame.pFormula(pFormula(formula), data)
##     }
  
##     if (is.null(lhs))
##         if (length(formula)[1] == 0) stop("no response") else lhs <- 1
##     formula <- formula(paste("~ ", deparse(attr(formula, "lhs")[[lhs]]), " - 1", sep = ""))
    
##     y <- model.matrix(pFormula(formula), data = data,
##                       model = model, effect = effect,
##                       lhs = lhs, theta = theta, ...)
##                                         #  dim(y) <- NULL
##     namesy <- rownames(y)
##     y <- as.numeric(y)
##     names(y) <- namesy
##     y
## }

model.matrix.plm <- function(object, ...){
    dots <- list(...)
    model <- ifelse(is.null(dots$model), describe(object, "model"), dots$model)
    effect <- ifelse(is.null(dots$effect), describe(object, "effect"), dots$effect)
    rhs <- ifelse(is.null(dots$rhs), 1, dots$rhs)
    rm.cst <- ifelse(is.null(dots$rm.cst), FALSE, dots$rm.cst)
    formula <- formula(object)
    data <- model.frame(object)
    if (model != "random"){
        model.matrix(formula, data, model = model, effect = effect, rhs = rhs, rm.cst = rm.cst)
    }
    else{
        theta <- ercomp(object)$theta
        model.matrix(formula, data, model = model, effect = effect, theta = theta, rhs = rhs, rm.cst = rm.cst)
    }
}

## pmodel.response.plm <- function(object, ...){
##     dots <- list(...)
##     model <- ifelse(is.null(dots$model), describe(object, "model"), dots$model)
##     effect <- ifelse(is.null(dots$effect), describe(object, "effect"), dots$effect)
##     rm.cst <- ifelse(is.null(dots$rm.cst), FALSE, dots$rm.cst)
    
##     formula <- formula(object)
##     data <- model.frame(object)
##     if (model != "random"){
##         pmodel.response(formula, data, model = model, effect = effect, rm.cst = rm.cst)
##     }
##     else{
##         theta <- ercomp(object)$theta
##         pmodel.response(formula, data, model = model, effect = effect, theta = theta)
##     }
## }


#Comment extraire y comme une pseries ?

ptransform <- function(x, model = NULL, effect = NULL, theta = NULL, ...){
    if (model == "pooling") return(x)
    if (effect == "twoways" & model %in% c("between", "fd"))
        stop("twoways effect only relevant for within, random and pooling models")

    if (effect != "twoways"){
        if (effect == "individual") theindex <- index(x)[[1]] else theindex <- index(x)[[2]]
        if (model == "within") x <- Within(x, effect)
        if (model == "between") x <- between(x, effect)
        if (model == "Between") x <- Between(x, effect)
        if (model == "fd") x <- pdiff(x, theindex)
        if (model == "random"){
            if (is.null(theta))("a theta argument should be provided")
            if (effect != "nested") x <- x - theta * Between(x, effect)
            else x <- x - theta$id * Between(x, "individual") - theta$gp * Between(x, "group")
        }
    }
    else{
        if (model == "within"){
            if (is.pbalanced(x)){
                x <- x - Between(x, "individual") - Between(x, "time") + mean(x)
            }
            else{
                time <- index(x)[[2]]
                Dmu <- model.matrix(~ time - 1)
                W1 <- Within(x, "individual")
                WDmu <- Within(Dmu, index(x)[[1]])
                W2 <- fitted(lm.fit(WDmu, x))
                x <- W1 - W2
            }
        }
        if (model == "random"){
            if (is.pbalanced(x)){
                x <- x - theta$id * Between(x, "individual") - theta$time * Between(x, "time") +
                    theta$total * mean(x)
            }
        }
    }
    structure(x, index = index(x), class = union("pseries", class(x)))
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
    y <- ptransform(y, model = model, effect = effect, theta = theta)
    namesy <- names(y)
    y <- as.numeric(y)
    names(y) <- namesy
    y
}

pmodel.response.data.frame <- function(object, ...){
    dots <- list(...)
    if (is.null(attr(object, "terms"))) stop("not a model.frame")
    if (is.null(dots$model)) model <- "pooling" else model <- dots$model#stop("the model argument is mandatory")
    if (is.null(dots$effect)) effect <- "individual" else effect <- dots$effect#stop("the effect argument is mandatory")
    if (is.null(dots$theta)) theta <- NULL else theta <- dots$theta
    y <- model.response(object)
#    model <- dots$model
#    effect <- dots$effect
    y <- ptransform(y, model = model, effect = effect, theta = theta)
    namesy <- names(y)
    y <- as.numeric(y)
    names(y) <- namesy
    y
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
    y <- ptransform(y, model = model, effect = effect, theta = theta)
    namesy <- names(y)
    y <- as.numeric(y)
    names(y) <- namesy
    y
}

