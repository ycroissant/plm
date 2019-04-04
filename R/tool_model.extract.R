# model.frame method for pdata.frame ; the formula argument must be a
# pdata.frame and the data argument must be a formula, which is quite
# hesoteric, but consistent with the argument list of
# model.frame.Formula which is latter called.



#' model.frame and model.matrix for panel data
#' 
#' Methods to create model frame and model matrix for panel data.
#' 
#' The \code{lhs} and \code{rhs} arguments are inherited from
#' \code{Formula}, see there for more details.\cr The
#' \code{model.frame} methods return a \code{pdata.frame} object
#' suitable as an input to plm's \code{model.matrix}.\cr The
#' \code{model.matrix} methods builds a model matrix with
#' transformations performed as specified by the \code{model} and
#' \code{effect} arguments (and \code{theta} if \code{model =
#' "random"} is requested), in this case the supplied \code{data}
#' argument should be a model frame created by plm's
#' \code{model.frame} method. If not, it is tried to construct the
#' model frame from the data. Constructing the model frame first
#' ensures proper NA handling, see \bold{Examples}.
#'
#' @name model.frame.pdata.frame
#' @param object,formula an object of class \code{"pdata.frame"} or an
#'     estimated model object of class \code{"plm"},
#' @param x a `model.frame`
#' @param data a \code{formula}, see \bold{Details},
#' @param effect the effects introduced in the model, one of
#'     \code{"individual"}, \code{"time"}, \code{"twoways"} or
#'     \code{"nested"},
#' @param model one of \code{"pooling"}, \code{"within"},
#'     \code{"Sum"}, \code{"Between"}, \code{"between"},
#'     \code{"random",} \code{"fd"} and \code{"ht"},
#' @param theta the parameter for the transformation if \code{model =
#'     "random"},
#' @param cstcovar.rm remove the constant columns, one of
#'     \code{"none", "intercept", "covariates", "all")},
#' @param lhs inherited from package \code{\link[Formula]{Formula}}
#'     (see there),
#' @param rhs inherited from package \code{\link[Formula]{Formula}}
#'     (see there),
#' @param dot inherited from package \code{\link[Formula]{Formula}}
#'     (see there),
#' @param \dots further arguments.
#' @return The \code{model.frame} methods return a
#'     \code{pdata.frame}.\cr The \code{model.matrix} methods return a
#'     \code{matrix}.
#' @author Yves Croissant
#' @seealso \code{\link{pmodel.response}} for (transformed) response
#'     variable.\cr \code{\link[Formula]{Formula}} from package
#'     \code{Formula}, especially for the \code{lhs} and \code{rhs}
#'     arguments.
#' @keywords classes
#' @examples
#' 
#' # First, make a pdata.frame
#' data("Grunfeld", package = "plm")
#' pGrunfeld <- pdata.frame(Grunfeld)
#' 
#' # then make a model frame from a pFormula and a pdata.frame
#' #pform <- pFormula(inv ~ value + capital)
#' #mf <- model.frame(pform, data = pGrunfeld)
#' form <- inv ~ value
#' mf <- model.frame(pGrunfeld, form)
#' 
#' # then construct the (transformed) model matrix (design matrix)
#' # from formula and model frame
#' #modmat <- model.matrix(pform, data = mf, model = "within")
#' modmat <- model.matrix(mf, model = "within")
#' 
#' ## retrieve model frame and model matrix from an estimated plm object
#' #fe_model <- plm(pform, data = pGrunfeld, model = "within")
#' fe_model <- plm(form, data = pGrunfeld, model = "within")
#' model.frame(fe_model)
#' model.matrix(fe_model)
#' 
#' # same as constructed before
#' all.equal(mf, model.frame(fe_model), check.attributes = FALSE) # TRUE
#' all.equal(modmat, model.matrix(fe_model), check.attributes = FALSE) # TRUE
#' 
NULL

#' @rdname model.frame.pdata.frame
#' @export
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


#' @rdname model.frame.pdata.frame
#' @export
formula.pdata.frame <- function(x, ...){
    if (is.null(attr(x, "terms")))
        stop("formula expect a model.frame and not an ordinary pdata.frame")
    attr(x, "formula")
}
    

#' @rdname model.frame.pdata.frame
#' @export
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

Mean <- function(x) matrix(.colMeans(x, nrow(x), ncol(x)),
                           nrow(x), ncol(x), byrow = TRUE)

#' @rdname model.frame.pdata.frame
#' @export
model.matrix.pdata.frame <- function(object, 
                                     model = c("pooling", "within", "Between", "Sum",
                                               "between", "mean", "random", "fd"),
                                     effect = c("individual", "time", "twoways", "nested"),
                                     rhs = 1,
                                     theta = NULL,
                                     cstcovar.rm = NULL,
                                     ...){
    if (is.null(attr(object, "terms")))
        stop("model.matrix expects a model.frame and not an ordinary pdata.frame")
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
            if ((cstcovar.rm == "covariates" || !zeroint) && cstintercept) cstcol <- cstcol[- posintercept]
            if (length(cstcol) > 0){
                X <- X[, - match(cstcol, colnames(X)), drop = FALSE]
                attr(X, "constant") <- cstcol
            }
        }
    }
    structure(X, assign = X.assi, contrasts = X.contr, index = index)
}



#' A function to extract the model.response
#' 
#' pmodel.response has several methods to conveniently extract the
#' response of several objects.
#' 
#' The model response is extracted from a \code{pdata.frame} (where
#' the response must reside in the first column; this is the case for
#' a model frame), a \code{pFormula} + \code{data} or a \code{plm}
#' object, and the transformation specified by \code{effect} and
#' \code{model} is applied to it.\cr Constructing the model frame
#' first ensures proper NA handling and the response being placed in
#' the first column, see also \bold{Examples} for usage.
#' 
#' @aliases pmodel.response
#' @param object an object of class \code{"plm"}, or a formula of
#'     class \code{"pFormula"},
#' @param data a \code{data.frame}
#' @param \dots further arguments.
#' @return A pseries except if model responses' of a \code{"between"}
#'     or "fd" model as these models "compress" the data (the number
#'     of observations used in estimation is smaller than the original
#'     data due to the specific transformation). A numeric is returned
#'     for the "between" and "fd" model.
#' @export
#' @author Yves Croissant
#' @seealso \code{plm}'s \code{\link{model.matrix}} for (transformed)
#'     model matrix and the corresponding \code{\link{model.frame}}
#'     method to construct a model frame.
#' @keywords manip
#' @examples
#' 
#' # First, make a pdata.frame
#' data("Grunfeld", package = "plm")
#' pGrunfeld <- pdata.frame(Grunfeld)
#' 
#' # then make a model frame from a pFormula and a pdata.frame
#' 
#' 
#' form <- inv ~ value + capital
#' mf <- model.frame(pGrunfeld, form)
#' # construct (transformed) response of the within model
#' resp <- pmodel.response(form, data = mf, model = "within", effect = "individual")
#' # retrieve (transformed) response directly from model frame
#' resp_mf <- pmodel.response(mf, model = "within", effect = "individual")
#' 
#' # retrieve (transformed) response from a plm object, i.e. an estimated model
#' fe_model <- plm(form, data = pGrunfeld, model = "within")
#' pmodel.response(fe_model)
#' 
#' # same as constructed before
#' all.equal(resp, pmodel.response(fe_model), check.attributes = FALSE) # TRUE
#' 
pmodel.response <- function(object, ...) {
    UseMethod("pmodel.response")
}

#' @rdname pmodel.response
#' @export
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

#' @rdname pmodel.response
#' @export
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
#' @rdname pmodel.response
#' @export
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

