# model.frame method for pdata.frame ; the formula argument must be a
# pdata.frame and the data argument must be a formula, which is quite
# esoteric, but consistent with the argument list of
# model.frame.Formula which is latter called.



#' model.frame and model.matrix for panel data
#' 
#' Methods to create model frame and model matrix for panel data.
#' 
#' The `lhs` and `rhs` arguments are inherited from `Formula`, see
#' there for more details.\cr The `model.frame` methods return a
#' `pdata.frame` object suitable as an input to plm's
#' `model.matrix`.\cr The `model.matrix` methods builds a model matrix
#' with transformations performed as specified by the `model` and
#' `effect` arguments (and `theta` if `model = "random"` is
#' requested), in this case the supplied `data` argument should be a
#' model frame created by plm's `model.frame` method. If not, it is
#' tried to construct the model frame from the data. Constructing the
#' model frame first ensures proper `NA` handling, see **Examples**.
#'
#' @name model.frame.pdata.frame
#' @param object,formula an object of class `"pdata.frame"` or an
#'     estimated model object of class `"plm"`,
#' @param x a `model.frame`
#' @param data a `formula`, see **Details**,
#' @param effect the effects introduced in the model, one of
#'     `"individual"`, `"time"`, `"twoways"` or `"nested"`,
#' @param model one of `"pooling"`, `"within"`, `"Sum"`, `"Between"`,
#'     `"between"`, `"random",` `"fd"` and `"ht"`,
#' @param theta the parameter for the transformation if `model =
#'     "random"`,
#' @param cstcovar.rm remove the constant columns, one of `"none",
#'     "intercept", "covariates", "all")`,
#' @param lhs inherited from package [Formula::Formula()] (see
#'     there),
#' @param rhs inherited from package [Formula::Formula()] (see
#'     there),
#' @param dot inherited from package [Formula::Formula()] (see
#'     there),
#' @param \dots further arguments.
#' @return The `model.frame` methods return a `pdata.frame`.\cr The
#'     `model.matrix` methods return a `matrix`.
#' @author Yves Croissant
#' @seealso [pmodel.response()] for (transformed) response
#'     variable.\cr [Formula::Formula()] from package `Formula`,
#'     especially for the `lhs` and `rhs` arguments.
#' @keywords classes
#' @examples
#' 
#' # First, make a pdata.frame
#' data("Grunfeld", package = "plm")
#' pGrunfeld <- pdata.frame(Grunfeld)
#' 
#' # then make a model frame from a formula and a pdata.frame
#' form <- inv ~ value
#' mf <- model.frame(pGrunfeld, form)
#' 
#' # then construct the (transformed) model matrix (design matrix)
#' # from model frame
##modmat <- model.matrix(pform, data = mf, model = "within")
#' modmat <- model.matrix(mf, model = "within")
#' 
#' ## retrieve model frame and model matrix from an estimated plm object
## #fe_model <- plm(pform, data = pGrunfeld, model = "within")
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
                                    lhs = NULL, rhs = NULL, dot = "previous"){
    pdata <- formula
    formula <- as.Formula(data)
    if (is.null(rhs)) rhs <- seq_len(length(formula)[2L])
    if (is.null(lhs)) lhs <- if(length(formula)[1L] > 0L) 1 else 0
    index <- attr(pdata, "index")
    mf <- model.frame(formula, as.data.frame(pdata, row.names = FALSE), ..., # NB need row.names = FALSE to ensure mf has integer sequence as row names
                      lhs = lhs, rhs = rhs, dot = dot)
    index <- index[as.numeric(rownames(mf)), ] # reduce index down to rows left in model frame
    checkNA.index(index) # check for NAs in model.frame's index and error if any
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
    model  <- if(is.null(dots$model))  describe(object, "model")  else dots$model
    effect <- if(is.null(dots$effect)) describe(object, "effect") else dots$effect
    rhs    <- if(is.null(dots$rhs)) 1 else dots$rhs
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
    has.intercept <- has.intercept(formula, rhs = rhs, data = data)
    # relevant defaults for cstcovar.rm
    if(is.null(cstcovar.rm)) cstcovar.rm <- if(model == "within") "intercept" else "none"
    balanced <- is.pbalanced(data)
    X <- model.matrix(as.Formula(formula), data = data, rhs = rhs, dot = "previous", ...)
    # check for infinite or NA values and exit if there are some
    if(any(! is.finite(X)))
        stop(paste("model matrix or response contains non-finite",
                   "values (NA/NaN/Inf/-Inf)"))
    X.assi <- attr(X, "assign")
    X.contr <- attr(X, "contrasts")
    X.contr <- X.contr[ ! vapply(X.contr, is.null, FUN.VALUE = TRUE, USE.NAMES = FALSE) ]
    index <- index(data)
    attr(X, "index") <- index
    if(effect == "twoways" && model %in% c("between", "fd"))
        stop("twoways effect only relevant for within, random, and pooling models")
    if(model == "within")  X <- Within(X, effect)
    if(model == "Sum")     X <- Sum(X, effect)
    if(model == "Between") X <- Between(X, effect)
    if(model == "between") X <- between(X, effect)
    if(model == "mean")    X <- Mean(X)
    if(model == "fd")      X <- pdiff(X, effect = "individual",   # pdiff has its own handling of intercept and const. columns
                                   has.intercept = has.intercept, shift = "row")
    if(model == "random"){
        if(is.null(theta)) stop("a theta argument must be provided for model = \"random\"")
        if(effect %in% c("time", "individual")) X <- X - theta * Between(X, effect)
        if(effect == "nested") X <- X - theta$id * Between(X, "individual") -
                                    theta$gp * Between(X, "group")
        if(effect == "twoways" && balanced)
            X <- X - theta$id * Between(X, "individual") -
                theta$time * Between(X, "time") + theta$total * Mean(X)
        ## TODO: case unbalanced twoways not treated here. Catch and error gracefully?
        # if (effect == "twoways" && !balanced) stop("two-way unbalanced case not implemented in model.matrix.pdata.frame")
    }
    if(cstcovar.rm == "intercept"){
        posintercept <- match("(Intercept)", colnames(X))
        if (! is.na(posintercept)) X <- X[ , - posintercept, drop = FALSE]
    }
    if(cstcovar.rm %in% c("covariates", "all")){
        cols <- apply(X, 2, is.constant)
        cstcol <- names(cols)[cols]
        posintercept <- match("(Intercept)", cstcol)
        cstintercept <- if(is.na(posintercept)) FALSE else TRUE
        zeroint <- if(cstintercept &&
                          max(X[ , posintercept]) < sqrt(.Machine$double.eps))
                          TRUE else FALSE
        if(length(cstcol) > 0L){
            if((cstcovar.rm == "covariates" || !zeroint) && cstintercept) cstcol <- cstcol[- posintercept]
            if(length(cstcol) > 0L){
                X <- X[ , - match(cstcol, colnames(X)), drop = FALSE]
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
#' The model response is extracted from a `pdata.frame` (where the
#' response must reside in the first column; this is the case for a
#' model frame), a `Formula` + `data` (being a model frame) or a `plm` 
#' object, and the
#' transformation specified by `effect` and `model` is applied to
#' it.\cr Constructing the model frame first ensures proper `NA`
#' handling and the response being placed in the first column, see
#' also **Examples** for usage.
#' 
#' @aliases pmodel.response
#' @param object an object of class `"plm"`, or a formula of
#'     class `"Formula"`,
#' @param data a `data.frame`
#' @param \dots further arguments.
#' @return A pseries except if model responses' of a `"between"`
#'     or `"fd"` model as these models "compress" the data (the number
#'     of observations used in estimation is smaller than the original
#'     data due to the specific transformation). A numeric is returned
#'     for the `"between"` and `"fd"` model.
#' @export
#' @author Yves Croissant
#' @seealso `plm`'s [model.matrix()] for (transformed)
#'     model matrix and the corresponding [model.frame()]
#'     method to construct a model frame.
#' @keywords manip
#' @examples
#' 
#' # First, make a pdata.frame
#' data("Grunfeld", package = "plm")
#' pGrunfeld <- pdata.frame(Grunfeld)
#' 
#' # then make a model frame from a Formula and a pdata.frame
#' form <- inv ~ value + capital
#' mf <- model.frame(pGrunfeld, form)
#'
#' # retrieve (transformed) response directly from model frame
#' resp_mf <- pmodel.response(mf, model = "within", effect = "individual")
#' 
#' # retrieve (transformed) response from a plm object, i.e., an estimated model
#' fe_model <- plm(form, data = pGrunfeld, model = "within")
#' pmodel.response(fe_model)
#' 
#' # same as constructed before
#' all.equal(resp_mf, pmodel.response(fe_model), check.attributes = FALSE) # TRUE
#' 
pmodel.response <- function(object, ...) {
    UseMethod("pmodel.response")
}

#' @rdname pmodel.response
#' @export
pmodel.response.plm <- function(object, ...){
    y <- model.response(model.frame(object))
    dots <- list(...)
    model  <- if(is.null(dots$model))   describe(object, "model")  else dots$model 
    effect <- if(is.null(dots$effect))  describe(object, "effect") else dots$effect
    theta  <- if(is.null(dots$theta)) {
                    if(describe(object, "model") == "random") 
                        ercomp(object)$theta else NULL
                } else dots$theta
    ptransform(y, model = model, effect = effect, theta = theta)
}

#' @rdname pmodel.response
#' @export
pmodel.response.data.frame <- function(object, ...){
    dots <- list(...)
    if(is.null(attr(object, "terms"))) stop("not a model.frame")
    model  <- if(is.null(dots$model))  "pooling"    else dots$model
    effect <- if(is.null(dots$effect)) "individual" else dots$effect
    theta  <- if(is.null(dots$theta))  NULL         else dots$theta
    y <- model.response(object) # has index attribute but not class 'pseries'
    class(y) <- unique(c("pseries", class(y)))
    ptransform(y, model = model, effect = effect, theta = theta)
}

# "deprecated" (not advertised anymore)
#' @rdname pmodel.response
#' @export
pmodel.response.formula <- function(object, data, ...){
  #  print("pmodel.response.formula")
  dots <- list(...)
  if(is.null(data)) stop("the data argument is mandatory")
  if(! inherits(data, "pdata.frame")) stop("the data argument must be a pdata.frame")
  if(is.null(attr(data, "terms"))) data <- model.frame(data, object)
  model  <- dots$model
  effect <- dots$effect
  theta  <- dots$theta
  if(is.null(model)) model <- "pooling"
  if(is.null(effect)) effect <- "individual"
  if(model == "random" && is.null(theta)) stop("the theta argument is mandatory for model = \"random\"")
  y <- model.response(data)
  class(y) <- unique(c("pseries", class(y)))
  ptransform(y, model = model, effect = effect, theta = theta)
}


ptransform <- function(x, model = NULL, effect = NULL, theta = NULL, ...){
    # NB: ptransform (and hence pmodel.response) does not handle the random 2-way unbalanced case
    
    if(model == "pooling") return(x) # early exit
    if(effect == "twoways" && model %in% c("between", "fd"))
        stop("twoways effect only relevant for within, random, and pooling models")

    if(model == "within")  x <- Within(x, effect)
    if(model == "between") x <- between(x, effect)
    if(model == "Between") x <- Between(x, effect)
    if(model == "fd")      x <- pdiff(x, effect = "individual", shift = "row")
    if(model == "random") {
        balanced <- is.pbalanced(x) # need to check this right here as long as x is a pseries
        if(is.null(theta)) stop("a theta argument must be provided")
        if(effect %in% c("time", "individual")) x <- x - theta * Between(x, effect)
        if(effect == "nested") x <- x - theta$id * Between(x, "individual") -
                                         theta$gp * Between(x, "group")
        if(effect == "twoways" && balanced)
            x <- x - theta$id   * Between(x, "individual") -
                     theta$time * Between(x, "time") + theta$total * mean(x)
        ## TODO: could catch non-treated RE unbalanced twoways case to error gracefully:
        # if (effect == "twoways" && !balanced) warning("two-way unbalanced case not implemented in ptransform")
    }
    
    # between and fd models "compress" the data, thus an index does not make
    # sense for those, but add to all others (incl. Between (capital B))
    x <- if(model %in% c("between", "fd")) x
         else structure(x, index = index(x), class = unique(c("pseries", class(x))))
    return(x)
}

