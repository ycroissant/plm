# functions to aid in detecting linear dependent columns in the (transformed) 
# model matrix or estimated plm models:
#  * detect.lindep
#  * alias (the latter is a wrapper around alias.lm)
#  * detect_lin_dep for backward compatibility
#
# doc file provides an extensive example how linear dependence can arise after
# the data transformation, e. g., for within transformation

### detect.lindep.matrix, .data.frame, .plm




#' Functions to detect linear dependence
#' 
#' Little helper functions to aid users to detect linear dependent columns in a
#' two-dimensional data structure, especially in a (transformed) model matrix -
#' typically useful in interactive mode during model building phase.
#' 
#' 
#' Linear dependence of columns/variables is (usually) readily avoided when
#' building one's model.  However, linear dependence is sometimes not obvious
#' and harder to detect for less experienced applied statisticians. The so
#' called "dummy variable trap" is a common and probably the best--known
#' fallacy of this kind (see e. g. Wooldridge (2016), sec. 7-2.). When building
#' linear models with \code{lm} or \code{plm}'s \code{pooling} model, linear
#' dependence in one's model is easily detected, at times post hoc.
#' 
#' However, linear dependence might also occur after some transformations of
#' the data, albeit it is not present in the untransformed data. The within
#' transformation (also called fixed effect transformation) used in the
#' \code{"within"} model can result in such linear dependence and this is
#' harder to come to mind when building a model. See \bold{Examples} for two
#' examples of linear dependent columns after the within transformation: ex. 1)
#' the transformed variables have the opposite sign of one another; ex. 2) the
#' transformed variables are identical.
#' 
#' During \code{plm}'s model estimation, linear dependent columns and their
#' corresponding coefficients in the resulting object are silently dropped,
#' while the corresponding model frame and model matrix still contain the
#' affected columns.  The plm object contains an element \code{aliased} which
#' indicates any such aliased coefficients by a named logical.
#' 
#' Both functions, \code{detect.lindep} and \code{alias}, help to
#' detect linear dependence and accomplish almost the same:
#' \code{detect.lindep} is a stand alone implementation while
#' \code{alias} is a wrapper around
#' \code{\link[stats:alias]{alias.lm}}, extending the \code{alias}
#' generic to classes \code{"plm"} and \code{"pdata.frame"}.
#' \code{alias} hinges on the availability of the package
#' \CRANpkg{MASS} on the system. Not all arguments of \code{alias.lm}
#' are supported.  Output of \code{alias} is more informative as it
#' gives the linear combination of dependent columns (after data
#' transformations, i. e. after (quasi)-demeaning) while
#' \code{detect.lindep} only gives columns involved in the linear
#' dependence in a simple format (thus being more suited for automatic
#' post--processing of the information).
#' 
#' @aliases detect.lindep
#' @param object for \code{detect.lindep}: an object which should be checked
#' for linear dependence (of class \code{"matrix"}, \code{"data.frame"}, or
#' \code{"plm"}); for \code{alias}: either an estimated model of class
#' \code{"plm"} or a \code{"pdata.frame"}. Usually, one wants to input a model
#' matrix here or check an already estimated plm model,
#' @param suppressPrint for \code{detect.lindep} only: logical indicating
#' whether a message shall be printed; defaults to printing the message, i. e.
#' to \code{suppressPrint = FALSE},
#' @param \dots further arguments.
#' @return For \code{detect.lindep}: A named numeric vector containing column
#' numbers of the linear dependent columns in the object after data
#' transformation, if any are present. \code{NULL} if no linear dependent
#' columns are detected.
#' 
#' For \code{alias}: return value of \code{\link[stats:alias]{alias.lm}} run on
#' the (quasi-)demeaned model, i. e. the information outputted applies to the
#' transformed model matrix, not the original data.
#' @note function \code{detect.lindep} was called \code{detect_lin_dep}
#' initially but renamed for naming consistency later with a back-compatible
#' solution.
#' @author Kevin Tappe
#' @seealso \code{\link[stats:alias]{alias}}, \code{\link[stats]{model.matrix}}
#' and especially \code{plm}'s \code{\link{model.matrix}} for (transformed)
#' model matrices, plm's \code{\link{model.frame}}.
#' @references Wooldridge, J.M. (2016) \emph{Introductory Econometrics: A
#' Modern Approach}, 6th ed., Cengage Learning, Boston, sec. 7-2, pp. 206--211.
#' @keywords manip array
#' @examples
#' 
#' ### Example 1 ###
#' # prepare the data
#' data("Cigar" , package = "plm")
#' Cigar[ , "fact1"] <- c(0,1)
#' Cigar[ , "fact2"] <- c(1,0)
#' Cigar.p <- pdata.frame(Cigar)
#' 
#' # setup a formula and a model frame
#' form <- price ~ 0 + cpi + fact1 + fact2
#' mf <- model.frame(Cigar.p, form)
#' # no linear dependence in the pooling model's model matrix
#' # (with intercept in the formula, there would be linear depedence)
#' detect.lindep(model.matrix(mf, model = "pooling"))
#' # linear dependence present in the FE transformed model matrix
#' modmat_FE <- model.matrix(mf, model = "within")
#' detect.lindep(modmat_FE)
#' mod_FE <- plm(form, data = Cigar.p, model = "within")
#' detect.lindep(mod_FE) 
#' alias(mod_FE) # => fact1 == -1*fact2
#' plm(form, data = mf, model = "within")$aliased # "fact2" indicated as aliased
#' 
#' # look at the data: after FE transformation fact1 == -1*fact2
#' head(modmat_FE)
#' all.equal(modmat_FE[ , "fact1"], -1*modmat_FE[ , "fact2"])
#' 
#' ### Example 2 ###
#' # Setup the data:
#' # Assume CEOs stay with the firms of the Grunfeld data
#' # for the firm's entire lifetime and assume some fictional
#' # data about CEO tenure and age in year 1935 (first observation
#' # in the data set) to be at 1 to 10 years and 38 to 55 years, respectively.
#' # => CEO tenure and CEO age increase by same value (+1 year per year).
#' data("Grunfeld", package = "plm")
#' set.seed(42)
#' # add fictional data
#' Grunfeld$CEOtenure <- c(replicate(10, seq(from=s<-sample(1:10,  1), to=s+19, by=1)))
#' Grunfeld$CEOage    <- c(replicate(10, seq(from=s<-sample(38:65, 1), to=s+19, by=1)))
#' 
#' # look at the data
#' head(Grunfeld, 50)
#' 
#' form <- inv ~ value + capital + CEOtenure + CEOage
#' mf <- model.frame(pdata.frame(Grunfeld), form)
#' # no linear dependent columns in original data/pooling model
#' modmat_pool <- model.matrix(mf, model="pooling")
#' detect.lindep(modmat_pool)
#' mod_pool <- plm(form, data = Grunfeld, model = "pooling")
#' alias(mod_pool)
#' 
#' # CEOtenure and CEOage are linear dependent after FE transformation
#' # (demeaning per individual)
#' modmat_FE <- model.matrix(mf, model="within")
#' detect.lindep(modmat_FE)
#' mod_FE <- plm(form, data = Grunfeld, model = "within")
#' detect.lindep(mod_FE)
#' alias(mod_FE)
#' 
#' # look at the transformed data: after FE transformation CEOtenure == 1*CEOage
#' head(modmat_FE, 50)
#' all.equal(modmat_FE[ , "CEOtenure"], modmat_FE[ , "CEOage"])
#' 
detect.lindep <- function(object, ...) {
  UseMethod("detect.lindep")
}

detect_lin_dep <- function(object, ...) {
  .Deprecated(old = "detect_lin_dep", new = "detect.lindep",
              msg = "Function name 'detect_lin_dep' deprecated, please use 'detect.lindep'")
  UseMethod("detect.lindep")
}

#' @rdname detect.lindep
#' @method detect.lindep matrix
#' @export
detect.lindep.matrix <- function(object, suppressPrint = FALSE, ...) {
  if (!inherits(object, "matrix")) {
    stop("Input 'object' must be a matrix. Presumably, one wants a model matrix
         generated by some 'model.matrix' function.")}
  
  # do rank reduction to detect lin. dep. columns
  rank_rec <- sapply(1:ncol(object), function(col) qr(object[ , -col])$rank)
  
  if (diff(range(rank_rec)) == 0) {
    num <- NULL # return NULL if there is no linear dep.
  } else {
    num <- which(rank_rec == max(rank_rec))
    names(num) <- colnames(object)[num]
  }
  
  if(!suppressPrint) {
    if (is.null(num)) {
      print("No linear dependent column(s) detected.")
    } else {
      print(paste0("Suspiscious column number(s): ", paste(num,        collapse = ", ")))
      print(paste0("Suspiscious column name(s):   ", paste(names(num), collapse = ", ")))
    }
    return(invisible(num))
  }
  return(num)
}

#' @rdname detect.lindep
#' @method detect.lindep data.frame
#' @export
detect.lindep.data.frame <- function(object, suppressPrint = FALSE, ...) {
  if (!inherits(object, "data.frame")) {
    stop("Input 'object' must be a data.frame")}

  return(detect.lindep.matrix(as.matrix(object), ...))
}

#' @rdname detect.lindep
#' @method detect.lindep plm
#' @export
detect.lindep.plm <- function(object, suppressPrint = FALSE, ...) {
  if (!inherits(object, "plm")) {
    stop("Input 'object' must be of class \"plm\"")}

  return(detect.lindep.matrix(model.matrix(object), ...))
}


### alias.plm, alias.pFormula
# This is just a wrapper function to allow to apply the generic stats::alias on
# plm objects and pFormulas with the _transformed data_ (the transformed model.matrix).
# NB: arguments 'model' and 'effect' are not treated here.
alias.plm <- function(object, ...) {
  dots <- list(...)
  if (!is.null(dots$inst.method)) stop("alias.plm/alias.pFormula: IV not supported")
  if (length(formula(object))[2] == 2) stop("alias.plm/alias.pFormula: IV not supported")
  
  # catch unsupported alias.lm args and convert
  if (!is.null(dots[["partial"]])) {
    if (dots[["partial"]]) {
      dots[["partial"]] <- FALSE
      warning("alias.plm/alias.pFormula: arg partial = \"TRUE\" not supported, changed to FALSE")
    }
  } 
  if (!is.null(dots[["partial.pattern"]])) {
    if (dots[["partial.pattern"]]) {
      dots[["partial.pattern"]] <- FALSE
      warning("alias.plm/alias.pFormula: arg partial.pattern = \"TRUE\" not supported, changed to FALSE")
    }
  }
  
  X <- model.matrix(object)
  y <- pmodel.response(object)
  
  lm.fit.obj <- lm.fit(X, y)
  class(lm.fit.obj) <- "lm"
  lm.fit.obj$terms <- deparse(object$formula)
  
  ## use lm.fit rather than lm():
  ## could estimate lm model with lm(), but takes more resources and 
  ## need to remove extra classes "formula" for lm to prevent warning
  # form <- object$formula
  # form <- setdiff(class(form), c("pFormula", "Formula"))
  # Xdf <- as.data.frame(X)
  # ydf <- as.data.frame(y)
  # names(ydf) <- names(object$model)[1]
  # data <- cbind(ydf, Xdf)
  # lmobj <- lm(form, data = data)
  # return(stats::alias(lmobj))
  
  return(stats::alias(lm.fit.obj, ... = dots))
}

## alias.pFormula <- function(object, data,
##                            model = c("pooling", "within", "Between", "between",
##                                      "mean", "random", "fd"),
##                            effect = c("individual", "time", "twoways"),
##                            ...) {
##   dots <- list(...)
##   if (!is.null(dots$inst.method)) stop("alias.plm/alias.pFormula: IV not supported")
##   model <- match.arg(model)
##   effect <- match.arg(effect)
##   formula <- object

##   # check if object is already pFormula, try to convert if not    
##   if (!inherits(formula, "pFormula")) formula <- pFormula(formula)
  
##   # check if data is already a model frame, convert to if not
##   if (is.null(attr(data, "terms"))) {
##     data <- model.frame.pFormula(pFormula(formula), data)
##   }

##   plmobj <- plm(formula, data = data, model = model, effect = effect, ...)
## #  print(summary(plmobj))
##   return(alias(plmobj, ...))
## }


alias.pdata.frame <- function(object,
                              model = c("pooling", "within", "Between", "between",
                                        "mean", "random", "fd"),
                              effect = c("individual", "time", "twoways"),
                              ...) {
    dots <- list(...)
    if (!is.null(dots$inst.method)) stop("alias.plm/alias.pFormula: IV not supported")
    model <- match.arg(model)
    effect <- match.arg(effect)
      # check if data is already a model frame, if not exit
    if (is.null(attr(object, "terms")))
        stop("the argument must be a model.frame")
    formula <- attr(object, "formula")
    plmobj <- plm(formula, data = object, model = model, effect = effect, ...)
    return(alias(plmobj, ...))
}
