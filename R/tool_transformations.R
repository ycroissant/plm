## This file contains the relevant transformations used for panel data,
## namely of course Within and between/Between, but also Sum (useful for
## unbalanced panels).

## They are all generics and have default, pseries and matrix
## methods. The effect argument is an index vector for the default method
## and a character ("individual", "time", "group", "twoways") for the
## pseries method. It can be any of the two for the matrix method (the
## second one only if the matrix argument has an index attribute

## diff, lag and lead methods for pseries are also provided (lead is a
## generic exported by plm, lag and diff being generic exported by
## stats). All of them have a shift argument which can be either "time"
## or "row".



#' panel series
#' 
#' A class for panel series for which several useful computations and
#' data transformations are available.
#' 
#' The functions `between`, `Between`, `Within`, and `Sum` perform specific
#' data transformations, i. e., the between, within, and sum transformation,
#' respectively.
#' 
#' `between` returns a vector/matrix containing the individual means (over
#' time) with the length of the vector equal to the number of
#' individuals (if `effect = "individual"` (default); if `effect = "time"`,
#' it returns the time means (over individuals)). `Between`
#' duplicates the values and returns a vector/matrix which length/number of rows
#' is the number of total observations. `Within` returns a vector/matrix
#' containing the values in deviation from the individual means
#' (if `effect = "individual"`, from time means if `effect = "time"`), the so
#' called demeaned data. `Sum` returns a vector/matrix with sum per individual
#' (over time) or the sum per time period (over individuals) with
#' `effect = "individual"` or `effect = "time"`, respectively, and has length/
#' number of rows of the total observations (like `Between`).
#' 
#' For `between`, `Between`, `Within`, and `Sum` in presence of NA values it
#' can be useful to supply `na.rm = TRUE` as an additional argument to
#' keep as many observations as possible in the resulting transformation.
#' na.rm is passed on to the mean()/sum() function used by these transformations
#' (i.e., it does not remove NAs prior to any processing!), see also 
#' **Examples**. 
#' 
#' @name pseries
#' @aliases pseries
#' @param x,object a `pseries` or a matrix; or a `summary.pseries` object,
#' @param effect for the pseries methods: character string indicating the
#'     `"individual"`, `"time"`, or `"group"` effect, for `Within` 
#'     `"twoways"` additionally; for non-pseries methods, `effect` is a factor
#'     specifying the dimension (`"twoways"` is not possible),
#' @param idbyrow if `TRUE` in the `as.matrix` method, the lines of
#'     the matrix are the individuals,
#' @param plot,scale,transparency,col,lwd plot arguments,
#' @param \dots further arguments, e. g., `na.rm = TRUE` for
#'     transformation functions like `between`, see **Details**
#'     and **Examples**.
#' @return All these functions return an object of class `pseries` or a matrix,
#'     except:\cr `between`, which returns a numeric vector or a matrix;
#'     `as.matrix`, which returns a matrix.
#' @author Yves Croissant
#' @seealso [is.pseries()] to check if an object is a pseries. For
#'     more functions on class 'pseries' see [lag()], [lead()],
#'     [diff()] for lagging values, leading values (negative lags) and
#'     differencing.
#' @keywords classes
#' @examples
#' 
#' # First, create a pdata.frame
#' data("EmplUK", package = "plm")
#' Em <- pdata.frame(EmplUK)
#' 
#' # Then extract a series, which becomes additionally a pseries
#' z <- Em$output
#' class(z)
#' 
#' # obtain the matrix representation
#' as.matrix(z)
#' 
#' # compute the between and within transformations
#' between(z)
#' Within(z)
#' 
#' # Between and Sum replicate the values for each time observation
#' Between(z)
#' Sum(z)
#' 
#' # between, Between, Within, and Sum transformations on other dimension
#' between(z, effect = "time")
#' Between(z, effect = "time")
#' Within(z, effect = "time")
#' Sum(z, effect = "time")
#' 
#' # NA treatment for between, Between, Within, and Sum
#' z2 <- z
#' z2[length(z2)] <- NA # set last value to NA
#' between(z2, na.rm = TRUE) # non-NA value for last individual
#' Between(z2, na.rm = TRUE) # only the NA observation is lost
#' Within(z2, na.rm = TRUE)  # only the NA observation is lost
#' Sum(z2, na.rm = TRUE)     # only the NA observation is lost
#' 
#' sum(is.na(Between(z2))) # 9 observations lost due to one NA value
#' sum(is.na(Between(z2, na.rm = TRUE))) # only the NA observation is lost
#' sum(is.na(Within(z2))) # 9 observations lost due to one NA value
#' sum(is.na(Within(z2, na.rm = TRUE))) # only the NA observation is lost
#' sum(is.na(Sum(z2))) # 9 observations lost due to one NA value
#' sum(is.na(Sum(z2, na.rm = TRUE))) # only the NA observation is lost
#' 
NULL



#' @rdname pseries
#' @export
print.pseries <- function(x, ...){
  x.orig <- x
  attr(x, "index") <- NULL
  attr(x, "class") <- base::setdiff(attr(x, "class"), "pseries")
  if(length(attr(x, "class")) == 1L && class(x) %in% c("character", "logical", "numeric", "integer", "complex")) {
    attr(x, "class") <- NULL
  }
  print(x, ...)
  invisible(x.orig)
}

#' @rdname pseries
#' @export
as.matrix.pseries <- function(x, idbyrow = TRUE, ...){
    index <- unclass(attr(x, "index")) # unclass for speed
    id <- index[[1L]]
    time <- index[[2L]]
    time.names <- levels(time)
    x <- collapse::rsplit(data.frame(x, time), id)
    x <- lapply(x, function(x){
        rownames(x) <- x[ , 2L]
        x[ , -2L, drop = FALSE]
    })
    x <- lapply(x, function(x){
        x <- x[time.names, , drop = FALSE]
        rownames(x) <- time.names
        x
    }
    )
    id.names <- names(x)
    x <- as.matrix(as.data.frame((x)))
    colnames(x) <- id.names
    if(idbyrow) x <- t(x)
    x
}

## plots a panel series by time index
##
## can supply any panel function, e.g., a loess smoother
## > mypanel<-function(x,...) {
## + panel.xyplot(x,...)
## + panel.loess(x, col="red", ...)}
## >
## > plot(pres(mod), panel=mypanel)

#' @rdname pseries
#' @importFrom lattice xyplot
#' @export
plot.pseries <- function(x, plot = c("lattice", "superposed"),
                         scale = FALSE, transparency = TRUE,
                         col = "blue", lwd = 1, ...) {
    
    if(scale) {
        scalefun <- function(x) scale(x)
    } else {
        scalefun <- function(x) return(x)}
    
    nx <- as.numeric(x)
    ind <- attr(x, "index")[[1L]]
    tind <- attr(x, "index")[[2L]] # possibly as.numeric():
                                   # activates autom. tick
                                   # but loses time labels

    xdata <- data.frame(nx = nx, ind = ind, tind = tind)
    
    switch(match.arg(plot),
           "lattice" = {
               ##require(lattice) # make a ggplot2 version
               xyplot(nx ~ tind | ind, data = xdata, type = "l", col = col, ...)
           },
           "superposed" = {
               ylim <- c(min(tapply(scalefun(nx), ind, min, na.rm = TRUE)),
                         max(tapply(scalefun(nx), ind, max, na.rm = TRUE)))
               unind <- unique(ind)
               nx1 <- nx[ind == unind[1L]]
               tind1 <- as.numeric(tind[ind == unind[1L]])
               ## plot empty plot to provide frame
               plot(NA, xlim = c(min(as.numeric(tind)),
                                 max(as.numeric(tind))),
                    ylim = ylim, xlab = "", ylab = "", xaxt = "n", ...)
               axis(1, at = as.numeric(unique(tind)),
                    labels = unique(tind))
               
               ## determine lwd and transparency level as a function
               ## of n
               if(transparency) {
                   alpha <- 5 / length(unind)
                   col <- heat.colors(1, alpha = alpha)
                   lwd <- length(unind) / 10
               }
               ## plot lines (notice: tind. are factors, so they
               ## retain the correct labels which would be lost if
               ## using as.numeric
               for(i in seq_along(unind)) {
                   nxi <- nx[ind == unind[i]]
                   tindi <- tind[ind == unind[i]]
                   lines(x = tindi, y = scalefun(nxi),
                         col = col, lwd = lwd, ...)
               }
           }) 
}

#' @rdname pseries
#' @export
summary.pseries <- function(object, ...) {
    object.orig <- object
    special_treatment_vars <- c("factor", "logical", "character")
    if(!inherits(object, special_treatment_vars)) {
        Bid   <- Between(object, na.rm = TRUE)
        Btime <- Between(object, effect = "time", na.rm = TRUE)
        res <- structure(c(total        = sum( (na.omit(object) - mean(object, na.rm = TRUE)) ^ 2),
                           between_id   = sum( (na.omit(Bid)    - mean(Bid,    na.rm = TRUE)) ^ 2),
                           between_time = sum( (na.omit(Btime)  - mean(Btime,  na.rm = TRUE)) ^ 2)), 
                          class = c("summary.pseries"),
                          class.pseries = class(object.orig))
        attr(res, "SummaryDefault") <- summary(remove_pseries_features(object))
    } else {
        object <- remove_pseries_features(object)
        res <- summary(object, ...)
        attr(res, "class.pseries") <- class(object.orig)
        class(res) <- c("summary.pseries")
    }
    return(res)
}

#' @rdname pseries
#' @export
plot.summary.pseries <- function(x, ...){
    special_treatment_vars <- c("factor", "logical", "character")
    class.basic <- setdiff(attr(x, "class.pseries"), "pseries")
    if(!class.basic %in% special_treatment_vars) {
      x <- as.numeric(x) # get tss, id/time b by coercing summary.pseries to 'numeric'
      share <- x[-1L]/x[1L] # vec with length == 2
      names(share) <- c("id", "time")
      barplot(share, ...)
    } else NULL
}

#' @rdname pseries
#' @export
print.summary.pseries <- function(x, ...){
    x.orig <- x
    digits <- getOption("digits")
    special_treatment_vars <- c("factor", "logical", "character")
    class.basic <- setdiff(attr(x, "class.pseries"), "pseries")
    if(!class.basic %in% special_treatment_vars) {
        x <- as.numeric(x) # get tss, id/time b by coercing summary.pseries to 'numeric'
        share <- x[-1L]/x[1L] # vec with length == 2
        names(share) <- c("id", "time")
        cat(paste("total sum of squares:", signif(x[1L], digits = digits),"\n"))
        print.default(share, ...)
        cat("\n")
        print(attr(x.orig, "SummaryDefault"), ...)
    } else {
      # use base R's facilities
        attr(x, "class.pseries") <- NULL
        # factor is special once again:
        is.fac <- if(class.basic == "factor") TRUE else FALSE
        attr(x, "class") <- if(is.fac) NULL else "summaryDefault"
        print(x, ...)
    }
    invisible(x.orig)
}


Tapply <- function(x, ...) {
    UseMethod("Tapply")
}

myave <- function(x, ...) {
  UseMethod("myave")
}

Tapply.default <- function(x, effect, func, ...) {
    # argument 'effect' is assumed to be a factor
    na.x <- is.na(x)
    uniqval <- tapply(x, effect, func, ...)
    nms <- attr(uniqval, "dimnames")[[1L]]
    attr(uniqval, "dimnames") <- attr(uniqval, "dim") <- NULL
    names(uniqval) <- nms
    result <- uniqval[as.character(effect)]
    result[na.x] <- NA
    return(result)
}

#' @importFrom stats ave
myave.default <- function(x, effect, func, ...) {
  # argument 'effect' is assumed to be a factor
  na.x <- is.na(x)
  res <- ave(x, effect, FUN = function(x) func(x, ...))
  names(res) <- as.character(effect)
  res[na.x] <- NA
  return(res)
}

Tapply.pseries <- function(x, effect = c("individual", "time", "group"), func, ...){
    effect <- match.arg(effect)
    xindex <- unclass(attr(x, "index")) # unclass for speed
    checkNA.index(xindex) # index may not contain any NA
    effect <- switch(effect,
                     "individual"= xindex[[1L]],
                     "time"      = xindex[[2L]],
                     "group"     = xindex[[3L]]
                     )
    z <- as.numeric(x)
    z <- Tapply.default(z, effect, func, ...)
    attr(z, "index") <- attr(x, "index") # insert original index
    class(z) <- c("pseries", class(z))
    return(z)
}

myave.pseries <- function(x, effect = c("individual", "time", "group"), func, ...) {
  effect <- match.arg(effect)
  xindex <- unclass(attr(x, "index")) # unclass for speed
  checkNA.index(xindex) # index may not contain any NA
  eff.fac <- switch(effect,
                   "individual"= xindex[[1L]],
                   "time"      = xindex[[2L]],
                   "group"     = xindex[[3L]]
  )
  z <- as.numeric(x)
  z <- myave.default(z, eff.fac, func, ...)
  attr(z, "index") <- attr(x, "index") # insert original index
  class(z) <- c("pseries", class(z))
  return(z)
}

Tapply.matrix <- function(x, effect, func, ...) {
    # argument 'effect' is assumed to be a factor
    na.x <- is.na(x)
    uniqval <- apply(x, 2, tapply, effect, func, ...)
    result <- uniqval[as.character(effect), , drop = FALSE]
    result[na.x] <- NA_real_
    return(result)
}

myave.matrix <- function(x, effect, func, ...) {
    # argument 'effect' is assumed to be a factor
    na.x <- is.na(x)
    result <- apply(x, 2, FUN = function(x) ave(x, effect, FUN = function(y) func(y, ...)))
    rownames(result) <- as.character(effect)
    result[na.x] <- NA_real_
    return(result)
}

## non-exported
Mean <- function(x) matrix(.colMeans(x, nrow(x), ncol(x)),
                           nrow(x), ncol(x), byrow = TRUE)

#' @rdname pseries
#' @export
Sum <- function(x, ...) {
    UseMethod("Sum")
}

#' @rdname pseries
#' @export
Sum.default <- function(x, effect, ...) {
# print("Sum.default(.baseR)")
# browser()
  
    # argument 'effect' is assumed to be a factor
    if(!is.numeric(x)) stop("The Sum function only applies to numeric vectors")
    #   Tapply(x, effect, sum, ...)
    return(myave(x, droplevels(effect), sum, ...))
}

#' @rdname pseries
#' @export
Sum.pseries <- function(x, effect = c("individual", "time", "group"), ...) {
# print("Sum.pseries(.baseR)")
# browser()
  
    effect <- match.arg(effect)
    #   Tapply(x, effect, sum, ...)
    # myave.pseries takes care of checking the index for NAs
    return(myave(x, effect, sum, ...))
}

#' @rdname pseries
#' @export
Sum.matrix <- function(x, effect, ...) {
# print("Sum.matrix(.baseR)")
# browser()
  
  # if no index attribute, argument 'effect' is assumed to be a factor
  eff.fac <- if(is.null(xindex <- attr(x, "index"))) {
    droplevels(effect)
  } else {
    if(!is.character(effect) && length(effect) > 1L)
      stop("for matrices with index attributes, the effect argument must be a character")
    if(! effect %in% c("individual", "time", "group"))
      stop("irrelevant effect for a between transformation")
    eff.no <- switch(effect,
                     "individual" = 1L,
                     "time"       = 2L,
                     "group"      = 3L,
                     stop("unknown value of argument 'effect'"))
    xindex <- unclass(xindex) # unclass for speed
    checkNA.index(xindex) # index may not contain any NA
    xindex[[eff.no]]
  }
  return(myave(x, eff.fac, sum, ...))
}

#' @rdname pseries
#' @export
Between <- function(x, ...) {
    UseMethod("Between")
}

#' @rdname pseries
#' @export
Between.default <- function(x, effect, ...) {
# print("Between.default(.baseR)")
# browser()
  
    # argument 'effect' is assumed to be a factor
    if(!is.numeric(x)) stop("The Between function only applies to numeric vectors")
    #   Tapply(x, effect, mean, ...)
    return(myave(x, droplevels(effect), mean, ...))
}

#' @rdname pseries
#' @export
Between.pseries <- function(x, effect = c("individual", "time", "group"), ...) {
# print("Between.pseries(.baseR)")
# browser()
  
    effect <- match.arg(effect)
    #   Tapply(x, effect = effect, mean, ...)
    # myave.pseries takes care of checking the index for NAs
    return(myave(x, effect = effect, mean, ...))
}

#' @rdname pseries
#' @export
Between.matrix <- function(x, effect, ...) {
# print("Between.matrix(.baseR)")
# browser()
  
  # if no index attribute, argument 'effect' is assumed to be a factor
  eff.fac <- if(is.null(xindex <- attr(x, "index"))) {
    droplevels(effect)
  } else {
    if(!is.character(effect) && length(effect) > 1L)
      stop("for matrices with index attributes, the effect argument must be a character")
    if(! effect %in% c("individual", "time", "group"))
      stop("irrelevant effect for a between transformation")
    eff.no <- switch(effect,
                     "individual" = 1L,
                     "time"       = 2L,
                     "group"      = 3L,
                     stop("unknown value of argument 'effect'"))
    xindex <- unclass(xindex)
    checkNA.index(xindex) # index may not contain any NA
    xindex[[eff.no]]
  }
  return(myave.matrix(x, eff.fac, mean, ...))
}

#' @rdname pseries
#' @export
between <- function(x, ...) {
    UseMethod("between")
}

#' @rdname pseries
#' @export
between.default <- function(x, effect, ...) {
# print("between.default(.baseR)")
# browser()
  
    # argument 'effect' is assumed to be a factor
    if(!is.numeric(x)) stop("The between function only applies to numeric vectors")

    # use tapply here as tapply's output is sorted by levels factor effect (unlike ave's output)
    # difference is only relevant for between (small "b") as data is compressed down to # levels
    res <- tapply(x, droplevels(effect), mean, ...)
    nms <- attr(res, "dimnames")[[1L]]
    attr(res, "dimnames") <- attr(res, "dim") <- NULL
    names(res) <- nms
    return(res)
}

#' @rdname pseries
#' @export
between.pseries <- function(x, effect = c("individual", "time", "group"), ...) {
# print("between.pseries(.baseR)")
# browser()
  
    effect <- match.arg(effect)
    xindex <- unclass(attr(x, "index")) # unclass for speed
    checkNA.index(xindex) # index may not contain any NA
    eff.fac <- switch(effect,
                     "individual" = xindex[[1L]],
                     "time"       = xindex[[2L]],
                     "group"      = xindex[[3L]],
                     )
    res <- between.default(x, effect = eff.fac, ...)
    # data compressed by transformation, so pseries features, esp. index, do not make sense
    res <- remove_pseries_features(res)
    return(res)
}

#' @rdname pseries
#' @export
between.matrix <- function(x, effect, ...) {
# print("between.matrix(.baseR)")
# browser()
  
  # if no index attribute, argument 'effect' is assumed to be a factor
  eff.fac <- if(is.null(xindex <- attr(x, "index"))) {
    droplevels(effect)
  } else {
    if(!is.character(effect) && length(effect) > 1L)
      stop("for matrices with index attributes, the effect argument must be a character")
    if(! effect %in% c("individual", "time", "group"))
      stop("irrelevant effect for a between transformation")
    eff.no <- switch(effect,
                     "individual" = 1L,
                     "time"       = 2L,
                     "group"      = 3L,
                     stop("unknown value of argument 'effect'"))
    xindex <- unclass(xindex) # unclass for speed
    checkNA.index(xindex) # index may not contain any NA
    xindex[[eff.no]]
  }

  # use tapply here as tapply's output is sorted by levels factor effect (unlike ave's output)
  # difference is only relevant for between (small "b") as data is compressed down to # levels
  res <- apply(x, 2, tapply, eff.fac, mean, ...)
  return(res)
}

#' @rdname pseries
#' @export
Within <- function(x, ...) {
    UseMethod("Within")
}

#' @rdname pseries
#' @export
Within.default <- function(x, effect, ...) {
# print("Within.default(.baseR)")
# browser()
  
  # arg 'effect' is assumed to be a factor
  
  # NB: Contrary to the other Within.* methods, Within.default does not handle
  #     twoways effects
  # TODO: could add support for twoways by supplying a list containing two factors
    if(!is.numeric(x)) stop("the within function only applies to numeric vectors")
    return(x - Between(x, droplevels(effect), ...))
}

#' @rdname pseries
#' @export
Within.pseries <- function(x, effect = c("individual", "time", "group", "twoways"), ...) {
# print("Within.pseries(.baseR)")
# browser()
  
    effect <- match.arg(effect)
    xindex <- unclass(attr(x, "index")) # unclass for speed
    checkNA.index(xindex) # index may not contain any NA
    if(effect != "twoways") result <- x - Between(x, effect, ...)
    else {
        if(is.pbalanced(x)) result <- x - Between(x, "individual", ...) - Between(x, "time") + mean(x, ...)
        else {
            time <- xindex[[2L]]
            Dmu <- model.matrix(~ time - 1)
            attr(Dmu, "index") <- attr(x, "index") # need original index
            W1   <- Within(x,   "individual", ...)
            WDmu <- Within(Dmu, "individual", ...)
            W2 <- lm.fit(WDmu, x)$fitted.values
            result <- W1 - W2
        }
    }
    return(result)
}

#' @rdname pseries
#' @export
Within.matrix <- function(x, effect, ...) {
# print("Within.matrix(.baseR)")
# browser()
  
    if(is.null(xindex <- unclass(attr(x, "index")))) { # unclass for speed
      # non-index case
        result <- Within.default(x, effect, ...)
        # NB: effect is assumed to be a factor; contrary to the other Within.* 
        #     methods, Within.default does not handle twoways effects
    }
    else {
      # index case
        if(effect %in% c("individual", "time", "group")) result <- x - Between(x, effect, ...)
        if(effect == "twoways") {
            checkNA.index(xindex) # index may not contain any NA
            if(is.pbalanced(xindex[[1L]], xindex[[2L]])) {
                result <- x - Between(x, "individual", ...) - Between(x, "time", ...) +
                    matrix(colMeans(x, ...), nrow = nrow(x), ncol = ncol(x), byrow = TRUE)
            }
            else { # unbalanced twoways
                time <- xindex[[2L]]
                Dmu <- model.matrix(~ time - 1)
                attr(Dmu, "index") <- attr(x, "index") # need orig. index here
                W1   <- Within(x,   "individual", ...)
                WDmu <- Within(Dmu, "individual", ...)
                W2 <- lm.fit(WDmu, x)$fitted.values
                result <- W1 - W2
            }
        }
    }
    return(result)
}

############### LAG and DIFF
#
# lag/lead/diff for pseries are a wrappers for lagt, leadt, difft (if shift = "time") and 
#                                          for lagr, leadr, diffr (if shift = "row")
#
# The "t" and "r" methods are not exported (by intention).
#
# The "t" methods perform shifting while taking the time period into
# account (they "look" at the value in the time dimension).
#
# The "r" methods perform shifting row-wise (without taking the value
# in the time dimension into account).
#
# Generic needed only for lead (lag and diff generics are already included in base R)


#' lag, lead, and diff for panel data
#' 
#' lag, lead, and diff functions for class pseries.
#' 
#' This set of functions perform lagging, leading (lagging in the
#' opposite direction), and differencing operations on `pseries`
#' objects, i. e., they take the panel structure of the data into
#' account by performing the operations per individual.
#' 
#' Argument `shift` controls the shifting of observations to be used
#' by methods `lag`, `lead`, and `diff`:
#' 
#' - `shift = "time"` (default): Methods respect the
#' numerical value in the time dimension of the index. The time
#' dimension needs to be interpretable as a sequence t, t+1, t+2,
#' \ldots{} where t is an integer (from a technical viewpoint,
#' `as.numeric(as.character(index(your_pdata.frame)[[2]]))` needs to
#' result in a meaningful integer).
#' 
#' - `shift = "row": `Methods perform the shifting operation based
#' solely on the "physical position" of the observations,
#' i.e., neighbouring rows are shifted per individual. The value in the
#' time index is not relevant in this case.
#' 
#' For consecutive time periods per individual, a switch of shifting
#' behaviour results in no difference. Different return values will
#' occur for non-consecutive time periods per individual
#' ("holes in time"), see also Examples.
#' 
#' @name lag.plm
#' @aliases lag lead diff
#' @importFrom stats lag
#' @param x a `pseries` object,
#' @param k an integer, the number of lags for the `lag` and `lead`
#'     methods (can also be negative).  For the `lag` method, a
#'     positive (negative) `k` gives lagged (leading) values.  For the
#'     `lead` method, a positive (negative) `k` gives leading (lagged)
#'     values, thus, `lag(x, k = -1L)` yields the same as `lead(x, k = 1L)`.
#'     If `k` is an integer with length > 1 (`k = c(k1, k2, ...)`), a 
#'     `matrix` with multiple lagged `pseries` is returned,
#' @param lag integer, the number of lags for the `diff` method, can also be of
#'     length > 1 (see argument `k`) (only non--negative values in
#'     argument `lag` are allowed for `diff`),
#' @param shift character, either `"time"` (default) or `"row"`
#'     determining how the shifting in the `lag`/`lead`/`diff`
#'     functions is performed (see Details and Examples).
#' @param ... further arguments (currently none evaluated).
#' @return
#'
#' - An object of class `pseries`, if the argument specifying the lag
#'     has length 1 (argument `k` in functions `lag` and `lead`,
#'     argument `lag` in function `diff`).
#' 
#' - A matrix containing the various series in its columns, if the
#'     argument specifying the lag has length > 1.
#' 
#' @note The sign of `k` in `lag.pseries` results in inverse behaviour
#'     compared to [stats::lag()] and [zoo::lag.zoo()].
#' @author Yves Croissant and Kevin Tappe
#' @seealso To check if the time periods are consecutive per
#'     individual, see [is.pconsecutive()].
#' 
#' For further function for 'pseries' objects: [between()],
#' [Between()], [Within()], [summary.pseries()],
#' [print.summary.pseries()], [as.matrix.pseries()].
#' @keywords classes
#' @examples
#' 
#' # First, create a pdata.frame
#' data("EmplUK", package = "plm")
#' Em <- pdata.frame(EmplUK)
#' 
#' # Then extract a series, which becomes additionally a pseries
#' z <- Em$output
#' class(z)
#' 
#' # compute the first and third lag, and the difference lagged twice
#' lag(z)
#' lag(z, 3L)
#' diff(z, 2L)
#' 
#' # compute negative lags (= leading values)
#' lag(z, -1L)
#' lead(z, 1L) # same as line above
#' identical(lead(z, 1L), lag(z, -1L)) # TRUE
#'  
#' # compute more than one lag and diff at once (matrix returned)
#' lag(z, c(1L,2L))
#' diff(z, c(1L,2L))
#' 
#' ## demonstrate behaviour of shift = "time" vs. shift = "row"
#' # delete 2nd time period for first individual (1978 is missing (not NA)):
#' Em_hole <- Em[-2L, ]
#' is.pconsecutive(Em_hole) # check: non-consecutive for 1st individual now
#' 
#' # original non-consecutive data:
#' head(Em_hole$emp, 10) 
#' # for shift = "time", 1-1979 contains the value of former 1-1977 (2 periods lagged):
#' head(lag(Em_hole$emp, k = 2L, shift = "time"), 10L)
#' # for shift = "row", 1-1979 contains NA (2 rows lagged (and no entry for 1976):
#' head(lag(Em_hole$emp, k = 2L, shift = "row"), 10L)
#' 
NULL

#' @rdname lag.plm
#' @export
lead <- function(x, k = 1L, ...) {
  UseMethod("lead")
}

#' @rdname lag.plm
#' @exportS3Method
#' @export lag
lag.pseries <- function(x, k = 1L, shift = c("time", "row"), ...) {
  shift <- match.arg(shift)
  
  if (shift == "time") {
    if (!isTRUE(getOption("plm.fast"))) {
      res <- lagt.pseries(x = x, k = k, ...) # base R
    } else {
      if (!isTRUE(getOption("plm.fast.pkg.collapse"))) stop(txt.no.collapse, call. = FALSE)
      res <- collapse2plm_lag_diff(collapse::flag(x = x, n = k, shift = "time"), k = k)
    }
  } else {
    ## row-wise shifting
    if (!isTRUE(getOption("plm.fast"))) {
      res <- lagr.pseries(x = x, k = k, ...) # base R
    } else {
      if (!isTRUE(getOption("plm.fast.pkg.collapse"))) stop(txt.no.collapse, call. = FALSE)
      res <- collapse2plm_lag_diff(collapse::flag(x = x, n = k, shift = "row"), k = k)
    }
  }
  res
}

#' @rdname lag.plm
#' @export
lead.pseries <- function(x, k = 1L, shift = c("time", "row"), ...) {
  shift <- match.arg(shift)
  
  if (shift == "time") {
    if (!isTRUE(getOption("plm.fast"))) {
      res <- leadt.pseries(x = x, k = k, ...) # base R
    } else {
      if (!isTRUE(getOption("plm.fast.pkg.collapse"))) stop(txt.no.collapse, call. = FALSE)
      res <- collapse2plm_lag_diff(collapse::flag(x = x, n = -k, shift = "time"), k = k)
    }
  } else {
    ## row-wise shifting
    if (!isTRUE(getOption("plm.fast"))) {
      res <- leadr.pseries(x = x, k = k, ...) # base R
    } else {
      if (!isTRUE(getOption("plm.fast.pkg.collapse"))) stop(txt.no.collapse, call. = FALSE)
      res <- collapse2plm_lag_diff(collapse::flag(x = x, n = -k, shift = "row"), k = k)
    }
  }
  res
}

#' @rdname lag.plm
#' @exportS3Method
diff.pseries <- function(x, lag = 1L, shift = c("time", "row"), ...) {
  shift <- match.arg(shift)
  
  if (shift == "time") {
    if (!isTRUE(getOption("plm.fast"))) {
      res <- difft.pseries(x = x, lag = lag, ...) # base R
    } else {
      if (!isTRUE(getOption("plm.fast.pkg.collapse"))) stop(txt.no.collapse, call. = FALSE)
      res <- collapse2plm_lag_diff(collapse::fdiff(x = x, n = lag, shift = "time"), k = lag)
    }
  } else {
    ## row-wise shifting
    if (!isTRUE(getOption("plm.fast"))) {
      res <- diffr.pseries(x = x, lag = lag, ...) # base R
    } else {
      if (!isTRUE(getOption("plm.fast.pkg.collapse"))) stop(txt.no.collapse, call. = FALSE)
      res <- collapse2plm_lag_diff(collapse::fdiff(x = x, n = lag, shift = "row"), k = lag)
    }
  }
  res
}

## lagt.pseries lagging taking the time variable into account
lagt.pseries <- function(x, k = 1L, ...) {
  index <- unclass(attr(x, "index")) # unclass for speed
  id <- index[[1L]]
  time <- index[[2L]]
  
  if(length(k) > 1L) {
    rval <- sapply(k, function(i) alagt(x, i))
    colnames(rval) <- k
  }
  else {
    rval <- alagt(x, k)
  }
  rval
}

## leadt.pseries(x, k) is a wrapper for lagt.pseries(x, -k)
leadt.pseries <- function(x, k = 1L, ...) {
  ret <- lagt.pseries(x, k = -k)
  if(length(k) > 1L) colnames(ret) <- k
  return(ret)
}

## difft: diff-ing taking the time variable into account
difft.pseries <- function(x, lag = 1L, ...){
  ## copied/adapted from diffr.pseries except lines which use lagt() ("t") instead of lagr() ("r")
  islogi <- is.logical(x)
  if(! (is.numeric(x) || islogi)) stop("diff is only relevant for numeric or logical series")
  
  non.int <- vapply(lag, function(l) round(l) != l, FUN.VALUE = TRUE, USE.NAMES = FALSE)
  if(any(non.int)) stop("Lagging value(s) in 'lag' must be whole-numbered (and non-negative)")
  
  # prevent input of negative values, because it will most likely confuse users
  # what difft would do in this case
  neg <- vapply(lag, function(l) l < 0L, FUN.VALUE = TRUE, USE.NAMES = FALSE)
  if(any(neg)) stop("diff is only relevant for non-negative values in 'lag'")
  
  lagtx <- lagt.pseries(x, k = lag) # use "time-based" lagging for difft
  
  res <- if (is.matrix(lagtx)) {
    # if 'lagtx' is matrix (case length(lag) > 1):
    # perform subtraction without pseries feature of 'x', because otherwise
    # the result would be c("pseries", "matrix") which is not supported
    as.numeric(x) - lagtx
  } else {
    x - lagtx
  }
  
  res
}

## alagt: non-exported helper function for lagt (actual work horse),
## performs shifting of observations while respecting the time dimension
alagt <- function(x, ak) {
  if(round(ak) != ak) stop("Lagging value 'k' must be whole-numbered (positive, negative or zero)")
  if(ak != 0) {
    index <- unclass(attr(x, "index")) # unclass for speed
    id   <- index[[1L]]
    time <- index[[2L]]
    
    # Idea: split times in blocks per individuals and do lagging there
    # by computation of correct time shifting
    
    # need to convert to numeric, do this by coercing to character
    # first (otherwise wrong results!)
    #  see R FAQ 7.10 for coercing factors to numeric: 
    #      as.numeric(levels(factor_var))[as.integer(factor_var)] is
    #      more efficient than
    #      as.numeric(as.character(factor_var))

    # YC 2019/08/29 only works if time values can be coerced to
    ## numeric, ie integers like years. When year is period (ie 5 years),
    ## values used to be 1950 for the 1950-54 period, time is now a
    ## factor in the original data.frame with levels "1950-54",
    ## "1955-59", ... In this case coercing the levels to a numeric gives
    ## NA so coerce the *factor* to a numeric.
    
    levtime <- levels(time)
    numlevtime <- suppressWarnings(as.numeric(levtime))
    if(! anyNA(numlevtime)) time <- as.numeric(levels(time))[as.integer(time)]
    else time <- as.numeric(time)
    
    list_id_timevar <- collapse::gsplit(time, id, drop = TRUE, use.names = FALSE)
    
    index_lag_ak_all_list <- sapply(list_id_timevar,
                                    function(x) match(x - ak, x, incomparables = NA),
                                    simplify = FALSE, USE.NAMES = FALSE)
    
    # translate block-wise positions to positions in full vector
    substitute_blockwise <- unlist(index_lag_ak_all_list, use.names = FALSE)
    
    NApos <- is.na(substitute_blockwise) # save NA positions for later
    
    block_lengths <- lengths(index_lag_ak_all_list, use.names = FALSE)
    
    # not needed but leave here for illustration:
    #    startpos_block <- cumsum(block_lengths) - block_lengths + 1
    #    endpos_block <- startpos_block + block_lengths - 1
    
    indexes_blockwise <- unlist(sapply(block_lengths, function(x) seq(from = 1, to = x), simplify = FALSE), use.names = FALSE)
    
    orig_pos_x <- seq.int(x) # make vector with indexes for original input
    new_pos <- orig_pos_x - (indexes_blockwise - substitute_blockwise) # calc. new positions
    new_pos[NApos] <- orig_pos_x[NApos] # fill NAs with arbitrary values to allow proper subsetting in next step
    
    orig_attr <- attributes(x)
    x <- x[new_pos] # re-arrange according to lagging
    x[NApos] <- NA  # set NAs where necessary
    attributes(x) <- orig_attr # restore original names and 'pseries' class (lost by subsetting x)
  }
  x
} # END alagt


## lagr: lagging row-wise
lagr.pseries <- function(x, k = 1L, ...) {
    index <- unclass(attr(x, "index")) # unclass for speed
    id <- index[[1L]]
    time <- index[[2L]]
  
    # catch the case when an index of pdata.frame shall be lagged
    # (index variables are always factors) NB: this catches -
    # unintentionally - also the case when a factor variable is the
    # same "on the character level" as one of the corresponding index
    # variables but not the index variable itself
    #
    # -> shall we prevent lagging of index variables at all? -> turned
    # off for now, 2016-03-03 if(is.factor(x)) if
    # (all(as.character(x) == as.character(id)) |
    # all(as.character(x)==as.character(time))) stop("Lagged vector
    # cannot be index.")
  
    alagr <- function(x, ak){
        if(round(ak) != ak) stop("Lagging value 'k' must be whole-numbered (positive, negative or zero)")
        if(ak > 0L) {
      
        # NB: this code does row-wise shifting
        # delete first ak observations for each unit
            isNAtime <- c(rep(TRUE, ak), (diff(as.numeric(time), lag = ak) != ak))
            isNAid   <- c(rep(TRUE, ak), (diff(as.numeric(id),   lag = ak) != 0L))
            isNA <- (isNAtime | isNAid)
      
            result <- x                                               # copy x first ...
            result[1:ak] <- NA                                        # ... then make first ak obs NA ... 
            result[(ak+1):length(result)] <- x[seq_len(length(x)-ak)] # ... shift and ...
            result[isNA] <- NA                                        # ... make more NAs in between: this way, we keep: all factor levels, names, classes
      
        } else if(ak < 0L) { # => compute leading values
      
        # delete last |ak| observations for each unit
            num_time <- as.numeric(time)
            num_id   <- as.numeric(id)
            isNAtime <- c(c((num_time[seq_len(length(num_time)+ak)] - num_time[(-ak+1):length(num_time)]) != ak), rep(TRUE, -ak))
            isNAid   <- c(c((num_id[seq_len(length(num_id)+ak)]     - num_id[(-ak+1):length(num_id)])     != 0L), rep(TRUE, -ak))
            isNA <- (isNAtime | isNAid)
      
            result <- x                                                 # copy x first ...
            result[(length(result)+ak+1):length(result)] <- NA          # ... then make last |ak| obs NA ... 
            result[seq_len(length(result)+ak)] <- x[(1-ak):(length(x))] # ... shift and ...
            result[isNA] <- NA                                          # ... make more NAs in between: this way, we keep: all factor levels, names, classes
      
        } else { # ak == 0 => nothing to do, return original pseries (no lagging/no leading)
            result <- x
        }
        
        return(result)
    } # END function alagr
  
    if(length(k) > 1L) {
        rval <- sapply(k, function(i) alagr(x, i))
        colnames(rval) <- k
    }
    else {
        rval <- alagr(x, k)
    }
    return(rval)
}


# leadr.pseries(x, k) is a wrapper for lagr.pseries(x, -k)
leadr.pseries <- function(x, k = 1L, ...) {
    ret <- lagr.pseries(x, k = -k)
    if(length(k) > 1L) colnames(ret) <- k
    ret
}

## diffr: lagging row-wise
diffr.pseries <- function(x, lag = 1L, ...) {
    islogi <- is.logical(x)
    if(! (is.numeric(x) || islogi)) stop("diff is only relevant for numeric or logical series")
    
    non.int <- vapply(lag, function(l) round(l) != l, FUN.VALUE = TRUE, USE.NAMES = FALSE)
    if(any(non.int)) stop("Lagging value(s) in 'lag' must be whole-numbered (and non-negative)")
    
    # prevent input of negative values, because it will most likely confuse users
    # what diff would do in this case
    neg <- vapply(lag, function(l) l < 0L, FUN.VALUE = TRUE, USE.NAMES = FALSE)
    if(any(neg)) stop("diff is only relevant for non-negative values in 'lag'")

    lagrx <- lagr.pseries(x, k = lag)
    
    res <- if (is.matrix(lagrx)) {
      # if 'lagrx' is matrix (case length(lag) > 1):
      # perform subtraction without pseries feature of 'x', because otherwise
      # the result would be c("pseries", "matrix") which is not supported
      as.numeric(x) - lagrx
    } else {
      x - lagrx
    }
    
    res
}


## pdiff is (only) used in model.matrix to calculate the
## model.matrix for FD models
## wrapper
pdiff <- function(x, effect = c("individual", "time"),
                  has.intercept = FALSE,
                  shift = c("time", "row")) {
  
    shift <- match.arg(shift)
    res <- if (shift == "time")
              pdifft(x = x,
                     effect = effect,
                     has.intercept = has.intercept)
            else
              pdiffr(x = x,
                     effect = effect,
                     has.intercept = has.intercept)
    res
}
  
## pdiffr works for effect = "individual" only,
## see model.matrix on how to call pdiff. Result is in order (id,
## time) for both effects
##
## Performs row-wise shifting (note the 'r' in pdiffr)
pdiffr <- function(x, effect = c("individual", "time"), has.intercept = FALSE){
  # NB: x is assumed to have an index attribute, e.g., a pseries
  #     can check with has.index(x)
  # TODO: pdiff's usage in model.matrix is not very elegant as pdiff does its own
  #     removal of constant columns and intercept handling which could be handled
  #     via model.matrix.
  
    effect <- match.arg(effect)
    cond <- as.numeric(unclass(attr(x, "index"))[[1L]]) # unclass for speed
    n <- if(is.matrix(x)) nrow(x) else length(x)
    cond <- c(NA, cond[2:n] - cond[seq_len(n-1)]) # this assumes a certain ordering
    cond[cond != 0] <- NA
    if(! is.matrix(x)){

        result <- c(NA , x[2:n] - x[seq_len(n-1)])
        result[is.na(cond)] <- NA
        result <- na.omit(result)
    }
    else{
        result <- rbind(NA, x[2:n, , drop = FALSE] - x[seq_len(n-1), , drop = FALSE])
        result[is.na(cond), ] <- NA
        result <- na.omit(result)
        # remove constant columns
        cst.col <- apply(result, 2, is.constant)
        result <- result[ , !cst.col, drop = FALSE]
        if(has.intercept){
            result <- cbind(1, result)
            colnames(result)[1L] <- "(Intercept)"
        }
    }

    attr(result, "na.action") <- NULL
    result
}

## performs time-wise shifting (note the 't' in pdifft)
pdifft <- function(x, effect = c("individual", "time"), has.intercept = FALSE) {
  effect <- match.arg(effect)
  x.index <- attr(x, "index")
  x.pdf <- as.data.frame(x, make.names = FALSE)
  x.pdf <- cbind(x.index, x.pdf)
  x.pdf <- pdata.frame(x.pdf, drop.index = TRUE)
  
  if(!is.matrix(x)) {
    # pseries case (LHS)
    res <- diff(x, effect = effect, shift = "time")
    res <- subset_pseries(res, !is.na(res)) # TODO: use [.pseries (pseries subsetting) once implemented
  } else {
    # matrix case (RHS)
    res <- apply(x, 2, function(col) diff(add_pseries_features(col, x.index),
                                          effect = effect, shift = "time"))
    res <- na.omit(res)
    # if intercept is requested, set intercept column to 1 as it was diff'ed out
    if(has.intercept) res[ , 1L] <- 1L
  }
  res
}

### non-exported helper function to align collapse::flag to plm's original lag
## https://github.com/SebKrantz/collapse/issues/183 and there is no factor matrix
## https://stackoverflow.com/a/28724756/4640346
collapse2plm_lag_diff <- function(x, k) {
  if(is.matrix(x)) {
    dim.nam1 <- dimnames(x)[[1L]]
    if(inherits(x, "factor")) {
      # cater for collapse's special factor matrix case
      x <- matrix(as.character(x), ncol = dim(x)[[2L]])
      dimnames(x)[[1L]] <- dim.nam1
    }
    class(x) <- NULL
    attr(x, "index") <- NULL
    dimnames(x)[[2L]] <- k
  }
  x
}
