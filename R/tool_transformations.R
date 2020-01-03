## This file contains the relevant transformations used for panel data,
## namely of course Within and between/Between, but also Sum (usefull for
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
#' The functions `between`, `Between`, and `Within` perform specific
#' data transformations, i. e. the between and within transformation.
#' 
#' `between` returns a vector containing the individual means (over
#' time) with the length of the vector equal to the number of
#' individuals (if `effect = "individual"` (default); if `effect = "time"`,
#' it returns the time means (over individuals)). `Between`
#' duplicates the values and returns a vector which length is the
#' number of total observations. `Within` returns a vector containing
#' the values in deviation from the individual means (if `effect = "individual"`,
#' from time means if `effect = "time"`), the so called
#' demeaned data.
#' 
#' For `between`, `Between`, and `Within` in presence of NA values it
#' can be useful to supply `na.rm = TRUE` as an additional argument to
#' keep as many observations as possible in the resulting
#' transformation, see also **Examples**.
#' 
#' @name pseries
#' @aliases pseries
#' @param x,object a `pseries` or a `summary.pseries` object,
#' @param effect character string indicating the `"individual"` or
#'     `"time"` effect,
#' @param idbyrow if `TRUE` in the `as.matrix` method, the lines of
#'     the matrix are the individuals,
#' @param rm.null if `TRUE`, for the `Within.matrix` method, remove
#'     the empty columns,
#' @param plot,scale,transparency,col,lwd plot arguments,
#' @param \dots further arguments, e. g. `na.rm = TRUE` for
#'     transformation functions like `beetween`, see **Details**
#'     and **Examples**.
#' @return All these functions return an object of class `pseries`,
#'     except:\cr `between`, which returns a numeric vector,
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
#' # Between replicates the values for each time observation
#' Between(z)
#' 
#' # between, Between, and Within transformations on other dimension
#' between(z, effect = "time")
#' Between(z, effect = "time")
#' Within(z, effect = "time")
#' 
#' # NA treatment for between, Between, and Within
#' z2 <- z
#' z2[length(z2)] <- NA # set last value to NA
#' between(z2, na.rm = TRUE) # non-NA value for last individual
#' Between(z2, na.rm = TRUE) # only the NA observation is lost
#' Within(z2, na.rm = TRUE)  # only the NA observation is lost
#' 
#' sum(is.na(Between(z2))) # 9 observations lost due to one NA value
#' sum(is.na(Between(z2, na.rm = TRUE))) # only the NA observation is lost
#' sum(is.na(Within(z2))) # 9 observations lost due to one NA value
#' sum(is.na(Within(z2, na.rm = TRUE))) # only the NA observation is lost
#' 
NULL



#' @rdname pseries
#' @export
print.pseries <- function(x, ...){
  attr(x, "index") <- NULL
  attr(x, "class") <- base::setdiff(attr(x, "class"), "pseries")
  if (length(attr(x, "class")) == 1 && class(x) %in% c("character", "logical", "numeric", "integer", "complex")) {
    attr(x, "class") <- NULL
  }
  print(x, ...)
}

#' @rdname pseries
#' @export
as.matrix.pseries <- function(x, idbyrow = TRUE, ...){
    index <- attr(x, "index")
    id <- index[[1]]
    time <- index[[2]]
    time.names <- levels(time)
    x <- split(data.frame(x, time), id)
    x <- lapply(x, function(x){
        rownames(x) <- x[ , 2]
        x[ , -2, drop = FALSE]
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
    if (idbyrow) x <- t(x)
    x
}

## plots a panel series by time index
##
## can supply any panel function, e.g. a loess smoother
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
    ind <- attr(x, "index")[[1]]
    tind <- attr(x, "index")[[2]] # possibly as.numeric():
                                  # activates autom. tick
                                  # but loses time labels

    xdata <- data.frame(nx=nx, ind=ind, tind=tind)
    
    switch(match.arg(plot),
           lattice = {
               ##require(lattice) # make a ggplot2 version
               xyplot(nx ~ tind | ind, data = xdata, type = "l", col = col, ...)
               
           }, superposed = {
               ylim <- c(min(tapply(scalefun(nx), ind, min, na.rm = TRUE)),
                         max(tapply(scalefun(nx), ind, max, na.rm = TRUE)))
               unind <- unique(ind)
               nx1 <- nx[ind == unind[1]]
               tind1 <- as.numeric(tind[ind == unind[1]])
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
               for(i in 1:length(unind)) {
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
    if (!inherits(object, c("factor", "logical", "character"))) {
        id <- attr(object, "index")[[1]]
        time <- attr(object, "index")[[2]]
        xm <- mean(object, na.rm = TRUE)
        Bid <-  Between(object, na.rm = TRUE)
        Btime <-  Between(object, effect = "time", na.rm = TRUE)
        ## res <- structure(c(total = sumsq(object),
        ##                    between_id = sumsq(Bid),
        ##                    between_time = sumsq(Btime)), 
        ##                  class = c("summary.pseries", "numeric"))
        res <- structure(c(total = sum( (na.omit(object) - mean(object, na.rm = TRUE)) ^ 2),
                           between_id = sum( (na.omit(Bid) - mean(Bid, na.rm = TRUE)) ^ 2),
                           between_time = sum( (na.omit(Btime) - mean(Btime, na.rm = TRUE)) ^ 2)), 
                           class = c("summary.pseries", "numeric"))
        
    } else {
        class(object) <- setdiff(class(object), c("pseries"))
        res <- summary(object, ...)
        class(res) <- c("summary.pseries", class(object), class(res))
    }
    return(res)
}

#' @rdname pseries
#' @export
plot.summary.pseries <- function(x, ...){
    x <- as.numeric(x)
    share <- x[-1]/x[1] # vec with length == 2
    names(share) <- c("id", "time")
    barplot(share, ...)
}

#' @rdname pseries
#' @export
print.summary.pseries <- function(x, ...){
    digits <- getOption("digits")
    special_treatment_vars <- c("factor", "logical", "character")
    if (!inherits(x, special_treatment_vars)) {
        x <- as.numeric(x)
        share <- x[-1]/x[1] # vec with length == 2
        names(share) <- c("id", "time")
        cat(paste("total sum of squares:", signif(x[1], digits = digits),"\n"))
        print.default(share, ...)
    } else {
        class(x) <- setdiff(class(x), c("summary.pseries", special_treatment_vars))
        print(x, ...)
    }
}


Tapply <- function(x, ...){
    UseMethod("Tapply")
}

Tapply.default <- function(x, effect, func, ...){
    na.x <- is.na(x)
    uniqval <- tapply(x, effect, func, ...)
    nms <- attr(uniqval, "dimnames")[[1]]
    attr(uniqval, "dimnames") <- attr(uniqval, "dim") <- NULL
    names(uniqval) <- nms
    result <- uniqval[as.character(effect)]
    result[na.x] <- NA
    result
}

Tapply.pseries <- function(x, effect = c("individual", "time", "group"), func, ...){
    effect <- match.arg(effect)
    index <- attr(x, "index")
    effect <- switch(effect,
                     "individual"= index[[1]],
                     "time"      = index[[2]],
                     "group"     = index[[3]]
                     )
    x <- as.numeric(x)
    z <- Tapply.default(x, effect, func, ...)
    attr(z, "index") <- index
    class(z) <- c("pseries", class(z))
    z
}

Tapply.matrix <- function(x, effect, func, ...){
    na.x <- is.na(x)
    uniqval <- apply(x, 2, tapply, effect, func)
    result <- uniqval[as.character(effect), , drop = F]
    result[na.x] <- NA
    result
}

Sum <- function(x, ...){
    UseMethod("Sum")
}

Sum.default <- function(x, effect, ...){
    if (!is.numeric(x)) stop("The Sum function only applies to numeric vectors")
    Tapply(x, effect, sum, ...)
}

Sum.pseries <- function(x, effect = c("individual", "time", "group"), ...){
    effect <- match.arg(effect)
    Tapply(x, effect, sum, ...)
}

Sum.matrix <- function(x, effect,...){
    if (! effect %in% c("individual", "time", "group"))
        stop("irrelevant effect for a Sum transformation")
    if (is.null(attr(x, "index"))) Sum.default(x, effect)
    else{
        if (length(effect) > 1)
            stop("for matrices with index attributes, the effect argument must be a character")
        xindex <- attr(x, "index")
        effect <- index(xindex, effect)
        Tapply(x, effect, sum)
    }        
}

#' @rdname pseries
#' @export
Between <- function(x, ...){
    UseMethod("Between")
}

#' @rdname pseries
#' @export
Between.default <- function(x, effect, ...){
    if (!is.numeric(x)) stop("The Between function only applies to numeric vectors")
    Tapply(x, effect, mean, ...)
}

#' @rdname pseries
#' @export
Between.pseries <- function(x, effect = c("individual", "time", "group"), ...){
    effect <- match.arg(effect)
    Tapply(x, effect = effect, mean, ...)
}

#' @rdname pseries
#' @export
Between.matrix <- function(x, effect,...){
    #YC20180916 In the previous version the matrix wasn't returned
    #when there is no index attribute
    if (is.null(attr(x, "index"))) return(Between.default(x, effect))
    if (! effect %in% c("individual", "time", "group"))
        stop("irrelevant effect for a between transformation")
    else{
        if (length(effect) > 1)
            stop("for matrices with index attributes, the effect argument must be a character")
        xindex <- attr(x, "index")
        effect <- index(xindex, effect)
        Tapply(x, effect, mean)
    }        
}

#' @rdname pseries
#' @export
between <- function(x, ...){
    UseMethod("between")
}

#' @rdname pseries
#' @export
between.default <- function(x, effect, ...){
    if (!is.numeric(x)) stop("The between function only applies to numeric vectors")
    tapply(x, effect, mean, ...)
}

#' @rdname pseries
#' @export
between.pseries <- function(x, effect = c("individual", "time", "group"), ...){
    effect <- match.arg(effect)
    index <- attr(x, "index")
    effect <- switch(effect,
                     "individual" = index[[1]],
                     "time"       = index[[2]],
                     "group"      = index[[3]]
                     )
    x <- between.default(x, effect = effect, ...)
    nms <- attr(x, "dimnames")[[1]]
    attr(x, "dimnames") <- attr(x, "dim") <- NULL
    names(x) <- nms
    x
}

#' @rdname pseries
#' @export
between.matrix <- function(x, effect,...){
    if (! effect %in% c("individual", "time", "group"))
        stop("irrelevant effect for a between transformation")
    if (is.null(attr(x, "index"))) Between.default(x, effect)
    else{
        if (length(effect) > 1)
            stop("for matrices with index attributes, the effect argument must be a character")
        xindex <- attr(x, "index")
        effect <- index(xindex, effect)
        apply(x, 2, tapply, effect, mean, ...)
    }        
}

#' @rdname pseries
#' @export
Within <- function(x, ...){
    UseMethod("Within")
}

#' @rdname pseries
#' @export
Within.default <- function(x, effect, ...){
    if (!is.numeric(x)) stop("the within function only applies to numeric vectors")
    x - Between(x, effect, ...)
}

#' @rdname pseries
#' @export
Within.pseries <- function(x, effect = c("individual", "time", "group", "twoways"), ...){
    effect <- match.arg(effect)
    if (effect != "twoways") x - Between(x, effect, ...)
    else{
        if (is.pbalanced(x)) x - Between(x, "individual", ...) - Between(x, "time") + mean(x)
        else{
            time <- index(x)[[2]]
            Dmu <- model.matrix(~ time - 1)
            attr(Dmu, "index") <- index(x)
            W1 <- Within(x, "individual")
            WDmu <- Within(Dmu, "individual")
            W2 <- fitted(lm.fit(WDmu, x))
            W1 - W2
        }
    }
}

#' @rdname pseries
#' @export
Within.matrix <- function(x, effect, rm.null = TRUE, ...){
    if (is.null(attr(x, "index"))){
        result <- Within.default(x, effect, ...)
        othervar <- colSums(abs(x)) > sqrt(.Machine$double.eps)
        if (rm.null){
            result <- result[ , othervar, drop = FALSE]
            attr(result, "constant") <- character(0)
        }
        else attr(result, "constant") <- colnames(x)[! othervar]
        result
    }
    else{
        if (effect %in% c("individual", "time", "group")) result <- x - Between(x, effect)
        if (effect == "twoways"){
            xindex <- attr(x, "index")
            if (is.pbalanced(xindex)) {
                result <- x - Between(x, "individual") - Between(x, "time") +
                    matrix(colMeans(x), nrow = nrow(x), ncol = ncol(x), byrow = TRUE)
            }
            else{ # unbalanced twoways
                time <- index(xindex, "time")
                id <- index(xindex, "individual")
                Dmu <- model.matrix(~ time - 1)
                attr(Dmu, "index") <- xindex
                W1 <- Within(x, "individual", rm.null = FALSE)
                WDmu <- Within(Dmu, "individual")
                W2 <- fitted(lm.fit(WDmu, x))
                result <- W1 - W2
            }
        }
    }
    result
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

# The "r" methods perform shifting row-wise (without taking the value
# in the time dimension into account).

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
#' #' - `shift = "time"` (default): Methods respect the
#' numerical value in the time dimension of the index.  The time
#' dimension needs to be interpretable as a sequence t, t+1, t+2,
#' \ldots{} where t is an integer (from a technical viewpoint,
#' `as.numeric(as.character(index(your_pdata.frame)[[2]]))` needs to
#' result in a meaningful integer).
#' 
#' - `shift = "row": `Methods perform the shifting operation based
#' solely on the "physical position" of the observations,
#' i.e. neighbouring rows are shifted per individual. The value in the
#' time index is not relevant in this case.
#' 
#' For consecutive time periods per individual, a switch of shifting
#' behaviour results in no difference. Different return values will
#' occur for non-consecutive time periods per individual
#' ("holes in time"), see also Examples.
#' 
#' @name lag.plm
#' @param x a `pseries` object,
#' @param k an integer, the number of lags for the `lag` and `lead`
#'     methods (can also be negative).  For the `lag` method, a
#'     positive (negative) `k` gives lagged (leading) values.  For the
#'     `lead` method, a positive (negative) `k` gives leading (lagged)
#'     values, thus, `lag(x, k = -1)` yields the same as `lead(x, k = 1)`.
#'     If `k` is an integer with length > 1 (`k = c(k1, k2, ...)`), a 
#'     `matrix` with multiple lagged `pseries` is returned,
#' @param lag the number of lags for the `diff` method, can also be of
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
#' lag(z, 3)
#' diff(z, 2)
#' 
#' # compute negative lags (= leading values)
#' lag(z, -1)
#' lead(z, 1) # same as line above
#' identical(lead(z, 1), lag(z, -1)) # TRUE
#'  
#' # compute more than one lag and diff at once (matrix returned)
#' lag(z, c(1,2))
#' diff(z, c(1,2))
#' 
#' ## demonstrate behaviour of shift = "time" vs. shift = "row"
#' # delete 2nd time period for first individual (1978 is missing (not NA)):
#' Em_hole <- Em[-2, ]
#' is.pconsecutive(Em_hole) # check: non-consecutive for 1st individual now
#' 
#' # original non-consecutive data:
#' head(Em_hole$emp, 10) 
#' # for shift = "time", 1-1979 contains the value of former 1-1977 (2 periods lagged):
#' head(lag(Em_hole$emp, k = 2, shift = "time"), 10)
#' # for shift = "row", 1-1979 contains NA (2 rows lagged (and no entry for 1976):
#' head(lag(Em_hole$emp, k = 2, shift = "row"), 10)
#' 
NULL

#' @rdname lag.plm
#' @export
lead <- function(x, k = 1, ...) {
  UseMethod("lead")
}

#' @rdname lag.plm
#' @export
lag.pseries <- function(x, k = 1, shift = c("time", "row"), ...) {
  shift <- match.arg(shift)
  res <- if (shift == "time") lagt.pseries(x = x, k = k, ...) else lagr.pseries(x = x, k = k, ...)
  return(res)
}

#' @rdname lag.plm
#' @export
lead.pseries <- function(x, k = 1, shift = c("time", "row"), ...) {
  shift <- match.arg(shift)
  res <- if (shift == "time") leadt.pseries(x = x, k = k, ...) else leadr.pseries(x = x, k = k, ...)
  return(res)
}

#' @rdname lag.plm
#' @export
diff.pseries <- function(x, lag = 1, shift = c("time", "row"), ...) {
  shift <- match.arg(shift)
  res <- if (shift == "time") difft.pseries(x = x, lag = lag, ...) else diffr.pseries(x = x, lag = lag, ...)
  return(res)
}

## lagt.pseries lagging taking the time variable into account
lagt.pseries <- function(x, k = 1, ...) {
  index <- attr(x, "index")
  id <- index[[1]]
  time <- index[[2]]
  
  if (length(k) > 1) {
    rval <- sapply(k, function(i) alagt(x, i))
    colnames(rval) <- k
  }
  else {
    rval <- alagt(x, k)
  }
  return(rval)
}

## leadt.pseries(x, k) is a wrapper for lagt.pseries(x, -k)
leadt.pseries <- function(x, k = 1, ...) {
  ret <- lagt.pseries(x, k = -k)
  if (length(k) > 1) colnames(ret) <- k
  return(ret)
}

## difft: diff-ing taking the time variable into account
difft.pseries <- function(x, lag = 1, ...){
  ## copied/adapted from diffr.pseries except lines which use lagt() ("t") instead of lagr() ("r")
  islogi <- is.logical(x)
  if (! (is.numeric(x) || islogi)) stop("diff is only relevant for numeric or logical series")
  
  non.int <- vapply(lag, function(l) round(l) != l, FUN.VALUE = TRUE)
  if (any(non.int)) stop("Lagging value(s) in 'lag' must be whole-numbered (and non-negative)")
  
  # prevent input of negative values, because it will most likely confuse users
  # what difft would do in this case
  neg <- vapply(lag, function(l) l < 0, FUN.VALUE = TRUE)
  if (any(neg)) stop("diff is only relevant for non-negative values in 'lag'")
  
  lagtx <- lagt.pseries(x, k = lag) # use "time-based" lagging for difft
  
  if (is.matrix(lagtx)) {
    # if 'lagtx' is matrix (case length(lag) > 1):
    # perform subtraction without pseries feature of 'x', because otherwise 
    # the result would be c("pseries", "matrix") which is not supported
    res <- as.numeric(x) - lagtx
  } else {
    res <- x - lagtx
  }
  
  return(res)
}

## alagt: non-exported helper function for lagt (actual work horse),
## performes shifting of observations while respecting the time dimension
alagt <- function(x, ak) {
  if (round(ak) != ak) stop("Lagging value 'k' must be whole-numbered (positive, negative or zero)")
  if (ak != 0) {
    index <- attr(x, "index")
    id   <- index[[1]]
    time <- index[[2]]
    
    # Idea: split times in blocks per individuals and do lagging there
    # by computation of correct time shifting
    
    # need to convert to numeric, do this by coering to character
    # first (otherwise wrong results!)
    #  see R FAQ 7.10 for coercing factors to numeric: 
    #      as.numeric(levels(factor_var))[as.integer(factor_var)] is
    #      more efficient than
    #      as.numeric(as.character(factor_var))

    # YC 2019/08/29 only works of time values can be coerced to
    ## numeric, ie integers like years. When year is period (ie 5 years),
    ## values used to be 1950 for the 1950-54 period, time is now a
    ## factor in the original data.frame with levels "1950-54",
    ## "1955-59", ... In this case coercing the levels to a numeric gives
    ## NA so coerce the *factor* to a numeric.
    
    levtime <- levels(time)
    numlevtime <- suppressWarnings(as.numeric(levtime))
    if (! any(is.na(numlevtime))) time <- as.numeric(levels(time))[as.integer(time)]
    else time <- as.numeric(time)
    
    list_id_timevar <- split(time, id, drop = TRUE)
    
    index_lag_ak_all_list <- sapply(X = list_id_timevar, 
                                    FUN = function(id_timevar) { 
                                      index_lag_ak <- match(id_timevar - ak, id_timevar, incomparables = NA)
                                    },
                                    simplify = FALSE)
    
    # translate blockwise positions to positions in full vector
    index_lag_ak_all <- unlist(index_lag_ak_all_list, use.names = FALSE)
    
    NApos <- is.na(index_lag_ak_all) # save NA positions for later
    substitute_blockwise <- index_lag_ak_all
    
    block_lengths <- vapply(index_lag_ak_all_list, length, FUN.VALUE = 1L) # lengths (with an "s") would be more efficient, but requires R >= 3.2
    
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
  return(x)
} # END alagt


## lagr: lagging row-wise
lagr.pseries <- function(x, k = 1, ...) {
    index <- attr(x, "index")
    id <- index[[1]]
    time <- index[[2]]
  
    # catch the case when an index of pdata.frame shall be lagged
    # (index variables are always factors) NB: this catches -
    # unintentionally - also the case when a factor variable is the
    # same "on the character level" as one of the corresponding index
    # variables but not the index variable itself
    #
    # -> shall we prevent lagging of index variables at all? -> turned
    # off for now, 2016-03-03 if (is.factor(x)) if
    # (all(as.character(x) == as.character(id)) |
    # all(as.character(x)==as.character(time))) stop("Lagged vector
    # cannot be index.")
  
    alagr <- function(x, ak){
        if (round(ak) != ak) stop("Lagging value 'k' must be whole-numbered (positive, negative or zero)")
        if (ak > 0) {
      
        # NB: this code does row-wise shifting

        # delete first ak observations for each unit
            isNAtime <- c(rep(T, ak), (diff(as.numeric(time), lag = ak) != ak))
            isNAid   <- c(rep(T, ak), (diff(as.numeric(id),   lag = ak) != 0))
            isNA <- (isNAtime | isNAid)
      
            result <- x                                             # copy x first ...
            result[1:ak] <- NA                                      # ... then make first ak obs NA ... 
            result[(ak+1):length(result)] <- x[1:(length(x)-ak)]    # ... shift and ...
            result[isNA] <- NA                                      # ... make more NAs in between: this way, we keep: all factor levels, names, classes
      
        } else if (ak < 0) { # => compute leading values
      
        # delete last |ak| observations for each unit
            num_time <- as.numeric(time)
            num_id   <- as.numeric(id)
            isNAtime <- c(c((num_time[1:(length(num_time)+ak)] - num_time[(-ak+1):length(num_time)]) != ak), rep(T, -ak))
            isNAid   <- c(c((num_id[1:(length(num_id)+ak)]     - num_id[(-ak+1):length(num_id)])     != 0),  rep(T, -ak))
            isNA <- (isNAtime | isNAid)
      
            result <- x                                            # copy x first ...
            result[(length(result)+ak+1):length(result)] <- NA     # ... then make last |ak| obs NA ... 
            result[1:(length(result)+ak)] <- x[(1-ak):(length(x))] # ... shift and ...
            result[isNA] <- NA                                     # ... make more NAs in between: this way, we keep: all factor levels, names, classes
      
        } else { # ak == 0 => nothing to do, return original pseries (no lagging/no leading)
            result <- x
        }
        
        return(result)
    } # END function alagr
  
    if (length(k) > 1) {
        rval <- sapply(k, function(i) alagr(x, i))
        colnames(rval) <- k
    }
    else {
        rval <- alagr(x, k)
    }
    return(rval)
}


# leadr.pseries(x, k) is a wrapper for lagr.pseries(x, -k)
leadr.pseries <- function(x, k = 1, ...) {
    ret <- lagr.pseries(x, k = -k)
    if (length(k) > 1) colnames(ret) <- k
    return(ret)
}

## diffr: lagging row-wise
diffr.pseries <- function(x, lag = 1, ...){
    islogi <- is.logical(x)
    if (! (is.numeric(x) || islogi)) stop("diff is only relevant for numeric or logical series")
    
    non.int <- vapply(lag, function(l) round(l) != l, FUN.VALUE = TRUE)
    if (any(non.int)) stop("Lagging value(s) in 'lag' must be whole-numbered (and non-negative)")
    
    # prevent input of negative values, because it will most likely confuse users
    # what diff would do in this case
    neg <- vapply(lag, function(l) l < 0, FUN.VALUE = TRUE)
    if (any(neg)) stop("diff is only relevant for non-negative values in 'lag'")

    lagrx <- lagr.pseries(x, k = lag)
    
    if (is.matrix(lagrx)) {
      # if 'lagrx' is matrix (case length(lag) > 1):
      # perform subtraction without pseries feature of 'x', because otherwise 
      # the result would be c("pseries", "matrix") which is not supported
      res <- as.numeric(x) - lagrx
    } else {
      res <- x - lagrx
    }
    return(res)
}

## pdiff is (only) used in model.matrix.pFormula to calculate the
## model.matrix for FD models, works for effect = "individual" only,
## see model.matrix on how to call pdiff.  Result is in order (id,
## time) for both effects Performs row-wise shifting
pdiff <- function(x, effect = c("individual", "time"), has.intercept = FALSE){
  # NB: x is assumed to have an index attribute, e.g., a pseries
  #     can check with has.index(x)
    effect <- match.arg(effect)
    cond <- as.numeric(attr(x, "index")[[1]])
    n <- ifelse(is.matrix(x), nrow(x), length(x))
    cond <- c(NA, cond[2:n] - cond[1:(n-1)]) # this assumes a certain ordering
    cond[cond != 0] <- NA
    if (! is.matrix(x)){
        result <- c(NA , x[2:n] - x[1:(n-1)])
        result[is.na(cond)] <- NA
        result <- na.omit(result)
    }
    else{
        result <- rbind(NA, x[2:n, , drop = FALSE] - x[1:(n-1), , drop = FALSE])
        result[is.na(cond), ] <- NA
        result <- na.omit(result)
        result <- result[ , apply(result, 2, var) > 1E-12, drop = FALSE]
        if (has.intercept){
            result <- cbind(1, result)
            colnames(result)[1] <- "(Intercept)"
        }
    }
    attr(result, "na.action") <- NULL
    result
}

