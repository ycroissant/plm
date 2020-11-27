########### is.pconsecutive ##############
# little helper function to determine if the time periods of an object are consecutive per id.
# By consecutive we mean "consecutive in the numbers", i.e. is.pconsecutive takes the numerical
# value of the time variable into account: t, t+1, t+2, ... where t is an integer
#
# For this, we need as.numeric(as.character(time_var)) where as.character is a crucial part!
# Equivalent but more efficient is as.numeric(levels(id_timevar))[as.integer(id_timevar)]
# (see R FAQ 7.10 for coercing factors to numeric]
# and the coerction of time_var in this manner needs to be meaningful numbers.
#
# see also in separate file make.pconsecutive.R:
#   * make.pconsecutive
#   * make.pbalanced


#' Check if time periods are consecutive
#' 
#' This function checks for each individual if its associated time periods are
#' consecutive (no "gaps" in time dimension per individual)
#' 
#' (p)data.frame, pseries and estimated panelmodel objects can be tested if
#' their time periods are consecutive per individual.  For evaluation of
#' consecutiveness, the time dimension is interpreted to be numeric, and the
#' data are tested for being a regularly spaced sequence with distance 1
#' between the time periods for each individual (for each individual the time
#' dimension can be interpreted as sequence t, t+1, t+2, \ldots{} where t is an
#' integer). As such, the "numerical content" of the time index variable is
#' considered for consecutiveness, not the "physical position" of the various
#' observations for an individuals in the (p)data.frame/pseries (it is not
#' about "neighbouring" rows). If the object to be evaluated is a pseries or a
#' pdata.frame, the time index is coerced from factor via as.character to
#' numeric, i.e. the series
#' `as.numeric(as.character(index(<pseries/pdata.frame>)[[2]]))]` is
#' evaluated for gaps.
#' 
#' The default method also works for argument `x` being an arbitrary
#' vector (see **Examples**), provided one can supply arguments `id`
#' and `time`, which need to ordered as stacked time series. As only
#' `id` and `time` are really necessary for the default method to
#' evaluate the consecutiveness, `x = NULL` is also possible. However, if
#' the vector `x` is also supplied, additional input checking for equality
#' of the lengths of `x`, `id` and `time` is performed, which is
#' safer.
#' 
#' For the data.frame interface, the data is ordered in the appropriate way
#' (stacked time series) before the consecutiveness is evaluated. For the
#' pdata.frame and pseries interface, ordering is not performed because both
#' data types are already ordered in the appropriate way when created.
#' 
#' Note: Only the presence of the time period itself in the object is tested,
#' not if there are any other variables.  `NA` values in individual index
#' are not examined but silently dropped - In this case, it is not clear which
#' individual is meant by id value `NA`, thus no statement about
#' consecutiveness of time periods for those "`NA`-individuals" is
#' possible.
#'
#' @name is.pconsecutive
#' @aliases is.pconsecutive
#' @param x usually, an object of class `pdata.frame`,
#'     `data.frame`, `pseries`, or an estimated
#'     `panelmodel`; for the default method `x` can also be
#'     an arbitrary vector or `NULL`, see **Details**,
#' @param na.rm.tindex logical indicating whether any `NA` values
#'     in the time index are removed before consecutiveness is
#'     evaluated (defaults to `FALSE`),
#' @param index only relevant for `data.frame` interface; if
#'     `NULL`, the first two columns of the data.frame are
#'     assumed to be the index variables; if not `NULL`, both
#'     dimensions ('individual', 'time') need to be specified by
#'     `index` for `is.pconsecutive` on data frames, for
#'     further details see [pdata.frame()],
#' @param id,time only relevant for default method: vectors specifying
#'     the id and time dimensions, i. e. a sequence of individual and
#'     time identifiers, each as stacked time series,
#' @param \dots further arguments.
#' @return A named `logical` vector (names are those of the
#'     individuals).  The i-th element of the returned vector
#'     corresponds to the i-th individual.  The values of the i-th
#'     element can be: \item{"TRUE"}{if the i-th individual has
#'     consecutive time periods,} \item{"FALSE"}{if the i-th
#'     individual has non-consecutive time periods,}
#'     \item{"NA"}{if there are any NA values in time index of
#'     the i-th the individual; see also argument `na.rm.tindex`
#'     to remove those.}
#' @export
#' @author Kevin Tappe
#' @seealso [make.pconsecutive()] to make data consecutive
#'     (and, as an option, balanced at the same time) and
#'     [make.pbalanced()] to make data balanced.\cr
#'     [pdim()] to check the dimensions of a 'pdata.frame'
#'     (and other objects), [pvar()] to check for individual
#'     and time variation of a 'pdata.frame' (and other objects),
#'     [lag()] for lagged (and leading) values of a
#'     'pseries' object.\cr
#' 
#' [pseries()], [data.frame()], [pdata.frame()],
#' for class 'panelmodel' see [plm()] and [pgmm()].
#' @keywords attribute
#' @examples
#' 
#' data("Grunfeld", package = "plm")
#' is.pconsecutive(Grunfeld)
#' is.pconsecutive(Grunfeld, index=c("firm", "year"))
#' 
#' # delete 2nd row (2nd time period for first individual)
#' # -> non consecutive 
#' Grunfeld_missing_period <- Grunfeld[-2, ]
#' is.pconsecutive(Grunfeld_missing_period)
#' all(is.pconsecutive(Grunfeld_missing_period)) # FALSE
#' 
#' # delete rows 1 and 2 (1st and 2nd time period for first individual)
#' # -> consecutive
#' Grunfeld_missing_period_other <- Grunfeld[-c(1,2), ]
#' is.pconsecutive(Grunfeld_missing_period_other) # all TRUE
#' 
#' # delete year 1937 (3rd period) for _all_ individuals
#' Grunfeld_wo_1937 <- Grunfeld[Grunfeld$year != 1937, ]
#' is.pconsecutive(Grunfeld_wo_1937) # all FALSE
#' 
#' # pdata.frame interface
#' pGrunfeld <- pdata.frame(Grunfeld)
#' pGrunfeld_missing_period <- pdata.frame(Grunfeld_missing_period)
#' is.pconsecutive(pGrunfeld) # all TRUE
#' is.pconsecutive(pGrunfeld_missing_period) # first FALSE, others TRUE
#' 
#' 
#' # panelmodel interface (first, estimate some models)
#' mod_pGrunfeld <- plm(inv ~ value + capital, data = Grunfeld)
#' mod_pGrunfeld_missing_period <- plm(inv ~ value + capital, data = Grunfeld_missing_period)
#' 
#' is.pconsecutive(mod_pGrunfeld)
#' is.pconsecutive(mod_pGrunfeld_missing_period)
#' 
#' nobs(mod_pGrunfeld) # 200
#' nobs(mod_pGrunfeld_missing_period) # 199
#' 
#' 
#' # pseries interface
#' pinv <- pGrunfeld$inv
#' pinv_missing_period <- pGrunfeld_missing_period$inv
#' 
#' is.pconsecutive(pinv)
#' is.pconsecutive(pinv_missing_period)
#' 
#' # default method for arbitrary vectors or NULL
#' inv <- Grunfeld$inv
#' inv_missing_period <- Grunfeld_missing_period$inv
#' is.pconsecutive(inv, id = Grunfeld$firm, time = Grunfeld$year)
#' is.pconsecutive(inv_missing_period, id = Grunfeld_missing_period$firm, 
#'                                     time = Grunfeld_missing_period$year)
#' 
#' # (not run) demonstrate mismatch lengths of x, id, time 
#' # is.pconsecutive(x = inv_missing_period, id = Grunfeld$firm, time = Grunfeld$year)
#' 
#' # only id and time are needed for evaluation
#' is.pconsecutive(NULL, id = Grunfeld$firm, time = Grunfeld$year)
#' 
is.pconsecutive <- function(x, ...){
  UseMethod("is.pconsecutive")
}

#' @rdname is.pconsecutive
#' @export
is.pconsecutive.default <- function(x, id, time, na.rm.tindex = FALSE, ...) {
  # argument 'x' just used for input check (if it is not NULL and is a vector)
  
  # input checks
  if (length(id) != length(time)) 
    stop(paste0("arguments 'id' and 'time' must have same length: length(id): ", length(id), ", length(time) ", length(time)))
  
  if (!is.null(x) && is.vector(x)) {
    if (!(length(x) == length(id) && length(x) == length(time) && length(id) == length(time)))
      stop(paste0("arguments 'x', 'id', 'time' must have same length: length(x): ", 
                  length(x), ", length(id): ", length(id), ", length(time): ", length(time)))
    
  }
  
  # NB: 'time' is assumed to be organised as stacked time series (sorted for each individual)
  #     (successive blocks of individuals, each block being a time series for the respective individual))
  #
  #   'time' is in the correct order if is.pconsecutive.default is called by
  #   is.pconsecutive.pdata.frame or is.pconsecutive.pseries as a pdata.frame (which is sorted) was constructed 
  #   in the first place; for data.frame interface the ordering is done in the respective function
  
  if (na.rm.tindex) {
    NA_tindex <- is.na(time)
    time <- time[!NA_tindex]
    id <- id[!NA_tindex]
  }
  
  # if time var is factor (as is TRUE for pdata.frames, pseries):
  # need to convert to numeric, do this by coering to character first (otherwise wrong results!)
  #  see R FAQ 7.10 for coercing factors to numeric: 
  #      as.numeric(levels(factor_var))[as.integer(factor_var)]   is more efficient than
  #      as.numeric(as.character(factor_var))
  if (!is.numeric(time) && is.factor(time)) time <- as.numeric(levels(time))[as.integer(time)]
  
  list_id_timevar <- split(time, id, drop = TRUE)

  res <- sapply(list_id_timevar, function(id_timevar) { if(anyNA(id_timevar)) {
                                                           NA # return NA if NA found in the time periods for individual
                                                          } else {
                                                              begin <- id_timevar[1L]
                                                              end   <- id_timevar[length(id_timevar)]
                                                 
                                                              # compare to length(original id_timevar) to find out if times are consecutive
                                                              (end - begin + 1L) == length(id_timevar)
                                                              
                                                              # Alternative way of checking:
                                                                # consecutive time periods from begin to end (if id_timevar were consecutive)
                                                                # consecutive <- seq(from = begin, to = end, by = 1)
                                                                # length(consecutive) == length(id_timevar)
                                                          }
                                                      })
  return(res)
}

#' @rdname is.pconsecutive
#' @export
is.pconsecutive.data.frame <- function(x, index = NULL, na.rm.tindex = FALSE, ...){
  if (!is.null(index) && length(index) != 2L)
    stop("if argument 'index' is not NULL, 'index' needs to specify
         'individual' and 'time' dimension for is.pconsecutive to work on a data.frame")
  
  if (is.null(index)) index_orig_names <- names(x)[1:2] # assume first two columns to be the index vars
    else index_orig_names <- index
  
  id   <- x[ , index_orig_names[1L]]
  time <- x[ , index_orig_names[2L]]

  # order as stacked time series (by id and time) first, otherwise default method does not work correctly!
  ord <- order(id, time)
  x_ordered    <- x[ord, ]
  id_ordered   <- id[ord]
  time_ordered <- time[ord]
  
#  if (!identical(x, x_ordered)) 
#    print("Note: for test of consecutiveness of time periods, the data.frame was ordered by index variables (id, time)")
  
  return(is.pconsecutive.default(x_ordered, id_ordered, time_ordered, na.rm.tindex = na.rm.tindex, ...))
}

#' @rdname is.pconsecutive
#' @export
is.pconsecutive.pseries <- function(x, na.rm.tindex = FALSE, ...){
  index <- attr(x, "index")
  id   <- index[[1L]]
  time <- index[[2L]]
  return(is.pconsecutive.default(x, id, time, na.rm.tindex = na.rm.tindex, ...))
}


#' @rdname is.pconsecutive
#' @export
is.pconsecutive.pdata.frame <- function(x, na.rm.tindex = FALSE, ...){
  index <- attr(x, "index")
  id   <- index[[1L]]
  time <- index[[2L]]
  return(is.pconsecutive.default(x, id, time, na.rm.tindex = na.rm.tindex, ...))
}

#' @rdname is.pconsecutive
#' @export
is.pconsecutive.panelmodel <- function(x, na.rm.tindex = FALSE, ...){
  index <- attr(x$model, "index")
  id   <- index[[1L]]
  time <- index[[2L]]
  return(is.pconsecutive.default(x, id, time, na.rm.tindex = na.rm.tindex, ...))
}


########### is.pbalanced ##############
### for convenience and to be faster than pdim() for the purpose
### of the determination of balancedness only, because it avoids
### pdim()'s calculations which are unnecessary for balancedness.
###
### copied (and adapted) methods and code from pdim.*
### (only relevant parts to determine balancedness)


#' Check if data are balanced
#' 
#' This function checks if the data are balanced, i.e. if each individual has
#' the same time periods
#' 
#' Balanced data are data for which each individual has the same time periods.
#' The returned values of the `is.pbalanced(object)` methods are identical
#' to `pdim(object)$balanced`.  `is.pbalanced` is provided as a short
#' cut and is faster than `pdim(object)$balanced` because it avoids those
#' computations performed by `pdim` which are unnecessary to determine the
#' balancedness of the data.
#' 
#' @aliases is.pbalanced
#' @param x an object of class `pdata.frame`, `data.frame`,
#'     `pseries`, `panelmodel`, or `pgmm`,
#' @param y (only in default method) the time index variable (2nd index
#' variable),
#' @param index only relevant for `data.frame` interface; if
#'     `NULL`, the first two columns of the data.frame are
#'     assumed to be the index variables; if not `NULL`, both
#'     dimensions ('individual', 'time') need to be specified by
#'     `index` as character of length 2 for data frames, for
#'     further details see [pdata.frame()],
#' @param \dots further arguments.
#' @return A logical indicating whether the data associated with
#'     object `x` are balanced (`TRUE`) or not
#'     (`FALSE`).
#' @seealso [punbalancedness()] for two measures of
#'     unbalancedness, [make.pbalanced()] to make data
#'     balanced; [is.pconsecutive()] to check if data are
#'     consecutive; [make.pconsecutive()] to make data
#'     consecutive (and, optionally, also balanced).\cr
#'     [pdim()] to check the dimensions of a 'pdata.frame'
#'     (and other objects), [pvar()] to check for individual
#'     and time variation of a 'pdata.frame' (and other objects),
#'     [pseries()], [data.frame()],
#'     [pdata.frame()].
#' @export
#' @keywords attribute
#' @examples
#' 
#' # take balanced data and make it unbalanced
#' # by deletion of 2nd row (2nd time period for first individual)
#' data("Grunfeld", package = "plm")
#' Grunfeld_missing_period <- Grunfeld[-2, ]
#' is.pbalanced(Grunfeld_missing_period)     # check if balanced: FALSE
#' pdim(Grunfeld_missing_period)$balanced    # same
#' 
#' # pdata.frame interface
#' pGrunfeld_missing_period <- pdata.frame(Grunfeld_missing_period)
#' is.pbalanced(Grunfeld_missing_period)
#' 
#' # pseries interface
#' is.pbalanced(pGrunfeld_missing_period$inv)
#' 
is.pbalanced <- function(x, ...) {
  UseMethod("is.pbalanced")
}

#' @rdname is.pbalanced
#' @export
is.pbalanced.default <- function(x, y, ...) {
  if (length(x) != length(y)) stop("The length of the two vectors differs\n")
  x <- x[drop = TRUE] # drop unused factor levels so that table 
  y <- y[drop = TRUE] # gives only needed combinations
  z <- table(x, y)
  if (any(v <- as.vector(z) == 0L)) {
    balanced <- FALSE
  } else { balanced <- TRUE }
  
  if (any(v > 1L)) warning("duplicate couples (id-time)\n")
  
  return(balanced)
}

#' @rdname is.pbalanced
#' @export
is.pbalanced.data.frame <- function(x, index = NULL, ...) {
  x <- pdata.frame(x, index)
  index <- attr(x, "index")
  id <- index[[1L]]
  time <- index[[2L]]
  return(is.pbalanced(id, time))
}

#' @rdname is.pbalanced
#' @export
is.pbalanced.pdata.frame <- function(x, ...) {
  index <- attr(x, "index")
  return(is.pbalanced(index[[1L]], index[[2L]]))
}

#' @rdname is.pbalanced
#' @export
is.pbalanced.pseries <- function(x, ...) {
  index <- attr(x, "index")
  return(is.pbalanced(index[[1L]], index[[2L]]))
}

#' @rdname is.pbalanced
#' @export
is.pbalanced.panelmodel <- function(x, ...) {
  x <- model.frame(x)
  return(is.pbalanced(x))
}

#' @rdname is.pbalanced
#' @export
is.pbalanced.pgmm <- function(x, ...) {
## pgmm is also class panelmodel, but take advantage of the pdim attribute in it
  return(attr(x, "pdim")$balanced)
}
