### This file:
### make.pconsecutive.*
### make.pbalanced.*
###
### is.pconsecutive.* is in separate file is.pconsecutive.R

#  consecutive: "consecutive in the numbers": t, t+1, t+2, ... where t is an integer,
#                i.e., the time index var is interpreted as a numerical
# 

## in the future, maybe make.pconsective could gain an additional argument 'fill' for the filled value (currently NA)
##      if so, check other packages (data.table, dplyr, tidyr, ...) what the argument is called there
##      arg would need to be a (named) list (for (p)data.frame methods) because columns of 
##      (p)data.frames are of arbitrary classes


#' Make data consecutive (and, optionally, also balanced)
#' 
#' This function makes the data consecutive for each individual (no "gaps" in
#' time dimension per individual) and, optionally, also balanced
#' 
#' (p)data.frame and pseries objects are made consecutive, meaning their time
#' periods are made consecutive per individual.  For consecutiveness, the time
#' dimension is interpreted to be numeric, and the data are extended to a
#' regularly spaced sequence with distance 1 between the time periods for each
#' individual (for each individual the time dimension become a sequence t, t+1,
#' t+2, \ldots{}, where t is an integer). Non--index variables are filled with
#' `NA` for the inserted elements (rows for (p)data.frames, vector
#' elements for pseries).
#' 
#' With argument `balanced = TRUE`, additionally to be made consecutive,
#' the data also can be made a balanced panel/pseries.  Note: This means
#' consecutive AND balanced; balancedness does not imply consecutiveness. In
#' the result, each individual will have the same time periods in their time
#' dimension by taking the min and max of the time index variable over all
#' individuals (w/o `NA` values) and inserting the missing time periods.
#' Looking at the number of rows of the resulting (pdata.frame) (elements for
#' pseries), this results in `nrow(make.pconsecutive(<.>, balanced = FALSE))` <=
#' `nrow(make.pconsecutive(<.>, balanced = TRUE))`. For making the data only
#' balanced, i.e., not demanding consecutiveness at the same time, use
#' [make.pbalanced()] (see **Examples** for a comparison)).
#' 
#' Note: rows of (p)data.frames (elements for pseries) with `NA` values in
#' individual or time index are not examined but silently dropped before the
#' data are made consecutive. In this case, it is not clear which individual or
#' time period is meant by the missing value(s). Especially, this means: If
#' there are `NA` values in the first/last position of the original time
#' periods for an individual, which usually depicts the beginning and ending of
#' the time series for that individual, the beginning/end of the resulting time
#' series is taken to be the min and max (w/o `NA` values) of the original
#' time series for that individual, see also **Examples**. Thus, one might
#' want to check if there are any `NA` values in the index variables
#' before applying `make.pconsecutive`, and especially check for `NA` values
#' in the first and last position for each individual in original data and, if
#' so, maybe set those to some meaningful begin/end value for the time series.
#' 
#' @aliases make.pconsecutive
#' @param x an object of class `pdata.frame`, `data.frame`,
#'     or `pseries`,
#' @param balanced logical, indicating whether the data should
#'     _additionally_ be made balanced (default: FALSE),
#' @param index only relevant for `data.frame` interface; if
#'     `NULL`, the first two columns of the data.frame are
#'     assumed to be the index variables; if not `NULL`, both
#'     dimensions ('individual', 'time') need to be specified by
#'     `index` as character of length 2 for data frames, for
#'     further details see [pdata.frame()],
#' @param \dots further arguments.
#' @return An object of the same class as the input `x`, i.e., a
#'     pdata.frame, data.frame or a pseries which is made
#'     time--consecutive based on the index variables. The returned
#'     data are sorted as a stacked time series.
#' @export
#' @author Kevin Tappe
#' @seealso [is.pconsecutive()] to check if data are
#'     consecutive; [make.pbalanced()] to make data only
#'     balanced (not consecutive).\cr [punbalancedness()]
#'     for two measures of unbalancedness, [pdim()] to check
#'     the dimensions of a 'pdata.frame' (and other objects),
#'     [pvar()] to check for individual and time variation
#'     of a 'pdata.frame' (and other objects), [lag()] for
#'     lagged (and leading) values of a 'pseries' object.\cr
#'     [pseries()], [data.frame()],
#'     [pdata.frame()].
#' @keywords attribute
#' @examples
#' 
#' # take data and make it non-consecutive
#' # by deletion of 2nd row (2nd time period for first individual)
#' data("Grunfeld", package = "plm")
#' nrow(Grunfeld)                             # 200 rows
#' Grunfeld_missing_period <- Grunfeld[-2, ]
#' is.pconsecutive(Grunfeld_missing_period)   # check for consecutiveness
#' make.pconsecutive(Grunfeld_missing_period) # make it consecutiveness
#' 
#' 
#' # argument balanced:
#' # First, make data non-consecutive and unbalanced
#' # by deletion of 2nd time period (year 1936) for all individuals
#' # and more time periods for first individual only
#' Grunfeld_unbalanced <- Grunfeld[Grunfeld$year != 1936, ]
#' Grunfeld_unbalanced <- Grunfeld_unbalanced[-c(1,4), ]
#' all(is.pconsecutive(Grunfeld_unbalanced)) # FALSE
#' pdim(Grunfeld_unbalanced)$balanced        # FALSE
#' 
#' g_consec_bal <- make.pconsecutive(Grunfeld_unbalanced, balanced = TRUE)
#' all(is.pconsecutive(g_consec_bal)) # TRUE
#' pdim(g_consec_bal)$balanced        # TRUE
#' nrow(g_consec_bal)                 # 200 rows
#' head(g_consec_bal)                 # 1st individual: years 1935, 1936, 1939 are NA
#' 
#' g_consec <- make.pconsecutive(Grunfeld_unbalanced) # default: balanced = FALSE
#' all(is.pconsecutive(g_consec)) # TRUE
#' pdim(g_consec)$balanced        # FALSE
#' nrow(g_consec)                 # 198 rows
#' head(g_consec)                 # 1st individual: years 1935, 1936 dropped, 1939 is NA 
#' 
#' 
#' # NA in 1st, 3rd time period (years 1935, 1937) for first individual
#' Grunfeld_NA <- Grunfeld
#' Grunfeld_NA[c(1, 3), "year"] <- NA
#' g_NA <- make.pconsecutive(Grunfeld_NA)
#' head(g_NA)        # 1936 is begin for 1st individual, 1937: NA for non-index vars
#' nrow(g_NA)        # 199, year 1935 from original data is dropped
#' 
#' 
#' # pdata.frame interface
#' pGrunfeld_missing_period <- pdata.frame(Grunfeld_missing_period)
#' make.pconsecutive(Grunfeld_missing_period)
#' 
#' 
#' # pseries interface
#' make.pconsecutive(pGrunfeld_missing_period$inv)
#' 
#' 
#' # comparison to make.pbalanced (makes the data only balanced, not consecutive)
#' g_bal <- make.pbalanced(Grunfeld_unbalanced)
#' all(is.pconsecutive(g_bal)) # FALSE
#' pdim(g_bal)$balanced        # TRUE
#' nrow(g_bal) # 190 rows
#' 
make.pconsecutive <- function(x, ...){
  UseMethod("make.pconsecutive")
}

# no export needed
make.pconsecutive_indexes <- function(x, index, balanced = FALSE, ...) {
# make.pconsecutive_indexes: helper function, not exported
# returns list with 3 elements:
#   1 "consec_index":        consecutive data.frame to serve as the new index data.frame in other functions, 
#   2 "NArows_former_index": information about dropped lines (logical vector with length of original data)
#   3 "has_fancy_rownames":  logical whether fancy row.names were used in original data (can only be TRUE for pdata.frame or pseries)
 
  if (inherits(x, "pdata.frame") || inherits(x, "pseries")) {
    pdataframe_or_pseries <- TRUE
    index_orig <- attr(x, which = "index")
    id_orig    <- index_orig[[1L]] # can leave as factor if it is a factor
    times_orig <- index_orig[[2L]]
    if (!is.numeric(times_orig) && is.factor(times_orig)) times_orig <- as.numeric(levels(times_orig))[as.integer(times_orig)]
    # time var needs to be numeric [as.character needed here!]
    # [R FAQ 7.10 for coercing factors to numeric 
    # as.numeric(levels(factor_var))[as.integer(factor_var)] is more efficient than as.numeric(as.character(factor_var))
    
    # check if fancy rownames are used (to restore them later)
    if (inherits(x, "pseries")) {
      has_fancy_rownames <- isTRUE(all.equal(names(x), fancy.row.names(index_orig)))
      rownames_mode <- mode(attr(x, "names"))
      rownames_typeof <- typeof(attr(x, "names"))
    } else {
      # pdata.frame
      has_fancy_rownames <- isTRUE(all.equal(row.names(x), fancy.row.names(index_orig)))
      rownames_mode <- mode(attr(x, "row.names"))
      rownames_typeof <- typeof(attr(attr(x, "index"), "row.names")) # here we want the typeof of the index

    }
  }
  if (inherits(x, "data.frame") && !inherits(x, "pdata.frame")) {
    # x is a data.frame, but no pdata.frame
    pdataframe_or_pseries <- FALSE
    has_fancy_rownames    <- FALSE
    index_orig <- x[ , index]
    id_orig    <- index_orig[[1L]]
    times_orig <- index_orig[[2L]]
    id_orig_typeof    <- typeof(id_orig)
    times_orig_typeof <- typeof(times_orig)
    rownames_mode <- mode(attr(x, "row.names"))
    rownames_typeof <- typeof(attr(x, "row.names"))
    
  }
  
  df_index <- data.frame(id = id_orig, times = times_orig)
  
  # remove any rows with NA in id or time variable as it is impossible to
  # infer their values, thus: drop them
  is_NA <- is.na(id_orig) | is.na(times_orig)
  df_index <- df_index[!is_NA, ]
  
  n_id_orig <- length(unique(id_orig))
  
  if (!balanced) { 
    min_values <- by(df_index[ , "times"], df_index[ , "id"], min)
    max_values <- by(df_index[ , "times"], df_index[ , "id"], max)
    
    times_filled_list <- sapply(seq_len(n_id_orig), function(i) {
        seq(from = min_values[i], to = max_values[i], by = 1)
      }, simplify = FALSE, USE.NAMES = FALSE)
  
  } else {
    min_value <- min(df_index[, "times"])
    max_value <- max(df_index[, "times"])
    
    times_filled_list <- sapply(seq_len(n_id_orig), function(i) {
        seq(from = min_value, to = max_value, by = 1)
      }, simplify = FALSE, USE.NAMES = FALSE)
  }
  
  times_filled_vector <- unlist(times_filled_list, use.names = FALSE)
  id_times <- lengths(times_filled_list, use.names = FALSE)
  
  id_filled_vector <- unlist(mapply(rep, unique(id_orig), id_times, SIMPLIFY = FALSE), use.names = FALSE)
                      # SIMPLIFY = FALSE => always return list

  df_index_filled <- data.frame(id = id_filled_vector, times = times_filled_vector)
  names(df_index_filled)[1:2] <- names(index_orig)[1:2] # set original index names


  if (pdataframe_or_pseries) {
    df_index_filled[ , 1L] <- as.factor(df_index_filled[ , 1L])
    df_index_filled[ , 2L] <- as.factor(df_index_filled[ , 2L])
    class(df_index_filled) <- c("pindex", class(df_index_filled))
  } else {
    if (typeof(df_index_filled[ , 1L]) != id_orig_typeof)    { mode(df_index_filled[ , 1L]) <- id_orig_typeof    }
    if (typeof(df_index_filled[ , 2L]) != times_orig_typeof) { mode(df_index_filled[ , 2L]) <- times_orig_typeof }
  }

  # restore mode of row.names attribute
  # [was changed by above code due to some simplification by R's standard behaviour]
  mode(attr(df_index_filled, "row.names")) <- rownames_typeof
  
  res <- list(consec_index         = df_index_filled,
              NArows_former_index  = is_NA,
              has_fancy_rownames   = has_fancy_rownames)
  
  return(res)
} ### END: make.pconsecutive_indexes


#' @rdname make.pconsecutive
#' @export
make.pconsecutive.data.frame <- function(x, balanced = FALSE, index = NULL, ...){
  # if not NULL, index is must be character of length 2
  if (!is.null(index) && length(index) != 2L)
    stop("if argument 'index' is not NULL, 'index' needs to specify
         'individual' and 'time' dimension for make.pconsecutive to work on a data.frame")
  
  # assume first two columns to be the index vars
  index_orig_names <- if(is.null(index)) names(x)[1:2] else index
    
  list_ret_make_index <- make.pconsecutive_indexes(x, index_orig_names, balanced = balanced, ...)
  
  index_df_filled    <- list_ret_make_index[["consec_index"]]
  NArows_old_index   <- list_ret_make_index[["NArows_former_index"]]
  has_fancy_rownames <- list_ret_make_index[["has_fancy_rownames"]]
  
  # silently drop rows with NA in either individual or time variable of original index
  x <- x[!NArows_old_index, ]
  
  index_df_filled_plus_x <- merge(index_df_filled, x, by.x = names(index_df_filled)[1:2],
                                                      by.y = index_orig_names,
                                                      all.x = TRUE)
  
  # restore mode of row.names attribute [was changed by above code due to some simplification as R's standard behaviour]
  mode(attr(index_df_filled_plus_x, "row.names")) <- typeof(attr(index_df_filled, "row.names"))
  
  # restore original order of columns, esp. place index vars at original position
  index_df_filled_plus_x <- index_df_filled_plus_x[ , names(x)]
    
  return(index_df_filled_plus_x)
} ### END: make.pconsecutive.data.frame

#' @rdname make.pconsecutive
#' @export
make.pconsecutive.pdata.frame <- function(x, balanced = FALSE, ...){
  orig_column_names <- names(x)
    
  list_ret_make_index <- make.pconsecutive_indexes(x, balanced = balanced, ...)
  index_df_filled    <- list_ret_make_index[["consec_index"]]
  NArows_old_index   <- list_ret_make_index[["NArows_former_index"]]
  has_fancy_rownames <- list_ret_make_index[["has_fancy_rownames"]]
  
  # silently drop rows with NA in either individual or time variable of original index
  # do dropping only if there is any NA row, because calling the subsetting slightly changes the pdata.frame
  if (any(NArows_old_index)) x <- x[!NArows_old_index, ]
  
  # if index not as vars in pdata.frame: pad index vars in columns 1,2 to enable merging
  # determine position of index vars is c(NA, NA) if index vars are not columns in x
  pos_indexvars <- pos.index(x)
  index_orig_names <- names(pos_indexvars)
  if (anyNA(pos_indexvars)) {
    index_orig <- attr(x, "index")
    x <- cbind(index_orig, x)
  }
  
  x_df_filled <- merge(index_df_filled, x, by = index_orig_names, all.x = TRUE)
  # merge produces a pdata.frame with 'pseries' in columns (if [.pseries is active])
  # -> remove pseries features from columns
  x_df_filled <- lapply(x_df_filled, remove_pseries_features)
  
  # make pdata.frame (index vars are already in columns 1,2)
  x_pdf_filled <- pdata.frame(x_df_filled, row.names = has_fancy_rownames)
  
  # save order of attributes to restore order later
  # attrib_names_before <- names(attributes(x_pdf_filled))
  
  # restore original order of columns:
  # this also places index vars at original position or drops them if they were not in original pdata.frame
  # (do only if order of columns differs or index is not in pdata.frame to avoid adding extra attributes by subsetting)
  if (!isTRUE(all.equal(orig_column_names, names(x_pdf_filled)))) x_pdf_filled <- x_pdf_filled[ , orig_column_names]
  
  # restore mode of row.names attribute [was changed by above code due to some simplification as R's standard behaviour]
  mode(attr(attr(x_pdf_filled, "index"), "row.names")) <- typeof(attr(index_df_filled, "row.names"))

  # reorder attributes: subsetting with R's [.data.frame changes order
  # order of attribute shall be assumed to be a set rather than having an order, see do not reorder (see ?attributes)
    ##  attributes(x_pdf_filled) <- attributes(x_pdf_filled)[attrib_names_before]
    
  return(x_pdf_filled)
} ### END: make.pconsecutive.pdata.frame

#' @rdname make.pconsecutive
#' @export
make.pconsecutive.pseries <- function(x, balanced = FALSE, ...) {
  is_p <- is.pconsecutive(x)
  is_bal <- is.pbalanced(x)
  make_balanced <- balanced == TRUE && !is_bal # consecutive AND balancedness requested but data not balanced 
                                               #  -> independent of the consecutiveness, we need to treat the balancedness
  
  if (anyNA(is_p) || !all(is_p) || make_balanced) {
    
    list_ret_make_index <- make.pconsecutive_indexes(x, balanced = balanced, ...)
    df_index_filled    <- list_ret_make_index[["consec_index"]]
    NArows_old_index   <- list_ret_make_index[["NArows_former_index"]]
    has_fancy_rownames <- list_ret_make_index[["has_fancy_rownames"]]
    
    df_old_index <- attr(x, "index")
    class(df_old_index) <- "data.frame"
    
    # strip x to its pure form (no index, no class pseries)
    df_old_index$x <- remove_pseries_features(x)
    
    # silently drop entries with NA in either individual or time variable of original index
    df_old_index <- df_old_index[!NArows_old_index, ]
    
    df_index_filled_plus_x <- merge(df_index_filled, df_old_index, by.x = names(df_index_filled)[1:2],
                                    by.y = names(df_old_index)[1:2],
                                    all.x = TRUE)
    
    pdf_index_filled_plus_x <- pdata.frame(df_index_filled_plus_x,
                                           drop.index = FALSE,
                                           row.names = has_fancy_rownames)
    
    x <- pdf_index_filled_plus_x$x
  }
  return(x)
}



############# make.pbalanced #############
## make.pbalanced.* methods make the input balanced (but not consecutive).
## It does so by either 
## balance.type = "fill": filling in only those missing time periods are 
##                        introduced that are present for at least one individual
##                        (union of time periods)
##
## balance.type = "shared.times": remove all observations with time periods
##                                not shared among all individuals
##                                (keep intersect of time periods)
##
##                "shared.individuals": drop individuals which don't have all time periods
##                                      (symmetric to "shared.times")



#' Make data balanced
#' 
#' This function makes the data balanced, i.e., each individual has the same
#' time periods, by filling in or dropping observations
#' 
#' (p)data.frame and pseries objects are made balanced, meaning each
#' individual has the same time periods.  Depending on the value of
#' `balance.type`, the balancing is done in different ways:
#' \itemize{ \item `balance.type = "fill"` (default): The union
#' of available time periods over all individuals is taken (w/o
#' `NA` values).  Missing time periods for an individual are
#' identified and corresponding rows (elements for pseries) are
#' inserted and filled with `NA` for the non--index variables
#' (elements for a pseries).  This means, only time periods present
#' for at least one individual are inserted, if missing.
#' 
#' \item `balance.type = "shared.times"`: The intersect of available time
#' periods over all individuals is taken (w/o `NA` values).  Thus, time
#' periods not available for all individuals are discarded, i. e., only time
#' periods shared by all individuals are left in the result).
#' 
#' \item `balance.type = "shared.individuals"`: All available time periods
#' are kept and those individuals are dropped for which not all time periods
#' are available, i. e., only individuals shared by all time periods are left
#' in the result (symmetric to `"shared.times"`).  }
#' 
#' The data are not necessarily made consecutive (regular time series
#' with distance 1), because balancedness does not imply
#' consecutiveness. For making the data consecutive, use
#' [make.pconsecutive()] (and, optionally, set argument
#' `balanced = TRUE` to make consecutive and balanced, see also
#' **Examples** for a comparison of the two functions.
#' 
#' Note: Rows of (p)data.frames (elements for pseries) with `NA`
#' values in individual or time index are not examined but silently
#' dropped before the data are made balanced. In this case, it cannot
#' be inferred which individual or time period is meant by the missing
#' value(s) (see also **Examples**).  Especially, this means:
#' `NA` values in the first/last position of the original time
#' periods for an individual are dropped, which are usually meant to
#' depict the beginning and ending of the time series for that
#' individual.  Thus, one might want to check if there are any
#' `NA` values in the index variables before applying
#' `make.pbalanced`, and especially check for `NA` values in the
#' first and last position for each individual in original data and,
#' if so, maybe set those to some meaningful begin/end value for the
#' time series.
#' 
#' @aliases make.pbalanced
#' @param x an object of class `pdata.frame`, `data.frame`,
#'     or `pseries`;
#' @param balance.type character, one of `"fill"`,
#'     `"shared.times"`, or `"shared.individuals"`, see
#'     **Details**,
#' @param index only relevant for `data.frame` interface; if
#'     `NULL`, the first two columns of the data.frame are
#'     assumed to be the index variables; if not `NULL`, both
#'     dimensions ('individual', 'time') need to be specified by
#'     `index` as character of length 2 for data frames, for
#'     further details see [pdata.frame()],
#' @param \dots further arguments.
#' @return An object of the same class as the input `x`, i.e., a
#'     pdata.frame, data.frame or a pseries which is made balanced
#'     based on the index variables. The returned data are sorted as a
#'     stacked time series.
#' @export
#' @author Kevin Tappe
#' @seealso [is.pbalanced()] to check if data are balanced;
#'     [is.pconsecutive()] to check if data are consecutive;
#'     [make.pconsecutive()] to make data consecutive (and,
#'     optionally, also balanced).\cr [punbalancedness()]
#'     for two measures of unbalancedness, [pdim()] to check
#'     the dimensions of a 'pdata.frame' (and other objects),
#'     [pvar()] to check for individual and time variation
#'     of a 'pdata.frame' (and other objects), [lag()] for
#'     lagging (and leading) values of a 'pseries' object.\cr
#'     [pseries()], [data.frame()],
#'     [pdata.frame()].
#' @keywords attribute
#' @examples
#' 
#' # take data and make it unbalanced
#' # by deletion of 2nd row (2nd time period for first individual)
#' data("Grunfeld", package = "plm")
#' nrow(Grunfeld)                            # 200 rows
#' Grunfeld_missing_period <- Grunfeld[-2, ]
#' pdim(Grunfeld_missing_period)$balanced    # check if balanced: FALSE
#' make.pbalanced(Grunfeld_missing_period)   # make it balanced (by filling)
#' make.pbalanced(Grunfeld_missing_period, balance.type = "shared.times") # (shared periods)
#' nrow(make.pbalanced(Grunfeld_missing_period))
#' nrow(make.pbalanced(Grunfeld_missing_period, balance.type = "shared.times"))
#' 
#' # more complex data:
#' # First, make data unbalanced (and non-consecutive) 
#' # by deletion of 2nd time period (year 1936) for all individuals
#' # and more time periods for first individual only
#' Grunfeld_unbalanced <- Grunfeld[Grunfeld$year != 1936, ]
#' Grunfeld_unbalanced <- Grunfeld_unbalanced[-c(1,4), ]
#' pdim(Grunfeld_unbalanced)$balanced        # FALSE
#' all(is.pconsecutive(Grunfeld_unbalanced)) # FALSE
#' 
#' g_bal <- make.pbalanced(Grunfeld_unbalanced)
#' pdim(g_bal)$balanced        # TRUE
#' unique(g_bal$year)          # all years but 1936
#' nrow(g_bal)                 # 190 rows
#' head(g_bal)                 # 1st individual: years 1935, 1939 are NA
#' 
#' # NA in 1st, 3rd time period (years 1935, 1937) for first individual
#' Grunfeld_NA <- Grunfeld
#' Grunfeld_NA[c(1, 3), "year"] <- NA
#' g_bal_NA <- make.pbalanced(Grunfeld_NA)
#' head(g_bal_NA)        # years 1935, 1937: NA for non-index vars
#' nrow(g_bal_NA)        # 200
#' 
#' # pdata.frame interface
#' pGrunfeld_missing_period <- pdata.frame(Grunfeld_missing_period)
#' make.pbalanced(Grunfeld_missing_period)
#' 
#' # pseries interface
#' make.pbalanced(pGrunfeld_missing_period$inv)
#' 
#' # comparison to make.pconsecutive
#' g_consec <- make.pconsecutive(Grunfeld_unbalanced)
#' all(is.pconsecutive(g_consec)) # TRUE
#' pdim(g_consec)$balanced        # FALSE
#' head(g_consec, 22)             # 1st individual:   no years 1935/6; 1939 is NA; 
#'                                # other indviduals: years 1935-1954, 1936 is NA
#' nrow(g_consec)                 # 198 rows
#' 
#' g_consec_bal <- make.pconsecutive(Grunfeld_unbalanced, balanced = TRUE)
#' all(is.pconsecutive(g_consec_bal)) # TRUE
#' pdim(g_consec_bal)$balanced        # TRUE
#' head(g_consec_bal)                 # year 1936 is NA for all individuals
#' nrow(g_consec_bal)                 # 200 rows
#' 
#' head(g_bal)                        # no year 1936 at all
#' nrow(g_bal)                        # 190 rows
#' 
make.pbalanced <- function(x, balance.type = c("fill", "shared.times", "shared.individuals"), ...) {
  UseMethod("make.pbalanced")
}


#' @rdname make.pbalanced
#' @export
make.pbalanced.pdata.frame <- function(x, balance.type = c("fill", "shared.times", "shared.individuals"), ...) {

  balance.type <- match.arg(balance.type)
  index <- attr(x, "index")
  
  switch(balance.type,
         "fill" = {
            x_consec_bal <- make.pconsecutive(x, balanced = TRUE)
            
            # delete time periods that were not present for any individual, but introduced by
            # making data consecutive
            # result: no time periods are added that are not present for at least one individual
            times_present_orig <- attr(x_consec_bal, "index")[[2L]] %in% unique(index[[2L]])
            result <- x_consec_bal[times_present_orig, ]
            
            # drop not present factor levels (some new levels were introduced by making data consecutive first):
              # drop from index
              index_result <- attr(result, "index")
              index_result[[2L]] <- droplevels(index_result[[2L]])
              attr(result, "index") <- index_result
              
              # drop from time column (if time index column present in pdata.frame)
              pos_indexvars <- pos.index(result) # position of index vars is c(NA, NA) if index vars are not present as columns
              index_orig_names <- names(pos_indexvars)
              if (!anyNA(pos_indexvars)) {
                result[ , pos_indexvars[2L]] <- droplevels(result[ , pos_indexvars[2L]])
              }
        },
        "shared.times" = {
            keep <- intersect_index(index, "time")
            result <- x[keep, ]
        },
        "shared.individuals" = {
            keep <- intersect_index(index, "individual")
            result <- x[keep, ]
        })
  return(result)
} ## END make.pbalanced.pdata.frame


#' @rdname make.pbalanced
#' @export
make.pbalanced.pseries <- function(x, balance.type = c("fill", "shared.times", "shared.individuals"), ...) {

  balance.type <- match.arg(balance.type)
  index <- attr(x, "index")

  switch(balance.type,
         "fill" = {
            x_consec_bal <- make.pconsecutive(x, balanced = TRUE)
            
            # delete time periods that were not present for any individual, but introduced by
            # making data consecutive
            # result: no time periods are added that are not present for at least one individual
              x_consec_bal_index <- attr(x_consec_bal, "index")
              times_present_orig <- x_consec_bal_index[[2L]] %in% unique(index[[2L]])
              result <- x_consec_bal[times_present_orig] # this drops the pseries features (index, class "pseries")
                                                         # because there is no function "[.pseries]" (as of 2016-05-14)
              
              # drop introduced extra periods also from index
              x_consec_bal_index <- x_consec_bal_index[times_present_orig, ]
            # re-attach index and restore original class(es)
            attr(result, "index") <- x_consec_bal_index
            attr(result, "class") <- attr(x, "class")
            },
         
         "shared.times" = {
           keep <- intersect_index(index, "time")
           result <- x[keep]
           # restore 'pseries' features
           # (no subsetting method for pseries in the package (yet),
           #  usual vector subsetting removes the pseries features)
           attr(result, "index") <- index[keep, ]
           class(result) <- unique(c("pseries", class(result)))
           },
         
         "shared.individuals" = {
           keep <- intersect_index(index, "individual")
           result <- x[keep]
           # restore 'pseries' features
           # (no subsetting method for pseries in the package (yet),
           #  usual vector subsetting removes the pseries features)
           attr(result, "index") <- index[keep, ]
           class(result) <- unique(c("pseries", class(result)))
         })
  return(result)
} ## END make.pbalanced.pseries


#' @rdname make.pbalanced
#' @export
make.pbalanced.data.frame <- function(x, balance.type = c("fill", "shared.times", "shared.individuals"), index = NULL, ...) {
  # NB: for data.frame interface: the data is also sorted as stack time series

  balance.type <- match.arg(balance.type)

  ## identify index of data.frame  
      # if not NULL, index is must be character of length 2
      if (!is.null(index) && length(index) != 2L)
        stop("if argument 'index' is not NULL, 'index' needs to specify
             'individual' and 'time' dimension for make.pconsecutive to work on a data.frame")
      
      # assume first two columns to be the index vars
      if (is.null(index)) index_orig_names <- names(x)[1:2]
        else index_orig_names <- index
      
      index_df <- x[ , index_orig_names]
  
  switch(balance.type,
         "fill" = {
            x_consec_bal <- make.pconsecutive(x, index = index_orig_names, balanced = TRUE)
            
            # delete time periods that were not present for any individual, but introduced by
            # making data consecutive
            # result: no time periods are added that are not present for at least one individual
            times_present_orig <- x_consec_bal[ , index_orig_names[2L]] %in% unique(index_df[[2L]])
            result <- x_consec_bal[times_present_orig , ]},
         
        "shared.times" = {
           keep <- intersect_index(index_df, "time")
           result <- x[keep, ]},
        
        "shared.individuals" = {
           keep <- intersect_index(index_df, "individual")
           result <- x[keep, ]
         })
  return(result)
} ## END make.pbalanced.data.frame



# helper function: returns logical vector which rows/entries to keep
#                  when balance.type = "shared.times" or "shared.individuals"
#                  (intersect of all time periods or individuals)
intersect_index <- function(index, by) {
  # intersect() is defined on vectors (not factors)
  #  -> convert respective index to character before
  unclass(index) # unclass for speed
  switch(by,
         "time" = {
           id <- index[[1L]]
           time <- as.character(index[[2L]])
         },
         "individual" = {
           id <- index[[2L]]
           time <- as.character(index[[1L]])
         })
  
  times_by_ids <- collapse::gsplit(time, id) # was: times_by_ids <- split(time, id)
  common_times <- Reduce(intersect, times_by_ids) 
  keep_entries <- time %in% common_times
  return(keep_entries)
}

