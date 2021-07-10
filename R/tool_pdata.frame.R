## pdata.frame and pseries are adaptations of respectively data.frame
## and vector for panel data. An index attribute is added to both,
## which is a data.frame containing the indexes. There is no pseries
## function, it is the class of series extracted from a
## pdata.frame. index and pdim functions are used to extract
## respectively the data.frame containing the index and the dimensions
## of the panel

## pdata.frame:
## - $<-
## - [
## - $
## - [[
## - print
## - as.list
## - as.data.frame

## pseries:
## - print
## - as.matrix
## - plot
## - summary
## - plot.summary
## - print.summary
## - is.pseries

## pdim:
## - pdim.default
## - pdim.data.frame
## - pdim.pdata.frame
## - pdim.pseries
## - pdim.panelmodel
## - pdim.pgmm
## - print.pdim
 
## index:
## - index.pindex
## - index.pdata.frame
## - index.pseries
## - index.panelmodel
## - is.index (non-exported)
## - has.index (non-exported)
## - checkNA.index (non-exported)
## - pos.index (non-exported)

fancy.row.names <- function(index, sep = "-") {
  if (length(index) == 2L) {result <- paste(index[[1L]], index[[2L]], sep = sep)}
  # this in the order also used for sorting (group, id, time):
  if (length(index) == 3L) {result <- paste(index[[3L]], index[[1L]], index[[2L]], sep = sep)}
  return(result)
}




#' data.frame for panel data
#' 
#' An object of class 'pdata.frame' is a data.frame with an index
#' attribute that describes its individual and time dimensions.
#' 
#' The `index` argument indicates the dimensions of the panel. It can
#' be: \itemize{
#' \item a vector of two character strings which
#' contains the names of the individual and of the time indexes,
#' \item
#' a character string which is the name of the individual index
#' variable. In this case, the time index is created automatically and
#' a new variable called "time" is added, assuming consecutive and
#' ascending time periods in the order of the original data,
#' \item an integer, the number of individuals. In this case, the data
#' need to be a balanced panel and be organized as a stacked time series
#' (successive blocks of individuals, each block being a time series
#' for the respective individual) assuming consecutive and ascending
#' time periods in the order of the original data. Two new variables
#' are added: "id" and "time" which contain the individual and the
#' time indexes.
#' }
#' 
#' The `"[["` and `"$"` extract a series from the `pdata.frame`.  The
#' `"index"` attribute is then added to the series and a class
#' attribute `"pseries"` is added. The `"["` method behaves as for
#' `data.frame`, except that the extraction is also applied to the
#' `index` attribute.  A safe way to extract the index attribute is to
#' use the function [index()] for 'pdata.frames' (and other objects).
#' 
#' `as.data.frame` removes the index attribute from the `pdata.frame`
#' and adds it to each column. For its argument `row.names` set to 
#' `FALSE` row names are an integer series, `TRUE` gives "fancy" row
#' names; if a character (with length of the resulting data frame),
#' the row names will be the character's elements.
#' 
#' `as.list` behaves by default identical to
#' [base::as.list.data.frame()] which means it drops the
#' attributes specific to a pdata.frame; if a list of pseries is
#' wanted, the attribute `keep.attributes` can to be set to
#' `TRUE`. This also makes `lapply` work as expected on a pdata.frame
#' (see also **Examples**).
#' 
#' @param x a `data.frame` for the `pdata.frame` function and a
#'     `pdata.frame` for the methods,
#' @param i see [Extract()],
#' @param j see [Extract()],
#' @param y one of the columns of the `data.frame`,
#' @param index this argument indicates the individual and time
#'     indexes. See **Details**,
#' @param drop see [Extract()],
#' @param drop.index logical, indicates whether the indexes are to be
#'     excluded from the resulting pdata.frame,
#' @param optional see [as.data.frame()],
#' @param row.names `NULL` or logical, indicates whether "fancy" row
#'     names (combination of individual index and time index) are to
#'     be added to the returned (p)data.frame (`NULL` and `FALSE` have
#'     the same meaning for `pdata.frame`; for
#'     `as.data.frame.pdata.frame` see Details),
#' @param stringsAsFactors logical, indicating whether character
#'     vectors are to be converted to factors,
#' @param replace.non.finite logical, indicating whether values for
#'     which `is.finite()` yields `TRUE` are to be replaced by `NA`
#'     values, except for character variables (defaults to `FALSE`),
#' @param drop.NA.series logical, indicating whether all-NA columns
#'     are to be removed from the pdata.frame (defaults to `FALSE`),
#' @param drop.const.series logical, indicating whether constant
#'     columns are to be removed from the pdata.frame (defaults to
#'     `FALSE`),
#' @param drop.unused.levels logical, indicating whether unused levels
#'     of factors are to be dropped (defaults to `FALSE`) (unused
#'     levels are always dropped from variables serving to construct
#'     the index variables),
#' @param keep.attributes logical, only for as.list and as.data.frame
#'     methods, indicating whether the elements of the returned
#'     list/columns of the data.frame should have the pdata.frame's
#'     attributes added (default: FALSE for as.list, TRUE for
#'     as.data.frame),
#' @param name the name of the `data.frame`,
#' @param value the name of the variable to include,
#' @param \dots further arguments.
#' @return a `pdata.frame` object: this is a `data.frame` with an
#'     `index` attribute which is a `data.frame` with two variables,
#'     the individual and the time indexes, both being factors.  The
#'     resulting pdata.frame is sorted by the individual index, then
#'     by the time index.
#' @export
#' @author Yves Croissant
#' @seealso [index()] to extract the index variables from a
#'     'pdata.frame' (and other objects), [pdim()] to check the
#'     dimensions of a 'pdata.frame' (and other objects), [pvar()] to
#'     check for each variable if it varies cross-sectionally and over
#'     time.  To check if the time periods are consecutive per
#'     individual, see [is.pconsecutive()].
#' @keywords classes
#' @examples
#' 
#' # Gasoline contains two variables which are individual and time
#' # indexes
#' data("Gasoline", package = "plm")
#' Gas <- pdata.frame(Gasoline, index = c("country", "year"), drop.index = TRUE)
#' 
#' # Hedonic is an unbalanced panel, townid is the individual index
#' data("Hedonic", package = "plm")
#' Hed <- pdata.frame(Hedonic, index = "townid", row.names = FALSE)
#' 
#' # In case of balanced panel, it is sufficient to give number of
#' # individuals data set 'Wages' is organized as a stacked time
#' # series
#' data("Wages", package = "plm")
#' Wag <- pdata.frame(Wages, 595)
#' 
#' # lapply on a pdata.frame by making it a list of pseries first
#' lapply(as.list(Wag[ , c("ed", "lwage")], keep.attributes = TRUE), lag)
#' 
#' 
pdata.frame <- function(x, index = NULL, drop.index = FALSE, row.names = TRUE,
                        stringsAsFactors = default.stringsAsFactors(),
                        replace.non.finite = FALSE,
                        drop.NA.series = FALSE, drop.const.series = FALSE,
                        drop.unused.levels = FALSE) {

    if (inherits(x, "pdata.frame")) stop("already a pdata.frame")
  
    if (length(index) > 3L){
        stop("'index' can be of length 3 at the most (one index variable for individual, time, group)")
    }
    
    # prune input: x is supposed to be a plain data.frame. Other classes building
    # on top of R's data frame can inject attributes etc. that confuse functions
    # in pkg plm.
    x <- data.frame(x)
    
    # if requested: coerce character vectors to factors
    if (stringsAsFactors) {
        x.char <- names(x)[sapply(x, is.character)]
        for (i in x.char){
            x[[i]] <- factor(x[[i]])
        }
    }
  
    # if requested: replace Inf, -Inf, NaN (everything for which is.finite is FALSE) by NA
    # (for all but any character columns [relevant if stringAsFactors == FALSE])
    if (replace.non.finite) {
      for (i in names(x)) {
        if (!inherits(x[[i]], "character")) {
          x[[i]][!is.finite(x[[i]])] <- NA
        }
      }
    }
  
    # if requested: check and remove complete NA series
    if (drop.NA.series) {
      na.check <- sapply(x, function(x) sum(!is.na(x)) == 0L)
      na.serie <- names(x)[na.check]
      if (length(na.serie) > 0L){
        if (length(na.serie) == 1L)
          cat(paste0("This series is NA and has been removed: ", na.serie, "\n"))
        else
          cat(paste0("These series are NA and have been removed: ", paste(na.serie, collapse = ", "), "\n"))
      }
      x <- x[ , !na.check]
    }

    # if requested: check for constant series and remove
    if (drop.const.series) {
      # -> var() and sd() on factors is deprecated as of R 3.2.3 -> use duplicated()
      cst.check <- sapply(x, function(x) {
                              if (is.factor(x) || is.character(x)) {
                                all(duplicated(x[!is.na(x)])[-1L])
                              } else {
                                x[! is.finite(x)] <- NA # infinite elements set to NA only for this check
                                var(as.numeric(x), na.rm = TRUE) == 0
                              }
                            })
      
      # following line: bug fixed thanks to Marciej Szelfer
      cst.check <- cst.check | is.na(cst.check)
      cst.serie <- names(x)[cst.check]
      if (length(cst.serie) > 0L){
        if (length(cst.serie) == 1L){
          cat(paste0("This series is constant and has been removed: ", cst.serie, "\n"))
        }
        else{
            cat(paste0("These series are constants and have been removed: ",
                       paste(cst.serie, collapse = ", "), "\n"))
        }
      }
      x <- x[ , !cst.check]
    }
  
    # sanity check for 'index' argument. First, check the presence of a
    # grouping variable, this should be the third element of the index
    # vector or any "group" named element of this vector
    group.name <- NULL
    if (! is.null(names(index)) || length(index == 3L)){
        if (! is.null(names(index))){
            grouppos <- match("group", names(index))
            if (! is.na(grouppos)){
                group.name <- index[grouppos]
                index <- index[- grouppos]
            }
        }
        if (length(index) == 3L){
            group.name <- index[3L]
            index <- index[-3L]
        }
    }
    if (length(index) == 0L) index <- NULL

    # if index is NULL, both id and time are NULL
    if (is.null(index)){
        id <- NULL
        time <- NULL
    }
    # if the length of index is 1, id = index and time is NULL
    if (length(index) == 1L){
        id <- index
        time <- NULL
    }
    # if the length of index is 2, the first element is id, the second
    # is time
    if (length(index) == 2L){
        id <- index[1L]
        time <- index[2L]
    }
    # if both id and time are NULL, the names of the index are the first
    # two names of x
    if (is.null(id) && is.null(time)){
        id.name <- names(x)[1L]
        time.name <- names(x)[2L]
    }
    else{
        id.name <- id
        time.name <- time
    }
    
    # if index is numeric, this indicats a balanced panel with no. of
    # individuals equal to id.name
    if(is.numeric(id.name)){
        if(!is.null(time.name))
            warning("The time index (second element of 'index' argument) will be ignored\n")
        N <- nrow(x)
        if( (N %% id.name) != 0){
            stop(paste0("unbalanced panel, in this case the individual index may not be indicated by an integer\n",
                        "but by specifying a column of the data.frame in the first element of the 'index' argument\n"))
        }
        else{
            T <- N %/% id.name
            n <- N %/% T
            time <- rep((1:T), n)
            id <- rep((1:n), rep(T, n))
            id.name <- "id"
            time.name <- "time"
            if (id.name %in% names(x)) warning(paste0("column '", id.name, "' overwritten by id index"))
            if (time.name %in% names(x)) warning(paste0("column '", time.name, "' overwritten by time index"))
            x[[id.name]] <- id <- as.factor(id)
            x[[time.name]] <- time <- as.factor(time)
        }
    }
    else{
        # id.name is not numeric, i.e., individual index is supplied
        if (!id.name %in% names(x)) stop(paste("variable ", id.name, " does not exist (individual index)", sep=""))
        if (is.factor(x[[id.name]])){
            id <- x[[id.name]] <- x[[id.name]][drop = TRUE] # drops unused levels of factor
        }
        else{
            id <- x[[id.name]] <- as.factor(x[[id.name]])
        }
        
        if (is.null(time.name)){
            # if no time index is supplied, add time variable
            # automatically order data by individual index, necessary
            # for the automatic addition of time index to be
            # successful if no time index was supplied
            x <- x[order(x[[id.name]]), ]
            Ti <- table(x[[id.name]]) # was: Ti <- table(id)
            n <- length(Ti)
            time <- c()
            for (i in 1:n){
                time <- c(time, 1:Ti[i])
            }
            time.name <- "time"
            if (time.name %in% names(x))
                warning(paste0("column '", time.name, "' overwritten by time index"))
            time <- x[[time.name]] <- as.factor(time)
        }
        else{
            # use supplied time index
            if (!time.name %in% names(x))
                stop(paste0("variable ", time.name, " does not exist (time index)"))
            
            if (is.factor(x[[time.name]])){
                time <- x[[time.name]] <- x[[time.name]][drop = TRUE]
            }
            else{
                time <- x[[time.name]] <- as.factor(x[[time.name]])
            }
        }
    }
    
    # if present, make group variable a factor (just like for id and
    # time variables)
    if (!is.null(group.name)) {
        if (is.factor(x[[group.name]])){
            group <- x[[group.name]] <- x[[group.name]][drop = TRUE]
        }
        else{
            group <- x[[group.name]] <- as.factor(x[[group.name]])
        }
    }
    
    # sort by group (if given), then by id, then by time
    if (! is.null(group.name)) x <- x[order(x[[group.name]], x[[id.name]], x[[time.name]]), ]
    else x <- x[order(x[[id.name]], x[[time.name]]), ]

    # if requested: drop unused levels from factor variables (spare
    # those serving for the index as their unused levels are dropped
    # already (at least in the attribute "index" they need to be
    # dropped b/c much code relies on it))
    if (drop.unused.levels) {
        var.names <- setdiff(names(x), c(id.name, time.name, group.name))
        for (i in var.names){
            if (is.factor(x[[i]])){
                x[[i]] <- droplevels(x[[i]])
            }
        }
    }
    
    posindex <- match(c(id.name, time.name, group.name), names(x))
    index <- x[ , posindex]
    if (drop.index) {
        x <- x[ , -posindex, drop = FALSE]
        if (ncol(x) == 0L) warning("after dropping of index variables, the pdata.frame contains 0 columns")
    }
    
    ### warn if duplicate couples
    test_doub <- table(index[[1L]], index[[2L]], useNA = "ifany")
    if (any(as.vector(test_doub[!is.na(rownames(test_doub)), !is.na(colnames(test_doub))]) > 1L))
      warning(paste("duplicate couples (id-time) in resulting pdata.frame\n to find out which,",
                    "use, e.g., table(index(your_pdataframe), useNA = \"ifany\")"))
    
    ### warn if NAs in index as likely not sane
    if (anyNA(index[[1L]]) || anyNA(index[[2L]]) || (if(ncol(index) == 3L) anyNA(index[[3L]]) else FALSE))
        warning(paste0("at least one NA in at least one index dimension ",
                       "in resulting pdata.frame\n to find out which, use, e.g., ",
                       "table(index(your_pdataframe), useNA = \"ifany\")\n"))
    
    ### Could also remove rows with NA in any index' dimension
    # drop.rows <- is.na(index[[1L]]) | is.na(index[[2L]])
    # if(ncol(index) == 3L) drop.rows <- drop.rows | is.na(index[[3L]])
    # if((no.drop.rows <- sum(drop.rows)) > 0L) {
    #   x <- x[!drop.rows, ]
    #   index <- index[!drop.rows, ]
    #   txt.drop.rows <- paste0(no.drop.rows, " row(s) dropped in resulting pdata.frame due to NA(s) in at least one index dimension")
    #   warning(txt.drop.rows)
    # }
    
    if (row.names) {
        attr(x, "row.names") <- fancy.row.names(index)
        # NB: attr(x, "row.names") allows for duplicate rownames (as
        # opposed to row.names(x) <- something)
        # NB: no fancy row.names for index attribute (!?):
        # maybe because so it is possible to restore original row.names?
    }
    
    class(index) <- c("pindex", "data.frame")
    attr(x, "index") <- index
    class(x) <- c("pdata.frame", "data.frame")
    
    return(x)
}

#' @rdname pdata.frame
#' @export
"$<-.pdata.frame" <- function(x, name, value) {
  if (inherits(value, "pseries")){
    # remove pseries features before adding value as a column to pdata.frame
    if (length(class(value)) == 1L) value <- unclass(value)
    else attr(value, "class") <- setdiff(class(value), "pseries")
    attr(value, "index") <- NULL
  }
  "$<-.data.frame"(x, name, value)
}

# NB: We don't have methods for [<-.pdata.frame and [[<-.pdata.frame, so these functions
#     dispatch to the respective data.frame methods which assign whatever is
#     handed over to the methods. Especially, if a pseries is handed over, this
#     results in really assigning a pseries to the pdata.frame in case of usage of
#     [<- and [[<-. This is inconsistent because the columns of a pdata.frame do not
#     have the 'pseries' features.
#     This can be seen by lapply(some_pdata.frame, class) after 
#     assigning with the respective .data.frame methods


# Extracting/subsetting function for class pseries, retaining the pseries features
#  Test cases are in tests/test_pdata.frame_subsetting.R 
#' @rdname pdata.frame
#' @export
"[.pseries" <- function(x, ...) {

 ## use '...' instead of only one specific argument, because subsetting for
 ## factors can have argument 'drop', e.g., x[i, drop=TRUE] see ?Extract.factor
  index <- attr(x, "index")
  if (is.null(index)) warning("pseries object with is.null(index(pseries)) == TRUE encountered")
  if (!is.null(index) && !is.index(index)) stop(paste0("pseries object has illegal index with class(index) == ", paste0(class(index), collapse = ", ")))
  names_orig <- names(x)
  keep_rownr <- seq_along(x) # full length row numbers original pseries
  names(keep_rownr) <- names_orig

  if (is.null(names_orig)) {
    names(x) <- keep_rownr # if no names are present, set names as integer sequence to identify rows to keep later
    names(keep_rownr) <- keep_rownr
  }
  x <- remove_pseries_features(x)
  result <- x[...] # actual subsetting

  # identify rows to keep in the index:
  keep_rownr <- keep_rownr[names(result)] # row numbers to keep after subsetting
  if (!is.null(names_orig)) names(result) <- names_orig[keep_rownr] else names(result) <- NULL # restore and subset original names if any

  # Subset index accordingly:
  # Check if index is null is a workaround for R's data frame subsetting not
  # stripping class pseries but its attributes for factor (for other data types, pseries class is dropped)
  # see https://bugs.r-project.org/bugzilla/show_bug.cgi?id=18140
  if (!is.null(index)) {
    index <- index[keep_rownr, ]
    index <- droplevels(index) # drop unused levels (like in subsetting of pdata.frames)
  }

  result <- add_pseries_features(result, index)
  return(result)
}

## Non-exported internal function for subsetting of pseries. Can be used
## internally. 
## Currently we do not have "proper" subsetting function for pseries
## ([.pseries) which applies in any case (but see the sketch to be tested in
##  tool_pdata.frame.R)
subset_pseries <- function(x, ...) {
  
  ## use '...' instead of only one specific argument, because subsetting for
  ## factors can have argument 'drop', e.g., x[i, drop=TRUE] see ?Extract.factor
  index <- attr(x, "index")
  if (is.null(index)) warning("pseries object with is.null(index(pseries)) == TRUE encountered")
  if (!is.null(index) && !is.index(index)) stop(paste0("pseries object has illegal index with class(index) == ", paste0(class(index), collapse = ", ")))
  names_orig <- names(x)
  keep_rownr <- seq_along(x) # full length row numbers original pseries
  names(keep_rownr) <- names_orig
  
  if (is.null(names_orig)) {
    names(x) <- keep_rownr # if no names are present, set names as integer sequence to identify rows to keep later
    names(keep_rownr) <- keep_rownr
  }
  x <- remove_pseries_features(x)
  result <- x[...] # actual subsetting
  
  # identify rows to keep in the index:
  keep_rownr <- keep_rownr[names(result)] # row numbers to keep after subsetting
  if (!is.null(names_orig)) names(result) <- names_orig[keep_rownr] else names(result) <- NULL # restore and subset original names if any
  
  # Subset index accordingly:
  # Check if index is null is a workaround for R's data frame subsetting not
  # stripping class pseries but its attributes for factor (for other data types, pseries class is dropped)
  # see https://bugs.r-project.org/bugzilla/show_bug.cgi?id=18140
  if (!is.null(index)) {
    index <- index[keep_rownr, ]
    index <- droplevels(index) # drop unused levels (like in subsetting of pdata.frames)
  }
  
  result <- add_pseries_features(result, index)
  return(result)
}


#' @rdname pdata.frame
#' @export
"[.pdata.frame" <- function(x, i, j, drop) {
    # signature of [.data.frame here
  
    missing.i    <- missing(i)    # missing is only guaranteed to yield correct results,
    missing.j    <- missing(j)    # if its argument was not modified before accessing it
    missing.drop <- missing(drop) # -> save information about missingness
    sc <- sys.call()
    # Nargs_mod to distinguish if called by [] (Nargs_mod == 2L); [,] (Nargs_mod == 3L); [,,] (Nargs_mod == 4L)
    Nargs_mod <- nargs() - (!missing.drop)
  
    ### subset index (and row names) appropriately:
    # subsetting data.frame by only j (x[ , j]) or missing j (x[i]) yields full-row
    # column(s) of data.frame, thus do not subset the index because it needs full rows (original index)
    #
    # subset index if:
    #      * [i,j] (supplied i AND supplied j) (in this case: Nargs_mod == 3L (or 4L depending on present/missing drop))
    #      * [i, ] (supplied i AND missing j)  (in this case: Nargs_mod == 3L (or 4L depending on present/missing drop))
    #
    # do not subset index in all other cases (here are the values of Nargs_mod)
    #      * [ ,j] (missing  i AND j supplied)                   (Nargs_mod == 3L (or 4L depending on present/missing drop))
    #      * [i]   (supplied i AND missing j)                    (Nargs_mod == 2L) [Nargs_mod distinguishes this case from the one where subsetting is needed!]
    #      * [i, drop = TRUE/FALSE] (supplied i AND missing j)   (Nargs_mod == 2L)
    #
    # => subset index (and row names) if: supplied i && Nargs_mod >= 3L
    
    index <- attr(x, "index")
    x.rownames <- row.names(x)
    if (!missing.i && Nargs_mod >= 3L) {
      iindex <- i
      if (is.character(iindex)) {
        # Kevin Tappe 2016-01-04 : in case of indexing (subsetting) a 
        # pdata.frame by a character, the subsetting vector should be 
        # converted to numeric by matching to the row names so that the 
        # index can be correctly subsetted (by this numeric value).
        # Motivation:
        # Row names of the pdata.frame and row names of the pdata.frame's 
        # index are not guaranteed to be the same!
        iindex <- match(iindex, rownames(x))
      }
      # subset index and row names
      index <- "[.data.frame"(index, iindex, )
      x.rownames <- x.rownames[iindex]
      
      # remove empty levels in index (if any)
      # NB: really do dropping of unused levels? Standard R behaviour is to leave the levels and not drop unused levels
      #     Maybe the dropping is needed for functions like lag.pseries/lagt.pseries to work correctly?
      index <- droplevels(index)
      # NB: use droplevels() rather than x[drop = TRUE] as x[drop = TRUE] can also coerce mode!
      # old (up to rev. 251): index <- data.frame(lapply(index, function(x) x[drop = TRUE]))
    }
    
    ### end of subsetting index
    
    # delete attribute with old index first:
    # this preserves the order of the attributes because 
    # order of non-standard attributes is scrambled by R's data.frame subsetting with `[.`
    # (need to add new index later anyway)
    attr(x, "index") <- NULL
    
    # Set class to "data.frame" first to avoid coercing which enlarges the (p)data.frame 
    # (probably by as.data.frame.pdata.frame).
    # Coercing is the built-in behaviour for extraction from data.frames by "[." (see ?`[.data.frame`) 
    # and it seems this cannot be avoided; thus we need to make sure, not to have any coercing going on
    # which adds extra data (such as as.matrix.pseries, as.data.frame.pdata.frame) by setting the class 
    # to "data.frame" first
    class(x) <- "data.frame"

    # call [.data.frame exactly as [.pdata.frame was called but arg is now 'x'
    # this is necessary because there could be several missing arguments
    # use sys.call (and not match.call) because arguments other than drop may not be named
    # need to evaluate i, j, drop, if supplied, before passing on (do not pass on as the sys.call caught originally)
    sc_mod <- sc
    sc_mod[[1L]] <- quote(`[.data.frame`)
    sc_mod[[2L]] <- quote(x)
    
    if (!missing.i) sc_mod[[3L]] <- i # if present, i is always in pos 3
    if (!missing.j) sc_mod[[4L]] <- j # if present, j is always in pos 4
    if (!missing.drop) sc_mod[[length(sc)]] <- drop # if present, drop is always in last position (4 or 5,
                                                    # depending on the call structure and whether missing j or not)
    
    mydata <- eval(sc_mod)

    if (is.null(dim(mydata))) {
      # if dim is NULL, subsetting did not return a data frame but  a vector or a
      #   factor or NULL (nothing more is left)
      if (is.null(mydata)) {
        # since R 3.4.0 NULL cannot have attributes, so special case it
        res <- NULL
      } else {
        # vector or factor -> make it a pseries
        res <- structure(mydata,
                         names = x.rownames,
                         index = index,
                         class = base::union("pseries", class(mydata)))
      }
    } else {
          # subsetting returned a data.frame -> add attributes to make it a pdata.frame again
          res <- structure(mydata,
                           index = index,
                           class = c("pdata.frame", "data.frame"))
    }
  
    return(res)
}

#' @rdname pdata.frame
#' @export
"[[.pdata.frame" <- function(x, y) {
  index <- attr(x, "index")
  attr(x, "index") <- NULL
  class(x) <- "data.frame"
  result <- "[[.data.frame"(x, y)
  if (!is.null(result)){
    # make extracted column a pseries
    # use this order for attributes to preserve original order of attributes for a pseries
    result <- structure(result,
                        names = row.names(x),
                        class = base::union("pseries", class(result)),
                        index = index 
                        )
  }
  result
}

#' @rdname pdata.frame
#' @export
"$.pdata.frame" <- function(x, y) {
    "[[.pdata.frame"(x, paste(as.name(y)))
}

#' @rdname pdata.frame
#' @export
print.pdata.frame <- function(x, ...) {
  attr(x, "index") <- NULL
  class(x) <- "data.frame"
  # This is a workaround: print.data.frame cannot handle
  # duplicated row names which are currently possible for pdata frames
  if (anyDuplicated(rownames(x))) {
      print("Note: pdata.frame contains duplicated row names, thus original row names are not printed")
      rownames(x) <- NULL 
  }
  print(x, ...)
}

# as.list.pdata.frame:
# The default is to behave identical to as.list.data.frame.
# This default is necessary, because some code relies on this 
# behaviour! Do not change this!
#
#  as.list.data.frame does:
#    * unclass
#    * strips all classes but "list"
#    * strips row.names
#
#  By setting argument keep.attributes = TRUE, the attributes of the pdata.frame
#  are preserved by as.list.pdata.frame: a list of pseries is returned
#  and lapply can be used as usual, now working on a list of pseries, e.g.,
#    lapply(as.list(pdata.frame[ , your_cols], keep.attributes), lag)
#  works as expected.

#' @rdname pdata.frame
#' @export
as.list.pdata.frame <- function(x, keep.attributes = FALSE, ...) {
    if (!keep.attributes) {
        x <- as.list.data.frame(x)
    } else {
        # make list of pseries objects
        x_names <- names(x)
        x <- lapply(x_names,
                    FUN = function(element, pdataframe){
                        "[[.pdata.frame"(x = pdataframe, y = element)
                    },
                    pdataframe = x)
        names(x) <- x_names
        
    # note: this function is slower than the corresponding
    # as.list.data.frame function,
    # because we cannot simply use unclass() on the pdata.frame:
    # need to add index etc to all columns to get proper pseries
    # back => thus the extraction function "[[.pdata.frame" is used
    }
    return(x)
}

#' @rdname pdata.frame
#' @export
as.data.frame.pdata.frame <- function(x, row.names = NULL, optional = FALSE, keep.attributes = TRUE, ...) {
    index <- attr(x, "index")

    if(!keep.attributes) {
      attr(x, "index") <- NULL
      class(x) <- "data.frame"
      rownames(x) <- NULL
    } else {
      # make each column a pseries (w/o names)
      x <- lapply(x,
                  function(z){
                  #     names(z) <- row.names(x) # it is not possible to keep the names in the 'pseries'/
                                                 # in columns because the call to data.frame later deletes
                                                 # the names attribute of columns (definition of data frame)
                    attr(z, "index") <- index
                    class(z) <- base::union("pseries", class(z))
                    return(z)
                  })
    }
    
    if(is.null(row.names)) {
      # do as base::as.data.frame does for NULL
      x <- as.data.frame(x, row.names = NULL)
    } else {
      if(is.logical(row.names) && row.names == FALSE) {
        # set row names to integer sequence 1, 2, 3, ...
        x <- as.data.frame(x)
        row.names(x) <- NULL
      }
      if(is.logical(row.names) && row.names == TRUE) {
        # set fancy row names
        x <- as.data.frame(x)
        row.names(x) <- fancy.row.names(index)
      }
      if(is.character(row.names)) {
        x <- as.data.frame(x)
        row.names(x) <- row.names
      }
      if(!(isTRUE(row.names) || isFALSE(row.names) || is.character(row.names)))
        stop("argument 'row.names' is none of NULL, FALSE, TRUE, and not a character")
      # using row.names(x) <- "something" is safer (does not allow
      # duplicate row.names) than # attr(x,"row.names") <- "something"
    }
    return(x)
}


#' Check if an object is a pseries
#' 
#' This function checks if an object qualifies as a pseries
#' 
#' A `"pseries"` is a wrapper around a "basic class" (numeric, factor,
#' logical, character, or complex).
#' 
#' To qualify as a pseries, an object needs to have the following
#' features:
#'
#' - class contains `"pseries"` and there are at least two classes
#' (`"pseries"` and the basic class),
#'
#' - have an appropriate index attribute (defines the panel
#' structure),
#'
#' - any of `is.numeric`, `is.factor`, `is.logical`, `is.character`,
#' `is.complex` is `TRUE`.
#' 
#' @param object object to be checked for pseries features
#'
#' @export
#' @return A logical indicating whether the object is a pseries (`TRUE`)
#' or not (`FALSE`).
#' @seealso [pseries()] for some computations on pseries and some
#' further links.
#' @keywords attribute
#' @examples
#' 
#' # Create a pdata.frame and extract a series, which becomes a pseries
#' data("EmplUK", package = "plm")
#' Em <- pdata.frame(EmplUK)
#' z <- Em$output
#' 
#' class(z) # pseries as indicated by class
#' is.pseries(z) # and confirmed by check
#' 
#' # destroy index of pseries and re-check
#' attr(z, "index") <- NA
#' is.pseries(z) # now FALSE
#' 
is.pseries <- function(object) {
 # checks if an object has the necessary features to qualify as a 'pseries'
  res <- TRUE
  if (!inherits(object, "pseries")) res <- FALSE
  # class 'pseries' is always on top of basic class: min 2 classes needed, if 2 classes "pseries" needs to be first entry
  if (!length(class(object)) >= 2L) res <- FALSE
  if (length(class(object)) == 2L && class(object)[1L] != "pseries") res <- FALSE
  if (!has.index(object)) res <- FALSE
  if (!any(c(is.numeric(object), is.factor(object), is.logical(object), 
             is.character(object), is.complex(object)))) {
    res <- FALSE
  }
  
  return(res)
}


#' Check for the Dimensions of the Panel
#' 
#' This function checks the number of individuals and time observations in the
#' panel and whether it is balanced or not.
#' 
#' `pdim` is called by the estimation functions and can be also used
#' stand-alone.
#'
#' @name pdim
#' @aliases pdim
#' @param x a `data.frame`, a `pdata.frame`, a `pseries`, a
#'     `panelmodel`, or a `pgmm` object,
#' @param y a vector,
#' @param index see [pdata.frame()],
#' @param \dots further arguments.
#' @return An object of class `pdim` containing the following
#'     elements:
#' 
#' \item{nT}{a list containing `n`, the number of individuals, `T`,
#' the number of time observations, `N` the total number of
#' observations,}
#'
#' \item{Tint}{a list containing two vectors (of type integer): `Ti`
#' gives the number of observations for each individual and `nt` gives
#' the number of individuals observed for each period,}
#'
#' \item{balanced}{a logical value: `TRUE` for a balanced panel,
#' `FALSE` for an unbalanced panel,}
#'
#' \item{panel.names}{a list of character vectors: `id.names` contains
#' the names of each individual and `time.names` contains the names of
#' each period.}
#'
#' @note Calling `pdim` on an estimated `panelmodel` object
#'     and on the corresponding `(p)data.frame` used for this
#'     estimation does not necessarily yield the same result. When
#'     called on an estimated `panelmodel`, the number of
#'     observations (individual, time) actually used for model
#'     estimation are taken into account.  When called on a
#'     `(p)data.frame`, the rows in the `(p)data.frame` are
#'     considered, disregarding any NA values in the dependent or
#'     independent variable(s) which would be dropped during model
#'     estimation.
#' @export
#' @author Yves Croissant
#' @seealso [is.pbalanced()] to just determine balancedness
#'     of data (slightly faster than `pdim`),\cr
#'     [punbalancedness()] for measures of
#'     unbalancedness,\cr [nobs()],
#'     [pdata.frame()],\cr [pvar()] to check for
#'     each variable if it varies cross-sectionally and over time.
#' @keywords attribute
#' @examples
#' 
#' # There are 595 individuals
#' data("Wages", package = "plm")
#' pdim(Wages, 595)
#' 
#' # Gasoline contains two variables which are individual and time
#' # indexes and are the first two variables
#' data("Gasoline", package="plm")
#' pdim(Gasoline)
#' 
#' # Hedonic is an unbalanced panel, townid is the individual index
#' data("Hedonic", package = "plm")
#' pdim(Hedonic, "townid")
#' 
#' # An example of the panelmodel method
#' data("Produc", package = "plm")
#' z <- plm(log(gsp)~log(pcap)+log(pc)+log(emp)+unemp,data=Produc,
#'          model="random", subset = gsp > 5000)
#' pdim(z)
#' 
pdim <- function(x, ...) {
  UseMethod("pdim")
}

#' @rdname pdim
#' @export
pdim.default <- function(x, y, ...) {
  if (length(x) != length(y)) stop("The length of the two vectors differs\n")
  x <- x[drop = TRUE] # drop unused factor levels so that table() 
  y <- y[drop = TRUE] # gives only needed combinations
  z <- table(x,y)
  Ti <- rowSums(z) # faster than: apply(z, 1, sum)
  nt <- colSums(z) #              apply(z, 2, sum)
  n <- nrow(z)
  T <- ncol(z)
  N <- length(x)
  nT <- list(n = n, T = T, N = N)
  id.names <- rownames(z)
  time.names <- colnames(z)
  panel.names <- list(id.names = id.names, time.names = time.names)
  if (any(as.vector(z)==0)){
    balanced <- FALSE
  }
  else balanced <- TRUE
  if (any(as.vector(z) > 1)) stop("duplicate couples (id-time)\n")
  Tint <- list(Ti = Ti, nt = nt)
  z <- list(nT = nT, Tint = Tint, balanced = balanced, panel.names = panel.names)
  class(z) <- "pdim"
  z
}

#' @rdname pdim
#' @export
pdim.data.frame <- function(x, index = NULL, ...) {
  x <- pdata.frame(x, index)
  index <- attr(x, "index")
  id <- index[[1L]]
  time <- index[[2L]]
  pdim(id, time)
}

#' @rdname pdim
#' @export
pdim.pdata.frame <- function(x,...) {
  index <- attr(x, "index")
  pdim(index[[1L]], index[[2L]])
}

#' @rdname pdim
#' @export
pdim.pseries <- function(x,...) {
  index <- attr(x, "index")
  pdim(index[[1L]], index[[2L]])
}

#' @rdname pdim
#' @export
pdim.panelmodel <- function(x, ...) {
  x <- model.frame(x)
  pdim(x)
}

#' @rdname pdim
#' @export
pdim.pgmm <- function(x, ...) {
## pgmm is also class panelmodel, but take advantage of the pdim attribute in it
  attr(x, "pdim")
}

#' @rdname pdim
#' @export
print.pdim <- function(x, ...) {
  if (x$balanced){
      cat("Balanced Panel: ")
      cat(paste("n = ", x$nT$n, ", ", sep=""))
      cat(paste("T = ", x$nT$T, ", ", sep=""))
      cat(paste("N = ", x$nT$N, "\n", sep=""))
  }
  else{
      cat("Unbalanced Panel: ")
      cat(paste("n = ", x$nT$n,", ", sep=""))
      cat(paste("T = ", min(x$Tint$Ti), "-", max(x$Tint$Ti), ", ", sep=""))
      cat(paste("N = ", x$nT$N, "\n", sep=""))
  }
}

#' Extract the indexes of panel data
#' 
#' This function extracts the information about the structure of the
#' individual and time dimensions of panel data. Grouping information
#' can also be extracted if the panel data were created with a
#' grouping variable.
#' 
#' Panel data are stored in a `"pdata.frame"` which has an `"index"`
#' attribute. Fitted models in `"plm"` have a `"model"` element which
#' is also a `"pdata.frame"` and therefore also has an `"index"`
#' attribute. Finally, each series, once extracted from a
#' `"pdata.frame"`, becomes of class `"pseries"`, which also has this
#' `"index"` attribute.  `"index"` methods are available for all these
#' objects.  The argument `"which"` indicates which index should be
#' extracted. If `which = NULL`, all indexes are extracted. `"which"`
#' can also be a vector of length 1, 2, or 3 (3 only if the pdata
#' frame was constructed with an additional group index) containing
#' either characters (the names of the individual variable and/or of
#' the time variable and/or the group variable or `"id"` and `"time"`)
#' and `"group"` or integers (1 for the individual index, 2 for the
#' time index, and 3 for the group index (the latter only if the pdata
#' frame was constructed with such).)
#' 
#' @name index.plm
#' @aliases index
#' @importFrom zoo index 
#' @export index
#' @param x an object of class `"pindex"`, `"pdata.frame"`,
#'     `"pseries"` or `"panelmodel"`,
#' @param which the index(es) to be extracted (see details),
#' @param \dots further arguments.
#' @return A vector or an object of class `c("pindex","data.frame")`
#'     containing either one index, individual and time index, or (any
#'     combination of) individual, time and group indexes.
#' @author Yves Croissant
#' @seealso [pdata.frame()], [plm()]
#' @keywords attribute
#' @examples
#' 
#' data("Grunfeld", package = "plm")
#' Gr <- pdata.frame(Grunfeld, index = c("firm", "year"))
#' m <- plm(inv ~ value + capital, data = Gr)
#' index(Gr, "firm")
#' index(Gr, "time")
#' index(Gr$inv, c(2, 1))
#' index(m, "id")
#' 
#' # with additional group index
#' data("Produc", package = "plm")
#' pProduc <- pdata.frame(Produc, index = c("state", "year", "region"))
#' index(pProduc, 3)
#' index(pProduc, "region")
#' index(pProduc, "group")
#'
NULL

#' @rdname index.plm
#' @export
index.pindex <- function(x, which = NULL, ...) {

    if (is.null(which)) {
      # if no specific index is requested, select all index variables
      which <- names(x)
    }
    else{
      # catch case when someone enters "individual" albeit proper value is
      # "id" to extract individual index
      posindividual <- match("individual", which)
      if (! is.na(posindividual)) which[posindividual] <- "id"
    }
    if (length(which) >  3L) stop("the length of argument 'which' should be at most 3")
    if (is.numeric(which)){
        if (! all(which %in% 1:3))
            stop("if integer, argument 'which' should contain only 1, 2 and/or 3")
        if (ncol(x) == 2L && 3 %in% which) stop("no grouping variable, only 2 indexes")
        which <- names(x)[which]
    }
    nindex <- names(x)
    gindex <- c("id", "time")
    if (ncol(x) == 3L) gindex <- c(gindex, "group")
    if (any(! which %in% c(nindex, gindex))) stop("unknown variable")
    if ("id"    %in% which) {
      which[which == "id"]    <- names(x)[1L]
      if("id" %in% names(x)[-1L]) warning("an index variable not being the invidiual index is called 'id'. Likely, any results are distorted.") 
    }
    if ("time"  %in% which) {
      which[which == "time"]  <- names(x)[2L]
      if("time" %in% names(x)[-2L]) warning("an index variable not being the time index is called 'time'. Likely, any results are distorted.") 
    }
    if (ncol(x) == 3L) if ("group" %in% which) {
      which[which == "group"] <- names(x)[3L]
      if("group" %in% names(x)[-3L]) warning("an index variable not being the group index is called 'group'. Likely, any results are distorted.") 
    }
    
    result <- x[ , which]
    result
}

#' @rdname index.plm
#' @export
index.pdata.frame <- function(x, which = NULL, ...) {
  anindex <- attr(x, "index")
  index(x = anindex, which = which)
}

#' @rdname index.plm
#' @export
index.pseries <- function(x, which = NULL, ...) {
  anindex <- attr(x, "index")
  index(x = anindex, which = which)
}
  
#' @rdname index.plm
#' @export
index.panelmodel <- function(x, which = NULL, ...) {
  anindex <- attr(x$model, "index")
  index(x = anindex, which = which)
}


is.index <- function(index) {
  # not exported, helper function
  # checks if the index is an index in the sense of package plm
  res <- if (all(class(index) == c("pindex", "data.frame"))) TRUE else FALSE
  return(res)
}

has.index <- function(object) {
  # not exported, helper function
  # checks if an object has an index in sense of package plm
  # (esp. to distinguish from zoo::index() which always returns an index)
  index <- attr(object, "index")
  return(is.index(index))
}

checkNA.index <- function(index, which = "all", error = TRUE) {
  # not exported, helper function
  # check if any NA in indexes (all or specific dimension)
  
  feedback <- if(error) stop else warning
  
  if(which == "all") {
    if(anyNA(index[[1L]])) feedback("NA in the individual index variable")
    if(anyNA(index[[2L]])) feedback("NA in the time index variable")
    if(ncol(index) == 3L) { if(anyNA(index[[3L]])) feedback("NA in the group index variable") }
  }
  if(which == 1L) {
    if(anyNA(index[[1L]])) feedback("NA in the individual index variable")
  }
  if(which == 2L) {
    if(anyNA(index[[2L]])) feedback("NA in the time index variable")
  }
  if(which == 3L) {
    if(anyNA(index[[3L]])) feedback("NA in the group index variable")
  }
}

# pos.index:
# not exported, helper function
#
# determines column numbers of the index variables in a pdata.frame
# returns named numeric of length 2 or 3 with column numbers of the index variables
# (1: individual index, 2: time index, if available 3: group index), 
# names are the names of the index variables
#
# returns c(NA, NA) / c(NA, NA, NA) if the index variables are not a column in the pdata.frame
# (e.g., for pdata.frames created with drop.index = TRUE).
# Cannot detect index variables if their columns names were changed after creation of the pdata.frame

pos.index <- function(x, ...) {
  index <- attr(x, "index")
  index_names <- names(index)
  index_pos <- match(index_names, names(x))
  names(index_pos) <- index_names
  return(index_pos)
}