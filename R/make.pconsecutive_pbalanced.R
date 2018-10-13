### This file:
### make.pconsecutive.*
### make.pbalanced.*
###
### is.pconsecutive.* is in separate file is.pconsecutive.R

#  consecutive: "consecutive in the numbers": t, t+1, t+2, ... where t is an integer,
#                i.e. the time index var is interpreted as a numerical
# 

## in the future, maybe make.pconsective could gain an additional argument 'fill' for the filled value (currently NA)
##      if so, check other packages (data.table, dplyr, tidyr, ...) what the argument is called there
##      arg would need to be a (named) list (for (p)data.frame methods) because columns of 
##      (p)data.frames are of arbitraty classes

make.pconsecutive.indexes <- function(x, index, balanced = FALSE, ...) {
# make.pconsecutive.indexes: helper function, not exported
# returns list with 3 elements:
#   1 "consec_index":        consecutive data.frame to serve as the new index data.frame in other functions, 
#   2 "NArows_former_index": information about dropped lines (logical vector with length of original data)
#   3 "has_fancy_rownames":  logical whether fancy row.names were used in original data (can only be TRUE for pdata.frame or pseries)
 
  if (inherits(x, "pdata.frame") || inherits(x, "pseries")) {
    pdataframe_or_pseries <- TRUE
    index_orig <- attr(x, which = "index")
    id_orig    <- index_orig[[1]] # can leave as factor if it is a factor
    times_orig <- index_orig[[2]]
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
    id_orig    <- index_orig[[1]]
    times_orig <- index_orig[[2]]
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
      }, simplify = FALSE)
  
  } else {
    min_value <- min(df_index[, "times"])
    max_value <- max(df_index[, "times"])
    
    times_filled_list <- sapply(seq_len(n_id_orig), function(i) {
        seq(from = min_value, to = max_value, by = 1)
      }, simplify = FALSE, USE.NAMES = FALSE)
  }
  
  times_filled_vector <- unlist(times_filled_list)
  id_times <- sapply(times_filled_list, length) # lengths (with an "s") would be more efficient, but requires R >= 3.2
  id_filled_vector <- unlist(mapply(rep, unique(id_orig), id_times, SIMPLIFY = FALSE))
                      # SIMPLIFY = FALSE => always return list

  df_index_filled <- data.frame(id = id_filled_vector, times = times_filled_vector)
  names(df_index_filled)[1:2] <- names(index_orig)[1:2] # set original index names


  if (pdataframe_or_pseries) {
    df_index_filled[ , 1] <- as.factor(df_index_filled[ , 1])
    df_index_filled[ , 2] <- as.factor(df_index_filled[ , 2])
    class(df_index_filled) <- c("pindex", class(df_index_filled))
  } else {
    if (typeof(df_index_filled[ , 1]) != id_orig_typeof)    { mode(df_index_filled[ , 1]) <- id_orig_typeof    }
    if (typeof(df_index_filled[ , 2]) != times_orig_typeof) { mode(df_index_filled[ , 2]) <- times_orig_typeof }
  }

  # restore mode of row.names attribute
  # [was changed by above code due to some simplification by R's standard behaviour]
  mode(attr(df_index_filled, "row.names")) <- rownames_typeof
  
  res <- list(consec_index         = df_index_filled,
              NArows_former_index  = is_NA,
              has_fancy_rownames   = has_fancy_rownames)
  
  return(res)
} ### END: make.pconsecutive.indexes


make.pconsecutive.data.frame <- function(x, balanced = FALSE, index = NULL, ...){
  # if not NULL, index is must be character of length 2
  if (!is.null(index) & length(index) != 2)
    stop("if argument 'index' is not NULL, 'index' needs to specify
         'individual' and 'time' dimension for make.pconsecutive to work on a data.frame")
  
  # assume first two columns to be the index vars
  if (is.null(index)) index_orig_names <- names(x)[1:2]
    else index_orig_names <- index
    
  list_ret_make_index <- make.pconsecutive.indexes(x, index_orig_names, balanced = balanced, ...)
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

make.pconsecutive.pdata.frame <- function(x, balanced = FALSE, ...){
  orig_column_names <- names(x)
    
  list_ret_make_index <- make.pconsecutive.indexes(x, balanced = balanced, ...)
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

make.pconsecutive.pseries <- function(x, balanced = FALSE, ...) {
  is_p <- is.pconsecutive(x)
  is_bal <- is.pbalanced(x)
  make_balanced <- balanced == TRUE && !is_bal # consecutive AND balancedness requested but data not balanced 
                                               #  -> independent of the consecutiveness, we need to treat the balancedness
  
  if (anyNA(is_p) || !all(is_p) || make_balanced) {
    
    list_ret_make_index <- make.pconsecutive.indexes(x, balanced = balanced, ...)
    df_index_filled    <- list_ret_make_index[["consec_index"]]
    NArows_old_index   <- list_ret_make_index[["NArows_former_index"]]
    has_fancy_rownames <- list_ret_make_index[["has_fancy_rownames"]]
    
    df_old_index <- attr(x, "index")
    class(df_old_index) <- "data.frame"
    
    # strip x to its pure form (no index, no class pseries)
    attr(x, "index") <- NULL
    if (inherits(x, "pseries")) class(x) <- setdiff(class(x), "pseries")
    df_old_index$x <- x
    
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
} ## END: make.pconsecutive.pseries


make.pconsecutive <- function(x, ...){
  UseMethod("make.pconsecutive")
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
make.pbalanced.pdata.frame <- function(x, balance.type = c("fill", "shared.times", "shared.individuals"), ...) {

  if (length(balance.type) == 1 && balance.type == "shared") {
    # accept "shared" for backward compatibility
    balance.type <- "shared.times"
    warning("Use of balanced.type = 'shared' discouraged, set to 'shared.times'")
  }
  balance.type <- match.arg(balance.type)
  index <- attr(x, "index")
  
  switch(balance.type,
         "fill" = {
            x_consec_bal <- make.pconsecutive(x, balanced = TRUE)
            
            # delete time periods that were not present for any individual, but introduced by
            # making data consecutive
            # result: no time periods are added that are not present for at least one individual
            times_present_orig <- attr(x_consec_bal, "index")[[2]] %in% unique(index[[2]])
            result <- x_consec_bal[times_present_orig, ]
            
            # drop not present factor levels (some new levels were introduced by making data consecutive first):
              # drop from index
              index_result <- attr(result, "index")
              index_result[[2]] <- droplevels(index_result[[2]])
              attr(result, "index") <- index_result
              
              # drop from time column (if time index column present in pdata.frame)
              pos_indexvars <- pos.index(result) # position of index vars is c(NA, NA) if index vars are not present as columns
              index_orig_names <- names(pos_indexvars)
              if (!anyNA(pos_indexvars)) {
                result[ , pos_indexvars[2]] <- droplevels(result[ , pos_indexvars[2]])
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


make.pbalanced.pseries <- function(x, balance.type = c("fill", "shared.times", "shared.individuals"), ...) {

  if (length(balance.type) == 1 && balance.type == "shared") {
    # accept "shared" for backward compatibility
    balance.type <- "shared.times"
    warning("Use of balanced.type = 'shared' discouraged, set to 'shared.times'")
  }
  balance.type <- match.arg(balance.type)
  index <- attr(x, "index")

  switch(balance.type,
         "fill" = {
            x_consec_bal <- make.pconsecutive(x, balanced = TRUE)
            
            # delete time periods that were not present for any individual, but introduced by
            # making data consecutive
            # result: no time periods are added that are not present for at least one individual
              x_consec_bal_index <- attr(x_consec_bal, "index")
              times_present_orig <- x_consec_bal_index[[2]] %in% unique(index[[2]])
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
           class(result) <- union("pseries", class(result))
           },
         
         "shared.individuals" = {
           keep <- intersect_index(index, "individual")
           result <- x[keep]
           # restore 'pseries' features
           # (no subsetting method for pseries in the package (yet),
           #  usual vector subsetting removes the pseries features)
           attr(result, "index") <- index[keep, ]
           class(result) <- union("pseries", class(result))
         })
  return(result)
} ## END make.pbalanced.pseries

make.pbalanced.data.frame <- function(x, balance.type = c("fill", "shared.times", "shared.individuals"), index = NULL, ...) {
  # NB: for data.frame interface: the data is also sorted as stack time series

  if (length(balance.type) == 1 && balance.type == "shared") {
    # accept "shared" for backward compatibility
    balance.type <- "shared.times"
    warning("Use of balanced.type = 'shared' discouraged, set to 'shared.times'")
  }
  balance.type <- match.arg(balance.type)

  ## identify index of data.frame  
      # if not NULL, index is must be character of length 2
      if (!is.null(index) & length(index) != 2)
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
            times_present_orig <- x_consec_bal[ , index_orig_names[2]] %in% unique(index_df[[2]])
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

make.pbalanced <- function(x, balance.type = c("fill", "shared.times", "shared.individuals"), ...) {
  UseMethod("make.pbalanced")
}

# helper function: returns logical vector which rows/entries to keep
#                  when balance.type = "shared.times" or "shared.individuals"
#                  (intersect of all time periods or individuals)
intersect_index <- function(index, by) {
  # intersect() is defined on vectors (not factors)
  #  -> convert respective index to character before
  
  switch(by,
         "time" = {
           id <- index[[1]]
           time <- as.character(index[[2]])
         },
         "individual" = {
           id <- index[[2]]
           time <- as.character(index[[1]])
         })
  
  times_by_ids <- split(time, id)
  common_times <- Reduce(intersect, times_by_ids) 
  keep_entries <- time %in% common_times
  return(keep_entries)
}



