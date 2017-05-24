##
## not exported yet;
##
##
## functions: lagt.pseries/leadt.pseries/difft.pseries (note the "t")
##
## this lagging function does lagging based on the time periods, i. e. evaluates
## the time periods (as opposed to just shifting by row positions with 
## lag.pseries/lead.pseries/diff.pseries (without "t"))
## 
## see also test files tests/test_lagt_leadt.R and tests/test_lag_lead.R
##
## TODO: * maybe better names?
##       * integrate with original lag.pseries (the latter doing lagging based on rows)? lag(x, time = T) -> lagt()
##         (similiar for leadt, difft), keep lagt/leadt/difft as shortcuts

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

lagt <- function(x, k = 1, ...) {
  UseMethod("lagt")
}

## leadt.pseries(x, k) is a wrapper for lagt.pseries(x, -k)
leadt.pseries <- function(x, k = 1, ...) {
  ret <- lagt.pseries(x, k = -k)
  if (length(k) > 1) colnames(ret) <- k
  return(ret)
}

leadt <- function(x, k = 1, ...) {
  UseMethod("leadt")
}

## difft: diff-ing taking the time variable into account
difft.pseries <- function(x, lag = 1, ...){
  if (!is.numeric(x)) stop("diff is only relevant for numeric series")
  if (round(lag) != lag) stop("Lagging value 'lag' must be whole-numbered (and non-negative)")
  
  # prevent input of negative values, because it will most likely confuse users
  # what diff would do in this case
  if (lag < 0) stop("diff.pseries is only relevant for non-negative lags")
  
  lagtx <- lagt(x, k = lag)
  return(x-lagtx)
}

difft <- function(x, k = 1, ...) {
  UseMethod("difft")
}

### alagt: non-exported helper function for lagt (actual work horse)
alagt <- function(x, ak) {
  
  if (round(ak) != ak) stop("Lagging value 'k' must be whole-numbered (positive, negative or zero)")
  if (ak != 0) {
    index <- attr(x, "index")
    id   <- index[[1]]
    time <- index[[2]]
    
    # Idea: split times in blocks per individuals and do lagging there by computation of correct time shifting
    
    # need to convert to numeric, do this by coering to character first (otherwise wrong results!)
    #  see R FAQ 7.10 for coercing factors to numeric: 
    #      as.numeric(levels(factor_var))[as.integer(factor_var)]   is more efficient than
    #      as.numeric(as.character(factor_var))
    time <- as.numeric(levels(time))[as.integer(time)]
    
    list_id_timevar <- split(time, id, drop = T)
    
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
    x[NApos] <- NA   # set NAs where necessary
    attributes(x) <- orig_attr # restore original names and 'pseries' class (lost by subsetting x)
  }
  return(x)
} # END alagt
