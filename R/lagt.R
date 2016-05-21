

## this lagging function does lagging based on the time periods, i. e. evaluates the time periods
## (as opposed to just shifting by row positions [which is probably what ])
##
## not exported yet;
## also: maybe better name or integrate with original lag.pseries (the latter doing lagging based on rows)?
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

# helper function (actual work horse) for lagt
alagt <- function(x, ak) {
  index <- attr(x, "index")
  id   <- index[[1]]
  time <- index[[2]]
  
  if (round(ak) != ak) stop("Lagging value 'k' must be whole-numbered (positive, negative or zero)")
  if (ak != 0) {
    
    # Idea: split times in blocks per individuals and do lagging there by computation of correct time shifting
    
    
    # need to convert to numeric, do this by coering to character first (otherwise wrong results!)
    #  see R FAQ 7.10 for coercing factors to numeric: 
    #      as.numeric(levels(factor_var))[as.integer(factor_var)]   is more efficient than
    #      as.numeric(as.character(factor_var))
    time <- as.numeric(levels(time))[as.integer(time)]
    
    list_id_timevar <- split(time, id, drop = T)
    
    index_lag_ak_all_list <- sapply(X = list_id_timevar, 
                                    FUN = function(id_timevar) { 
                                      if(any(is.na(id_timevar))) {
                                        NA # return NA if NA found in the time periods for individual
                                      } else {
                                        index_lag_ak <- match(id_timevar - ak, id_timevar, incomparables = NA)
                                      }
                                    },
                                    simplify = FALSE)
    
    # translate blockwise positions to positions in full vector
    index_lag_ak_all <- unlist(index_lag_ak_all_list, use.names = FALSE)
    
    substitute_in_block <- index_lag_ak_all[!is.na(index_lag_ak_all)]
    is_NA <- is.na(index_lag_ak_all) # save NA positions
    
    block_lengths <- vapply(index_lag_ak_all_list, length, FUN.VALUE = 1L)
    # not needed but leave here for illustration
    #    startpos_block <- cumsum(block_lengths) - block_lengths + 1               
    #    endpos_block <- startpos_block + block_lengths - 1
    
    index_in_block <- unlist(sapply(block_lengths, function(x) seq(from = 1, to = x)), use.names = FALSE)
    index_in_block <- index_in_block[!is_NA]
    
    pos_in_x <- seq.int(x)[!is.na(index_lag_ak_all)]
    
    index_lag_ak_all[!is.na(index_lag_ak_all)] <- pos_in_x - (index_in_block - substitute_in_block)
    positions <- index_lag_ak_all
    
    res <- x[positions]
    attributes(res) <- attributes(x)
    
  } else { # ak == 0 -> return original vector
    res <- x
  }
  
  return(res)
} # END alagt

# leadt.pseries(x, k) is a wrapper for lagt.pseries(x, -k)
leadt.pseries <- function(x, k = 1, ...) {
  ret <- lagt.pseries(x, k = -k)
  if (length(k) > 1) colnames(ret) <- k
  return(ret)
}

leadt <- function(x, k = 1, ...) {
  UseMethod("leadt")
}
