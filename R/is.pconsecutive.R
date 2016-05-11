########### is.pconsecutive ##############
# little helper function to find out if the time periods of an object are consecutive per id
# by consecutive we mean "consecutive on the numbers": t, t+1, t+2, ... where t is an integer

is.pconsecutive.default <- function(x, id, time, na.rm.tindex = FALSE, ...) {

  # argument 'x' just used for input check (if not NULL and is a vector)
  
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
  #   is.pconsecutive.(p)data.frame, is.pconsecutive.pseries as a pdata.frame (which is sorted) was constructed 
  #   in the first place
  
  if(na.rm.tindex) {
    NA_tindex <- is.na(time)
    time <- time[!NA_tindex]
    id <- id[!NA_tindex]
  }
  
  list_id_timevar <- split(time, id, drop = T)

  res <- sapply(list_id_timevar, function(id_timevar) { if(any(is.na(id_timevar))) {
                                                           NA # return NA if NA found in the time periods for individual
                                                          } else {
                                                              # NB: as.character needed before as.numeric:
                                                              # if a time period is missing from the whole data set, this
                                                              # would not be detected without as.character, see testfile
                                                              times <- as.numeric(as.character(id_timevar)) 
                                                              begin <- times[1]
                                                              end   <- times[length(times)]
                                                 
                                                              # compare to length(original times) to find out if times are consecutive
                                                              (end - begin + 1) == length(times)
                                                              
                                                              # Alternative way of checking:
                                                                # consecutive time periods from begin to end (if times were consecutive)
                                                                # consecutive <- seq(from = begin, to = end, by = 1)
                                                                # length(consecutive) == length(times)
                                                          }
                                                      })
  
  return(res)
}

is.pconsecutive.data.frame <- function(x, index = NULL, na.rm.tindex = FALSE, ...){
  if (!is.null(index) & length(index) != 2)
    stop("if argument 'index' is not NULL, 'index' needs to specify
         'individual' and 'time' dimension for is.pconsecutive to work on a data.frame")
  
  x <- pdata.frame(x, index, ...)
  is.pconsecutive.pdata.frame(x, na.rm.tindex = na.rm.tindex)
}

is.pconsecutive.pseries <- function(x, na.rm.tindex = FALSE, ...){
  index <- attr(x, "index")
  id <- index[[1]]
  time <- index[[2]]
  is.pconsecutive.default(x, id, time, na.rm.tindex = na.rm.tindex)
}

is.pconsecutive.pdata.frame <- function(x, na.rm.tindex = FALSE, ...){
  index <- attr(x, "index")
  id <- index[[1]]
  time <- index[[2]]
  is.pconsecutive.default(x, id, time, na.rm.tindex = na.rm.tindex)
}

is.pconsecutive.panelmodel <- function(x, na.rm.tindex = FALSE, ...){
  index <- attr(x$model, "index")
  id <- index[[1]]
  time <- index[[2]]
  is.pconsecutive.default(x, id, time, na.rm.tindex = na.rm.tindex)
}

is.pconsecutive <- function(x, ...){
  UseMethod("is.pconsecutive")
}
