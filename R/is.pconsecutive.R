########### is.pconsecutive ##############
# little helper function to find out if the time periods of an object are consecutive per id
is.pconsecutive.default <- function(x, id, time) {

  list_id_timevar <- split(time, id)

  res <- sapply(list_id_timevar, function(id_timevar) { times <- as.numeric(id_timevar)
                                                        begin <- times[1]
                                                        end   <- times[length(times)]
                                                        # consecutive time periods from begin to end (if times were consecutive)
                                                        consecutive <- seq(from = begin, to = end, by = 1) 
                                                        # compare to length(original times) to find out if times are consecutive
                                                        length(consecutive) == length(times)
                                                      })
  return(res)
}



is.pconsecutive.data.frame <- function(x, index = NULL, ...){
  if (!is.null(index) & length(index) != 2)
    stop("if argument 'index' is not NULL, 'index' needs to specify
         'individual' and 'time' dimension for is.pconsecutive to work on a data.frame")
  
  x <- pdata.frame(x, index, ...)
  is.pconsecutive(x)
}

is.pconsecutive.pseries <- function(x, ...){
  index <- attr(x, "index")
  id <- index[[1]]
  time <- index[[2]]
  is.pconsecutive.default(x, id, time)
}

is.pconsecutive.pdata.frame <- function(x, ...){
  index <- attr(x, "index")
  id <- index[[1]]
  time <- index[[2]]
  is.pconsecutive.default(x, id, time)
}

is.pconsecutive.panelmodel <- function(x, ...){
  index <- attr(x$model, "index")
  id <- index[[1]]
  time <- index[[2]]
  is.pconsecutive.default(x, id, time)
}

is.pconsecutive <- function(x, ...){
  UseMethod("is.pconsecutive")
}
