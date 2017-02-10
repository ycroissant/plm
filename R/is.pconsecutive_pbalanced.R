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
  
  list_id_timevar <- split(time, id, drop = T)

  res <- sapply(list_id_timevar, function(id_timevar) { if(any(is.na(id_timevar))) {
                                                           NA # return NA if NA found in the time periods for individual
                                                          } else {
                                                              begin <- id_timevar[1]
                                                              end   <- id_timevar[length(id_timevar)]
                                                 
                                                              # compare to length(original id_timevar) to find out if times are consecutive
                                                              (end - begin + 1) == length(id_timevar)
                                                              
                                                              # Alternative way of checking:
                                                                # consecutive time periods from begin to end (if id_timevar were consecutive)
                                                                # consecutive <- seq(from = begin, to = end, by = 1)
                                                                # length(consecutive) == length(id_timevar)
                                                          }
                                                      })
  return(res)
}

is.pconsecutive.data.frame <- function(x, index = NULL, na.rm.tindex = FALSE, ...){
  if (!is.null(index) & length(index) != 2)
    stop("if argument 'index' is not NULL, 'index' needs to specify
         'individual' and 'time' dimension for is.pconsecutive to work on a data.frame")
  
  if (is.null(index)) index_orig_names <- names(x)[1:2] # assume first two columns to be the index vars
    else index_orig_names <- index
  
  id   <- x[ , index_orig_names[1]]
  time <- x[ , index_orig_names[2]]

  # order as stacked time series (by id and time) first, otherwise default method does not work correctly!
  ord <- order(id, time)
  x_ordered    <- x[ord, ]
  id_ordered   <- id[ord]
  time_ordered <- time[ord]
  
#  if (!identical(x, x_ordered)) 
#    print("Note: for test of consecutiveness of time periods, the data.frame was ordered by index variables (id, time)")
  
  return(is.pconsecutive.default(x_ordered, id_ordered, time_ordered, na.rm.tindex = na.rm.tindex, ...))
}

is.pconsecutive.pseries <- function(x, na.rm.tindex = FALSE, ...){
  index <- attr(x, "index")
  id   <- index[[1]]
  time <- index[[2]]
  return(is.pconsecutive.default(x, id, time, na.rm.tindex = na.rm.tindex, ...))
}

is.pconsecutive.pdata.frame <- function(x, na.rm.tindex = FALSE, ...){
  index <- attr(x, "index")
  id   <- index[[1]]
  time <- index[[2]]
  return(is.pconsecutive.default(x, id, time, na.rm.tindex = na.rm.tindex, ...))
}

is.pconsecutive.panelmodel <- function(x, na.rm.tindex = FALSE, ...){
  index <- attr(x$model, "index")
  id   <- index[[1]]
  time <- index[[2]]
  return(is.pconsecutive.default(x, id, time, na.rm.tindex = na.rm.tindex, ...))
}

is.pconsecutive <- function(x, ...){
  UseMethod("is.pconsecutive")
}

########### is.pbalanced ##############
### for convenience and to be faster than pdim() for the purpose
### of the determination of balancedness only, because it avoids
### pdim()'s calculations which are unnecessary for balancedness.
###
### copied (and adapted) methods and code from pdim.*
### (only relevant parts to determine balancedness)
is.pbalanced <- function(x, ...) {
  UseMethod("is.pbalanced")
}

is.pbalanced.default <- function(x, y, ...) {
  if (length(x) != length(y)) stop("The length of the two vectors differs\n")
  x <- x[drop = TRUE] # drop unused factor levels so that table 
  y <- y[drop = TRUE] # gives only needed combinations
  z <- table(x, y)
  if (any(as.vector(z) == 0)) {
    balanced <- FALSE
  } else { balanced <- TRUE }
  
  if (any(as.vector(z) > 1)) warning("duplicate couples (id-time)\n")
  
  return(balanced)
}

is.pbalanced.data.frame <- function(x, index = NULL, ...) {
  x <- pdata.frame(x, index)
  index <- attr(x, "index")
  id <- index[[1]]
  time <- index[[2]]
  return(is.pbalanced(id, time))
}

is.pbalanced.pdata.frame <- function(x, ...) {
  index <- attr(x, "index")
  return(is.pbalanced(index[[1]], index[[2]]))
}

is.pbalanced.pseries <- function(x, ...) {
  index <- attr(x, "index")
  return(is.pbalanced(index[[1]], index[[2]]))
}

is.pbalanced.panelmodel <- function(x, ...) {
  x <- model.frame(x)
  return(is.pbalanced(x))
}

is.pbalanced.pgmm <- function(x, ...) {
## pgmm is also class panelmodel, but take advantage of the pdim attribute in it
  return(attr(x, "pdim")$balanced)
}
