## groupGenerics for operations on pseries
## see ?groupGeneric
## see tests/test_groupGenerics_pseries.R for examples
##
## implemented wrappers for groups Ops, Math, Complex
##
## group generic for Summary (all, any, sum, prod, min, max, range) not needed 
## as functions in this group do not change the data type
##
## groupGenerics need to be registered in NAMESPACE
##
## groupGenerics are used to allow automatic propagation to higher/lower data type
## when operations are performed on pseries, 
## e.g., class c("pseries", "integer") -> c("pseries", "numeric") when a function
## takes an integer as input and outputs a numeric. Without the group generics,
## the class of the results would stay as c("pseries", "integer") while the values
## themselves are numerics. The associated test file demonstrates the behaviour,
## see tests/test_groupGenerics_pseries.R


## helper functions: remove_pseries_features and add_pseries_features
remove_pseries_features <- function(x) {
  
  # debug:
#  if (!is.pseries(x)) warning("removing pseries features now but object was not a proper pseries before")
  
  attr(x, "index") <- NULL
  x <- check_propagation_correct_class(x)
 # attr(x, "class") <- setdiff(class(x), "pseries") # cannot use this, don't know why
  class(x) <- setdiff(class(x), "pseries")
  return(x)
}

add_pseries_features <- function(x, index) {
  # debug:
#  if (is.null(index)) warning("'index' is null")
  
  attr(x, "index") <- index
  class(x) <- union("pseries", class(x))
  return(x)
}

#' @export
Ops.pseries <- function(e1, e2) {
#  print("Ops.pseries executed!") # debug output

  miss_e2 <- missing(e2)
  e1_pseries <- e2_pseries <- FALSE
  # either one or both could be pseries
  if (inherits(e1, "pseries")) {
    e1_pseries <- TRUE
    index_e1 <- attr(e1, "index")
    e1 <- remove_pseries_features(e1)
  }
  if (!miss_e2 && inherits(e2, "pseries")) {
    e2_pseries <- TRUE
    index_e2 <- attr(e2, "index")
    e2 <- remove_pseries_features(e2)
  }

  res <- if (!miss_e2) {
            get(.Generic)(e1, e2)
          } else {
            get(.Generic)(e1)
          }
  
  # result could be, e.g., matrix. So check if adding back pseries features
  # makes sense (e.g., do not create something of class c("pseries", "matrix")).
  # Need is.atomic because is.vector is too strict, however need to sort out
  # some other data types
  add_back_pseries <- if (is.atomic(res) && !is.matrix(res) && !is.pairlist(res)) TRUE else FALSE
  if (add_back_pseries) {
    if (miss_e2 && e1_pseries) relevant_index <- index_e1
    if ( e1_pseries && !e2_pseries) relevant_index <- index_e1
    if (!e1_pseries &&  e2_pseries) relevant_index <- index_e2
    if ( e1_pseries &&  e2_pseries) {
      # decide on index for result:
      # if objects vary in length: shorter object is recycled by R
      #  -> must take index of non-recycled object (= longer pseries)
      relevant_index <- if (nrow(index_e1) >= nrow(index_e2)) index_e1 else index_e2
      if ((nrow(index_e1) == nrow(index_e2)) && !isTRUE(all.equal(index_e1, index_e2)))
        warning("indexes of pseries have same length but not same content: result was assigned first operand's index")
    }
    res <- add_pseries_features(res, relevant_index)
  }
  
  return(res)
}

#' @export
Math.pseries <- function(x, ...) {
#  print("Math.pseries executed!") # debug output

  index <- attr(x, "index")
  if (inherits(x, "pseries")) x <- remove_pseries_features(x)
  
  x <- get(.Generic)(x, ...)
  x <- add_pseries_features(x, index)
  return(x)
}

#' @export
Complex.pseries <- function(z) {
#  print("Complex.pseries executed!") # debug output

  index <- attr(z, "index")
  if (inherits(z, "pseries")) z <- remove_pseries_features(z)

  z <- get(.Generic)(z)
  z <- add_pseries_features(z, index)
  return(z)
}

