
### Index methods

index.pindex <- function(x, which = NULL, ...){
    if (is.null(which)) which <- names(x)
    else{
        posindividual <- match("individual", which)
        if (! is.na(posindividual)) which[posindividual] <- "id"
    }
    if (length(which) >  3) stop("the length of which should be at most 3")
    if (is.numeric(which)){
        if (! all(which %in% 1:3))
            stop("if integers, which should contain only 1, 2 and/or 3")
        if (ncol(x) == 2 & 3 %in% which) stop("no grouping variable, only 2 indexes")
        which <- names(x)[which]
    }
    nindex <- names(x)
    gindex <- c("id", "time")
    if (ncol(x) == 3) gindex <- c(gindex, "group")
    if (any(! which %in% c(nindex, gindex))) stop("unknown variable")
    if ("id" %in% which) which[which == "id"] <- names(x)[1]
    if ("time" %in% which) which[which == "time"] <- names(x)[2]
    if ("group" %in% which) which[which == "group"] <- names(x)[3]
    result <- x[, which]
    result
}

index.pdata.frame <- function(x, which = NULL, ...){
  anindex <- attr(x, "index")
  index(x = anindex, which = which)
}

index.pseries <- function(x, which = NULL, ...){
  anindex <- attr(x, "index")
  index(x = anindex, which = which)
}
  
index.panelmodel <- function(x, which = NULL, ...){
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

# pos.index:
# not exported, helper function
#
# determines column numbers of the index variables in a pdata.frame
# returns named numeric of length 2 or 3 with column numbers of the index variables
# (1: individual index, 2: time index, if available 3: group index), 
# names are the names of the index variables
#
# returns c(NA, NA) / c(NA, NA, NA) if the index variables are not a column in the pdata.frame
# (e.g. for pdata.frames created with drop.index = TRUE).
# Cannot detect index variables if their columns names were changed after creation of the pdata.frame
pos.index <- function(x, ...) {
  index <- attr(x, "index")
  index_names <- names(index)
  index_pos <- match(index_names, names(x))
  names(index_pos) <- index_names
  return(index_pos)
}


# NB: Are functions indexes and print.indexes still needed?
# NB: print.indexes does not treat the new group index var
indexes <- function(x){
  if (!inherits(x, "pdata.frame")){
    stop("indexes function only for pdata.frame\n")
  }
  attr(x,"index")
}

print.indexes <- function(x, ...){
  cat(paste("Index: (individual=",x$id,") and (time=",x$time,")\n",sep=""))
}
