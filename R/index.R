
### Index methods

index.pindex <- function(x, which = NULL, ...){
  if (is.null(which)) which <- names(x)
  if (! (length(which) %in% c(1, 2))) stop("which should be of length 1 or 2")
  if (is.numeric(which)){
    if (! all(which %in% c(1, 2))) stop("if integers, which should contain 1 and/or 2")
    which <- names(x)[which]
  }
  if (length(which) == 2){
    if (which[1] == "id") which[1] = names(x)[1]
    if (which[2] == "time") which[2] = names(x)[2]
    for (i in 1:2){
      if (! (which[i] %in% names(x))) stop(paste("variable", which[i], "does not exist"))
    }
    result <- x[, which]
  }
  else{
    if (which == "id") which = names(x)[1]
    if (which == "time") which = names(x)[2]
    if (! (which %in% names(x))) stop(paste("variable", which, "does not exist"))
    result <- x[, which]
  }
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

# pos.index:
# not exported, helper function
# determines column numbers of the index variables in a pdata.frame
# returns named numeric of length 2 with column numbers of the index variables (1: individual index, 2: time index) and 
# names are the names of the index variables
#
# returns c(NA, NA) if the index variables are not a column in the pdata.frame (e.g. for pdata.frames created with drop.index = TRUE).
# Cannot detect index variables if their columns names were changed after creation of the pdata.frame
pos.index <- function(x, ...) {
  index <- attr(x, "index")
  index_names <- names(index)
  index_pos <- match(index_names, names(x))
  names(index_pos) <- index_names
  return(index_pos)
}


# NB: Are functions indexes and print.indexes still needed?
indexes <- function(x){
  if (!inherits(x, "pdata.frame")){
    stop("indexes function only for pdata.frame\n")
  }
  attr(x,"index")
}

print.indexes <- function(x, ...){
  cat(paste("Index: (individual=",x$id,") and (time=",x$time,")\n",sep=""))
}