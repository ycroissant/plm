###################################################
### chunk number 1: pdata.frame
###################################################

pdata.frame <- function(x, index = NULL, drop.index = FALSE, row.names = TRUE,
                        stringsAsFactors = default.stringsAsFactors(),
                        replace.non.finite = FALSE,
                        drop.NA.series = FALSE, drop.const.series = FALSE,
                        drop.unused.levels = FALSE) {

    if (inherits(x, "pdata.frame")) stop("already a pdata.frame")
  
    if (length(index) > 3){
        stop("'index' can be of length 3 at the most (one index variable for individual, time, group)")
    }
    
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
      na.check <- sapply(x,function(x) sum(!is.na(x))==0)
      na.serie <- names(x)[na.check]
      if (length(na.serie) > 0){
        if (length(na.serie) == 1)
          cat(paste0("This series is NA and has been removed: ", na.serie, "\n"))
        else
          cat(paste0("These series are NA and have been removed: ", paste(na.serie, collapse = ", "), "\n"))
      }
      x <- x[, !na.check]
    }

  
    # if requested: check and remove constant series
    if (drop.const.series) {
      # old: cst.check <- sapply(x, function(x) var(as.numeric(x), na.rm = TRUE)==0) # old
      # -> var() and sd() on factors is deprecated as of R 3.2.3 -> use duplicated()
      cst.check <- sapply(x, function(x) {
                              if (is.factor(x) || is.character(x)) {
                                all(duplicated(x[!is.na(x)])[-1L])
                              } else {
                                x[!is.finite(x)] <- NA # set infinite elements to NA only for check
                                var(as.numeric(x), na.rm = TRUE)==0
                              }
                            })
      
      # following line: bug fixed thanks to Marciej Szelfer 
      cst.check <- cst.check | is.na(cst.check)
      cst.serie <- names(x)[cst.check]
      if (length(cst.serie) > 0){
        if (length(cst.serie) == 1){
          cat(paste0("This series is constant and has been removed: ", cst.serie, "\n"))
        }
        else{
          cat(paste0("These series are constants and have been removed: ", paste(cst.serie, collapse = ", "), "\n"))
        }
      }
      x <- x[, !cst.check]
    }
  
    # sanity check for 'index' argument. First check the presence of a
    # grouping variable, this should be the third element of the index
    # vector or any "group" named element of this vector
    group.name <- NULL
    if (! is.null(names(index)) | length(index == 3)){
        if (! is.null(names(index))){
            grouppos <- match("group", names(index))
            if (! is.na(grouppos)){
                group.name <- index[grouppos]
                index <- index[- grouppos]
            }
        }
        if (length(index) == 3){
            group.name <- index[3]
            index <- index[- 3]
        }
    }
    if (length(index) == 0) index <- NULL

  # if index is NULL, both id and time are NULL
    if (is.null(index)){
        id <- NULL
        time <- NULL
    }
  # if the length of index is 1, id = index and time is NULL
    if (length(index)==1){
        id <- index
        time <- NULL
    }
  # if the length of index is 2, the first element is id, the second is time
    if (length(index)==2){
        id <- index[1]
        time <- index[2]
    }
  # if both id and time are NULL, the names of the index are the first
  # two names of x
    if (is.null(id) & is.null(time)){
        id.name <- names(x)[1]
        time.name <- names(x)[2]
    }
    else{
        id.name <- id
        time.name <- time
    }
    
  # if index is numeric, this indicats a balanced panel with no. of individuals equal to id.name
    if(is.numeric(id.name)){
        if(!is.null(time.name)){warning("The time index (second element of 'index' argument) will be ignored\n")}
        N <- nrow(x)
        if( (N %% id.name)!=0){
          stop(paste0("unbalanced panel, in this case the individual index may not be indicated by an integer\n",
                      "but by specifying a column of the data.frame in the first element of the 'index' argument\n"))
        }
        else{
            T <- N %/% id.name
            n <- N %/% T
            time <- rep(1:T, n)
            id <- rep(seq(1:n), rep(T, n))
            id.name <- "id"
            time.name <- "time"
            x[[id.name]] <- id <- as.factor(id)
            x[[time.name]] <- time <- as.factor(time)
        }
    }
    else{ # id.name is not numeric, i.e. individual index is supplied
        if (!id.name %in% names(x)) stop(paste("variable ",id.name," does not exist (individual index)", sep=""))
        
        if (is.factor(x[[id.name]])){
            id <- x[[id.name]] <- x[[id.name]][drop=T] # drops unused levels of factor
        }
        else{
            id <- x[[id.name]] <- as.factor(x[[id.name]])
        }
        
        if (is.null(time.name)){
        # if no time index is supplied, add time variable automatically
      
        # order data by individual index, necessary for the automatic
        # addition of time index to be succesfull if no time index was supplied
            x <- x[order(x[[id.name]]), ]
            
            Ti <- table(x[[id.name]]) # was: Ti <- table(id)
            n <- length(Ti)
            time <- c()
            for (i in 1:n){
                time <- c(time,1:Ti[i])
            }
            time.name <- "time"
            time <- x[[time.name]] <- as.factor(time)
        }
        else{
        # use supplied time index
            if (!time.name %in% names(x)) stop(paste("variable ",time.name," does not exist (time index)",sep=""))
            
            if (is.factor(x[[time.name]])){
                time <- x[[time.name]] <- x[[time.name]][drop=T] # drops unused levels of factor
            }
            else{
                time <- x[[time.name]] <- as.factor(x[[time.name]])
            }
        }
    }
    
    # if present, make group variable a factor (just like for id and time variables)
    if (!is.null(group.name)) {
        if (is.factor(x[[group.name]])){
            group <- x[[group.name]] <- x[[group.name]][drop=T] # drops unused levels of factor
        }
        else{
            group <- x[[group.name]] <- as.factor(x[[group.name]])
        }
    }

    # sort by group (if given), then by id, then by time
    if (! is.null(group.name)) x <- x[order(x[[group.name]], x[[id.name]], x[[time.name]]), ] # old: x <- x[order(id,time), ] 
    else x <- x[order(x[[id.name]], x[[time.name]]), ]

    # if requested: drop unused levels from factor variables
    # (spare those serving for the index as their unused levels are dropped already
    # (at least in the attribute "index" they need to be dropped b/c much code relies on it))
    if (drop.unused.levels) {
      var.names <- setdiff(names(x), c(id.name, time.name, group.name))
      for (i in var.names){
        if (is.factor(x[[i]])){
          x[[i]] <- droplevels(x[[i]])
        }
      }
    }

    posindex <- match(c(id.name, time.name, group.name), names(x))
    index <- x[, posindex]
    if (drop.index) {
      x <- x[ , -posindex]
      if (ncol(x) == 0L) cat("after dropping of index variables, the pdata.frame contains 0 columns")
    }
    
    test_doub <- table(index[[1]], index[[2]], useNA = "ifany")
    if (any(is.na(colnames(test_doub))) || any(is.na(rownames(test_doub))))
        cat(paste0("at least one couple (id-time) has NA in at least one index dimension in resulting pdata.frame\n to find out which, use e.g. table(index(your_pdataframe), useNA = \"ifany\")\n"))
    if (any(as.vector(test_doub[!is.na(rownames(test_doub)), !is.na(colnames(test_doub))]) > 1))
        warning("duplicate couples (id-time) in resulting pdata.frame\n to find out which, use e.g. table(index(your_pdataframe), useNA = \"ifany\")")
    
    if (row.names) {
        attr(x, "row.names") <- fancy.row.names(index)
        # NB: attr(x, "row.names") allows for duplicate rownames (as
        # opposed to row.names(x) <- something) no fancy row.names for
        # index attribute (!?): maybe because so it is possible to
        # restore original row.names?
    }
    
    class(index) <- c("pindex", "data.frame")
    attr(x, "index") <- index
    class(x) <- c("pdata.frame", "data.frame")
    
    return(x)
}



###################################################
### chunk number 2: assigning
###################################################

"$<-.pdata.frame" <- function(x, name, value){
  if (inherits(value, "pseries")){
    # remove pseries features before adding value as a column to pdata.frame
    if (length(class(value)) == 1) value <- unclass(value)
    else class(value) <- setdiff(class(value), "pseries")
    attr(value, "index") <- NULL
  }
  "$<-.data.frame"(x, name, value)
}


###################################################
### chunk number 3: extracting
###################################################

# NB: currently no extracting/subsetting function for class pseries, thus
#     vector subsetting is used which removes the pseries features
#    There is a working sketch below, but check if it does not interfere with anything else

# "[.pseries" <- function(x, ...) {
# 
#   ## use '...' instead of only one specific argument, because subsetting for
#   ## factors can have argument 'drop', e.g., x[i, drop=T] see ?Extract.factor
# 
#   index <- attr(x, "index")
# 
#   # to identify the entries which we need to keep in the index:
#   #  use names of the vector, but set names to integer sequence first (safer)
#   #  -> use this information (names_subsetted) to subset the index
#   #  this way, we can use the regular vector subsetting of R x[i] without
#   #  worrying about the form of i (logical, numeric, character, some expression, ...)
#   names_orig <- names(x)
#   names(x) <- seq_along(x)
#   # remove class 'pseries' and index attrib to use R's vector subsetting x[i]
#   class(x) <- setdiff(class(x), "pseries")
#   attr(x, "index") <- NULL
#   result <- x[...]
#   names_subsetted <- as.numeric(names(result))
# 
#   # make result a 'pseries' again:
#   # add back to result:
#   #    * subsetted original names                # TODO: not needed?
#   #    * subsetted index as attribute
#   #    * class 'pseries'
# #  names(result) <- names_orig[names_subsetted]           # TODO: not needed?
#   
#   ### TODO: test for is.null before adding back? see [[.pdata.frame
#   
#   attr(result, "index") <- index[names_subsetted, ]
#   class(result) <- union("pseries", class(x))
#   return(result)
# }



"[.pdata.frame" <- function(x, i, j, drop) {
                  # signature of [.data.frame here
  
    missing.i    <- missing(i)    # missing is only guaranteed to yield correct results,
    missing.j    <- missing(j)    # if its argument was not modified before accessing it
    missing.drop <- missing(drop) # -> save information about missingness
    sc <- sys.call()
    # Nargs_mod to distinguish if called by [] (Nargs_mod == 2L); [,] (Nargs_mod == 3L); [,,] (Nargs_mod == 4L)
    Nargs_mod <- nargs() - (!missing.drop)
  
    # # Kevin Tappe 2015-10-29 [code from old implementation]
    # if (missing(drop)){
    #     if (! missing(j) && length(j) == 1) { drop = TRUE
    #       } else { drop = FALSE }
    # }
  
    old.pdata.frame <- !inherits(x, "data.frame")
    if (! old.pdata.frame){
      # this part for backward compatibility (required by meboot)
      
      ### subset index appropriately:
      # subsetting data.frame by only j (x[ , j]) or missing j (x[i] yields full-row
      # columns of data.frame, thus do not subset index because it needs full full rows]
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
      # => subset index if: supplied i && Nargs_mod => 3L
      
        # Kevin Tappe 2016-01-04 : in case of indexing by a character
        # vector a pdata.frame, the subseting vector should be converted 
        # to numeric by matching to the rownames so that the index could 
        # be correctly indexed (by this numeric value)
        # (rownames of the pdata.frame and rownames of the pdata.frame's index are not guaranteed to be the same!)
        ## iindex <- i
        index <- attr(x, "index")
        if (!missing.i && Nargs_mod >= 3L) { 
            iindex <- i
            if (is.character(iindex)) iindex <- match(iindex, rownames(x))
            index <- "[.data.frame"(index, iindex, )
            
            # remove empty levels in index (if any)
            # NB: really do dropping of unused levels? Standard R behaviour is to leave the levels and not drop unused levels
            #     Maybe the dropping is needed for functions like lag.pseries to work correctly?
            index <- droplevels(index)
            # NB: use droplevels() rather than x[drop = TRUE] as x[drop = TRUE] can also coerce mode!
            # old (up to rev. 251): index <- data.frame(lapply(index, function(x) x[drop = TRUE]))
        }
    }
    
    # delete attribute for old index first:
    # this preseves the order of the attributes because 
    # order of non-standard attributes is scrambled by R's data.frame subsetting with `[.`
    # (need to add new index later anyway)
    attr(x, "index") <- NULL
    
    # Set class to "data.frame" first to avoid coering which enlarges the (p)data.frame 
    # (probably by as.data.frame.pdata.frame).
    # Coercing is the built-in behaviour for extraction from data.frames by "[." (see ?`[.data.frame`) 
    # and it seems this cannot be avoided; thus we need to make sure, not to have any coercing going on
    # which add extra data (such as as.matrix.pseries, as.data.frame.pdata.frame) by setting the class 
    # to "data.frame" first
    class(x) <- "data.frame"

    # call [.data.frame exactly as [.pdata.frame was called but arg is now 'x'
    # this is necessary because there could be several missing arguments
    # use sys.call (and not match.call) because arguments other than drop may not be named
    # need to evaluate i, j, drop, if supplied, before passing them on (do not pass on as originally catched sys.call)
    sc_mod <- sc
    sc_mod[[1]] <- quote(`[.data.frame`)
    sc_mod[[2]] <- quote(x)
    
    if (!missing.i) sc_mod[[3]] <- i # if present, i is always in pos 3
    if (!missing.j) sc_mod[[4]] <- j # if present, j is always in pos 4
    if (!missing.drop) sc_mod[[length(sc)]] <- drop # if present, drop is always in last position (4 or 5,
                                                    # depending on the call structure an whether missing j or not)
    
    mydata <- eval(sc_mod)

    if (is.null(dim(mydata))){
        # subsetting returned a vector (nothing more is left) -> make it a pseries
        res <- structure(mydata,
                         index = index,
                         class = base::union("pseries", class(mydata))) # use union to avoid doubling pseries if already present
                         
    }
    else{
        # subsetting returned a data.frame -> add missing info to make it a pdata.frame again
        res <- structure(mydata,
                         index = index,
                         class = c("pdata.frame", "data.frame"))
    }
    return(res)
}

"[[.pdata.frame" <- function(x, y){
  index <- attr(x, "index")
  attr(x, "index") <- NULL
  class(x) <- "data.frame"
  result <- "[[.data.frame"(x, y) # x[[y]]
  if (!is.null(result)){
    # make extracted column a pseries
    # use this order for attributes to preserve original order of attributes for a pseries
    result <- structure(result,
                        names = row.names(x),
                        class = base::union("pseries", class(result)), # class(x[[y]]) # use union to avoid doubling pseries if already present
                        index = index 
                        )
  }
  result
}

"$.pdata.frame" <- function(x, y){
  "[[.pdata.frame"(x, paste(as.name(y)))
}


###################################################
### chunk number 4: printing
###################################################
print.pdata.frame <- function(x, ...){
  attr(x, "index") <- NULL
  class(x) <- "data.frame"
  
  # This is a workaround: print.data.frame cannot handle
  # duplicated row names which are currently possible for pdata frames
  if (any(duplicated(rownames(x)))) {
   print("Note: pdata.frame contains duplicated row names, thus original row names are not printed")
   rownames(x) <- NULL 
  }
  
  print(x, ...)
}

print.pseries <- function(x, ...){
  attr(x, "index") <- NULL
  attr(x, "class") <- base::setdiff(attr(x, "class"), "pseries") # attr(x, "class")[-1]
  if (length(attr(x, "class")) == 1
      && class(x) %in% c("character", "logical", "numeric"))
    attr(x, "class") <- NULL
  print(x, ...)
}


###################################################
### chunk number 5: as.matrix
###################################################
as.matrix.pseries <- function(x, idbyrow = TRUE, ...){
  index <- attr(x, "index")
  id <- index[[1]]
  time <- index[[2]]
  time.names <- levels(as.factor(time))
  x <- split(data.frame(x,time), id)
  x <- lapply(x, function(x){
    rownames(x) <- x[,2]
    x[,-2,drop=F]
  })
  x <- lapply(x, function(x){
    x <- x[time.names,,drop=F]
    rownames(x) <- time.names
    x
  }
  )
  id.names <- names(x)
  x <- as.matrix(as.data.frame((x)))
  colnames(x) <- id.names
  if (idbyrow) x <- t(x)
  x
}

###################################################
### chunk number 6: as.list.pdata.frame
###################################################
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
#  and lapply can be used as usual, now working on a list of pseries, e.g.
#    lapply(as.list(pdata.frame[ , your_cols], keep.attributes), lag)
#  works as expected.
as.list.pdata.frame <- function(x, keep.attributes = FALSE, ...) {
  if (!keep.attributes) {
    x <- as.list.data.frame(x)
  } else {
    # make list of pseries objects
    x_names <- names(x)
    x <- lapply(x_names, FUN = function(element, pdataframe) {
                                    "[[.pdata.frame"(x = pdataframe, y = element)
                                    }, pdataframe = x)
    names(x) <- x_names
     
    # note: this function is slower than the corresponding
    # as.list.data.frame function,
    # because we cannot simply use unclass() on the pdata.frame:
    # need to add index etc to all columns to get proper pseries
    # back => thus the extraction function "[[.pdata.frame" is used
    }
  return(x)
}


###################################################
### chunk number 7: diff
###################################################

diff.pseries <- function(x, lag = 1, ...){
  if (!is.numeric(x)) stop("diff is only relevant for numeric series")
  if (round(lag) != lag) stop("Lagging value 'lag' must be whole-numbered (and non-negative)")

  # prevent input of negative values, because it will most likely confuse users
  # what diff would do in this case
  if (lag < 0) stop("diff.pseries is only relevant for non-negative lags")
  
  lagx <- lag(x, k = lag)
  return(x-lagx)
}


###################################################
### chunk number 8: Tapply
###################################################
Tapply <- function(x, ...){
  UseMethod("Tapply")
}

Tapply.default <- function(x, effect, func, ...){
  na.x <- is.na(x)
  uniqval <- tapply(x, effect, func, ...)
  nms <- attr(uniqval, "dimnames")[[1]]
  attr(uniqval, "dimnames") <- attr(uniqval, "dim") <- NULL
  names(uniqval) <- nms
  result <- uniqval[as.character(effect)]
  result[na.x] <- NA
  result
}

Tapply.pseries <- function(x, effect = c("individual", "time"), func, ...){
  effect <- match.arg(effect)
  index <- attr(x, "index")
  effect <- switch(effect,
                 "individual"= index[[1]],
                 "time"= index[[2]]
                 )
  z <- Tapply.default(x, effect, func, ...)
  attr(z, "index") <- index
  class(z) <- c("pseries", class(z))
  z
}

Tapply.matrix <- function(x, effect, func, ...){
   ## Note: this function is not robust wrt NA in effect
  na.x <- is.na(x)
  uniqval <- apply(x, 2, tapply, effect, func)
  result <- uniqval[as.character(effect), , drop = F]
  result[na.x] <- NA
  result
}


###################################################
### chunk number 10: Sum, Between, between, Within
###################################################
Sum <- function(x, ...){
  UseMethod("Sum")
}

Sum.default <- function(x, effect, ...){
  if (!is.numeric(x)) stop("The Sum function only applies to numeric vectors")
  Tapply(x, effect, sum, ...)
}

Sum.pseries <- function(x, effect = c("individual", "time", "group"), ...){
  effect <- match.arg(effect)
  Tapply(x, effect = sum, mean, ...)
}


Between <- function(x, ...){
  UseMethod("Between")
}

Between.default <- function(x, effect, ...){
  if (!is.numeric(x)) stop("The Between function only applies to numeric vectors")
  Tapply(x, effect, mean, ...)
}

Between.pseries <- function(x, effect = c("individual", "time"), ...){
  effect <- match.arg(effect)
  Tapply(x, effect = effect, mean, ...)
}

between <- function(x, ...){
  UseMethod("between")
}

between.default <- function(x, effect, ...){
  if (!is.numeric(x)) stop("The between function only applies to numeric vectors")
  tapply(x, effect, mean, ...)
}

between.pseries <- function(x, effect = c("individual", "time"), ...){
  effect <- match.arg(effect)
  index <- attr(x, "index")
  if (effect == "individual") effect <- index[[1]] else effect <- index[[2]]
  x <- between.default(x, effect = effect, ...)
  nms <- attr(x, "dimnames")[[1]]
  attr(x, "dimnames") <- attr(x, "dim") <- NULL
  names(x) <- nms
  x
}

between.matrix <- function(x, effect, ...){
  apply(x, 2, tapply, effect, mean, ...)
}


Within <- function(x, ...){
  UseMethod("Within")
}

Within.default <- function(x, effect, ...){
  if (!is.numeric(x)) stop("the within function only applies to numeric vectors")
  x - Between(x, effect, ...)
}

Within.pseries <- function(x, effect = c("individual", "time"), ...){
  effect <- match.arg(effect)
  Within.default(x, effect, ...)
}

Within.matrix <- function(x, effect, rm.null = TRUE,...){
  result <- Within.default(x, effect, ...)
  othervar <- apply(result, 2, function(x) sum(abs(x), na.rm = TRUE)) > 1E-12
  if (rm.null){
    result <- result[, othervar, drop = FALSE]
  }
  else{
    result <- result[, drop = FALSE]
  }
  attr(result, "constant") <- colnames(x)[!othervar]
  result
}



###################################################
### chunk number 12: methods for pseries
###################################################
sumsq <- function(x, ...){
  xb <- mean(x, na.rm = TRUE)
  sum((na.omit(x)-xb)^2)
}

summary.pseries <- function(object, ...){
  id <- attr(object, "index")[[1]]
  time <- attr(object, "index")[[2]]
  xm <- mean(object, na.rm = TRUE)
  Bid <-  Between(object, na.rm = TRUE)
  Btime <-  Between(object, effect = "time", na.rm = TRUE)
  structure( c(total = sumsq(object), between_id = sumsq(Bid), between_time = sumsq(Btime)), 
            class = c("summary.pseries", "numeric")
            )
}

plot.summary.pseries <- function(x, ...){
  x <- as.numeric(x)
  share <- x[-1]/x[1] # vec with length == 2
  names(share) <- c("id", "time")
  barplot(share, ...)
}

print.summary.pseries <- function(x, ...){
  digits <- getOption("digits")
  x <- as.numeric(x)
  share <- x[-1]/x[1] # vec with length == 2
  names(share) <- c("id", "time")
  cat(paste("total sum of squares :", signif(x[1], digits = digits),"\n"))
  print.default(share, ...)
}


###################################################
### chunk number 14: as.data.frame
###################################################
as.data.frame.pdata.frame <- function(x, row.names = NULL, optional = FALSE, ...){
  index <- attr(x, "index")
  x <- lapply(x,
                function(z){
                  attr(z, "index") <- index
                  class(z) <- base::union("pseries", class(z)) # use union to avoid doubling pseries if already present
                  return(z)
                }
              )
  
  if (is.null(row.names) || row.names == FALSE) {
    x <- data.frame(x)
  } else {
      if (row.names == TRUE) { # set fancy row names
        x <- data.frame(x)
        row.names(x) <- fancy.row.names(index) # using row.names(x)<-"something" is safer (does not allow duplicate row.names) 
                                               # than attr(x,"row.names")<-"something"
      }
    ## not implemented: if row.names is a character vector, row.names could also be passed here to base::data.frame,
    ## see ?base::data.frame
  } 
  
  return(x)
}

## pdiff is (only) used in model.matrix.pFormula to calculate the model.matrix for FD models
## works for effect = "individual" and "time", see model.matrix on how to call pdiff
## result is in order (id, time) for both effects
pdiff <- function(x, cond, effect = c("individual", "time"), has.intercept = FALSE){
  effect <- match.arg(effect)
  cond <- as.numeric(cond)
  n <- ifelse(is.matrix(x),nrow(x),length(x))
  
  # code below is written for effect="individual". If effect="time" is
  # requested, order x so that the code works and later restore original order of x
  if (effect == "time") { order_cond <- order(cond)
                          if (!is.matrix(x)) { x <- x[order_cond]} 
                            else {x <- x[order_cond, ] }
                          cond <- cond[order_cond]
                        }

  cond <- c(NA,cond[2:n]-cond[1:(n-1)]) # this assumes a certain ordering
  cond[cond != 0] <- NA
  
  if (!is.matrix(x)){
    result <- c(NA,x[2:n]-x[1:(n-1)])
    result[is.na(cond)] <- NA
    # for effect = "time": restore original order of x:
    if (effect == "time") result <- result[match(seq_len(n), order_cond)]
    result <- na.omit(result)
  }
  else{
    result <- rbind(NA,x[2:n,,drop=FALSE]-x[1:(n-1),,drop=FALSE])
    result[is.na(cond), ] <- NA
    # for effect = "time": restore original order of x:
    if (effect == "time") result <- result[match(seq_len(n), order_cond), ]
    result <- na.omit(result)
    result <- result[,apply(result,2, var) > 1E-12,drop = FALSE]
    if (has.intercept){
      result <- cbind(1,result)
      colnames(result)[1] <- "(intercept)"
    }
  }
  attr(result, "na.action") <- NULL
  result
}


# lag: compute lagged values (handles positive lags and negative lags (=leading values) [and 0 -> do nothing])
#
# NB: This method seems to be intended for rowwise (positionwise) shifting as lagging
#     There is also an (somwehat experimental) function called lagt.pseries in a seperate
#     file which respectes the time periods by looking at their content
lag.pseries <- function(x, k = 1, ...) {
  index <- attr(x, "index")
  id <- index[[1]]
  time <- index[[2]]
  
  # catch the case when an index of pdata.frame shall be lagged (index variables are always factors)
    # NB: this catches - unintentionally - also the case when a factor variable is the same "on the character level"
    # as one of the corresponding index variables but not the index variable itself
    #
    # -> shall we prevent lagging of index variables at all? -> turned off for now, 2016-03-03
    # if (is.factor(x)) if (all(as.character(x) == as.character(id)) | all(as.character(x)==as.character(time))) stop("Lagged vector cannot be index.")
  
  alag <- function(x, ak){
    if (round(ak) != ak) stop("Lagging value 'k' must be whole-numbered (positive, negative or zero)")
    if (ak > 0) {
      
      # NB: this code assumes consecutive time periods and produces wrong results
      #     for lag > 1 and non-consecutive time periods
      
      # delete first ak observations for each unit
      #  NB: as.character(time) before as.numeric() might be needed to catch the case of missing time period in whole data set
      #      see testfile test_lag_lead_factor_levels
      isNAtime <- c(rep(T, ak), (diff(as.numeric(time), lag = ak) != ak))
      isNAid   <- c(rep(T, ak), (diff(as.numeric(id),   lag = ak) != 0))
      isNA <- (isNAtime | isNAid)
      
      result <- x                                             # copy x first ...
      result[1:ak] <- NA                                      # ... then make first ak obs NA ... 
      result[(ak+1):length(result)] <- x[1:(length(x)-ak)]    # ... shift and ...
      result[isNA] <- NA                                      # ... make more NAs in between: this way, we keep: all factor levels, names, classes
      
    } else if (ak < 0) { # => compute leading values
      
      # NB: this code assumes consecutive time periods and produces wrong results
      #     for lag > 1 and non-consecutive time periods
      
      # delete last |ak| observations for each unit
      num_time <- as.numeric(time)
      num_id   <- as.numeric(id)
      isNAtime <- c(c((num_time[1:(length(num_time)+ak)] - num_time[(-ak+1):length(num_time)]) != ak), rep(T, -ak))
      isNAid   <- c(c((num_id[1:(length(num_id)+ak)]     - num_id[(-ak+1):length(num_id)])     != 0),  rep(T, -ak))
      isNA <- (isNAtime | isNAid)
      
      result <- x                                            # copy x first ...
      result[(length(result)+ak+1):length(result)] <- NA     # ... then make last |ak| obs NA ... 
      result[1:(length(result)+ak)] <- x[(1-ak):(length(x))] # ... shift and ...
      result[isNA] <- NA                                     # ... make more NAs in between: this way, we keep: all factor levels, names, classes
      
    } else { # ak == 0 => nothing to do, return original pseries (no lagging/no leading)
      result <- x
    }
    
    return(result)
  } # END function alag
  
  if (length(k) > 1) {
    rval <- sapply(k, function(i) alag(x, i))
    colnames(rval) <- k
  }
  else {
    rval <- alag(x, k)
  }
  return(rval)
}


# lead.pseries(x, k) is a wrapper for lag.pseries(x, -k)
lead.pseries <- function(x, k = 1, ...) {
  ret <- lag.pseries(x, k = -k)
  if (length(k) > 1) colnames(ret) <- k
  return(ret)
}

lead <- function(x, k = 1, ...) {
  UseMethod("lead")
}






pseries2pdata <- function(x) {
  ## transforms a pseries in a pdataframe with the indices as regular columns
  ## in positions 1 and 2 (individual index, time index)
  indices <- attr(x, "index")
  vx <- as.numeric(x)
  px <- cbind(indices, vx)
  dimnames(px)[[2]] <- c("ind","tind",deparse(substitute(x)))
  return(pdata.frame(px, index=c("ind","tind")))
}

pmerge <- function(x, y, ...) {
  ## transf. if pseries
  if("pseries" %in% class(x)) x <- pseries2pdata(x)
  if("pseries" %in% class(y)) y <- pseries2pdata(y)

  z <- merge(data.frame(x), data.frame(y), by.x=dimnames(x)[[2]][1:2],
             by.y=dimnames(y)[[2]][1:2], ...)

  return(z)
}


## plots a panel series by time index
##
## can supply any panel function, e.g. a loess smoother
## > mypanel<-function(x,...) {
## + panel.xyplot(x,...)
## + panel.loess(x, col="red", ...)}
## >
## > plot(pres(mod), panel=mypanel)

plot.pseries <- function(x, plot=c("lattice", "superposed"),
                         scale=FALSE, transparency=TRUE,
                         col="blue", lwd=1, ...) {

    if(scale) {scalefun <- function(x) scale(x)
               } else {
                   scalefun <- function(x) return(x)}

    nx <- as.numeric(x)
    ind <- attr(x, "index")[[1]]
    tind <- attr(x, "index")[[2]] # possibly as.numeric():
                                  # activates autom. tick
                                  # but loses time labels

    xdata <- data.frame(nx=nx, ind=ind, tind=tind)

    switch(match.arg(plot),
           lattice={

               ##require(lattice) # make a ggplot2 version
               xyplot(nx~tind|ind, data=xdata, type="l", col=col, ...)

           }, superposed={

                   ylim <- c(min(tapply(scalefun(nx), ind, min, na.rm=TRUE)),
                             max(tapply(scalefun(nx), ind, max, na.rm=TRUE)))
                   unind <- unique(ind)
                   nx1 <- nx[ind==unind[1]]
                   tind1 <- as.numeric(tind[ind==unind[1]])

                   ## plot empty plot to provide frame
                   plot(NA, xlim=c(min(as.numeric(tind)),
                            max(as.numeric(tind))),
                        ylim=ylim, xlab="", ylab="", xaxt="n", ...)
                        #x=tind1, y=scalefun(nx1), ylim=ylim, ...)

                   axis(1, at=as.numeric(unique(tind)),
                        labels=unique(tind))

                   ## determine lwd and transparency level as a function
                   ## of n
                   if(transparency) {
                       alpha <- 5/length(unind)
                       col <- heat.colors(1, alpha=alpha)
                       lwd <- length(unind)/10
                   }

                   ## plot lines (notice: tind. are factors, so they
                   ## retain the correct labels which would be lost if
                   ## using as.numeric
                   for(i in 1:length(unind)) {
                       nxi <- nx[ind==unind[i]]
                       tindi <- tind[ind==unind[i]]
                       lines(x=tindi, y=scalefun(nxi),
                             col=col, lwd=lwd, ...)
                       }

               })

}

