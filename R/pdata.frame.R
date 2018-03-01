# pdata.frame

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

    # if requested: check for constant series and remove
    if (drop.const.series) {
      # old: cst.check <- sapply(x, function(x) var(as.numeric(x), na.rm = TRUE)==0)
      # -> var() and sd() on factors is deprecated as of R 3.2.3 -> use duplicated()
      cst.check <- sapply(x, function(x) {
                              if (is.factor(x) || is.character(x)) {
                                all(duplicated(x[!is.na(x)])[-1L])
                              } else {
                                x[!is.finite(x)] <- NA # infinite elements set to NA only for this check
                                var(as.numeric(x), na.rm = TRUE) == 0
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
    if (length(index) == 1){
        id <- index
        time <- NULL
    }
    # if the length of index is 2, the first element is id, the second is time
    if (length(index) == 2){
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
        if( (N %% id.name) != 0){
            stop(paste0("unbalanced panel, in this case the individual index may not be indicated by an integer\n",
                        "but by specifying a column of the data.frame in the first element of the 'index' argument\n"))
        }
        else{
            T <- N %/% id.name
            n <- N %/% T
            time <- rep((1:T), n)
            id <- rep((1:n), rep(T, n))
            id.name <- "id"
            time.name <- "time"
            if (id.name %in% names(x)) warning(paste0("column '", id.name, "' overwritten by id index"))
            if (time.name %in% names(x)) warning(paste0("column '", time.name, "' overwritten by time index"))
            x[[id.name]] <- id <- as.factor(id)
            x[[time.name]] <- time <- as.factor(time)
        }
    }
    else{
        # id.name is not numeric, i.e. individual index is supplied
        if (!id.name %in% names(x)) stop(paste("variable ",id.name," does not exist (individual index)", sep=""))
        
        if (is.factor(x[[id.name]])){
            id <- x[[id.name]] <- x[[id.name]][drop=TRUE] # drops unused levels of factor
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
                time <- c(time, 1:Ti[i])
            }
            time.name <- "time"
            if (time.name %in% names(x)) warning(paste0("column '", time.name, "' overwritten by time index"))
            time <- x[[time.name]] <- as.factor(time)
        }
        else{
            # use supplied time index
            if (!time.name %in% names(x)) stop(paste0("variable ", time.name, " does not exist (time index)"))
            
            if (is.factor(x[[time.name]])){
                time <- x[[time.name]] <- x[[time.name]][drop=TRUE] # drops unused levels of factor
            }
            else{
                time <- x[[time.name]] <- as.factor(x[[time.name]])
            }
        }
    }
    
    # if present, make group variable a factor (just like for id and time variables)
    if (!is.null(group.name)) {
        if (is.factor(x[[group.name]])){
            group <- x[[group.name]] <- x[[group.name]][drop=TRUE] # drops unused levels of factor
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
        x <- x[ , -posindex, drop = FALSE]
        if (ncol(x) == 0L) cat("after dropping of index variables, the pdata.frame contains 0 columns")
    }
    
    test_doub <- table(index[[1]], index[[2]], useNA = "ifany")
    if (anyNA(colnames(test_doub)) || anyNA(rownames(test_doub)))
        cat(paste0("at least one couple (id-time) has NA in at least one index dimension in resulting pdata.frame\n to find out which, use e.g. table(index(your_pdataframe), useNA = \"ifany\")\n"))
    if (any(as.vector(test_doub[!is.na(rownames(test_doub)), !is.na(colnames(test_doub))]) > 1))
        warning("duplicate couples (id-time) in resulting pdata.frame\n to find out which, use e.g. table(index(your_pdataframe), useNA = \"ifany\")")
    
    if (row.names) {
        attr(x, "row.names") <- fancy.row.names(index)
        # NB: attr(x, "row.names") allows for duplicate rownames (as
        # opposed to row.names(x) <- something)
        # NB: no fancy row.names for index attribute (!?):
        # maybe because so it is possible to restore original row.names?
    }
    
    class(index) <- c("pindex", "data.frame")
    attr(x, "index") <- index
    class(x) <- c("pdata.frame", "data.frame")
    
    return(x)
}

"$<-.pdata.frame" <- function(x, name, value){
  if (inherits(value, "pseries")){
    # remove pseries features before adding value as a column to pdata.frame
    if (length(class(value)) == 1) value <- unclass(value)
    else attr(value, "class") <- setdiff(class(value), "pseries")
    attr(value, "index") <- NULL
  }
  "$<-.data.frame"(x, name, value)
}

# NB: We don't have methods for [<-.pdata.frame and [[<-.pdata.frame, so these functions
#     dispatch to the respective data.frame methods which assign whatever is
#     handed over to the methods. Especially, if a pseries is handed over, this
#     results in really assigning a pseries to the pdata.frame in case of usage of
#     [<- and [[<-. This is inconsistent because the columns of a pdata.frame do not
#     have the 'pseries' features.
#     This can be seen by lapply(some_pdata.frame, class) after 
#     assigning with the respective .data.frame methods


# NB: Currently no extracting/subsetting function for class pseries, thus
#     vector subsetting is used which removes the pseries features
#
#     Test cases in tests/test_pdata.frame_subsetting.R 
# 
#     There is a working sketch below, but check if it does not interfere with anything else.
#     * check if it works with FD and between models (these models compress the data and, thus,
#       pmodel.reponse for these does not return a pseries but a pure numeric)
#
# "[.pseries" <- function(x, ...) {
# 
#  ## use '...' instead of only one specific argument, because subsetting for
#  ## factors can have argument 'drop', e.g., x[i, drop=TRUE] see ?Extract.factor
# #stop()
#   index <- attr(x, "index")
#   if (is.null(index)) warning("pseries object with is.null(index(pseries)) == TRUE encountered, trying to continue anyway...")
#   if (is.index(index)) stop(paste0("pseries object has illegal index with class(index) == ", paste0(class(index), collapse = ", ")))
#   names_orig <- names(x)
#   x <- remove_pseries_features(x)
#   result <- x[...]
# 
#   # subset index / identify rows to keep in the index:
#   keep_rownr <- seq_along(names_orig)  # full length row numbers original pseries
#   names(keep_rownr) <- names_orig
#   keep_rownr <- keep_rownr[names(result)] # row numbers to keep after subsetting
#   index <- index[keep_rownr, ]
# 
#   # drop unused levels (like in subsetting of pdata.frames)
#   index <- droplevels(index)
# 
#   ### TODO: test for is.null before adding back? see [[.pdata.frame
# 
#   result <- add_pseries_features(result, index)
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
  
    old.pdata.frame <- !inherits(x, "data.frame")
    if (! old.pdata.frame){
        # this part for backward compatibility (required by meboot)
      
        ### subset index (and row names) appropriately:
        # subsetting data.frame by only j (x[ , j]) or missing j (x[i]) yields full-row
        # column(s) of data.frame, thus do not subset the index because it needs full rows (original index)
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
        # => subset index (and row names) if: supplied i && Nargs_mod >= 3L
      
        index <- attr(x, "index")
        x.rownames <- row.names(x)
        if (!missing.i && Nargs_mod >= 3L) {
            iindex <- i
            if (is.character(iindex)) {
              # Kevin Tappe 2016-01-04 : in case of indexing (subsetting) a 
              # pdata.frame by a character, the subsetting vector should be 
              # converted to numeric by matching to the row names so that the 
              # index can be correctly subsetted (by this numeric value).
              # Motivation:
              # Row names of the pdata.frame and row names of the pdata.frame's 
              # index are not guaranteed to be the same!
              iindex <- match(iindex, rownames(x))
            }
            # subset index and row names
            index <- "[.data.frame"(index, iindex, )
            x.rownames <- x.rownames[iindex]
            
            # remove empty levels in index (if any)
            # NB: really do dropping of unused levels? Standard R behaviour is to leave the levels and not drop unused levels
            #     Maybe the dropping is needed for functions like lag.pseries/lagt.pseries to work correctly?
            index <- droplevels(index)
            # NB: use droplevels() rather than x[drop = TRUE] as x[drop = TRUE] can also coerce mode!
            # old (up to rev. 251): index <- data.frame(lapply(index, function(x) x[drop = TRUE]))
        }
    }
    
    # delete attribute with old index first:
    # this preseves the order of the attributes because 
    # order of non-standard attributes is scrambled by R's data.frame subsetting with `[.`
    # (need to add new index later anyway)
    attr(x, "index") <- NULL
    
    # Set class to "data.frame" first to avoid coering which enlarges the (p)data.frame 
    # (probably by as.data.frame.pdata.frame).
    # Coercing is the built-in behaviour for extraction from data.frames by "[." (see ?`[.data.frame`) 
    # and it seems this cannot be avoided; thus we need to make sure, not to have any coercing going on
    # which adds extra data (such as as.matrix.pseries, as.data.frame.pdata.frame) by setting the class 
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
                                                    # depending on the call structure and whether missing j or not)
    
    mydata <- eval(sc_mod)

    if (is.null(dim(mydata))){
      # subsetting returned a vector or a factor or NULL (nothing more is left)
      if (is.null(mydata)) {
        # since R 3.4.0 NULL cannot have attributes, so special case it
        res <- NULL
      } else {
        # vector or factor -> make it a pseries
        res <- structure(mydata,
                         names = x.rownames,
                         index = index,
                         class = base::union("pseries", class(mydata)))
      }
    } else {
          # subsetting returned a data.frame -> add missing attributes to make it a pdata.frame again
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
  result <- "[[.data.frame"(x, y)
  if (!is.null(result)){
    # make extracted column a pseries
    # use this order for attributes to preserve original order of attributes for a pseries
    result <- structure(result,
                        names = row.names(x),
                        class = base::union("pseries", class(result)),
                        index = index 
                        )
  }
  result
}

"$.pdata.frame" <- function(x, y){
    "[[.pdata.frame"(x, paste(as.name(y)))
}

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

# as.list.pdata.frame:
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
        x <- lapply(x_names,
                    FUN = function(element, pdataframe){
                        "[[.pdata.frame"(x = pdataframe, y = element)
                    },
                    pdataframe = x)
        names(x) <- x_names
        
    # note: this function is slower than the corresponding
    # as.list.data.frame function,
    # because we cannot simply use unclass() on the pdata.frame:
    # need to add index etc to all columns to get proper pseries
    # back => thus the extraction function "[[.pdata.frame" is used
    }
    return(x)
}

as.data.frame.pdata.frame <- function(x, row.names = NULL, optional = FALSE, keep.attributes = TRUE, ...){
    index <- attr(x, "index")

     if (!keep.attributes) {
       attr(x, "index") <- NULL
       class(x) <- "data.frame"
       rownames(x) <- NULL
     } else {
      # make each column a pseries
      x <- lapply(x,
                  function(z){
               #     names(z) <- row.names(x) # it does not seem possible to keep the names in the 'pseries' because the call to data.frame later deletes the names 
                    attr(z, "index") <- index
                    class(z) <- base::union("pseries", class(z))
                    return(z)
                  }
      )
    }
    
    if (is.null(row.names) || row.names == FALSE) {
        x <- data.frame(x)
    } else {
        if (row.names == TRUE) { # set fancy row names
            x <- data.frame(x)
            row.names(x) <- fancy.row.names(index)
            # using row.names(x) <- "something" is safer (does not allow
            # duplicate row.names) than # attr(x,"row.names") <- "something"
    }
    ## not implemented: if row.names is a character vector, row.names
    ## could also be passed here to base::data.frame, see
    ## ?base::data.frame
    } 
    return(x)
}

# pseries

print.pseries <- function(x, ...){
  attr(x, "index") <- NULL
  attr(x, "class") <- base::setdiff(attr(x, "class"), "pseries")
  if (length(attr(x, "class")) == 1 && class(x) %in% c("character", "logical", "numeric", "integer")) {
    attr(x, "class") <- NULL
  }
  print(x, ...)
}

as.matrix.pseries <- function(x, idbyrow = TRUE, ...){
    index <- attr(x, "index")
    id <- index[[1]]
    time <- index[[2]]
    time.names <- levels(time)
    x <- split(data.frame(x, time), id)
    x <- lapply(x, function(x){
        rownames(x) <- x[ , 2]
        x[ , -2, drop = FALSE]
    })
    x <- lapply(x, function(x){
        x <- x[time.names, , drop = FALSE]
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

## plots a panel series by time index
##
## can supply any panel function, e.g. a loess smoother
## > mypanel<-function(x,...) {
## + panel.xyplot(x,...)
## + panel.loess(x, col="red", ...)}
## >
## > plot(pres(mod), panel=mypanel)

plot.pseries <- function(x, plot = c("lattice", "superposed"),
                         scale = FALSE, transparency = TRUE,
                         col = "blue", lwd = 1, ...) {

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
           lattice = {
               ##require(lattice) # make a ggplot2 version
               xyplot(nx ~ tind | ind, data = xdata, type = "l", col = col, ...)
               
           }, superposed = {
               ylim <- c(min(tapply(scalefun(nx), ind, min, na.rm = TRUE)),
                             max(tapply(scalefun(nx), ind, max, na.rm = TRUE)))
               unind <- unique(ind)
               nx1 <- nx[ind == unind[1]]
               tind1 <- as.numeric(tind[ind == unind[1]])
               ## plot empty plot to provide frame
               plot(NA, xlim = c(min(as.numeric(tind)),
                               max(as.numeric(tind))),
                    ylim = ylim, xlab = "", ylab = "", xaxt = "n", ...)
               axis(1, at = as.numeric(unique(tind)),
                    labels = unique(tind))

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
                   nxi <- nx[ind == unind[i]]
                   tindi <- tind[ind == unind[i]]
                   lines(x = tindi, y = scalefun(nxi),
                         col = col, lwd = lwd, ...)
               }
               
           })
    
}

summary.pseries <- function(object, ...) {
    if (!inherits(object, c("factor", "logical", "character"))) {
        id <- attr(object, "index")[[1]]
        time <- attr(object, "index")[[2]]
        xm <- mean(object, na.rm = TRUE)
        Bid <-  Between(object, na.rm = TRUE)
        Btime <-  Between(object, effect = "time", na.rm = TRUE)
        res <- structure(c(total = sumsq(object),
                           between_id = sumsq(Bid),
                           between_time = sumsq(Btime)), 
                         class = c("summary.pseries", "numeric"))
    } else {
        class(object) <- setdiff(class(object), c("pseries"))
        res <- summary(object, ...)
        class(res) <- c("summary.pseries", class(object), class(res))
    }
    return(res)
}

plot.summary.pseries <- function(x, ...){
    x <- as.numeric(x)
    share <- x[-1]/x[1] # vec with length == 2
    names(share) <- c("id", "time")
    barplot(share, ...)
}

print.summary.pseries <- function(x, ...){
    digits <- getOption("digits")
    special_treatment_vars <- c("factor", "logical", "character")
    if (!inherits(x, special_treatment_vars)) {
        x <- as.numeric(x)
        share <- x[-1]/x[1] # vec with length == 2
        names(share) <- c("id", "time")
        cat(paste("total sum of squares:", signif(x[1], digits = digits),"\n"))
        print.default(share, ...)
    } else {
        class(x) <- setdiff(class(x), c("summary.pseries", special_treatment_vars))
        print(x, ...)
    }
}
