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

Tapply.pseries <- function(x, effect = c("individual", "time", "group"), func, ...){
    effect <- match.arg(effect)
    index <- attr(x, "index")
    effect <- switch(effect,
                     "individual"= index[[1]],
                     "time"= index[[2]],
                     "group" = index[[3]]
                     )
    z <- Tapply.default(x, effect, func, ...)
    attr(z, "index") <- index
    class(z) <- c("pseries", class(z))
    z
}

Tapply.matrix <- function(x, effect, func, ...){
    na.x <- is.na(x)
    uniqval <- apply(x, 2, tapply, effect, func)
    result <- uniqval[as.character(effect), , drop = F]
    result[na.x] <- NA
    result
}

Sum <- function(x, ...){
    UseMethod("Sum")
}

Sum.default <- function(x, effect, ...){
    if (!is.numeric(x)) stop("The Sum function only applies to numeric vectors")
    Tapply(x, effect, sum, ...)
}

Sum.pseries <- function(x, effect = c("individual", "time", "group"), ...){
    effect <- match.arg(effect)
    Tapply(x, effect, sum, ...)
}

Sum.matrix <- function(x, effect,...){
    if (! effect %in% c("individual", "time", "group"))
        stop("irrelevant effect for a Sum transformation")
    if (is.null(attr(x, "index"))) Sum.default(x, effect)
    else{
        if (length(effect) > 1)
            stop("for matrices with index attributes, the effect argument must be a character")
        xindex <- attr(x, "index")
        effect <- index(xindex, effect)
        Tapply(x, effect, sum)
    }        
}

Between <- function(x, ...){
    UseMethod("Between")
}

Between.default <- function(x, effect, ...){
    if (!is.numeric(x)) stop("The Between function only applies to numeric vectors")
    Tapply(x, effect, mean, ...)
}

Between.pseries <- function(x, effect = c("individual", "time", "group"), ...){
    effect <- match.arg(effect)
    Tapply(x, effect = effect, mean, ...)
}

Between.matrix <- function(x, effect,...){
    if (! effect %in% c("individual", "time", "group"))
        stop("irrelevant effect for a between transformation")
    if (is.null(attr(x, "index"))) Between.default(x, effect)
    else{
        if (length(effect) > 1)
            stop("for matrices with index attributes, the effect argument must be a character")
        xindex <- attr(x, "index")
        effect <- index(xindex, effect)
        Tapply(x, effect, mean)
    }        
}

between <- function(x, ...){
    UseMethod("between")
}

between.default <- function(x, effect, ...){
    if (!is.numeric(x)) stop("The between function only applies to numeric vectors")
    tapply(x, effect, mean, ...)
}

between.pseries <- function(x, effect = c("individual", "time", "group"), ...){
    effect <- match.arg(effect)
    index <- attr(x, "index")
    effect <- switch(effect,
                     individual = index[[1]],
                     time = index[[2]],
                     group = index[[3]]
                     )
    x <- between.default(x, effect = effect, ...)
    nms <- attr(x, "dimnames")[[1]]
    attr(x, "dimnames") <- attr(x, "dim") <- NULL
    names(x) <- nms
    x
}

between.matrix <- function(x, effect,...){
    if (! effect %in% c("individual", "time", "group"))
        stop("irrelevant effect for a between transformation")
    if (is.null(attr(x, "index"))) Between.default(x, effect)
    else{
        if (length(effect) > 1)
            stop("for matrices with index attributes, the effect argument must be a character")
        xindex <- attr(x, "index")
        effect <- index(xindex, effect)
        apply(x, 2, tapply, effect, mean, ...)
    }        
}

Within <- function(x, ...){
    UseMethod("Within")
}

Within.default <- function(x, effect, ...){
    if (!is.numeric(x)) stop("the within function only applies to numeric vectors")
    x - Between(x, effect, ...)
}

Within.pseries <- function(x, effect = c("individual", "time", "group", "twoways"), ...){
    effect <- match.arg(effect)
    if (effect != "twoways") x - Between(x, effect, ...)
    else{
        if (is.pbalanced(x)) x - Between(x, "individual", ...) - Between(x, "time") + mean(x)
        else{
            time <- index(x)[[2]]
            Dmu <- model.matrix(~ time - 1)
            attr(Dmu, "index") <- index(x)
            W1 <- Within(x, "individual")
            WDmu <- Within(Dmu, "individual")
            W2 <- fitted(lm.fit(WDmu, x))
            W1 - W2
        }
    }
}

Within.matrix <- function(x, effect, rm.null = TRUE,...){
    if (is.null(attr(x, "index"))){
        result <- Within.default(x, effect, ...)
        othervar <- apply(result, 2, function(x) sum(abs(x), na.rm = TRUE)) > 1E-12
        if (rm.null){
            result <- result[, othervar, drop = FALSE]
            attr(result, "constant") <- character(0)
        }
        else{
            result <- result[, drop = FALSE]
            attr(result, "constant") <- colnames(x)[!othervar]
        }
        result
    }
    else{
        if (effect %in% c("individual", "time", "group")) result <- x - Between(x, effect)
        if (effect == "twoways"){
            xindex <- attr(x, "index")
            if (is.pbalanced(xindex)) result <- x - Between(x, "individual") - Between(x, "time") +
                                          matrix(rep(apply(x, 2, mean), nrow(x)), nrow = nrow(x), byrow = TRUE)
            else{
                time <- index(xindex, "time")
                id <- index(xindex, "individual")
                Dmu <- model.matrix(~ time - 1)
                attr(Dmu, "index") <- xindex
                W1 <- Within(x, "individual", rm.null = FALSE)
                WDmu <- Within(Dmu, "individual")
                W2 <- fitted(lm.fit(WDmu, x))
                result <- W1 - W2
            }
        }
    }
    result
}

############### LAG and DIFF
#
# lag/lead/diff for pseries are a wrappers for lagt, leadt, difft (if shift = "time") and 
#                                          for lagr, leadr, diffr (if shift = "row")
#
# The "t" and "r" methods are not exported (by intention).
#
# The "t" methods perform shifting while taking the time period into account (they "look" at the value in the time dimension).
# The "r" methods perform shifting row-wise (without taking the value in the time dimension into account).
#

# Generic needed only for lead (lag and diff generics are already included in base R)
lead <- function(x, k = 1, ...) {
  UseMethod("lead")
}

# Wrapper functions lag, lead, diff
lag.pseries <- function(x, k = 1, shift = c("time", "row"), ...) {
  shift <- match.arg(shift)
  res <- if (shift == "time") lagt.pseries(x = x, k = k, ...) else lagr.pseries(x = x, k = k, ...)
  return(res)
}

lead.pseries <- function(x, k = 1, shift = c("time", "row"), ...) {
  shift <- match.arg(shift)
  res <- if (shift == "time") leadt.pseries(x = x, k = k, ...) else leadr.pseries(x = x, k = k, ...)
  return(res)
}

diff.pseries <- function(x, lag = 1, shift = c("time", "row"), ...) {
  shift <- match.arg(shift)
  res <- if (shift == "time") difft.pseries(x = x, lag = lag, ...) else diffr.pseries(x = x, lag = lag, ...)
  return(res)
}

## lagt.pseries lagging taking the time variable into account
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

## leadt.pseries(x, k) is a wrapper for lagt.pseries(x, -k)
leadt.pseries <- function(x, k = 1, ...) {
  ret <- lagt.pseries(x, k = -k)
  if (length(k) > 1) colnames(ret) <- k
  return(ret)
}

## difft: diff-ing taking the time variable into account
difft.pseries <- function(x, lag = 1, ...){
  ## copied/adapted from diffr.pseries except lines which use lagt() ("t") instead of lagr() ("r")
  islogi <- is.logical(x)
  if (! (is.numeric(x) || islogi)) stop("difft is only relevant for numeric or logical series")
  
  non.int <- vapply(lag, function(l) round(l) != l, FUN.VALUE = TRUE)
  if (any(non.int)) stop("Lagging value(s) in 'lag' must be whole-numbered (and non-negative)")
  
  # prevent input of negative values, because it will most likely confuse users
  # what difft would do in this case
  neg <- vapply(lag, function(l) l < 0, FUN.VALUE = TRUE)
  if (any(neg)) stop("difft.pseries is only relevant for non-negative values in 'lag'")
  
  lagtx <- lagt.pseries(x, k = lag) # use "time-based" lagging for difft
  
  if (is.matrix(lagtx)) {
    # if 'lagtx' is matrix (case length(lag) > 1):
    # perform subtraction without pseries feature of 'x', because otherwise 
    # the result would be c("pseries", "matrix") which is not supported
    res <- as.numeric(x) - lagtx
  } else {
    res <- x - lagtx
  }
  
  return(res)
}

## alagt: non-exported helper function for lagt (actual work horse),
## performes shifting of observations while respecting the time dimension
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
    
    list_id_timevar <- split(time, id, drop = TRUE)
    
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
    x[NApos] <- NA  # set NAs where necessary
    attributes(x) <- orig_attr # restore original names and 'pseries' class (lost by subsetting x)
  }
  return(x)
} # END alagt


## lagr: lagging row-wise
lagr.pseries <- function(x, k = 1, ...) {
    index <- attr(x, "index")
    id <- index[[1]]
    time <- index[[2]]
  
    # catch the case when an index of pdata.frame shall be lagged (index variables are always factors)
    # NB: this catches - unintentionally - also the case when a factor variable is the same "on the character level"
    # as one of the corresponding index variables but not the index variable itself
    #
    # -> shall we prevent lagging of index variables at all? -> turned off for now, 2016-03-03
    # if (is.factor(x)) if (all(as.character(x) == as.character(id)) | all(as.character(x)==as.character(time))) stop("Lagged vector cannot be index.")
  
    alagr <- function(x, ak){
        if (round(ak) != ak) stop("Lagging value 'k' must be whole-numbered (positive, negative or zero)")
        if (ak > 0) {
      
        # NB: this code does row-wise shifting

        # delete first ak observations for each unit
            isNAtime <- c(rep(T, ak), (diff(as.numeric(time), lag = ak) != ak))
            isNAid   <- c(rep(T, ak), (diff(as.numeric(id),   lag = ak) != 0))
            isNA <- (isNAtime | isNAid)
      
            result <- x                                             # copy x first ...
            result[1:ak] <- NA                                      # ... then make first ak obs NA ... 
            result[(ak+1):length(result)] <- x[1:(length(x)-ak)]    # ... shift and ...
            result[isNA] <- NA                                      # ... make more NAs in between: this way, we keep: all factor levels, names, classes
      
        } else if (ak < 0) { # => compute leading values
      
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
    } # END function alagr
  
    if (length(k) > 1) {
        rval <- sapply(k, function(i) alagr(x, i))
        colnames(rval) <- k
    }
    else {
        rval <- alagr(x, k)
    }
    return(rval)
}


# leadr.pseries(x, k) is a wrapper for lagr.pseries(x, -k)
leadr.pseries <- function(x, k = 1, ...) {
    ret <- lagr.pseries(x, k = -k)
    if (length(k) > 1) colnames(ret) <- k
    return(ret)
}

## diffr: lagging row-wise
diffr.pseries <- function(x, lag = 1, ...){
    islogi <- is.logical(x)
    if (! (is.numeric(x) || islogi)) stop("diff is only relevant for numeric or logical series")
    
    non.int <- vapply(lag, function(l) round(l) != l, FUN.VALUE = TRUE)
    if (any(non.int)) stop("Lagging value(s) in 'lag' must be whole-numbered (and non-negative)")
    
    # prevent input of negative values, because it will most likely confuse users
    # what diff would do in this case
    neg <- vapply(lag, function(l) l < 0, FUN.VALUE = TRUE)
    if (any(neg)) stop("diff.pseries is only relevant for non-negative values in 'lag'")

    lagrx <- lagr.pseries(x, k = lag)
    
    if (is.matrix(lagrx)) {
      # if 'lagrx' is matrix (case length(lag) > 1):
      # perform subtraction without pseries feature of 'x', because otherwise 
      # the result would be c("pseries", "matrix") which is not supported
      res <- as.numeric(x) - lagrx
    } else {
      res <- x - lagrx
    }
    return(res)
}



## pdiff is (only) used in model.matrix.pFormula to calculate the model.matrix for FD models,
## works for effect = "individual" and "time", see model.matrix on how to call pdiff.
## Result is in order (id, time) for both effects
## Performs row-wise shifting
pdiff <- function(x, cond, effect = c("individual", "time"), has.intercept = FALSE){
    effect <- match.arg(effect)
    cond <- as.numeric(cond)
    n <- ifelse(is.matrix(x), nrow(x), length(x))
  
    # code below is written for effect="individual". If effect="time" is
    # requested, order x so that the code works and later restore original order of x
    if (effect == "time") { order_cond <- order(cond)
        if (!is.matrix(x)) { x <- x[order_cond]} 
        else {x <- x[order_cond, ] }
        cond <- cond[order_cond]
    }
  
    cond <- c(NA, cond[2:n] - cond[1:(n-1)]) # this assumes a certain ordering
    cond[cond != 0] <- NA
    
    if (!is.matrix(x)){
        result <- c(NA, x[2:n] - x[1:(n-1)])
        result[is.na(cond)] <- NA
        # for effect = "time": restore original order of x:
        if (effect == "time") result <- result[match(seq_len(n), order_cond)]
        result <- na.omit(result)
    }
    else{
        result <- rbind(NA, x[2:n, , drop=FALSE] - x[1:(n-1), , drop = FALSE])
        result[is.na(cond), ] <- NA
        # for effect = "time": restore original order of x:
        if (effect == "time") result <- result[match(seq_len(n), order_cond), ]
        result <- na.omit(result)
        result <- result[ , apply(result, 2, var) > 1E-12, drop = FALSE]
        if (has.intercept){
            result <- cbind(1, result)
            colnames(result)[1] <- "(intercept)"
        }
    }
    attr(result, "na.action") <- NULL
    result
}


## TODO: why do we have a second pdiff() now (introduced in rev. 622)?
pdiff <- function(x, effect = c("individual", "time"), has.intercept = FALSE){
  # NB: x is assumed to have an index attribute, e.g., a pseries
  #     can check with has.index(x)
    effect <- match.arg(effect)
    cond <- as.numeric(attr(x, "index")[[1]])
    n <- ifelse(is.matrix(x), nrow(x), length(x))
    cond <- c(NA, cond[2:n] - cond[1:(n-1)]) # this assumes a certain ordering
    cond[cond != 0] <- NA
    if (! is.matrix(x)){
        result <- c(NA , x[2:n] - x[1:(n-1)])
        result[is.na(cond)] <- NA
        result <- na.omit(result)
    }
    else{
        result <- rbind(NA, x[2:n, , drop = FALSE] - x[1:(n-1), , drop = FALSE])
        result[is.na(cond), ] <- NA
        result <- na.omit(result)
#        result <- result[ , apply(result, 2, var) > 1E-12, drop = FALSE]
        if (has.intercept){
            result <- cbind(1, result)
            colnames(result)[1] <- "(Intercept)"
        }
    }
    attr(result, "na.action") <- NULL
    result
}

