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


# lag: compute lagged values (handles positive lags and negative lags (=leading values) [and 0 -> do nothing])
#
# NB: This method seems to be intended for rowwise (positionwise) shifting as lagging       # TODO: adapt wording once lagt ("t") etc is exported
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
      
        # NB: this code assumes consecutive time periods and produces   # TODO: better wording: does row-wise shifting, thus its results might be unexpected if lag > 1 and non-consecutive time periods
        #     wrong/unexpected results for lag > 1 and non-consecutive time periods

        # delete first ak observations for each unit
        #  NB: as.character(time) before as.numeric() might be needed to catch the case of missing time period in whole data set    ## TODO: check if this comment is valid at all. might be a left over from KT's analysis of the code
        #      see testfile test_lag_lead_factor_levels
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


diff.pseries <- function(x, lag = 1, ...){
    islogi <- is.logical(x)
    if (! (is.numeric(x) || islogi)) stop("diff is only relevant for numeric or logical series")
    
    non.int <- vapply(lag, function(l) round(l) != l, FUN.VALUE = TRUE)
    if (any(non.int)) stop("Lagging value(s) in 'lag' must be whole-numbered (and non-negative)")
    
    # prevent input of negative values, because it will most likely confuse users
    # what diff would do in this case
    neg <- vapply(lag, function(l) l < 0, FUN.VALUE = TRUE)
    if (any(neg)) stop("diff.pseries is only relevant for non-negative values in 'lag'")

    lagx <- lag(x, k = lag)
    res <- x-lagx
  
    # if x is logical and lagged x is a vector (as opposed to a matrix),
    # diff'ed x is an integer vector (mimics base::diff behaviour):
    if (islogi && length(lag) == 1) class(res) <- c("pseries", "integer")
    return(res)
}


## pdiff is (only) used in model.matrix.pFormula to calculate the model.matrix for FD models,
## works for effect = "individual" and "time", see model.matrix on how to call pdiff.
## Result is in order (id, time) for both effects
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

