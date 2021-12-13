## Structural changes made to plm's original data transformation functions
## need to be mimicked in the *.collapse(.*) versions and vice versa.

## 1) Give the base-R version of the functions defined in tool_transformations.R
##    a new name (*.baseR).
## 2) Implement wrapper switched which call the *.baseR or *.collapse versions
##    based on the option plm.fast (a logical, can be set via R's regular option
##    mechanism: options("plm.fast" = TRUE).

## ad 1) new name for base R functions defined in tool_transformations.R
Sum.default.baseR <- plm:::Sum.default
Sum.pseries.baseR <- plm:::Sum.pseries
Sum.matrix.baseR  <- plm:::Sum.matrix

between.default.baseR <- plm:::between.default
between.pseries.baseR <- plm:::between.pseries
between.matrix.baseR  <- plm:::between.matrix

Between.default.baseR <- plm:::Between.default
Between.pseries.baseR <- plm:::Between.pseries
Between.matrix.baseR  <- plm:::Between.matrix

Within.default.baseR <- plm:::Within.default
Within.pseries.baseR <- plm:::Within.pseries
Within.matrix.baseR  <- plm:::Within.matrix

pseriesfy.baseR      <- plm:::pseriesfy # ... in tool_pdata.frame.R:


## ad 2) implement wrapper switches

#### Sum wrapper switches ####
Sum.default <- function(x, effect, ...) {
  if(!isTRUE(getOption("plm.fast"))) {
    Sum.default.baseR(x, effect, ...) } else {
    if(!isTRUE(getOption("plm.fast.pkg.collapse"))) stop(txt.no.collapse, call. = FALSE)
    Sum.default.collapse(x, effect, ...) }
}

Sum.pseries <- function(x, effect = c("individual", "time", "group"), ...) {
  if(!isTRUE(getOption("plm.fast"))) {
    Sum.pseries.baseR(x, effect, ...) } else {
    if(!isTRUE(getOption("plm.fast.pkg.collapse"))) stop(txt.no.collapse, call. = FALSE)
    Sum.pseries.collapse(x, effect, ...) }
}

Sum.matrix <- function(x, effect, ...) {
  if(!isTRUE(getOption("plm.fast"))) {
    Sum.matrix.baseR(x, effect, ...) } else {
    if(!isTRUE(getOption("plm.fast.pkg.collapse"))) stop(txt.no.collapse, call. = FALSE)
    Sum.matrix.collapse(x, effect, ...) }
}

#### Between wrapper switches ####
Between.default <- function(x, effect, ...) {
  if(!isTRUE(getOption("plm.fast"))) {
    Between.default.baseR(x, effect, ...) } else {
    if(!isTRUE(getOption("plm.fast.pkg.collapse"))) stop(txt.no.collapse, call. = FALSE)
    Between.default.collapse(x, effect, ...) }
}
  
Between.pseries <- function(x, effect = c("individual", "time", "group"), ...) {
  if(!isTRUE(getOption("plm.fast"))) {
    Between.pseries.baseR(x, effect, ...) } else {
    if(!isTRUE(getOption("plm.fast.pkg.collapse"))) stop(txt.no.collapse, call. = FALSE)
    Between.pseries.collapse(x, effect, ...) }
}

Between.matrix <- function(x, effect, ...) {
  if(!isTRUE(getOption("plm.fast"))) {
    Between.matrix.baseR(x, effect, ...) } else {
    if(!isTRUE(getOption("plm.fast.pkg.collapse"))) stop(txt.no.collapse, call. = FALSE)
    Between.matrix.collapse(x, effect, ...) }
}

#### between wrapper switches ####
between.default <- function(x, effect, ...) {
  if(!isTRUE(getOption("plm.fast"))) {
     between.default.baseR(x, effect, ...) } else {
     if(!isTRUE(getOption("plm.fast.pkg.collapse"))) stop(txt.no.collapse, call. = FALSE)
     between.default.collapse(x, effect, ...) }
}

between.pseries <- function(x, effect = c("individual", "time", "group"), ...) {
  if(!isTRUE(getOption("plm.fast"))) {
    between.pseries.baseR(x, effect, ...) } else {
    if(!isTRUE(getOption("plm.fast.pkg.collapse"))) stop(txt.no.collapse, call. = FALSE)
    between.pseries.collapse(x, effect, ...) }
}

between.matrix <- function(x, effect, ...) {
  if(!isTRUE(getOption("plm.fast"))) {
    between.matrix.baseR(x, effect, ...) } else {
    if(!isTRUE(getOption("plm.fast.pkg.collapse"))) stop(txt.no.collapse, call. = FALSE)
    between.matrix.collapse(x, effect, ...) }
}

#### Within wrapper switches ####
Within.default <- function(x, effect, ...) {
  if(!isTRUE(getOption("plm.fast"))) {
    Within.default.baseR(x, effect, ...) } else {
    if(!isTRUE(getOption("plm.fast.pkg.collapse"))) stop(txt.no.collapse, call. = FALSE)
    Within.default.collapse(x, effect, ...) }
}

Within.pseries <- function(x, effect = c("individual", "time", "group", "twoways"), ...) {
  if(!isTRUE(getOption("plm.fast"))) {
    Within.pseries.baseR(x, effect, ...)
  } else {
    if(!isTRUE(getOption("plm.fast.pkg.collapse"))) stop(txt.no.collapse, call. = FALSE)

    if(is.null(getOption("plm.fast.pkg.FE.tw"))) options("plm.fast.pkg.FE.tw" = "collapse")
    switch(getOption("plm.fast.pkg.FE.tw"),
           "collapse" = Within.pseries.collapse(       x, effect, ...), # collapse only,
           "fixest"   = Within.pseries.collapse.fixest(x, effect, ...), # collapse for 1-way FE + fixest for 2-way FE,
           "lfe"      = Within.pseries.collapse.lfe(   x, effect, ...), # collapse for 1-way FE + lfe for 2-way FE,
           stop("unknown value of option 'plm.fast.pkg.FE.tw'"))
  }
}

Within.matrix <- function(x, effect, ...) {
  if(!isTRUE(getOption("plm.fast"))) {
    Within.matrix.baseR(x, effect, ...) 
  } else {
    if (!isTRUE(getOption("plm.fast.pkg.collapse"))) stop(txt.no.collapse, call. = FALSE)
    
    if(is.null(getOption("plm.fast.pkg.FE.tw"))) options("plm.fast.pkg.FE.tw" = "collapse")
    switch(getOption("plm.fast.pkg.FE.tw"),
           "collapse" = Within.matrix.collapse(       x, effect, ...), # collapse only,
           "fixest"   = Within.matrix.collapse.fixest(x, effect, ...), # collapse for 1-way FE + fixest for 2-way FE,
           "lfe"      = Within.matrix.collapse.lfe(   x, effect, ...), # collapse for 1-way FE + lfe for 2-way FE,
           stop("unknown value of option 'plm.fast.pkg.FE.tw'"))
  }
}


#### Sum ####

Sum.default.collapse <- function(x, effect, ...) {
# print("Sum.default.collapse")
# browser()
  # argument 'effect' is assumed to be a factor

  if(!is.numeric(x)) stop("The Sum function only applies to numeric vectors")
  # check for presence of na.rm in dots, if not present set to FALSE
  na.rm <- if(missing(...) || is.null(na.rm <- list(...)$na.rm)) FALSE else na.rm
  res <- collapse::fsum(x, g = effect, w = NULL, na.rm = na.rm, TRA = "replace")
  names(res) <- as.character(effect)
  return(res)
}

Sum.pseries.collapse <- function(x, effect = c("individual", "time", "group"), ...) {
# print("Sum.pseries.collapse")
# browser()
  effect <- match.arg(effect)
  # check for presence of na.rm in dots, if not present set to FALSE
  na.rm <- if(missing(...) || is.null(na.rm <- list(...)$na.rm)) FALSE else na.rm
  eff.no <- switch(effect,
                   "individual" = 1L,
                   "time"       = 2L,
                   "group"      = 3L,
                   stop("unknown value of argument 'effect'"))
  xindex <- unclass(attr(x, "index")) # unclass for speed
  checkNA.index(xindex) # index may not contain any NA
  eff.fac <- xindex[[eff.no]]
  res <- collapse::fsum(x, g = eff.fac, w = NULL, na.rm = na.rm, TRA = "replace")
  names(res) <- as.character(eff.fac)
  res <- add_pseries_features(res, attr(x, "index"))
  return(res)
}

Sum.matrix.collapse <- function(x, effect, ...) {
# print("Sum.matrix.collapse")
# browser()
  # if no index attribute, argument 'effect' is assumed to be a factor
  eff.fac <- if(is.null(xindex <- attr(x, "index"))) {
    effect
  } else {
    if(!is.character(effect) && length(effect) > 1L)
      stop("for matrices with index attributes, the effect argument must be a character")
    if(! effect %in% c("individual", "time", "group"))
      stop("irrelevant effect for a Sum transformation")
    eff.no <- switch(effect,
                     "individual" = 1L,
                     "time"       = 2L,
                     "group"      = 3L,
                     stop("unknown value of argument 'effect'"))
    xindex <- unclass(xindex) # unclass for speed
    checkNA.index(xindex) # index may not contain any NA
    xindex[[eff.no]]
  }
  # check for presence of na.rm in dots, if not present set to FALSE
  na.rm <- if(missing(...) || is.null(na.rm <- list(...)$na.rm)) FALSE else na.rm
  res <- collapse::fsum(x, g = eff.fac, w = NULL, na.rm = na.rm, drop = FALSE, TRA = "replace")
  rownames(res) <- as.character(eff.fac)
  attr(res, "index") <- NULL
  return(res)
}

#### B/between ####

# Need separate implementations of Between.pseries and between.pseries due to different NA handling

Between.default.collapse <- function(x, effect, ...) {
# print("Between.default.collapse")
# browser()
  
  # argument 'effect' is assumed to be a factor
  if(!is.numeric(x)) stop("The Between function only applies to numeric vectors")
  # check for presence of na.rm in dots, if not present set to FALSE
  na.rm <- if(missing(...) || is.null(na.rm <- list(...)$na.rm)) FALSE else na.rm
  nms <- as.character(effect)
  res <- collapse::fbetween(x, g = effect, w = NULL, na.rm = na.rm)
  names(res) <- nms
  return(res)
}

between.default.collapse <- function(x, effect, ...) {
# print("between.default.collapse")
# browser()
  
  # argument 'effect' is assumed to be a factor
  if(!is.numeric(x)) stop("The Between function only applies to numeric vectors")
  # check for presence of na.rm in dots, if not present set to FALSE
  na.rm <- if(missing(...) || is.null(na.rm <- list(...)$na.rm)) FALSE else na.rm
  res <- collapse::fbetween(x, g = effect, w = NULL, na.rm = na.rm, fill = TRUE)
  keep <- !duplicated(effect)
  res <- res[keep]
  names(res) <- as.character(effect[keep])
  # bring into factor level order (not order as appears in orig. data)
  lvl <- levels(collapse::fdroplevels(effect))
  res <- res[lvl]
  return(res)
}

Between.pseries.collapse <- function(x, effect = c("individual", "time", "group"), ...) {
# print("Between.pseries.collapse")
# browser()
  
  # translate arguments
  effect <- match.arg(effect)
  # check for presence of na.rm in dots, if not present set to FALSE
  na.rm <- if(missing(...) || is.null(na.rm <- list(...)$na.rm)) FALSE else na.rm
  eff.no <- switch(effect,
            "individual" = 1L,
            "time"       = 2L,
            "group"      = 3L,
            stop("unknown value of argument 'effect'"))
  
  xindex <- unclass(attr(x, "index")) # unclass for speed
  checkNA.index(xindex) # index may not contain any NA
  nms <- as.character(xindex[[eff.no]])
  na.x <- is.na(x)
  # must be fill = TRUE [to catch case when 1 obs of an individual is NA (otherwise result could contain non-intended NA)]
  res <- collapse::fbetween(x, effect = eff.no, w = NULL, na.rm = na.rm, fill = TRUE)
  names(res) <- nms
  res[na.x] <- NA
  return(res)
}

between.pseries.collapse <- function(x, effect = c("individual", "time", "group"), ...) {
# print("between.pseries.collapse")
# browser()
  effect <- match.arg(effect)
  # check for presence of na.rm in dots, if not present set to FALSE
  na.rm <- if(missing(...) || is.null(na.rm <- list(...)$na.rm)) FALSE else na.rm
  eff.no <-  switch(effect,
           "individual" = 1L,
           "time"       = 2L,
           "group"      = 3L,
           stop("unknown value of argument 'effect'"))
  
  xindex <- unclass(attr(x, "index")) # unclass for speed
  checkNA.index(xindex) # index may not contain any NA
  i <- xindex[[eff.no]]
  # must be fill = TRUE [to catch case when 1 obs of an individual is NA
  # (otherwise result could contain non-intended NA)]
  res <- collapse::fbetween(x, effect = eff.no, w = NULL, na.rm = na.rm, fill = TRUE)
  res <- remove_pseries_features(res)
  keep <- !duplicated(i)
  res <- res[keep]
  names(res) <- as.character(i[keep])
  # bring into factor level order (not order as appears in orig. data)
  lvl <- levels(collapse::fdroplevels(i))
  res <- res[lvl]
  return(res)
}



Between.matrix.collapse <- function(x, effect, ...) {
# print("Between.matrix.collapse")
# browser()
  # if no index attribute, argument 'effect' is assumed to be a factor
  eff.fac <- if(is.null(xindex <- attr(x, "index"))) {
    effect
  } else {
    if(!is.character(effect) && length(effect) > 1L)
      stop("for matrices with index attributes, the effect argument must be a character")
    if(! effect %in% c("individual", "time", "group"))
      stop("irrelevant effect for a between transformation")
    eff.no <- switch(effect,
                     "individual" = 1L,
                     "time"       = 2L,
                     "group"      = 3L,
                     stop("unknown value of argument 'effect'"))
    xindex <- unclass(xindex) # unclass for speed
    checkNA.index(xindex) # index may not contain any NA
    xindex[[eff.no]]
  }
  # check for presence of na.rm in dots, if not present set to FALSE
  na.rm <- if(missing(...) || is.null(na.rm <- list(...)$na.rm)) FALSE else na.rm
  na.x <- is.na(x)
  res <- collapse::fbetween(x, g = eff.fac, w = NULL, na.rm = na.rm, fill = TRUE)
  attr(res, "index") <- NULL
  rownames(res) <- as.character(eff.fac)
  res[na.x] <- NA
  return(res)
}

between.matrix.collapse <- function(x, effect, ...) {
# print("between.matrix.collapse")
# browser()
  # if no index attribute, argument 'effect' is assumed to be a factor
  eff.fac <- if(is.null(xindex <- attr(x, "index"))) {
    effect
  } else {
    if(!is.character(effect) && length(effect) > 1L)
      stop("for matrices with index attributes, the effect argument must be a character")
    if(! effect %in% c("individual", "time", "group"))
      stop("irrelevant effect for a between transformation")
    eff.no <- switch(effect,
                     "individual" = 1L,
                     "time"       = 2L,
                     "group"      = 3L,
                     stop("unknown value of argument 'effect'"))
    xindex <- unclass(xindex) # unclass for speed
    checkNA.index(xindex) # index may not contain any NA
    xindex[[eff.no]]
  }
  # check for presence of na.rm in dots, if not present set to FALSE
  na.rm <- if(missing(...) || is.null(na.rm <- list(...)$na.rm)) FALSE else na.rm
  res <- collapse::fbetween(x, g = eff.fac, w = NULL, na.rm = na.rm, fill = TRUE)
  rownames(res) <- as.character(eff.fac)
  # compress data to number of unique individuals (or time periods)
  res <- res[!duplicated(eff.fac), , drop = FALSE]
  # bring into factor level order (not order as appears in orig. data)
  lvl <- levels(collapse::fdroplevels(eff.fac))
  res <- res[lvl, , drop = FALSE]
  return(res)
}


#### Within ####
# Within - default

Within.default.collapse <- function(x, effect, ...) {
# print("Within.default.collapse")
# browser()
  
  # argument 'effect' is assumed to be a factor
  if(!is.numeric(x)) stop("the within function only applies to numeric vectors")
  # check for presence of na.rm in dots, if not present set to FALSE
  na.rm <- if(missing(...) || is.null(na.rm <- list(...)$na.rm)) FALSE else na.rm
  res <- collapse::fwithin(x, g = effect, w = NULL, na.rm = na.rm)
  # =(plm)= res <- x - Between(x, effect, ...)
  names(res) <- as.character(effect)
  return(res)
}


Within.pseries.collapse <- function(x, effect = c("individual", "time", "group", "twoways"), ...) {
# print("Within.pseries.collapse")
# browser()
  effect <- match.arg(effect)
  # check for presence of na.rm in dots, if not present set to FALSE
  na.rm <- if(missing(...) || is.null(na.rm <- list(...)$na.rm)) FALSE else na.rm
  xindex <- unclass(attr(x, "index")) # unclass for speed
  checkNA.index(xindex) # index may not contain any NA
  if(effect != "twoways") {
    eff.no <- switch(effect,
             "individual" = 1L,
             "time"       = 2L,
             "group"      = 3L,
             stop("unknown value of argument 'effect'"))
    res <- collapse::fwithin(x, effect = eff.no, w = NULL, na.rm = na.rm, mean = 0)
  } else {
    eff.ind.fac  <- xindex[[1L]]
    eff.time.fac <- xindex[[2L]]
    if(is.pbalanced(eff.ind.fac, eff.time.fac)) {
      # effect = "twoways" - balanced
      res <- collapse::fwithin(  x, effect = 1L, w = NULL, na.rm = na.rm, mean = "overall.mean") -
              collapse::fbetween(x, effect = 2L, w = NULL, na.rm = na.rm, fill = TRUE)
            # =(plm)= res <- x - Between(x, "individual", ...) - Between(x, "time", ...) + mean(x, ...)
    } else {
      # effect = "twoways" - unbalanced
      Dmu <- model.matrix(~ eff.time.fac - 1)
      W1   <- collapse::fwithin(x,   effect = 1L,          w = NULL, na.rm = na.rm, mean = 0) # pseries interface
      WDmu <- collapse::fwithin(Dmu, g      = eff.ind.fac, w = NULL, na.rm = na.rm, mean = 0) # matrix interface
      W2 <- lm.fit(WDmu, x)$fitted.values
      res <- W1 - W2
    }
  }
  return(res)
}

Within.matrix.collapse <- function(x, effect, ...) {
# print("Within.matrix.collapse")
# browser()
  
  # check for presence of na.rm in dots, if not present set to FALSE
  na.rm <- if(missing(...) || is.null(na.rm <- list(...)$na.rm)) FALSE else na.rm
  
  if(is.null(xindex <- attr(x, "index"))) {
    # non-index case, 'effect' needs to be a factor
    result <- collapse::fwithin(x, g = effect, w = NULL, na.rm = na.rm)
  }
  else {
    # index case
    xindex <- unclass(xindex) # unclass for speed
    checkNA.index(xindex) # index may not contain any NA
    
    if(effect != "twoways") {
      eff.fac <- switch(effect,
                       "individual" = xindex[[1L]],
                       "time"       = xindex[[2L]],
                       "group"      = xindex[[3L]],
                       stop("unknown value of argument 'effect'"))
      
      result <- collapse::fwithin(x, g = eff.fac, w = NULL, na.rm = na.rm, mean = 0)
      # =(plm)= result <- x - Between(x, effect)
      
    } else {
      # effect = "twoways"
      eff.ind.fac  <- xindex[[1L]]
      eff.time.fac <- xindex[[2L]]
      
      if(is.pbalanced(eff.ind.fac, eff.time.fac)) {
        # balanced twoways
        result <- collapse::fwithin(  x, g = eff.ind.fac,  w = NULL, na.rm = na.rm, mean = "overall.mean") -
                   collapse::fbetween(x, g = eff.time.fac, w = NULL, na.rm = na.rm, fill = TRUE)
        # =(plm)= result <- x - Between(x, "individual", ...) - Between(x, "time", ...) +
        #                        matrix(colMeans(x, ...), nrow = nrow(x), ncol = ncol(x), byrow = TRUE)
      }
      else { # unbalanced twoways
        # as factor is used twice below, make it a collapse::GRP object -> should give some speed-up
        eff.ind.fac <- collapse::GRP(eff.ind.fac, group.sizes = FALSE, return.groups = FALSE, call = FALSE)
        Dmu <- model.matrix(~ eff.time.fac - 1)
        W1   <- collapse::fwithin(x,   g = eff.ind.fac, w = NULL, na.rm = na.rm, mean = 0)
        WDmu <- collapse::fwithin(Dmu, g = eff.ind.fac, w = NULL, na.rm = na.rm, mean = 0)
        W2 <- lm.fit(WDmu, x)$fitted.values
        result <- W1 - W2
      }
    }
  }
  return(result)
}

#### These functions use collpase::fhdwithin (using internally fixest::demean)
#### or lfe::demeanlist respectively, for
#### the 2-way within transformation which are dramatically faster than
#### the implementation via separate collapse::fwithin calls (due to the special
#### algorithms used to partial out the fixed effects)
Within.pseries.collapse.fixest <- function(x, effect = c("individual", "time", "group", "twoways"), ...) {
# print("Within.pseries.collapse.fixest")
# browser()
  effect <- match.arg(effect)
  # check for presence of na.rm in dots, if not present set to FALSE
  na.rm <- if(missing(...) || is.null(na.rm <- list(...)$na.rm)) FALSE else na.rm
  xindex <- unclass(attr(x, "index")) # unclass for speed
  checkNA.index(xindex) # index may not contain any NA
  if(effect != "twoways") {
    eff.no <- switch(effect,
                     "individual" = 1L,
                     "time"       = 2L,
                     "group"      = 3L,
                     stop("unknown value of argument 'effect'"))
    # in 1-way case fwithin seems faster than fhdwithin, so keep 1-way and 2-way
    # cases separated
    res <- collapse::fwithin(x, effect = eff.no, w = NULL, na.rm = na.rm, mean = 0)
  } else {
    # effect = "twoways"

    # dispatches to pseries method
    res <- collapse::fhdwithin(x, effect = 1:2, w = NULL, na.rm = na.rm)
  }
  return(res)
}

Within.matrix.collapse.fixest <- function(x, effect, ...) {
# print("Within.matrix.collapse.fixest")
# browser()
  
  # check for presence of na.rm in dots, if not present set to FALSE
  na.rm <- if(missing(...) || is.null(na.rm <- list(...)$na.rm)) FALSE else na.rm
  
  if(is.null(xindex <- attr(x, "index"))) {
    # non-index case, 'effect' needs to be a factor
    result <- collapse::fwithin(x, g = effect, w = NULL, na.rm = na.rm)
  }
  else {
    # index case
    xindex <- unclass(xindex) # unclass for speed
    checkNA.index(xindex) # index may not contain any NA
    
    if(effect != "twoways") {
      eff.fac <- switch(effect,
                        "individual" = xindex[[1L]],
                        "time"       = xindex[[2L]],
                        "group"      = xindex[[3L]],
                        stop("unknown value of argument 'effect'"))
      
      ## result <- collapse::fhdwithin(x, eff.fac) # needs pkg fixest
      # --> for one-way effect, this seems slower than collapse::fwithin
      result <- collapse::fwithin(x, g = eff.fac, w = NULL, na.rm = na.rm, mean = 0)
      # =(plm)= result <- x - Between(x, effect)
    } else {
      # effect = "twoways"
      # no need to distinguish between balanced/unbalanced
      # as this is fully handled by collapse::fhdwithin()
      # collapse::fhdwithin needs pkg fixest as it uses fixest::demean
      result <- collapse::fhdwithin(x, fl = xindex[1:2], w = NULL, na.rm = na.rm)
    }
  }
  return(result)
}

Within.pseries.collapse.lfe <- function(x, effect = c("individual", "time", "group", "twoways"), ...) {
# print("Within.pseries.collapse.lfe")
# browser()

  effect <- match.arg(effect)
  xindex <- unclass(attr(x, "index"))
  checkNA.index(xindex) # index may not contain any NA
  # check for presence of na.rm in dots, if not present set to FALSE
  na.rm <- if(missing(...) || is.null(na.rm <- list(...)$na.rm)) FALSE else na.rm
  if(effect != "twoways") {
    eff.no <- switch(effect,
                     "individual" = 1L,
                     "time"       = 2L,
                     "group"      = 3L,
                     stop("unknown value of argument 'effect'"))
    # collapse::fwithin is faster in 1-ways case than lfe::demanlist, so
    # keep cases separated
    res <- collapse::fwithin(x, effect = eff.no, w = NULL, na.rm = na.rm, mean = 0)
  } else {
    # effect = "twoways"
    # no need to distinguish between balanced/unbalanced
    # as this is fully handled by lfe::dmeanlist()
      res <- unlist(lfe::demeanlist(x, fl = xindex[1:2], na.rm = na.rm))
      res <- add_pseries_features(res, attr(x, "index")) # index needs to be a proper pindex here!
    }
  return(res)
}

Within.matrix.collapse.lfe <- function(x, effect,  ...) {
# print("Within.matrix.collapse.lfe")
# browser()
  
  # check for presence of na.rm in dots, if not present set to FALSE
  na.rm <- if(missing(...) || is.null(na.rm <- list(...)$na.rm)) FALSE else na.rm
  
  if(is.null(xindex <- attr(x, "index"))) {
    # non-index case, 'effect' needs to be a factor
    result <- collapse::fwithin(x, g = effect, w = NULL, na.rm = na.rm)
  }
  else {
    # index case
    xindex <- unclass(xindex)
    checkNA.index(xindex) # index may not contain any NA
    
    if(effect != "twoways") {
      eff.fac <- switch(effect,
                        "individual" = xindex[[1L]],
                        "time"       = xindex[[2L]],
                        "group"      = xindex[[3L]],
                        stop("unknown value of argument 'effect'"))
      # collapse::fwithin is faster in 1-ways case than lfe::demanlist, so
      # keep cases separated
      result <- collapse::fwithin(x, g = eff.fac, w = NULL, na.rm = na.rm, mean = 0)
      # =(plm)= result <- x - Between(x, effect)
    } else {
      # effect = "twoways"
      # no need to distinguish between balanced/unbalanced
      # as this is fully handled by lfe::dmeanlist()
      #
      # lfe::demeanlist (lfe vers. 2.8-6) return value for matrix input is
      # inconsistent / depends on value of argument na.rm,
      # see https://github.com/sgaure/lfe/issues/50.
      result <- lfe::demeanlist(x, fl = xindex[1:2], na.rm = na.rm)
      if(is.list(result)) result <- result[[1L]]
      attr(result, "index") <- attr(x, "index") # index needs to be a proper pindex here!
    }
  }
  return(result)
}

#### wrapper for pseriesfy ####
# both pseriesfy functions are in file tool_pdata.frame.R 
pseriesfy <- function(x,  ...) {
  if(!isTRUE(getOption("plm.fast"))) {
    pseriesfy.baseR(x, ...) } else {
      if(!isTRUE(getOption("plm.fast.pkg.collapse"))) stop(txt.no.collapse, call. = FALSE)
      pseriesfy.collapse(x, ...) }
}

.onAttach <- function(libname, pkgname) {
  options("plm.fast" = TRUE) # since 2.6: needs pkg collapse as hard dependency
  
  # determine when pkg plm is attached whether pkg collapse, fixest, and lfe are
  # available and set (non-documented) options, which packages are available.
  # These options are used to determine in the wrappers if fast mode can be used
  # and if the speed up by fixest or lfe for the 2-way FE case can be used.
  avail.collapse <- requireNamespace("collapse", quietly = TRUE)
  avail.fixest   <- requireNamespace("fixest",   quietly = TRUE)
  avail.lfe      <- requireNamespace("lfe",      quietly = TRUE)
  
  if(avail.collapse) {
    options("plm.fast.pkg.collapse" = TRUE)
    options("plm.fast.pkg.FE.tw" = "collapse")
    # fixest wins over lfe
    if(avail.fixest) {
      options("plm.fast.pkg.FE.tw" = "fixest")
    } else {
      if(avail.lfe) {
        options("plm.fast.pkg.FE.tw" = "lfe")
      }
    }
  }
  else options("plm.fast.pkg.collapse" = FALSE)
}


#' Option to Switch On/Off Fast Data Transformations
#' 
#' A significant speed up can be gained by using fast (panel) data transformation
#' functions from package `collapse`. 
#' An additional significant speed up for the two-way fixed effects case can be
#' achieved if package `fixest` or `lfe` is installed (package `collapse`
#' needs to be installed for the fast mode in any case).
#' 
#' @details By default, this speed up is enabled.
#' Option `plm.fast` can be used to enable/disable the speed up. The option is
#' evaluated prior to execution of supported transformations (see below), so 
#' `option("plm.fast" = TRUE)` enables the speed up while 
#' `option("plm.fast" = FALSE)` disables the speed up.
#' 
#' To have it always switched off, put `options("plm.fast" = FALSE)` in your 
#' .Rprofile file.
#' 
#' See **Examples** for how to use the option and for a benchmarking example.
#'
#' For long, package `plm` used base R implementations and R-based code. The
#' package `collapse` provides fast data transformation functions written
#' in C/C++, among them some especially suitable for panel data.
#' Having package `collapse` installed is a requirement for the speed up, so
#' this package is a hard dependency for package `plm`.
#' 
#' Availability of packages `fixest` and `lfe` is checked for once when
#' package plm is attached and the additional speed up for the two-way fixed
#' effect case is enabled automatically (`fixest` wins over `lfe`),
#' given one of the packages is detected and `options("plm.fast" = TRUE)` 
#' (default) is set. If so, the packages' fast algorithms to partial out fixed 
#' effects are #' used (`fixest::demean` (via `collapse::fhdwithin`), 
#' `lfe::demeanlist`). Both packages are 'Suggests' dependencies.
#' 
#' Users might experience neglectable numerical differences between enabled and
#' disabled fast mode and base R implementation, depending on the platform and 
#' the additional packages installed.
#' 
#' Currently, these basic functions benefit from the speed-up, used as building 
#' blocks in most model estimation functions, e.g., in `plm` (more functions are
#' under investigation):
#' \itemize{
#'   \item between,
#'   \item Between,
#'   \item Sum,
#'   \item Within,
#'   \item pseriesfy.
#' }
#' 
#' @name plm.fast
#' @importFrom collapse fhdwithin fwithin fbetween dapply fdroplevels
#' @keywords sysdata manip
#' @examples
#' \dontrun{
#' ### A benchmark of plm without and with speed-up
#' library("plm")
#' library("collapse")
#' library("microbenchmark")
#' rm(list = ls())
#' data("wlddev", package = "collapse")
#' form <- LIFEEX ~ PCGDP + GINI
#'
#' # produce big data set (taken from collapse's vignette)
#' wlddevsmall <- get_vars(wlddev, c("iso3c","year","OECD","PCGDP","LIFEEX","GINI","ODA"))
#' wlddevsmall$iso3c <- as.character(wlddevsmall$iso3c)
#' data <- replicate(100, wlddevsmall, simplify = FALSE)
#' rm(wlddevsmall)
#' uniquify <- function(x, i) {
#'   x$iso3c <- paste0(x$iso3c, i)
#'   x
#' }
#' data <- unlist2d(Map(uniquify, data, as.list(1:100)), idcols = FALSE)
#' data <- pdata.frame(data, index = c("iso3c", "year"))
#' pdim(data) # Balanced Panel: n = 21600, T = 59, N = 1274400 // but many NAs
#' # data <- na.omit(data)
#' # pdim(data) # Unbalanced Panel: n = 13300, T = 1-31, N = 93900
#'
#' times <- 1 # no. of repetitions for benchmark - this takes quite long!
#' 
#' onewayFE <- microbenchmark(
#'  {options("plm.fast" = FALSE); plm(form, data = data, model = "within")},
#'  {options("plm.fast" = TRUE);  plm(form, data = data, model = "within")},
#'   times = times, unit = "relative")
#'
#' summary(onewayFE)
#'
#' ## two-ways FE benchmark requires pkg fixest and lfe
#' ## (End-users shall only set option plm.fast. Option plm.fast.pkg.FE.tw shall
#' ##  _not_ be set by the end-user, it is determined automatically when pkg plm
#' ## is attached; however, it needs to be set explicitly in this example for the
#' ## benchmark.)
#' if(requireNamespace("fixest", quietly = TRUE) &&
#'    requireNamespace("lfe", quietly = TRUE)) {
#' 
#' twowayFE <-  microbenchmark(
#'  {options("plm.fast" = FALSE);
#'     plm(form, data = data, model = "within", effect = "twoways")},
#'  {options("plm.fast" = TRUE, "plm.fast.pkg.FE.tw" = "collapse");
#'     plm(form, data = data, model = "within", effect = "twoways")},
#'  {options("plm.fast" = TRUE, "plm.fast.pkg.FE.tw" = "fixest");
#'     plm(form, data = data, model = "within", effect = "twoways")},
#'  {options("plm.fast" = TRUE, "plm.fast.pkg.FE.tw" = "lfe");
#'     plm(form, data = data, model = "within", effect = "twoways")},
#'   times = times, unit = "relative")
#'
#' summary(twowayFE)
#' }
#' 
#' onewayRE <- microbenchmark(
#'  {options("plm.fast" = FALSE); plm(form, data = data, model = "random")},
#'  {options("plm.fast" = TRUE);  plm(form, data = data, model = "random")},
#'   times = times, unit = "relative")
#'
#' summary(onewayRE)
#' 
#' twowayRE <-  microbenchmark(
#'  {options("plm.fast" = FALSE); plm(form, data = data, model = "random", effect = "twoways")},
#'  {options("plm.fast" = TRUE);  plm(form, data = data, model = "random", effect = "twoways")},
#'   times = times, unit = "relative")
#' 
#' summary(twowayRE)
#' }
NULL


txt.no.collapse <- paste0("options(\"plm.fast\") is set to TRUE but package 'collapse' ",
                          "is not available which is needed for fast data transformation functions. ",
                          "Either set 'options(\"plm.fast\" = FALSE)' or install the ",
                          "missing package, e.g., with 'install.packages(\"collapse\")'. \n",
                          "Having additionally package 'fixest' or 'lfe' installed ",
                          "will speed up the two-way fixed effect case further. \n",
                          "Availability of packages is determined only when ",
                          "plm is attached, so restart R/reload plm when mentioned ",
                          "packages have been installed.")
