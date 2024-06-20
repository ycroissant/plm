## Structural changes made to plm's original data transformation functions
## need to be mimicked in the *.collapse(.*) versions and vice versa.

## 1) Give the base-R version of the functions defined in tool_transformations.R
##    a new name (*.baseR).
## 2) Implement wrapper switched which call the *.baseR or *.collapse versions
##    based on the option plm.fast (a logical, can be set via R's regular option
##    mechanism: options("plm.fast" = TRUE).

## ad 1) new name for base R functions defined in tool_transformations.R
Sum_default_baseR <- plm:::Sum.default
Sum_pseries_baseR <- plm:::Sum.pseries
Sum_matrix_baseR  <- plm:::Sum.matrix

between_default_baseR <- plm:::between.default
between_pseries_baseR <- plm:::between.pseries
between_matrix_baseR  <- plm:::between.matrix

Between_default_baseR <- plm:::Between.default
Between_pseries_baseR <- plm:::Between.pseries
Between_matrix_baseR  <- plm:::Between.matrix

Within_default_baseR  <- plm:::Within.default
Within_pseries_baseR <- plm:::Within.pseries
Within_matrix_baseR   <- plm:::Within.matrix

pdiff_baseR           <- plm:::pdiff

pseriesfy_baseR       <- plm:::pseriesfy # ... in tool_pdata.frame.R:

## lag, lead, diff: are collapse-powered but in file tool_transformations.R (incl. their wrappers).

## ad 2) implement wrapper switches

#### Sum wrapper switches ####
Sum.default <- function(x, effect, ...) {
  if(!isTRUE(getOption("plm.fast"))) {
    Sum_default_baseR(x, effect, ...) } else {
    if(!isTRUE(getOption("plm.fast.pkg.collapse"))) stop(txt.no.collapse, call. = FALSE)
    Sum_default_collapse(x, effect, ...) }
}

Sum.pseries <- function(x, effect = c("individual", "time", "group"), ...) {
  if(!isTRUE(getOption("plm.fast"))) {
    Sum_pseries_baseR(x, effect, ...) } else {
    if(!isTRUE(getOption("plm.fast.pkg.collapse"))) stop(txt.no.collapse, call. = FALSE)
    Sum_pseries_collapse(x, effect, ...) }
}

Sum.matrix <- function(x, effect, ...) {
  if(!isTRUE(getOption("plm.fast"))) {
    Sum_matrix_baseR(x, effect, ...) } else {
    if(!isTRUE(getOption("plm.fast.pkg.collapse"))) stop(txt.no.collapse, call. = FALSE)
    Sum_matrix_collapse(x, effect, ...) }
}

#### Between wrapper switches ####
Between.default <- function(x, effect, ...) {
  if(!isTRUE(getOption("plm.fast"))) {
    Between_default_baseR(x, effect, ...) } else {
    if(!isTRUE(getOption("plm.fast.pkg.collapse"))) stop(txt.no.collapse, call. = FALSE)
    Between_default_collapse(x, effect, ...) }
}
  
Between.pseries <- function(x, effect = c("individual", "time", "group"), ...) {
  if(!isTRUE(getOption("plm.fast"))) {
    Between_pseries_baseR(x, effect, ...) } else {
    if(!isTRUE(getOption("plm.fast.pkg.collapse"))) stop(txt.no.collapse, call. = FALSE)
    Between_pseries_collapse(x, effect, ...) }
}

Between.matrix <- function(x, effect, ...) {
  if(!isTRUE(getOption("plm.fast"))) {
    Between_matrix_baseR(x, effect, ...) } else {
    if(!isTRUE(getOption("plm.fast.pkg.collapse"))) stop(txt.no.collapse, call. = FALSE)
    Between_matrix_collapse(x, effect, ...) }
}

#### between wrapper switches ####
between.default <- function(x, effect, ...) {
  if(!isTRUE(getOption("plm.fast"))) {
     between_default_baseR(x, effect, ...) } else {
     if(!isTRUE(getOption("plm.fast.pkg.collapse"))) stop(txt.no.collapse, call. = FALSE)
     between_default_collapse(x, effect, ...) }
}

between.pseries <- function(x, effect = c("individual", "time", "group"), ...) {
  if(!isTRUE(getOption("plm.fast"))) {
    between_pseries_baseR(x, effect, ...) } else {
    if(!isTRUE(getOption("plm.fast.pkg.collapse"))) stop(txt.no.collapse, call. = FALSE)
    between_pseries_collapse(x, effect, ...) }
}

between.matrix <- function(x, effect, ...) {
  if(!isTRUE(getOption("plm.fast"))) {
    between_matrix_baseR(x, effect, ...) } else {
    if(!isTRUE(getOption("plm.fast.pkg.collapse"))) stop(txt.no.collapse, call. = FALSE)
    between_matrix_collapse(x, effect, ...) }
}

#### Within wrapper switches ####
Within.default <- function(x, effect, ...) {
  if(!isTRUE(getOption("plm.fast"))) {
    Within_default_baseR (x, effect, ...) } else {
    if(!isTRUE(getOption("plm.fast.pkg.collapse"))) stop(txt.no.collapse, call. = FALSE)
    Within_default_collapse(x, effect, ...) }
}

Within.pseries <- function(x, effect = c("individual", "time", "group", "twoways"), ...) {
  if(!isTRUE(getOption("plm.fast"))) {
    Within_pseries_baseR(x, effect, ...)
  } else {
    if(!isTRUE(getOption("plm.fast.pkg.collapse"))) stop(txt.no.collapse, call. = FALSE)

    if(is.null(getOption("plm.fast.pkg.FE.tw"))) options("plm.fast.pkg.FE.tw" = "collapse")
    switch(getOption("plm.fast.pkg.FE.tw"),
           "collapse" = Within_pseries_collapse(       x, effect, ...),  # collapse only,
           "fixest"   = Within_pseries_collapse_fixest(x, effect, ...),  # collapse for 1-way FE + fixest for 2-way FE,
           "lfe"      = Within_pseriesc_collapse_lfe(   x, effect, ...), # collapse for 1-way FE + lfe    for 2-way FE
           stop("unknown value of option 'plm.fast.pkg.FE.tw'"))
  }
}

Within.matrix <- function(x, effect, ...) {
  if(!isTRUE(getOption("plm.fast"))) {
    Within_matrix_baseR(x, effect, ...) 
  } else {
    if (!isTRUE(getOption("plm.fast.pkg.collapse"))) stop(txt.no.collapse, call. = FALSE)
    
    if(is.null(getOption("plm.fast.pkg.FE.tw"))) options("plm.fast.pkg.FE.tw" = "collapse")
    switch(getOption("plm.fast.pkg.FE.tw"),
           "collapse" = Within_matrix_collapse(       x, effect, ...), # collapse only,
           "fixest"   = Within_matrix_collapse_fixest(x, effect, ...), # collapse for 1-way FE + fixest for 2-way FE,
           "lfe"      = Within_matrix_collapse_lfe(   x, effect, ...), # collapse for 1-way FE + lfe    for 2-way FE
           stop("unknown value of option 'plm.fast.pkg.FE.tw'"))
  }
}

#### wrapper for pdiff ####
pdiff <- function(x, effect = c("individual", "time"), has.intercept = FALSE,
                  shift = c("time", "row")) {
  if(!isTRUE(getOption("plm.fast"))) {
    pdiff_baseR(x, effect, has.intercept, shift) } else {
      if(!isTRUE(getOption("plm.fast.pkg.collapse"))) stop(txt.no.collapse, call. = FALSE)
      pdiff.collapse(x, effect, has.intercept, shift) }
}


#### wrapper for pseriesfy ####
# both pseriesfy functions are in file tool_pdata.frame.R 
pseriesfy <- function(x,  ...) {
  if(!isTRUE(getOption("plm.fast"))) {
    pseriesfy_baseR(x, ...) } else {
      if(!isTRUE(getOption("plm.fast.pkg.collapse"))) stop(txt.no.collapse, call. = FALSE)
      pseriesfy.collapse(x, ...) }
}


#### Sum ####

Sum_default_collapse <- function(x, effect, ...) {
# print("Sum_default_collapse")
# browser()
  # argument 'effect' is assumed to be a factor

  if(!is.numeric(x)) stop("The Sum function only applies to numeric vectors")
  # check for presence of na.rm in dots, if not present set to FALSE
  na.rm <- if(missing(...) || is.null(na.rm <- list(...)$na.rm)) FALSE else na.rm
  res <- collapse::fsum(x, g = effect, w = NULL, na.rm = na.rm, TRA = "replace")
  names(res) <- as.character(effect)
  return(res)
}

Sum_pseries_collapse <- function(x, effect = c("individual", "time", "group"), ...) {
# print("Sum_pseries_collapse")
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

Sum_matrix_collapse <- function(x, effect, ...) {
# print("Sum_matrix_collapse")
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

Between_default_collapse <- function(x, effect, ...) {
# print("Between_default_collapse")
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

between_default_collapse <- function(x, effect, ...) {
# print("between_default_collapse")
# browser()
  
  # argument 'effect' is assumed to be a factor
  if(!is.numeric(x)) stop("The Between function only applies to numeric vectors")
  # check for presence of na.rm in dots, if not present set to FALSE
  na.rm <- if(missing(...) || is.null(na.rm <- list(...)$na.rm)) FALSE else na.rm
  res <- collapse::fbetween(x, g = effect, w = NULL, na.rm = na.rm, fill = TRUE)
  keep <- !collapse::fduplicated(effect, all = FALSE)
  res <- res[keep]
  names(res) <- as.character(effect[keep])
  # bring into factor level order (not order as appears in orig. data)
  lvl <- levels(collapse::fdroplevels(effect))
  res <- res[lvl]
  return(res)
}

Between_pseries_collapse <- function(x, effect = c("individual", "time", "group"), ...) {
# print("Between_pseries_collapse")
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

between_pseries_collapse <- function(x, effect = c("individual", "time", "group"), ...) {
# print("between_pseries_collapse")
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
  keep <- !collapse::fduplicated(i, all = FALSE)
  res <- res[keep]
  names(res) <- as.character(i[keep])
  # bring into factor level order (not order as appears in orig. data)
  lvl <- levels(collapse::fdroplevels(i))
  res <- res[lvl]
  return(res)
}



Between_matrix_collapse <- function(x, effect, ...) {
# print("Between_matrix_collapse")
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

between_matrix_collapse <- function(x, effect, ...) {
# print("between_matrix_collapse")
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
  res <- res[!collapse::fduplicated(eff.fac, all = FALSE), , drop = FALSE]
  # bring into factor level order (not order as appears in orig. data)
  lvl <- levels(collapse::fdroplevels(eff.fac))
  res <- res[lvl, , drop = FALSE]
  return(res)
}


#### Within ####
# Within - default

Within_default_collapse <- function(x, effect, ...) {
# print("Within_default_collapse")
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


Within_pseries_collapse <- function(x, effect = c("individual", "time", "group", "twoways"), ...) {
# print("Within_pseries_collapse")
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

Within_matrix_collapse <- function(x, effect, ...) {
# print("Within_matrix_collapse")
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
Within_pseries_collapse_fixest <- function(x, effect = c("individual", "time", "group", "twoways"), ...) {
# print("Within_pseries_collapse_fixest")
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

Within_matrix_collapse_fixest <- function(x, effect, ...) {
# print("Within_matrix_collapse_fixest")
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

Within_pseriesc_collapse_lfe <- function(x, effect = c("individual", "time", "group", "twoways"), ...) {
# print("Within_pseriesc_collapse_lfe")
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

Within_matrix_collapse_lfe <- function(x, effect,  ...) {
# print("Within_matrix_collapse_lfe")
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

pdiff.collapse <- function(x, effect = c("individual", "time"), has.intercept = FALSE, shift = c("time", "row")){
  # NB: x is assumed to have an index attribute
  #     can check with has.index(x)
  # TODO: pdiff's usage in model.matrix is not very elegant as pdiff does its own
  #     removal of constant columns and intercept handling which could be handled
  #     via model.matrix.
  
  effect <- match.arg(effect)
  shift <- match.arg(shift)
  xindex <- unclass(attr(x, "index"))
  checkNA.index(xindex) # index may not contain any NA

  if(shift == "row") {
    eff.no <- switch(effect,
                     "individual" = 1L,
                     "time"       = 2L,
                     stop("unknown value of argument 'effect'"))
    
    eff.fac <- xindex[[eff.no]]
    
    if(inherits(x, "pseries")) x <- remove_pseries_features(x)
    res <- collapse::fdiff(x, g = eff.fac)
    
  } else {
    # shift = "time"
    
    # make a pdata.frame the dirty way (esp. to keep names like "(Intercept)")
    # .. works as x is already ensured to be panel-stacked
    # and apply collapse::fdiff on it
    if(is.matrix(x)) {
      x.pdf <- as.data.frame(x)
      class(x.pdf) <- c("pdata.frame", class(x.pdf))
      attr(x.pdf, "index") <- attr(x, "index")
      
      res <- collapse::fdiff(x.pdf)
      res <- as.matrix(res)  
    } else {
      # pseries case
      res <- collapse::fdiff(x)
    }
  }

  ## last data preparation before return
  res <- na.omit(res)
  if(is.matrix(x)) {
    # original pdiff (coded in base R) removes constant columns in matrix, 
    # so do likewise collapse-powered version
    cst.col <- apply(res, 2, is.constant)
    res <- res[ , !cst.col, drop = FALSE]
    
    # if intercept is requested, set intercept column to 1 as it was 
    # diff'ed out by collapse::fdiff and anyways removed by the removal of
    # constant columns just above
    if(has.intercept){
      res <- cbind(1, res)
      colnames(res)[1L] <- "(Intercept)"
    }
  }
  res
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
#' effects are used (`fixest::demean` (via `collapse::fhdwithin`), 
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
#'   \item lag, lead, and diff,
#'   \item pseriesfy,
#'   \item pdiff (internal function).
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
#'   times = times)
#'
#' summary(onewayFE, unit = "relative")
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
#'   times = times)
#'
#' summary(twowayFE, unit = "relative")
#' }
#' 
#' onewayRE <- microbenchmark(
#'  {options("plm.fast" = FALSE); plm(form, data = data, model = "random")},
#'  {options("plm.fast" = TRUE);  plm(form, data = data, model = "random")},
#'   times = times)
#'
#' summary(onewayRE, unit = "relative")
#' 
#' twowayRE <-  microbenchmark(
#'  {options("plm.fast" = FALSE); plm(form, data = data, model = "random", effect = "twoways")},
#'  {options("plm.fast" = TRUE);  plm(form, data = data, model = "random", effect = "twoways")},
#'   times = times)
#' 
#' summary(twowayRE, unit = "relative")
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
