residuals_overall_exp.plm <- function(x, ...) { #### experimental, non-exported function
# residuals_overall.plm: gives the residuals of the "overall"/outer model for all types of plm models.
# In the future, this could be integrated with residuals.plm by some argument, e.g. overall = FALSE (default).
# see also test file tests/test_residuals_overall_fitted_exp.R
  
  # no na.action eval yet
  
  model <- describe(x, "model")
  
  if (model == "ht") stop("model \"ht\" not (yet?) supported")
  
  # for all effects of within models: residuals of (quasi-)demeaned (inner) model
  # are also the residuals of the "overall" model
  if (model == "random") {
    # get untransformed data to calculate overall residuals
    X <- model.matrix(x, model = "pooling")
    y <- pmodel.response(x, model = "pooling")
    # take care of any aliased coefficients:
    # they are not in x$coefficients but assoc. variables are still present in model.matrix
    if (any(x$aliased, na.rm = TRUE)) { # na.rm = TRUE because currently, RE tw unbalanced models set aliased differently
      X <- X[ , !x$aliased, drop = FALSE]
    }
    
    est <- as.numeric(tcrossprod(coef(x), X))
    res <- y - est
    names(res) <- rownames(X)

    # make residuals a pseries
    res <- structure(res, index = index(x), class = c("pseries", class(res)))
      
  } else { # all plm models except random (and also except ht)
    res <- residuals(x)
  }
  return(res)
}

residuals_overall_e_exp <- function(object) { ### experimental non-exported function
  ## residuals of "overall" RE model minus random effects (=e_it)
  ## e.g.: two-way model: residual_overall_it = random_component_individual_i + random_component_time_t + e_it
  model <- describe(object, "model")
  if (model != "random") stop("only for random effect models")
  obj.eff <- describe(object, "effect")
  res_ov <- residuals_overall_exp.plm(object)
  if (obj.eff == "twoways") {
    res_ov_e <- res_ov - ranef(object, "individual")[index(object, "id")] - ranef(object, "time")[index(object, "time")]
  } else {
    res_ov_e <- res_ov - ranef(object)[index(object, ifelse(obj.eff == "individual", "id", "time"))]
  }
  names(res_ov_e) <- names(res_ov)
  return(res_ov_e)
}

fitted_exp.plm <- function(x, ...) { #### experimental, non-exported function
# fitted_exp.plm: gives the fitted values of all types of plm models by substracting the overall
#                 residuals from the untransformed response variable; does not have
#                 a model argument so it is not as versatile as 'fitted.plm' below.
# see also test file tests/test_residuals_overall_fitted_exp.R
  model <- describe(x, "model")
  res <- residuals_overall_exp.plm(x)
  
  # For "between" and "fd" models, the number of fitted values is not equal to the
  # number of original observations. Thus, model.frame cannot be used but rather
  # pmodel.response because it has the right length. However, pmodel.response
  # shall not be used for the other models because we want the untransformed data.
  if (model %in% c("between", "fd")) {
    y <- pmodel.response(x)
  } else {
    y <- model.frame(x)[ , 1]
  }
  return(y - res)
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

# check_propagation_correct_class: helper function
# Function checks if the class and storage mode (type) of an object match 
# and corrects its class attribute if not
#
# A mismatch can occur if a pseries of lower class and type logical or integer
# are propagated to higher type by an arithmetic operation as R's arithmetic
# operations do not change the first value of class attribute for
# c("pseries", "logical/integer"). However, using groupGenerics as wrapper around
# pseries objects, this does not happen anymore.
# E.g.
#  x <- c(1L, 2L, 3L)
#  x + 1.5
# results in class propagation from class "integer" to "numeric" 
# but not if x is of class c("myclass", "integer")
check_propagation_correct_class <- function(x) {
  # x: a pseries object (usually)
  if (any((pos <- inherits(x, c("logical" ,"integer", "numeric"), which = TRUE)) > 0)) {
    pos <- pos[pos > 0] # non-matches in inherits(..., which = TRUE) results in 0
    switch(typeof(x),
           "double"  = { attr(x, "class")[pos] <- "numeric" },
           "integer" = { attr(x, "class")[pos] <- "integer" },
           "complex" = { attr(x, "class")[pos] <- "complex" }
    )
  }
  return(x)
}

