## Compute the individual and/or time effects for panel model. plm
## methods for the fixef and ranef generics of the nlme
## package. print, summary and print.summary methods are provided for
## fixef objects.
## The within_intercept.plm function computes the overall intercept of
## within fitted models.



#' @title
#' Extract the Fixed Effects
#' 
#' @description 
#' Function to extract the fixed effects from a `plm` object and
#' associated summary method.
#' 
#' @details
#' Function `fixef` calculates the fixed effects and returns an object
#' of class `c("fixef", "numeric")`. By setting the `type` argument,
#' the fixed effects may be returned in levels (`"level"`), as
#' deviations from the first value of the index (`"dfirst"`), or as
#' deviations from the overall mean (`"dmean"`). If the argument
#' `vcov` was specified, the standard errors (stored as attribute "se"
#' in the return value) are the respective robust standard errors.
#' For two-way fixed-effect models, argument `effect` controls which
#' of the fixed effects are to be extracted: `"individual"`, `"time"`, or 
#' the sum of individual and time effects (`"twoways"`).
#' NB: See **Examples** for how the sum of effects can be split in an individual
#' and a time component.
#' For one-way models, the effects of the model are extracted and the
#' argument `effect` is disrespected.
#' 
#' The associated `summary` method returns an extended object of class
#' `c("summary.fixef", "matrix")` with more information (see sections
#' **Value** and **Examples**).
#' 
#' References with formulae (except for the two-ways unbalanced case)
#' are, e.g., \insertCite{GREE:12;textual}{plm}, Ch. 11.4.4, p. 364,
#' formulae (11-25); \insertCite{WOOL:10;textual}{plm}, Ch. 10.5.3,
#' pp. 308-309, formula (10.58).
#' @name fixef.plm
#' @aliases fixef
#' @param x,object an object of class `"plm"`, an object of class
#'     `"fixef"` for the `print` and the `summary` method,
#' @param effect one of `"individual"`, `"time"`, or `"twoways"`, only relevant in
#'     case of two--ways effects models (where it defaults to `"individual"`),
#' @param vcov a variance--covariance matrix furnished by the user or
#'     a function to calculate one (see **Examples**),
#' @param type one of `"level"`, `"dfirst"`, or `"dmean"`,
#' @param digits digits,
#' @param width the maximum length of the lines in the print output,
#' @param \dots further arguments.
#' @return For function `fixef`, an object of class `c("fixef", "numeric")`
#'     is returned: It is a numeric vector containing
#'     the fixed effects with attribute `se` which contains the
#'     standard errors. There are two further attributes: attribute
#'     `type` contains the chosen type (the value of argument `type`
#'     as a character); attribute `df.residual` holds the residual
#'     degrees of freedom (integer) from the fixed effects model (plm
#'     object) on which `fixef` was run. For the two-way unbalanced case, only
#'     attribute `type` is added.
#'     
#' For function `summary.fixef`, an object of class
#' `c("summary.fixef", "matrix")` is returned: It is a matrix with four
#' columns in this order: the estimated fixed effects, their standard
#' errors and associated t--values and p--values. 
#' For the two-ways unbalanced case, the matrix contains only the estimates.
#' The type of the fixed effects and the standard errors in the 
#' summary.fixef object correspond to was requested in the `fixef`
#' function by arguments `type` and `vcov`, respectively.
#'     
#' @author Yves Croissant
#' @seealso [within_intercept()] for the overall intercept of fixed
#'     effect models along its standard error, [plm()] for plm objects
#'     and within models (= fixed effects models) in general. See
#'     [ranef()] to extract the random effects from a random effects
#'     model.
#' @references \insertAllCited{}
#' @keywords regression
#' @examples
#' 
#' data("Grunfeld", package = "plm")
#' gi <- plm(inv ~ value + capital, data = Grunfeld, model = "within")
#' fixef(gi)
#' summary(fixef(gi))
#' summary(fixef(gi))[ , c("Estimate", "Pr(>|t|)")] # only estimates and p-values
#' 
#' # relationship of type = "dmean" and "level" and overall intercept
#' fx_level <- fixef(gi, type = "level")
#' fx_dmean <- fixef(gi, type = "dmean")
#' overallint <- within_intercept(gi)
#' all.equal(overallint + fx_dmean, fx_level, check.attributes = FALSE) # TRUE
#' 
#' # extract time effects in a twoways effects model
#' gi_tw <- plm(inv ~ value + capital, data = Grunfeld,
#'           model = "within", effect = "twoways")
#' fixef(gi_tw, effect = "time")
#' 
#' # with supplied variance-covariance matrix as matrix, function,
#' # and function with additional arguments
#' fx_level_robust1 <- fixef(gi, vcov = vcovHC(gi))
#' fx_level_robust2 <- fixef(gi, vcov = vcovHC)
#' fx_level_robust3 <- fixef(gi, vcov = function(x) vcovHC(x, method = "white2"))
#' summary(fx_level_robust1) # gives fixed effects, robust SEs, t- and p-values
#' 
#' # calc. fitted values of oneway within model:
#' fixefs <- fixef(gi)[index(gi, which = "id")]
#' fitted_by_hand <- fixefs + gi$coefficients["value"]   * gi$model$value +
#'                            gi$coefficients["capital"] * gi$model$capital
#'
#' # calc. fittes values of twoway unbalanced within model via effects:
#' gtw_u <- plm(inv ~ value + capital, data = Grunfeld[-200, ], effect = "twoways")
#' yhat <- as.numeric(gtw_u$model[ , 1] - gtw_u$residuals) # reference
#' pred_beta <- as.numeric(tcrossprod(coef(gtw_u), as.matrix(gtw_u$model[ , -1])))
#' pred_effs <- as.numeric(fixef(gtw_u, "twoways")) # sum of ind and time effects
#' all.equal(pred_effs + pred_beta, yhat) # TRUE
#' 
#' # Splits of summed up individual and time effects:
#' # use one "level" and one "dfirst"
#' ii <- index(gtw_u)[[1L]]; it <- index(gtw_u)[[2L]]
#' eff_id_dfirst <- c(0, as.numeric(fixef(gtw_u, "individual", "dfirst")))[ii]
#' eff_ti_dfirst <- c(0, as.numeric(fixef(gtw_u, "time",       "dfirst")))[it]
#' eff_id_level <- as.numeric(fixef(gtw_u, "individual"))[ii]
#' eff_ti_level <- as.numeric(fixef(gtw_u, "time"))[it]
#' 
#' all.equal(pred_effs, eff_id_level  + eff_ti_dfirst) # TRUE
#' all.equal(pred_effs, eff_id_dfirst + eff_ti_level)  # TRUE
#'
#' @importFrom nlme fixef
#' @export fixef
NULL

#' @rdname fixef.plm
#' @importFrom stats weighted.mean
#' @export
fixef.plm <- function(object, effect = NULL,
                      type = c("level", "dfirst", "dmean"),
                      vcov = NULL, ...){
    
    model.effect <- describe(object, "effect")
    if(is.null(effect)){
        # default for twoway model to individual effect
        effect <- switch(model.effect,
                           "individual" = "individual",
                           "time"       = "time",
                           "twoways"    = "individual")
    }
    else{
        if(model.effect != "twoways" && model.effect != effect) stop("wrong effect argument")
        if(!effect %in% c("individual", "time", "twoways")) stop("wrong effect argument")
    }
    
    type <- match.arg(type)
    if(!is.null(object$call)){
        if(describe(object, "model") != "within")
          stop("fixef is relevant only for within models")
    }
    formula <- formula(object)
    data <- model.frame(object)
    pdim <- pdim(object)
    # the between model may contain time independent variables, the
    # within model doesn't. So select the relevant elements using nw
    # (names of the within variables)
    nw <- names(coef(object))
    
    # For procedure to get the individual/time effects by multiplying the within
    # estimates with the between-ed data, see, e.g.:
    #  Wooldridge (2010), Econometric Analysis of Cross Section and Panel Data, 2nd ed., 
    #                     Ch. 10.5.3, pp. 308-309, formula (10.58)
    #  Greene (2012), Econometric Analysis,
    #                 Ch. 11.4.4, p. 364, formulae (11-25)
    #
    # NB: These textbook formulae do not give the correct results in the two-ways unbalanced case,
    #     all other cases (twoways/balanced; oneway(ind/time)/balanced/unbalanced) are correct
    #     for these formulae.
    if(model.effect != "twoways") {
      Xb <- model.matrix(data, rhs = 1, model = "between", effect = effect)
      yb <- pmodel.response(data, model = "between", effect = effect)
      fixef <- yb - as.vector(crossprod(t(Xb[ , nw, drop = FALSE]), coef(object)))
      
      # use robust vcov if supplied
      if (! is.null(vcov)) {
        if (is.matrix(vcov))   vcov <- vcov[nw, nw]
        if (is.function(vcov)) vcov <- vcov(object)[nw, nw]
      } else {
        vcov <- vcov(object)[nw, nw]
      }
      
      nother <- switch(effect,
                       "individual" = pdim$Tint$Ti,
                       "time"       = pdim$Tint$nt)
      
      s2 <- deviance(object) / df.residual(object)
      
      sefixef <- if (type != "dfirst") {
                    sqrt(s2 / nother + apply(Xb[, nw, drop = FALSE], 1,
                                            function(x) t(x) %*% vcov %*% x) )
                  } else {
                    Xb <- t(t(Xb[-1L, , drop = FALSE]) - Xb[1L, ])
                    sqrt(s2 * (1 / nother[-1L] + 1 / nother[1L]) +
                                      apply(Xb[, nw, drop = FALSE], 1,
                                            function(x) t(x) %*% vcov %*% x) )
                  }

      res <- switch(type,
                      "level"  = fixef,
                      "dfirst" = fixef[seq_along(fixef)[-1L]] - fixef[1L],
                      "dmean"  = (fixef - weighted.mean(fixef, w = nother)))
      
      res <- structure(res, se = sefixef, class = c("fixef", "numeric"),
                       type = type, df.residual = df.residual(object))  
    } else {
    ## case model.effect == "twoways"
    ##  * two-way balanced/unbalanced model for all effects
    ## TODO: SEs are not computed in this case yet
    ##       (can be computed for effect = "individual" and "time"; also for "twoways"?)
      beta.data <- as.numeric(tcrossprod(coef(object),
                                         model.matrix(object, model = "pooling")[ , nw, drop = FALSE]))
      yhat <- object$model[ , 1L] - object$residuals
      tw.fixef.lvl <- yhat - beta.data # sum of both effects in levels
      
      idx <- switch(effect,
                    "individual" = 1L,
                    "time"       = 2L,
                    "twoways"    = NA_integer_) # needed for weighted.mean below -> leads to no weights

      indexl <- unclass(index(object)) # unclass to list for speed
      
      if(effect %in% c("individual", "time")) {
        other.eff <- switch(effect,
                            "individual" = "time",
                            "time"       = "individual")
        
        other.idx <- switch(effect,
                          "individual" = 2L,
                          "time"       = 1L)

        Xb <- model.matrix(data, rhs = 1, model = "between", effect = other.eff)
        yb <- pmodel.response(data, model = "between", effect = other.eff)
        other.fixef.lvl <- yb - as.vector(crossprod(t(Xb[ , nw, drop = FALSE]), coef(object)))
        
        ## other dfirst
        other.fixef.dfirst <- other.fixef.lvl - other.fixef.lvl[1L]
        tw.fixef.lvl <- tw.fixef.lvl - other.fixef.dfirst[indexl[[other.idx]]]
        tw.fixef.lvl <- tw.fixef.lvl[!collapse::fduplicated(indexl[[idx]], all = FALSE)]
        names(tw.fixef.lvl) <- pdim[["panel.names"]][[idx]]
      } else {
        # effect = "twoways": everything already computed, just set names
        names(tw.fixef.lvl) <- paste0(pdim[["panel.names"]][[1L]][indexl[[1L]]], "-",
                                      pdim[["panel.names"]][[2L]][indexl[[2L]]])
      }
      
      res <- switch(type,
                      "level"  = tw.fixef.lvl,
                      "dfirst" = tw.fixef.lvl[seq_along(tw.fixef.lvl)[-1L]] - tw.fixef.lvl[1L],
                      "dmean"  = {
                          if(pdim$balanced || effect == "twoways") {
                            tw.fixef.lvl - mean(tw.fixef.lvl)
                          } else {
                            tw.fixef.lvl - weighted.mean(tw.fixef.lvl, w = pdim$Tint[[idx]])
                          }})
      
      res <- structure(res, se = NULL, class = c("fixef", "numeric"),
                            type = type, df.residual = NULL)
    }
    res
}


#' @rdname fixef.plm
#' @export
print.fixef <- function(x, digits = max(3, getOption("digits") - 2),
                        width = getOption("width"), ...){
    x.orig <- x
    # prevent attributes from being printed
    attr(x, "se") <- attr(x, "type") <- attr(x, "class") <- attr(x, "df.residual") <- attr(x, "index") <- NULL
    print.default(x, digits, width, ...)
    invisible(x.orig)
}


#' @rdname fixef.plm
#' @export
summary.fixef <- function(object, ...) {
  # for 2-way unbalanced, there are currently no further attributes -> skip construction
  res <- if(!is.null(attr(object, "se"))) {
    se <- attr(object, "se")
    df.res <- attr(object, "df.residual")
    tvalue <- (object) / se
    # was: res <- cbind(object, se, zvalue, (1 - pnorm(abs(zvalue))) * 2)
    res <- cbind(object, se, tvalue, (2 * pt(abs(tvalue), df = df.res, lower.tail = FALSE)))
    # see for distribution and degrees of freedom
    #   Greene (2003, 5th ed.), p.  288     (formula 13-7) 
    # = Greene (2012, 7th ed.), pp. 361-362 (formula 11-19)
    colnames(res) <- c("Estimate", "Std. Error", "t-value", "Pr(>|t|)")
    class(res) <- c("summary.fixef", "matrix")
    attr(res, "type") <- attr(object, "type")
    attr(res, "df.residual") <- df.res
    res
  } else {
    matrix(object, dimnames = list(names(object), "Estimate"))
  }
  res
}

#' @rdname fixef.plm
#' @export
print.summary.fixef <- function(x, digits = max(3, getOption("digits") - 2),
                                width = getOption("width"), ...){
  printCoefmat(x, digits = digits)
  invisible(x)
}

#' @rdname fixef.plm
#' @export
fixef.pggls <- fixef.plm







#' Extract the Random Effects
#' 
#' Function to calculate the random effects from a `plm` object
#' (random effects model).
#' 
#' Function `ranef` calculates the random effects of a fitted random
#' effects model. For one-way models, the effects of the estimated
#' model are extracted (either individual or time effects). For
#' two-way models, extracting the individual effects is the default
#' (both, argument `effect = NULL` and `effect = "individual"` will
#' give individual effects). Time effects can be extracted by setting
#' `effect = "time"`.
#' 
#' Not all random effect model types are supported (yet?).
#' 
#' @param object an object of class `"plm"`, needs to be a fitted
#'     random effects model,
#' @param effect `NULL`, `"individual"`, or `"time"`, the effects to
#'     be extracted, see **Details**,
#' @param \dots further arguments (currently not used).
#' @return A named numeric with the random effects per dimension
#'     (individual or time).
#' @name ranef.plm
#' @aliases ranef
#' @importFrom nlme ranef
#' @export ranef
#' @author Kevin Tappe
#' @seealso [fixef()] to extract the fixed effects from a fixed
#'     effects model (within model).
#' @keywords regression
#' @examples
#' 
#' data("Grunfeld", package = "plm")
#' m1 <- plm(inv ~ value + capital, data = Grunfeld, model = "random")
#' ranef(m1) # individual random effects
#' 
#' # compare to random effects by ML estimation via lme from package nlme
#' library(nlme)
#' m2 <- lme(inv ~ value + capital, random = ~1|firm, data = Grunfeld)
#' cbind("plm" = ranef(m1), "lme" = unname(ranef(m2)))
#' 
#' # two-ways RE model, calculate individual and time random effects
#' data("Cigar", package = "plm")
#' tw <- plm(sales ~ pop + price, data = Cigar, model = "random", effect = "twoways")
#' ranef(tw)                   # individual random effects
#' ranef(tw, effect = "time")  # time random effects
#'
NULL

#' @rdname ranef.plm
#' @export
ranef.plm <- function(object, effect = NULL, ...) {
  # TODO:
  #      Check if the same procedure can be applied to
  #       * unbalanced two-way case (for now: implemented the same way, but not entirely sure)
  #       * random IV models
  #       * nested random effect models
  model <- describe(object, "model")
  obj.effect <- describe(object, "effect")
  balanced <- is.pbalanced(object)
  
  if(model != "random") stop("only applicable to random effect models")
  # TODO: Are random effects for nested models and IV models calculated the same way?
  #       Be defensive here and error for such models.
  if(obj.effect == "nested")  stop("nested random effect models are not supported (yet?)")
  if(length(object$formula)[2L] >= 2L) stop("ranef: IV models not supported (yet?)")
  
  if(!is.null(effect) && !(effect %in% c("individual", "time"))) 
      stop("argument 'effect' must be NULL, \"individual\", or \"time\"")
  if(obj.effect != "twoways" && !is.null(effect) && effect != obj.effect) 
      stop(paste0("for one-way models, argument \"effect\" must be NULL or match the effect introduced in model estimation"))

  # default effect is the model's effect
  # for two-ways RE models: set default to effect = "individual"
  if(obj.effect == "twoways" && is.null(effect)) effect <- "individual"
  if(is.null(effect)) effect <- obj.effect
  
  erc <- ercomp(object)
  # extract theta, but depending on model/effect, it is adjusted/overwritten later
  theta <- unlist(erc["theta"], use.names = FALSE) 
  
  # res <- object$residuals                # gives residuals of quasi-demeaned model
  res <- residuals_overall_exp.plm(object) # but need RE residuals of overall model
  
  if(!inherits(res, "pseries")) {
    # just make sure we have a pseries for the following between() to work
    attr(res, "index") <- index(object$model)
    class(res) <- c("pseries", class(res))
  }
   
  # mean_res <- Between(res, effect = effect)  # has length == # observations
  mean_res <- between(res, effect = effect)    # but need length == # individuals
  
  if(obj.effect == "twoways" && balanced) {
    theta <- switch(effect,
                    "individual" = theta[1L],
                    "time"       = theta[2L])
  }
  if(obj.effect == "twoways" && !balanced) {
    theta <- erc[["theta"]][[if(effect == "individual") "id" else "time"]]
  }
  
  if(!balanced) {
    # in the unbalanced cases, ercomp[["theta"]] is full length (# obs)
    #  -> reduce to per id/time
    select <- switch(effect,
                     "individual" = !collapse::fduplicated(index(object$model)[1L], all = FALSE),
                     "time"       = !collapse::fduplicated(index(object$model)[2L], all = FALSE))
    theta <- theta[select]
  }
  
  # calculate random effects:
  # This formula works (at least) for:
  #  balanced one-way (is symmetric for individual/time)
  #  unbalanced one-way (symmetric) is also caught by this line as theta is reduced before
  #  balanced two-way case (symmetric)
  raneffects <- (1 - (1 - theta)^2) * mean_res
  names(raneffects) <- names(mean_res)
  return(raneffects)
}




#' Overall Intercept for Within Models Along its Standard Error
#' 
#' This function gives an overall intercept for within models and its
#' accompanying standard error or a within model with the overall intercept
#' 
#' The (somewhat artificial) intercept for within models (fixed
#' effects models) was made popular by Stata of StataCorp
#' \insertCite{@see @GOUL:13}{plm}, EViews of IHS, and gretl
#' \insertCite{@see @GRETL:2021, p. 200-201, listing 23.1}{plm}, see for
#' treatment in the literature,
#' e.g., \insertCite{GREE:12;textual}{plm}, Ch. 11.4.4, p. 364. It can
#' be considered an overall intercept in the within model framework
#' and is the weighted mean of fixed effects (see **Examples** for the
#' relationship).
#' 
#' `within_intercept` estimates a new model which is
#' computationally more demanding than just taking the weighted
#' mean. However, with `within_intercept` one also gets the
#' associated standard error and it is possible to get an overall
#' intercept for two-way fixed effect models.
#' 
#' Users can set argument `vcov` to a function to calculate a
#' specific (robust) variance--covariance matrix and get the
#' respective (robust) standard error for the overall intercept,
#' e.g., the function [vcovHC()], see examples for
#' usage. Note: The argument `vcov` must be a function, not a
#' matrix, because the model to calculate the overall intercept for
#' the within model is different from the within model itself.
#' 
#' If argument `return.model = TRUE` is set, the full model object is returned,
#' while in the default case only the intercept is returned.
#' 
#' @aliases within_intercept
#' @param object object of class `plm` which must be a within
#'     model (fixed effects model),
#' @param vcov if not `NULL` (default), a function to calculate a
#'     user defined variance--covariance matrix (function for robust
#'     vcov), only used if `return.model = FALSE`,
#' @param return.model a logical to indicate whether only the overall intercept
#'     (`FALSE` is default) or a full model object (`TRUE`) is to be returned, 
#' @param \dots further arguments (currently none).
#' @return Depending on argument `return.model`:  If `FALSE` (default), a named
#' `numeric` of length one: The overall intercept for the estimated within model
#'  along attribute "se" which contains the standard error for the intercept.
#'  If `return.model = TRUE`, the full model object, a within model with the
#'  overall intercept (NB: the model identifies itself as a pooling model, e.g.,
#'  in summary()).
#'  
#' @export
#' @author Kevin Tappe
#' @seealso [fixef()] to extract the fixed effects of a within model.
#' @references
#'
#' \insertAllCited{}
#'
#' @keywords attribute
#' @examples
#' data("Hedonic", package = "plm")
#' mod_fe <- plm(mv ~ age + crim, data = Hedonic, index = "townid")
#' overallint <- within_intercept(mod_fe)
#' attr(overallint, "se") # standard error
#' 
#' # overall intercept is the weighted mean of fixed effects in the
#' # one-way case
#' weighted.mean(fixef(mod_fe), pdim(mod_fe)$Tint$Ti)
#' 
#' ### relationship of type="dmean", "level" and within_intercept
#' ## one-way balanced case
#' data("Grunfeld", package = "plm")
#' gi <- plm(inv ~ value + capital, data = Grunfeld, model = "within")
#' fx_level <- fixef(gi, type = "level")
#' fx_dmean <- fixef(gi, type = "dmean")
#' overallint <- within_intercept(gi)
#' all.equal(overallint + fx_dmean, fx_level, check.attributes = FALSE) # TRUE
#' ## two-ways unbalanced case
#' gtw_u <- plm(inv ~ value + capital, data = Grunfeld[-200, ], effect = "twoways")
#' int_tw_u <- within_intercept(gtw_u)
#' fx_dmean_tw_i_u <- fixef(gtw_u, type = "dmean", effect = "individual")[index(gtw_u)[[1L]]]
#' fx_dmean_tw_t_u <- fixef(gtw_u, type = "dmean", effect = "time")[index(gtw_u)[[2L]]]
#' fx_level_tw_u <- as.numeric(fixef(gtw_u, "twoways", "level"))
#' fx_level_tw_u2 <- int_tw_u + fx_dmean_tw_i_u + fx_dmean_tw_t_u
#' all.equal(fx_level_tw_u, fx_level_tw_u2, check.attributes = FALSE) # TRUE
#' 
#' ## overall intercept with robust standard error
#' within_intercept(gi, vcov = function(x) vcovHC(x, method="arellano", type="HC0"))
#'
#' ## have a model returned
#' mod_fe_int <- within_intercept(gi, return.model = TRUE)
#' summary(mod_fe_int)
#' # replicates Stata's robust standard errors exactly as model is with intercept
#' summary(mod_fe_int, vcov = function(x) vcovHC(x, type = "sss")) 
# 
within_intercept <- function(object, ...) {
  UseMethod("within_intercept")
}

# Note: The name of the function (within_intercept) with an underscore does not
#       follow the regular naming scheme where one would expect a dot (within.intercept).
#       Due to the S3 class system, calling the function within.intercept would result in
#       a name clash as we have a function called 'within' and in this case the S3 
#       system interprets '.intercept' as a class called 'intercept'.

# Note: return value of within_intercept is related to return values of fixef.plm,
#       see inst/tests/test_within_intercept.R

#' @rdname within_intercept
#' @export
within_intercept.plm <- function(object, vcov = NULL, return.model = FALSE, ...) {
  if(!inherits(object, "plm")) stop("input 'object' needs to be a \"within\" model estimated by plm()")
  if(length(object$formula)[2L] >= 2L) stop("within_intercept: IV models not supported (yet?)")
  model  <- describe(object, what = "model")
  effect <- describe(object, what = "effect")
  if(model != "within") stop("input 'object' needs to be a \"within\" model estimated by plm(..., model = \"within\", ...)")
  
  # vcov must be a function, because the auxiliary model estimated to get the
  # overall intercept next to its standard errors is different from
  # the FE model for which the intercept is estimated, e.g., dimensions
  # of vcov differ for FE and for auxiliary model.
  if(!is.null(vcov)) {
    if(is.matrix(vcov)) stop("for within_intercept, 'vcov' may not be of class 'matrix', it must be supplied as a function, e.g., vcov = function(x) vcovHC(x)")
    if(!is.function(vcov)) stop("for within_intercept, argument 'vcov' must be a function, e.g., vcov = function(x) vcovHC(x)")
  }
  
  index <- attr(object$model, which = "index")
  
  # Transformation to get the overall intercept is:
  # demean groupwise and add back grand mean of each variable, then run OLS
  mf      <- model.frame(object)
  withinY <- pmodel.response(object) # returns the response specific to the 'effect' of the est. FE model object
  meanY   <- mean(mf[ , 1L]) # mean of original data's response
  transY  <- withinY + meanY
  
  withinM <- model.matrix(object) # returns the model.matrix specific to the 'effect' of the est. FE model object
  M <- model.matrix(mf, cstcovar.rm = "all")
  M <- M[ , colnames(M) %in% colnames(withinM), drop = FALSE] # just to be sure: should be same columns
  meansM <- colMeans(M)
  transM <- t(t(withinM) + meansM)
  
  # estimation by lm()
    # data <- data.frame(cbind(transY, transM))
    # auxreg <- lm(data)
    # summary(auxreg)

  # estimation by plm() - to apply robust vcov function if supplied
  # NB: this changes variable names slightly (data.frame uses make.names to, e.g., get rid of parentheses in variable names)
  data <- pdata.frame(data.frame(cbind(index, transY, transM)), drop.index = TRUE)
  form <- as.formula(paste0(names(data)[1L], "~", paste(names(data)[-1L], collapse = "+")))
  auxreg <- plm(form, data = data, model = "pooling")
  
  # degrees of freedom correction due to FE transformation for "normal" vcov [copied over from plm.fit]
  pdim <- pdim(index)
  card.fixef <- switch(effect,
                       "individual" = pdim$nT$n,
                       "time"       = pdim$nT$T,
                       "twoways"    = pdim$nT$n + pdim$nT$T - 1L)
  df <- df.residual(auxreg) - card.fixef  + 1L # just for within_intercept: here we need '+1' to correct for the intercept
  
  vcov_mat <- vcov(auxreg)
  vcov_mat <- vcov_mat * df.residual(auxreg) / df
  auxreg$vcov <- vcov_mat # plug in new vcov (adjusted "normal" vcov) in auxiliary model

  res <- if(!return.model) {
    #### return only intercept with SE as attribute
    ##  in case of robust vcov, which is supplied by a function
    ##  no adjustment to the robust vcov is necessary
    if(is.function(vcov)) vcov_mat <- vcov(auxreg) # robust vcov as supplied by a function
    intercept <- auxreg[["coefficients"]]["(Intercept)"]
    attr(intercept, which = "se") <- sqrt(vcov_mat[1L, 1L])
    names(intercept) <- "(overall_intercept)"
    intercept
  } else {
    ### return model
    if(!is.null(vcov)) warning("argument 'vcov' is non-NULL and is ignored as 'return.model = TRUE' is set")
    auxreg
  }
  return(res)
} # END within_intercept.plm
