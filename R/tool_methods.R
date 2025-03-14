# panelmodel and plm methods :

## panelmodel methods :
# - terms
# - vcov
# - fitted
# - residuals
# - df.residual
# - coef
# - print
# - update
# - deviance
# - nobs

## plm methods :
# - summary
# - print.summary
# - predict
# - formula
# - plot
# - residuals
# - fitted


#' @rdname plm
#' @export
terms.panelmodel <- function(x, ...){
  terms(formula(x))
}

#' @rdname plm
#' @export
vcov.panelmodel <- function(object, ...){
  object$vcov
}

#' @rdname plm
#' @export
fitted.panelmodel <- function(object, ...){
  object$fitted.values 
}

#' @rdname plm
#' @export
residuals.panelmodel <- function(object, ...){
  object$residuals
}

#' @rdname plm
#' @export
df.residual.panelmodel <- function(object, ...){
  object$df.residual
}

#' @rdname plm
#' @export
coef.panelmodel <- function(object, ...){
  object$coefficients
}

#' @rdname plm
#' @export
print.panelmodel <- function(x, digits = max(3, getOption("digits") - 2),
                             width = getOption("width"), ...){
  cat("\nModel Formula: ")
  print(formula(x))
  cat("\nCoefficients:\n")
  print(coef(x), digits = digits)
  cat("\n")
  invisible(x)
}


#' Extract Total Number of Observations Used in Estimated Panelmodel
#' 
#' This function extracts the total number of 'observations' from a
#' fitted panel model.
#' 
#' The number of observations is usually the length of the residuals
#' vector. Thus, `nobs` gives the number of observations actually
#' used by the estimation procedure. It is not necessarily the number
#' of observations of the model frame (number of rows in the model
#' frame), because sometimes the model frame is further reduced by the
#' estimation procedure. This is, e.g., the case for first--difference
#' models estimated by `plm(..., model = "fd")` where the model
#' frame does not yet contain the differences (see also
#' **Examples**).
#'
#' @name nobs.plm
#' @aliases nobs
#' @importFrom stats nobs
#' @export nobs
#' @param object a `panelmodel` object for which the number of
#'     total observations is to be extracted,
#' @param \dots further arguments.
#' @return A single number, normally an integer.
#' @seealso [pdim()]
#' @keywords attribute
#' @examples
#' 
#' # estimate a panelmodel
#' data("Produc", package = "plm")
#' z <- plm(log(gsp)~log(pcap)+log(pc)+log(emp)+unemp,data=Produc,
#'          model="random", subset = gsp > 5000)
#'          
#' nobs(z)       # total observations used in estimation
#' pdim(z)$nT$N  # same information
#' pdim(z)       # more information about the dimensions (no. of individuals and time periods)
#' 
#' # illustrate difference between nobs and pdim for first-difference model
#' data("Grunfeld", package = "plm")
#' fdmod <- plm(inv ~ value + capital, data = Grunfeld, model = "fd")
#' nobs(fdmod)      # 190
#' pdim(fdmod)$nT$N # 200
#' 
NULL

# nobs() function to extract total number of observations used for estimating the panelmodel
# like stats::nobs for lm objects
# NB: here, use object$residuals rather than residuals(object)
#     [b/c the latter could do NA padding once NA padding works for plm objects.
#      NA padded residuals would yield wrong result for nobs!]

#' @rdname nobs.plm
#' @export
nobs.panelmodel <- function(object, ...) {
  if (inherits(object, "plm") || inherits(object, "panelmodel")) return(length(object$residuals))
    else stop("Input 'object' needs to be of class 'plm' or 'panelmodel'")
}

# No of obs calculated as in print.summary.pgmm [code copied from there]
#' @rdname nobs.plm
#' @export
nobs.pgmm <- function(object, ...) {
  if (inherits(object, "pgmm")) return(sum(unlist(object$residuals, use.names = FALSE) != 0))
    else stop("Input 'object' needs to be of class 'pgmm', i. e., a GMM estimation with panel data estimated by pgmm()")
}




# Almost the same as the default method except that update.formula is
# replaced by update, so that the Formula method is used to update the
# formula

#' @rdname plm
#' @export
update.panelmodel <- function (object, formula., ..., evaluate = TRUE){
    if (is.null(call <- object$call)) # was: getCall(object))) 
        stop("need an object with call component")
    extras <- match.call(expand.dots = FALSE)$...
    # update.Formula fails if latter rhs are . ; simplify the formula
    # by removing the latter parts

    if (! missing(formula.)){
        newform <- Formula(formula.)
        if (length(newform)[2L] == 2L && attr(newform, "rhs")[2L] == as.name("."))
            newform <- formula(newform, rhs = 1)
        call$formula <- update(formula(object), newform)
    }
    if (length(extras)) {
        existing <- !is.na(match(names(extras), names(call)))
        for (a in names(extras)[existing]) call[[a]] <- extras[[a]]
        if (any(!existing)) {
            call <- c(as.list(call), extras[!existing])
            call <- as.call(call)
        }
    }
    if (evaluate) 
        eval(call, parent.frame())
    else call
}

#' @rdname plm
#' @export
deviance.panelmodel <- function(object, model = NULL, ...){
  if (is.null(model)) as.numeric(crossprod(resid(object)))
  else as.numeric(crossprod(residuals(object, model = model)))
}





#' Summary for plm objects
#' 
#' The summary method for plm objects generates some more information about
#' estimated plm models.
#' 
#' The `summary` method for plm objects (`summary.plm`) creates an
#' object of class `c("summary.plm", "plm", "panelmodel")` that
#' extends the plm object it is run on with various information about
#' the estimated model like (inferential) statistics, see
#' **Value**. It has an associated print method
#' (`print.summary.plm`).
#' 
#' @aliases summary.plm
#' @param object an object of class `"plm"`,
#' @param x an object of class `"summary.plm"`,
#' @param subset a character or numeric vector indicating a subset of
#'     the table of coefficients to be printed for
#'     `"print.summary.plm"`,
#' @param vcov a variance--covariance matrix furnished by the user or
#'     a function to calculate one (see **Examples**),
#' @param digits number of digits for printed output,
#' @param width the maximum length of the lines in the printed output,
#' @param eq the selected equation for list objects
#' @param \dots further arguments.
#' @return An object of class `c("summary.plm", "plm",
#'     "panelmodel")`.  Some of its elements are carried over from the
#'     associated plm object and described there
#'     ([plm()]). The following elements are new or changed
#'     relative to the elements of a plm object:
#' 
#' \item{fstatistic}{'htest' object: joint test of significance of
#' coefficients (F or Chi-square test) (robust statistic in case of
#' supplied argument `vcov`, see [pwaldtest()] for details),}
#' 
#' \item{coefficients}{a matrix with the estimated coefficients,
#' standard errors, t--values, and p--values, if argument `vcov` was
#' set to non-`NULL` the standard errors (and t-- and p--values) in
#' their respective robust variant,}
#'
#' \item{vcov}{the "regular" variance--covariance matrix of the coefficients (class "matrix"),}
#'
#' \item{rvcov}{only present if argument `vcov` was set to non-`NULL`:
#' the furnished variance--covariance matrix of the coefficients
#' (class "matrix"),}
#'
#' \item{r.squared}{a named numeric containing the R-squared ("rsq")
#' and the adjusted R-squared ("adjrsq") of the model,}
#'
#' \item{df}{an integer vector with 3 components, (p, n-p, p*), where
#' p is the number of estimated (non-aliased) coefficients of the
#' model, n-p are the residual degrees of freedom (n being number of
#' observations), and p* is the total number of coefficients
#' (incl. any aliased ones).}
#'
#' @export
#' @author Yves Croissant
#' @seealso [plm()] for estimation of various models; [vcovHC()] for
#'     an example of a robust estimation of variance--covariance
#'     matrix; [r.squared()] for the function to calculate R-squared;
#'     [stats::print.power.htest()] for some information about class
#'     "htest"; [fixef()] to compute the fixed effects for "within"
#'     (=fixed effects) models and [within_intercept()] for an
#'     "overall intercept" for such models; [pwaldtest()]
#' @keywords regression
#' @examples
#' 
#' data("Produc", package = "plm")
#' zz <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
#'           data = Produc, index = c("state","year"))
#' summary(zz)
#' 
#' # summary with a furnished vcov, passed as matrix, as function, and
#' # as function with additional argument
#' data("Grunfeld", package = "plm")
#' wi <- plm(inv ~ value + capital,
#'           data = Grunfeld, model="within", effect = "individual")
#' summary(wi, vcov = vcovHC(wi))
#' summary(wi, vcov = vcovHC)
#' summary(wi, vcov = function(x) vcovHC(x, method = "white2"))
#' 
#' # extract F statistic
#' wi_summary <- summary(wi)
#' Fstat <- wi_summary[["fstatistic"]]
#' 
#' # extract estimates and p-values
#' est <- wi_summary[["coefficients"]][ , "Estimate"]
#' pval <- wi_summary[["coefficients"]][ , "Pr(>|t|)"]
#' 
#' # print summary only for coefficient "value"
#' print(wi_summary, subset = "value")
#' 
summary.plm <- function(object, vcov = NULL, ...){
  # summary.plm creates a specific summary.plm object that is derived
  # from the associated plm object
  
  vcov_arg <- vcov
  model <- describe(object, "model")
  
  # determine if intercept-only model (no other regressors)
  coef_wo_int <- object$coefficients[!(names(coef(object)) %in% "(Intercept)")]
  int.only <- !length(coef_wo_int)
  
  # as cor() is not defined for intercept-only models, use different approach
  # for R-squared ("rss" and "ess" are defined)
  object$r.squared <- if(!int.only) {
      c(rsq    = r.squared(object),
        adjrsq = r.squared(object, dfcor = TRUE))
    } else { 
      c(rsq    = r.squared(object, type = "rss"),
        adjrsq = r.squared(object, type = "rss", dfcor = TRUE))
    }
  
  ## determine if standard normal and Chisq test or t distribution and F test to be used
  ## (normal/chisq for all random models, all IV models, and HT via plm(., model="ht"))
  use.norm.chisq <- if(model == "random" || 
                       length(formula(object))[2L] >= 2L || 
                       model == "ht") TRUE else FALSE
  
  # perform Wald test of joint sign. of regressors only if there are
  # other regressors besides the intercept
  if(!int.only) {
    object$fstatistic <- pwaldtest(object,
                                   test = if(use.norm.chisq) "Chisq" else "F",
                                   vcov = vcov_arg)
  }
  
  
  # construct the table of coefficients
  if (!is.null(vcov_arg)) {
    if (is.matrix(vcov_arg))   rvcov <- vcov_arg
    if (is.function(vcov_arg)) rvcov <- vcov_arg(object)
    std.err <- sqrt(diag(rvcov))
  } else {
    std.err <- sqrt(diag(stats::vcov(object)))
  }
  b <- coefficients(object)
  z <- b / std.err
  p <- if(use.norm.chisq) {
    2 * pnorm(abs(z), lower.tail = FALSE)
  } else {
    2 * pt(abs(z), df = object$df.residual, lower.tail = FALSE)
  }
  
  # construct the object of class summary.plm
  object$coefficients <- cbind(b, std.err, z, p)
  colnames(object$coefficients) <- if(use.norm.chisq) {
           c("Estimate", "Std. Error", "z-value", "Pr(>|z|)")
  } else { c("Estimate", "Std. Error", "t-value", "Pr(>|t|)") }
  
  ## add some info to summary.plm object 
  # robust vcov (next to "normal" vcov)
  if (!is.null(vcov_arg)) {
    object$rvcov <- rvcov
    rvcov.name <- paste0(deparse(substitute(vcov)))
    attr(object$rvcov, which = "rvcov.name") <- rvcov.name 
  }
  
  # mimics summary.lm's 'df' component
  # 1st entry: no. coefs (w/o aliased coefs); 2nd: residual df; 3rd no. coefs /w aliased coefs
  # NB: do not use length(object$coefficients) for 3rd entry!
  object$df <- c(length(b), object$df.residual, length(object$aliased))
  
  class(object) <- c("summary.plm", "plm", "panelmodel")
  object
}

#' @rdname summary.plm
#' @export
print.summary.plm <- function(x, digits = max(3, getOption("digits") - 2),
                              width = getOption("width"), subset = NULL, ...){
  formula <- formula(x)
  has.instruments <- (length(formula)[2L] >= 2L)
  effect <- describe(x, "effect")
  model  <- describe(x, "model")
  if (model != "pooling") { cat(paste(effect.plm.list[effect], " ", sep = "")) }
  cat(paste(model.plm.list[model], " Model", sep = ""))
  
  if (model == "random"){
    ercomp <- describe(x, "random.method")
    cat(paste(" \n   (", random.method.list[ercomp], "'s transformation)\n",
              sep = ""))
  }
  else{
    cat("\n")
  }
  
  if (has.instruments){
    cat("Instrumental variable estimation\n")
    if(model != "within") {
      # don't print transformation method for FE models as there is only one
      # such method for FE models but plenty for other model types
      ivar <- describe(x, "inst.method")
      cat(paste0("   (", inst.method.list[ivar], "'s transformation)\n"))
    }
  }
  
  if (!is.null(x$rvcov)) {
    cat("\nNote: Coefficient variance-covariance matrix supplied: ", 
        attr(x$rvcov, which = "rvcov.name"), "\n", sep = "")
  }
  
  cat("\nCall:\n")
  print(x$call)
  cat("\n")
  pdim <- pdim(x)
  print(pdim)
  if (model %in% c("fd", "between")) {
    # print this extra info, b/c model.frames of FD and between models
    # have original (undifferenced/"un-between-ed") obs/rows of the data
    cat(paste0("Observations used in estimation: ", nobs(x), "\n"))}
  
  if (model == "random"){
    cat("\nEffects:\n")
    print(x$ercomp)
  }
  cat("\nResiduals:\n")
  df <- x$df
  rdf <- df[2L]
  if (rdf > 5L) {
    save.digits <- unlist(options(digits = digits))
    on.exit(options(digits = save.digits))
    print(sumres(x))
  } else if (rdf > 0L) print(residuals(x), digits = digits)
  if (rdf == 0L) { # estimation is a perfect fit
    cat("ALL", x$df[1L], "residuals are 0: no residual degrees of freedom!")
    cat("\n")
  }
  
  if (any(x$aliased, na.rm = TRUE)) {
    # na.rm = TRUE because currently, RE tw unbalanced models might have NAs?
    naliased <- sum(x$aliased, na.rm = TRUE)
    cat("\nCoefficients: (", naliased, " dropped because of singularities)\n", sep = "")
  } else cat("\nCoefficients:\n")
  
  if (is.null(subset)) printCoefmat(coef(x), digits = digits)
  else printCoefmat(coef(x)[subset, , drop = FALSE], digits = digits)
  cat("\n")
  cat(paste("Total Sum of Squares:    ", signif(tss(x),      digits), "\n", sep = ""))
  cat(paste("Residual Sum of Squares: ", signif(deviance(x), digits), "\n", sep = ""))
  cat(paste("R-Squared:      ", signif(x$r.squared[1L], digits),      "\n", sep = ""))
  cat(paste("Adj. R-Squared: ", signif(x$r.squared[2L], digits),      "\n", sep = ""))

  # print Wald test of joint sign. of regressors only if there is a statistic
  # in summary.plm object (not computed by summary.plm if there are no other
  # regressors than the intercept
  if(!is.null(fstat <- x$fstatistic)) {
    if (names(fstat$statistic) == "F"){
      cat(paste("F-statistic: ", signif(fstat$statistic),
                " on ", fstat$parameter["df1"]," and ", fstat$parameter["df2"],
                " DF, p-value: ", format.pval(fstat$p.value,digits=digits), "\n", sep=""))
    }
    else{
      cat(paste("Chisq: ", signif(fstat$statistic),
                " on ", fstat$parameter,
                " DF, p-value: ", format.pval(fstat$p.value, digits = digits), "\n", sep=""))
    }
  }
  invisible(x)
}

#' Model Prediction for plm Objects
#' 
#' Predicted values of response based on plm models.
#' 
#' `predict`calculates predicted values by evaluating the regression function of
#' a plm model for `newdata` or, if `newdata = NULL`, it returns the fitted values
#' the plm model. 
#' 
#' The fixed effects (within) model is somewhat special in prediction as it has
#' fixed effects estimated per individual, time period (one-way) or both (two-ways
#' model) which should to be respected when predicting values relating to these
#' fixed effects in the model: To do so, it is recommended to supply a pdata.frame 
#' (and not a plain data.frame) in `newdata` as it describes the relationship 
#' between the data supplied to the individual. and/or time periods. In case
#' the `newdata`´'s pdata.frame has out-of-sample data (data contains individuals 
#' and/or time periods not contained in the original model), it is not clear
#' how values are to be predicted and the result will contain `NA` 
#' values for these out-of-sample data. Argument `na.fill` can be set to `TRUE`
#' to apply the original model's weighted mean of fixed effects for the 
#' out-of-sample data to derive a prediction.
#' 
#' If a plain data.frame is given in `newdata` for a fixed effects model, the 
#' weighted mean is used for all fixed effects as `newdata` for prediction as a 
#' plain data.frame cannot describe any relation to individuals/time periods 
#' (`na.fill` is automatically set to `TRUE` and the function warns).
#' 
#' See also **Examples**.
#' 
#' 
#' @param object An object of class `"plm"`,
#' @param newdata An optional pdata.frame in which to look for variables to be
#'                used for prediction. If `NULL`, the fitted values are returned.
#'                For fixed effects models, supplying a pdata.frame is recommended.
#' @param na.fill A logical, only relevant if `object` is a pdata.frame, indicating
#'                whether for any supplied out-of-sample indexes (individual, time,
#'                combination of both), the missing fixed effect estimate is filled 
#'                with the weighted mean of the model's present fixed effect estimates
#'                or not.
#' @param \dots further arguments.
#' @return A numeric (or a pseries if `newdata` is a pdata.frame) carrying the 
#'         predicted values with length equal to the number of rows as the data 
#'         supplied in `newdata` and with names the row names of `newdata` or, if 
#'         `newdata = NULL`, the original model's fitted values given in `object`.
#' @keywords regression
#' @export
#' @rdname predict.plm
#' @examples
#' library(plm)
#' data("Grunfeld", package = "plm")
#' 
#' # fit a fixed effect model
#' fit.fe <- plm(inv ~ value + capital, data = Grunfeld, model = "within")
#' 
#' # generate 55 new observations of three firms used for prediction:
#' #  * firm 1 with years 1935:1964 (has out-of-sample years 1955:1964), 
#' #  * firm 2 with years 1935:1949 (all in sample),
#' #  * firm 11 with years 1935:1944 (firm 11 is out-of-sample)
#' set.seed(42L)
#' 
#' new.value2   <- runif(55, min = min(Grunfeld$value),   max = max(Grunfeld$value))
#' new.capital2 <- runif(55, min = min(Grunfeld$capital), max = max(Grunfeld$capital))
#' 
#' newdata <- data.frame(firm = c(rep(1, 30), rep(2, 15), rep(11, 10)),
#'                       year = c(1935:(1935+29), 1935:(1935+14), 1935:(1935+9)),
#'                       value = new.value2, capital = new.capital2)
#' # make pdata.frame
#' newdata.p <- pdata.frame(newdata, index = c("firm", "year"))
#' 
#' ## predict from fixed effect model with new data as pdata.frame
#' predict(fit.fe, newdata = newdata.p)
#' 
#' ## set na.fill = TRUE to have the weighted mean used to for fixed effects -> no NA values
#' predict(fit.fe, newdata = newdata.p, na.fill = TRUE)
#' 
#' ## predict with plain data.frame from fixed effect model: uses mean fixed effects 
#' ## for prediction and, thus, yields different result with a warning
#' predict(fit.fe, newdata = newdata)
#' 
predict.plm <- function(object, newdata = NULL, na.fill = !inherits(newdata, "pdata.frame"), ...) {
  # NB (TODO?): for between and FD models: output is of different length for 
  #     newdata = NULL (output is compressed data) and for 
  #     newdata = original pdata.frame (output has original length)
  
  if(is.null(newdata)){ 
    # result is simply fitted values of estimated model
    result <- fitted_exp.plm(object, ...) # fitted_exp.plm gives outer model's fitted values
  }
  else{
    
    # begin case newdata != NULL
    is.pdf <- inherits(newdata, "pdata.frame")
    
    # model frame and model matrix without response (as not needed)
    if(is.pdf) {
      m <- model.frame(newdata, object$formula, lhs = 0)
      X <- model.matrix(m)
    } else {
      tt <- terms(object)
      Terms <- delete.response(tt)
      m <- model.frame(Terms, newdata)
      X <- model.matrix(Terms, m)
    }
    
    beta <- coef(object)
    model <- describe(object, "model")
    
    if(model == "within") {
      # remove intercept if contained in the formula/terms and, thus, in the 
      # model matrix
      #  (usually, users does not explicitly suppress the
      #  intercept in FE models (e.g., by response ~ 0 + depvars), but we need 
      #  to cater for that suppressed-case as well by has.intercept(object))
      if(has.intercept(object)) X <- X[ , -1L, drop = FALSE]
      effect <- describe(object, "effect")
      effs.orig <- fixef(object, effect = effect)
      
      idx <- switch(effect, "individual" = 1L, "time" = 2L)
      
      if(is.pdf) {
        model.idx   <- unclass(index(object)) # unclass for speed
        newdata.idx <- unclass(index(newdata))
        
        if(effect %in% c("individual", "time")) {
          # one-way
          
          model.idx.eff   <- model.idx[[idx]]
          newdata.idx.eff <- newdata.idx[[idx]]

          effs <- effs.orig[levels(newdata.idx.eff)][newdata.idx.eff] # has length corresponding to newdata
          
          if(na.fill & sum(out.of.sample <- !newdata.idx.eff %in% model.idx.eff) > 0L) {
            pdim <- pdim(object)
            effs[out.of.sample] <- weighted.mean(effs.orig, w = pdim$Tint[[idx]])
          }
        } else {
          # two-ways
          # two-ways case is a little special as the rows of id-time combinations
          # need to be determined first (not as simple subsetting as in one-way case)
          model.idx.id   <- model.idx[[1L]]
          model.idx.ti   <- model.idx[[2L]]
          
          newdata.idx.id <- newdata.idx[[1L]]
          newdata.idx.ti <- newdata.idx[[2L]]
          
          model.idx.tw   <- paste(model.idx.id,   model.idx.ti,   sep = "_")
          newdata.idx.tw <- paste(newdata.idx.id, newdata.idx.ti, sep = "_")
          
          names(effs.orig) <- model.idx.tw
          
          effs <- effs.orig[newdata.idx.tw] # has length corresponding to newdata
          
          if(na.fill & sum(out.of.sample <- !newdata.idx.tw %in% model.idx.tw) > 0L) {
            pdim <- pdim(object)
            effs[out.of.sample] <- mean(effs.orig)
          }
        }
      } else {
        # newdata is not a pdata.frame: use (weighted) mean of fixed effects as best guess
        # (argument na.fill is not evaluated here as for a data.frame all values would 
        # be NA, so rather weighted mean of fixed effects is always applied)
        warning("Data supplied in argument 'newdata' is not a pdata.frame; weighted mean of fixed effects as in original model used for prediction, see ?predict.plm.")
        pdim <- pdim(object)
        effs <- if(na.fill) {
                    if(effect == "twoways") mean(effs.orig) else weighted.mean(effs.orig, w = pdim$Tint[[idx]])
                  } else NA
        effs <- rep(effs, nrow(X))
      }
    } # end-if model == "within"
    
    result <- as.numeric(tcrossprod(beta, X)) + if(model == "within") effs else 0
    # if newdata is a pdata.frame output a pseries w/ index stripped down to what
    # is left after NA-omitting (performed implicitly by model.frame)
    rmrows <- unclass(attr(m, "na.action"))
    nms <- rownames(newdata)
    if(is.pdf) {
      result.index <- if(!is.null(rmrows)) index(newdata[-rmrows, ]) else index(newdata)
      result <- add_pseries_features(result, result.index)
    }
    names(result) <- if(!is.null(rmrows)) nms[-rmrows] else nms
  } # end case newdata != NULL
  result
}

#' @rdname plm
#' @export
formula.plm <- function(x, ...){
  x$formula
}

#' @rdname plm
#' @export
plot.plm <- function(x, dx = 0.2, N = NULL, seed = 1,
                     within = TRUE, pooling = TRUE,
                     between = FALSE, random = FALSE, ...){
    set.seed(seed)# 8 est bien pour beertax
    subs <- ! is.null(N)
    x <- update(x, model = "within")
    mco <- update(x, model = "pooling")
    if (random) re <- update(x, model = "random")
    if (between) be <- update(x, model = "between")
    pdim <- pdim(x)
    n <- pdim$nT$n
    if (! subs) N <- n
    ids <- unique(index(x, "id"))
    if (subs) ids <- ids[sample(seq_along(ids), N, replace = FALSE)]
    sel <- index(x, "id") %in% ids
    T. <- pdim$nT$T
    cols <- rainbow(N)
    pts <- sample(1:25, N, replace = TRUE)
    thex <- as.numeric(model.matrix(x, model = "pooling")[sel, 2L])
    they <- as.numeric(pmodel.response(x, model = "pooling")[sel])
    plot(thex, they, col = rep(cols, each = T.),
         pch = rep(pts, each = T.), ann = FALSE, las = 1)
    idsel <- as.numeric(index(x, "id")[sel])
    meanx <- tapply(thex, idsel, mean)
    meany <- tapply(they, idsel, mean)
    points(meanx, meany, pch = 19, col = cols, cex = 1.5)
    if (within){
        beta <- coef(x)
        alphas <- meany - meanx * beta
        dx <- dx * (max(thex) - min(thex))
        for (i in seq_len(N)){
            xmin <- meanx[i] - dx
            xmax <- meanx[i] + dx
            ymin <- alphas[i] + beta * xmin
            ymax <- alphas[i] + beta * xmax
            lines(c(xmin, xmax), c(ymin, ymax), col = cols[i])
        }
    }
    if(random) abline(coef(re)[1L], coef(re)[2L], lty = "dotted")
    if(pooling) abline(coef(mco), lty = "dashed")
    if(between) abline(coef(be), lty = "dotdash")
    # where to put the legends, depends on the sign of the OLS slope
    modploted <- c(random, pooling, between, within)
    if (sum(modploted)){
        poslegend <- ifelse(beta > 0, "topleft", "topright")
        ltylegend <- c("dotted", "dashed", "dotdash", "solid")[modploted]
        leglegend <- c("random", "pooling", "between", "within")[modploted]
        legend(poslegend, lty = ltylegend, legend = leglegend)
    }
}

#' @rdname plm
#' @export
residuals.plm <- function(object, model = NULL, effect = NULL,  ...){
    if (is.null(model) && is.null(effect)){
        model <- describe(object, "model")
        res <- object$residuals
    }
    else{
        cl <- match.call(expand.dots = FALSE)
        # fitted -> call to the plm method, used to be fitted.plm
        # which is not exported
#        cl[[1L]] <- as.name("fitted.plm")
        cl[[1L]] <- as.name("fitted")
        bX <- eval(cl, parent.frame())
        if (is.null(model))  model  <- describe(object, "model")
        if (is.null(effect)) effect <- describe(object, "effect")
        y <- pmodel.response(object, model = model, effect = effect)
        res <- y - bX
    }
    res <- if (model %in% c("between", "fd")) {
      # these models "compress" the data, thus an index does not make sense here
      # -> do not return pseries but plain numeric
      res
    } else {
      structure(res, index = index(object), class = unique(c("pseries", class(res))))
    }
    return(res)
}

#' @rdname plm
#' @export
fitted.plm <- function(object, model = NULL, effect = NULL, ...){
    fittedmodel <- describe(object, "model")
    if (is.null(model)) model <- fittedmodel
    if (is.null(effect)) effect <- describe(object, "effect")
    if (fittedmodel == "random") theta <- ercomp(object)$theta else theta <- NULL
    X <- model.matrix(object, model = "pooling")
    y <- pmodel.response(object, model = "pooling", effect = effect)
    beta <- coef(object)
    comonpars <- intersect(names(beta), colnames(X))
    bX <- as.numeric(crossprod(t(X[, comonpars, drop = FALSE]), beta[comonpars]))
    bX <- structure(bX, index = index(object), class = unique(c("pseries", class(bX))))
    if (fittedmodel == "within"){
        intercept <- mean(y - bX)
        bX <- bX + intercept
    }
    ptransform(bX, model = model, effect = effect, theta = theta)
}
