starX <- function(formula, data, model, rhs = 1, effect){
    apdim <- pdim(data)
    amatrix <- model.matrix(data, model, effect, rhs)
    T <- length(unique(index(data, 2)))
    N <- length(unique(index(data, 1)))
    if (apdim$balanced){
        result <- Reduce("cbind",
                        lapply(seq_len(ncol(amatrix)),
                               function(x)
                               matrix(amatrix[, x], 
                                      ncol = T, byrow = TRUE)[rep(1:N, each = T), ]))
    }
    else{
        Ti <- apdim$Tint$Ti
        result <- lapply(seq_len(ncol(amatrix)), function(x)
                     structure(amatrix[, x], index = index(data), 
                               class = c("pseries", class(amatrix[, x]))))
        result <- Reduce("cbind", lapply(result, as.matrix))
        result <- result[rep(1:N, times = Ti), ]
        result[is.na(result)] <- 0
    }
    result
}   

mylm <- function(y, X, W = NULL){
  names.X <- colnames(X)
  if (is.null(W)) result <- lm(y ~ X - 1)
  else result <- twosls(y, X, W)
  na.coef <- is.na(coef(result))
  if (any(na.coef)){
    ## for debug purpose:
    # warning("Coefficient(s) '", paste((names.X)[na.coef], collapse = ", "), 
    #"' could not be estimated and is (are) dropped.")
      X <- X[, ! na.coef, drop = FALSE]
      if (dim(X)[2] == 0L) stop(paste("estimation not possible: all coefficients",
                                      "omitted from estimation due to aliasing"))
      if (is.null(W)) result <- lm(y ~ X - 1)
      else result <- twosls(y, X, W)
  }
  result$vcov <- vcov(result)
  result$X <- X
  result$y <- y
  result$W <- W
  # aliased is an element of summary.lm-objects:
  # since plm drops aliased coefs, store this info in plm object
  result$aliased <- na.coef
  names(result$aliased) <- names.X
  names(result$coefficients) <- colnames(result$vcov) <- 
      rownames(result$vcov) <- colnames(X)
  result
}

# some elements not listed here...: "assign", "contrast",
# etc... \item{na.action}{if relevant, information about handling of
# NAs by the % model.frame function,} % NB: na.action is currently not
# included as it is not supported


#' Panel Data Estimators
#' 
#' Linear models for panel data estimated using the \code{lm} function on
#' transformed data.
#' 
#' \code{plm} is a general function for the estimation of linear panel
#' models.  It supports the following estimation methods: pooled OLS
#' (\code{model = "pooling"}), fixed effects (\code{"within"}), random
#' effects (\code{"random"}), first--differences (\code{"fd"}), and
#' between (\code{"between"}). It supports unbalanced panels and
#' two--way effects (although not with all methods).
#' 
#' For random effects models, four estimators of the transformation parameter
#' are available by setting \code{random.method} to one of \code{"swar"} (Swamy
#' and Arora (1972)) (default), \code{"amemiya"} (Amemiya (1971)),
#' \code{"walhus"} (Wallace and Hussain (1969)), or \code{"nerlove"} (Nerlove
#' (1971)).
#' 
#' For first--difference models, the intercept is maintained (which from a
#' specification viewpoint amounts to allowing for a trend in the levels
#' model). The user can exclude it from the estimated specification the usual
#' way by adding \code{"-1"} to the model formula.
#' 
#' Instrumental variables estimation is obtained using two--part formulas, the
#' second part indicating the instrumental variables used. This can be a
#' complete list of instrumental variables or an update of the first part. If,
#' for example, the model is \code{y ~ x1 + x2 + x3}, with \code{x1} and
#' \code{x2} endogenous and \code{z1} and \code{z2} external instruments, the
#' model can be estimated with:
#' 
#' \itemize{
#' \item \code{formula = y~x1+x2+x3 | x3+z1+z2},
#' \item \code{formula = y~x1+x2+x3 | . -x1-x2+z1+z2}.
#' }
#' 
#' If an instrument variable estimation is requested, argument
#' \code{inst.method} selects the instrument variable transformation method:
#' \itemize{
#' \item \code{"bvk"} (default) for Balestra and Varadharajan-Krishnakumar (1987),
#' \item \code{"baltagi"} for Baltagi (1981),
#' \item \code{"am"} for Amemiya and MaCurdy (1986),
#' \item \code{"bms"} for Breusch, Mizon, and Schmidt (1989).
#' }
#' 
#' The Hausman--Taylor estimator (Hausman and Taylor (1981)) is
#' computed with arguments \code{random.method = "ht"}, \code{model =
#' "random"}, \code{inst.method = "baltagi"} (the other way with only
#' \code{model = "ht"} is deprecated).
#' 
#' @aliases plm has.intercept
#' @param formula a symbolic description for the model to be
#'     estimated,
#' @param x,object an object of class \code{"plm"},
#' @param data a \code{data.frame},
#' @param subset see \code{\link{lm}},
#' @param weights see \code{\link{lm}},
#' @param na.action see \code{\link{lm}}; currently, not fully
#'     supported,
#' @param effect the effects introduced in the model, one of
#'     \code{"individual"}, \code{"time"}, \code{"twoways"}, or
#'     \code{"nested"},
#' @param model one of \code{"pooling"}, \code{"within"},
#'     \code{"between"}, \code{"random"} \code{"fd"}, or \code{"ht"},
#' @param random.method method of estimation for the variance
#'     components in the random effects model, one of \code{"swar"}
#'     (default), \code{"amemiya"}, \code{"walhus"}, or
#'     \code{"nerlove"},
#' @param random.models an alternative to the previous argument, the
#'     models used to compute the variance components estimations are
#'     indicated,
#' @param random.dfcor a numeric vector of length 2 indicating which
#'     degree of freedom should be used,
#' @param inst.method the instrumental variable transformation: one of
#'     \code{"bvk"}, \code{"baltagi"}, \code{"am"}, or \code{"bms"}
#'     (see also Details),
#' @param index the indexes,
#' @param restrict.matrix a matrix which defines linear restrictions
#'     on the coefficients,
#' @param restrict.rhs the right hand side vector of the linear
#'     restrictions on the coefficients,
#' @param digits number of digits for printed output,
#' @param width the maximum length of the lines in the printed output,
#' @param dx the half--length of the individual lines for the plot
#'     method (relative to x range),
#' @param N the number of individual to plot,
#' @param seed the seed which will lead to individual selection,
#' @param within if \code{TRUE}, the within model is plotted,
#' @param pooling if \code{TRUE}, the pooling model is plotted,
#' @param between if \code{TRUE}, the between model is plotted,
#' @param random if \code{TRUE}, the random effect model is plotted,
#' @param formula. a new formula for the update method,
#' @param evaluate a boolean for the update method, if `TRUE` the
#'     updated model is returned, if `FALSE` the call is returned,
#' @param newdata the new data set for the `predict` method,
#' @param \dots further arguments.
#' 
#' @return An object of class \code{"plm"}.
#'
#' 
#' A \code{"plm"} object has the following elements :
#'
#' \item{coefficients}{the vector of coefficients,}
#' \item{vcov}{the variance--covariance matrix of the coefficients,}
#' \item{residuals}{the vector of residuals (these are the residuals
#' of the (quasi-)demeaned model),}
#' \item{weights}{(only for weighted estimations) weights as
#' specified,}
#' \item{df.residual}{degrees of freedom of the residuals,}
#' \item{formula}{an object of class \code{"pFormula"} describing the model,}
#' \item{model}{the model frame as a \code{"pdata.frame"} containing the
#' variables used for estimation: the response is in first column followed by
#' the other variables, the individual and time indexes are in the 'index'
#' attribute of \code{model},}
#' \item{ercomp}{an object of class \code{"ercomp"} providing the
#' estimation of the components of the errors (for random effects
#' models only),}
#' \item{aliased}{named logical vector indicating any aliased
#' coefficients which are silently dropped by \code{plm} due to
#' linearly dependent terms (see also \code{\link{detect.lindep}}),}
#' \item{call}{the call.}
#' 
#' 
#' It has \code{print}, \code{summary} and \code{print.summary} methods. The
#' \code{summary} method creates an object of class \code{"summary.plm"} that
#' extends the object it is run on with information about (inter alia) F
#' statistic and (adjusted) R-squared of model, standard errors, t--values, and
#' p--values of coefficients, (if supplied) the furnished vcov, see
#' \code{\link{summary.plm}} for further details.
#' @import Formula
#' @importFrom stats alias approx as.formula coef coefficients cor delete.response
#' @importFrom stats deviance df.residual dnorm fitted formula lm lm.fit model.frame
#' @importFrom stats model.matrix model.response model.weights na.omit pchisq pf
#' @importFrom stats pnorm printCoefmat pt qnorm reshape resid residuals sd terms
#' @importFrom stats update var vcov
#' @importFrom grDevices heat.colors rainbow
#' @importFrom graphics abline axis barplot legend lines plot points
#' @export
#' @author Yves Croissant
#' @seealso \code{\link{summary.plm}} for further details about the associated
#' summary method and the "summary.plm" object both of which provide some model
#' tests and tests of coefficients.  \code{\link{fixef}} to compute the fixed
#' effects for "within" models (=fixed effects models).
#' @references
#'
#' \insertRef{AMEM:71}{plm}
#'
#' \insertRef{AMEM:MACUR:86}{plm}
#'
#' \insertRef{BALE:VARA:87}{plm}
#'
#' \insertRef{BALT:81}{plm}
#'
#' \insertRef{BALT:SONG:JUNG:01}{plm}
#'
#' \insertRef{BALT:13}{plm}
#'
#' \insertRef{BREU:MIZO:SCHMI:89}{plm}
#'
#' \insertRef{HAUS:TAYL:81}{plm}
#'
#' \insertRef{NERLO:71}{plm}
#'
#' \insertRef{SWAM:AROR:72}{plm}
#'
#' \insertRef{WALL:HUSS:69}{plm}
#' 
#' @keywords regression
#' @examples
#' 
#' data("Produc", package = "plm")
#' zz <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
#'           data = Produc, index = c("state","year"))
#' summary(zz)
#' 
#' # replicates some results from Baltagi (2013), table 3.1
#' data("Grunfeld", package = "plm")
#' p <- plm(inv ~ value + capital,
#'          data = Grunfeld, model = "pooling")
#' 
#' wi <- plm(inv ~ value + capital,
#'           data = Grunfeld, model = "within", effect = "twoways")
#' 
#' swar <- plm(inv ~ value + capital,
#'             data = Grunfeld, model = "random", effect = "twoways")
#' 
#' amemiya <- plm(inv ~ value + capital,
#'                data = Grunfeld, model = "random", random.method = "amemiya",
#'                effect = "twoways")
#' 
#' walhus <- plm(inv ~ value + capital,
#'               data = Grunfeld, model = "random", random.method = "walhus",
#'               effect = "twoways")
#' 
#' # summary and summary with a funished vcov (passed as matrix, 
#' # as function, and as function with additional argument)
#' summary(wi)
#' summary(wi, vcov = vcovHC(wi))
#' summary(wi, vcov = vcovHC)
#' summary(wi, vcov = function(x) vcovHC(x, method = "white2"))
#' 
#' 
#' # nested random effect model
#' # replicate Baltagi/Song/Jung (2001), p. 378 (table 6), columns SA, WH
#' # == Baltagi (2013), pp. 204-205
#' data("Produc", package = "plm")
#' pProduc <- pdata.frame(Produc, index = c("state", "year", "region"))
#' form <- log(gsp) ~ log(pc) + log(emp) + log(hwy) + log(water) + log(util) + unemp
#' summary(plm(form, data = pProduc, model = "random", effect = "nested"))
#' summary(plm(form, data = pProduc, model = "random", effect = "nested",
#'             random.method = "walhus"))
#' 
#' ## Hausman-Taylor estimator and Amemiya-MaCurdy estimator
#' ## replicate Baltagi (2005, 2013), table 7.4
#' data("Wages", package = "plm")
#' ht <- plm(lwage ~ wks + south + smsa + married + exp + I(exp ^ 2) + 
#'               bluecol + ind + union + sex + black + ed |
#'               bluecol + south + smsa + ind + sex + black |
#'               wks + married + union + exp + I(exp ^ 2), 
#'           data = Wages, index = 595,
#'           random.method = "ht", model = "random", inst.method = "baltagi")
#' summary(ht)
#' 
#' am <- plm(lwage ~ wks + south + smsa + married + exp + I(exp ^ 2) + 
#'               bluecol + ind + union + sex + black + ed |
#'               bluecol + south + smsa + ind + sex + black |
#'               wks + married + union + exp + I(exp ^ 2), 
#'           data = Wages, index = 595,
#'           random.method = "ht", model = "random", inst.method = "am")
#' summary(am)
#'
plm <- function(formula, data, subset, weights, na.action,
                effect = c("individual", "time", "twoways", "nested"),
                model = c("within", "random", "ht", "between", "pooling", "fd"),
                random.method = NULL,
                random.models = NULL,
                random.dfcor = NULL,
                inst.method = c("bvk", "baltagi", "am", "bms"),
                restrict.matrix = NULL,
                restrict.rhs = NULL,
                index = NULL,
                ...){

    if (is.list(formula)){
        # if the first argument is a list (of formulas), then call plmlist and exit
        plmlist <- match.call(expand.dots = FALSE)
        plmlist[[1]] <- as.name("plm.list")
        # eval in nframe and not the usual parent.frame(), relevant?
        nframe <- length(sys.calls())
        plmlist <- eval(plmlist, sys.frame(which = nframe))
        return(plmlist)
    }

    if ((! is.null(restrict.matrix) || ! is.null(restrict.rhs)) && ! is.list(formula)) {
        stop(paste0("arguments 'restrict.matrix' and 'restrict.rhs' cannot yet be used ",
                    "for single equations"))
    }
    dots <- list(...)
    
    # check and match the effect and model arguments
    effect <- match.arg(effect)
    # note that model can be NA, in this case the model.frame is returned
    if (! anyNA(model)) model <- ifelse(effect == "nested",
                                        "random", match.arg(model))

    # input checks for FD model: give informative error messages as
    # described in footnote in vignette
    if (! is.na(model) && model == "fd") {
        if (effect == "time") stop(paste("effect = \"time\" for first-difference model",
                                         "meaningless because cross-sections do not",
                                         "generally have a natural ordering"))
        if (effect == "twoways") stop(paste("effect = \"twoways\" is not defined",
                                            "for first-difference models"))
    }
    
    # Deprecated section
    
    # accept "bmc" (a long-standing typo) for Breusch-Mizon-Schmidt
    # due to backward compatibility
    if (length(inst.method) == 1 && inst.method == "bmc") {
      inst.method <- "bms"
        warning(paste("Use of inst.method = \"bmc\" discouraged, set to \"bms\"",
                      "for Breusch-Mizon-Schmidt instrumental variable transformation"))
    }
    inst.method <- match.arg(inst.method)
  
    
    # pht is no longer maintained
    if (! is.na(model) && model == "ht"){
        ht <- match.call(expand.dots = FALSE)
        m <- match(c("formula", "data", "subset", "na.action", "index"), names(ht), 0)
        ht <- ht[c(1, m)]
        ht[[1]] <- as.name("pht")
        ht <- eval(ht, parent.frame())
        return(ht)
    }
    
    # check whether data and formula are pdata.frame and pFormula and if not
    # coerce them
    orig_rownames <- row.names(data)

    if (! inherits(data, "pdata.frame")) data <- pdata.frame(data, index)
    if (! inherits(formula, "Formula")) formula <- as.Formula(formula)

    # in case of 2-part formula, check whether the second part should
    # be updated, e.g. y ~ x1 + x2 + x3 | . - x2 + z becomes 
    # y ~ x1 + x2 + x3 | x1 + x3 + z
    # use length(formula)[2] because the length is now a vector of length 2
#    if (length(formula)[2] == 2) formula <- expand.formula(formula)
    # eval the model.frame
    cl <- match.call()
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("data", "formula", "subset", "weights", "na.action"), names(mf), 0)
    mf <- mf[c(1, m)]
    names(mf)[2:3] <- c("formula", "data")
    mf$drop.unused.levels <- TRUE
    mf[[1]] <- as.name("model.frame")
    # use the pFormula and pdata.frame which were created if necessary (and not
    # the original formula / data)
    mf$formula <- data
    mf$data <- formula
    data <- eval(mf, parent.frame())

    # preserve original row.names for data [also fancy rownames]; so functions
    # like pmodel.response(), model.frame(), model.matrix(), residuals() return
    # the original row.names eval(mf, parent.frame()) returns row.names as
    # character vector containing the "row_number" with incomplete observations
    # dropped
    row.names(data) <- orig_rownames[as.numeric(row.names(data))]

    # return the model.frame or estimate the model
    if (is.na(model)){
        attr(data, "formula") <- formula
        return(data)
    }

    # note that the model.frame has as attributes the pFormula and the index
    # data.frame
    args <- list(model = model, effect = effect,
                 random.method = random.method,
                 random.models = random.models,
                 random.dfcor = random.dfcor,
                 inst.method = inst.method)
    result <- plm.fit(data, model, effect, random.method,
                      random.models, random.dfcor, inst.method)
    result$call <- cl
    result$args <- args
    result
}

plm.fit <- function(data, model, effect, random.method, 
                    random.models, random.dfcor, inst.method){
    formula <- attr(data, "formula")
    # check for 0 cases like in stats::lm.fit (e.g. due to NA dropping) 
    if (nrow(data) == 0L) stop("0 (non-NA) cases")

    # if a random effect model is estimated, compute the error components
    if (model == "random"){
        is.balanced <- is.pbalanced(data)
        estec <- ercomp(data, effect, method = random.method,
                        models = random.models, dfcor = random.dfcor)        
        sigma2 <- estec$sigma2
        theta <- estec$theta
        if (length(formula)[2] == 2 && effect == "twoways")
            stop(paste("Instrumental variable random effect estimation",
                       "not implemented for two-ways panels"))
    }
    else theta <- NULL

    # For all models except the unbalanced twoways random model, the
    # estimator is obtained as a linear regression on transformed data
    if (! (model == "random" & effect == "twoways" && ! is.balanced)){
        # extract the model.matrix and the model.response actually, this can be
        # done by providing model.matrix and pmodel.response's methods
        # to pdata.frames
        X <- model.matrix(data, rhs = 1, model = model, 
                          effect = effect, theta = theta, cstcovar.rm = "all")
        y <- pmodel.response(data, model = model, 
                             effect = effect, theta = theta)
        if (ncol(X) == 0) stop("empty model")
        
        w <- model.weights(data)
        if (! is.null(w)){
            if (! is.numeric(w)) stop("'weights' must be a numeric vector")
            X <- X * sqrt(w)
            y <- y * sqrt(w)
        }
        else w <- 1
        
        # extract the matrix of instruments if necessary (means here that we
        # have a multi-parts formula)
        if (length(formula)[2] > 1){
            if(!is.null(model.weights(data)) || any(w != 1)) stop("argument 'weights' not yet implemented for instrumental variable models")
            if (length(formula)[2] == 2){
                W <- model.matrix(data, rhs = 2,
                                  model = model, effect = effect,
                                  theta = theta, cstcovar.rm = "all")
            }
            else{
                W <- model.matrix(data, rhs = c(2, 3), model = model,
                                      effect = effect, theta = theta, cstcovar.rm = "all")
            }
            if (model == "random" && inst.method != "bvk"){
                # the bvk estimator seems to have disappeared
                X <- X / sqrt(sigma2["idios"])
                y <- y / sqrt(sigma2["idios"])
                W1 <- model.matrix(data, rhs = 2, model = "within",
                                   effect = effect, theta = theta, cstcovar.rm = "all")
                B1 <- model.matrix(data, rhs = 2, model = "Between",
                                   effect = effect, theta = theta, cstcovar.rm = "all")
                
                if (inst.method %in% c("am", "bms")) 
                    StarW1 <- starX(formula, data, rhs = 2, model = "within",
                                    effect = effect)
                if (length(formula)[2] == 3){
                    W2 <- model.matrix(data, rhs = 3, model = "within",
                                           effect = effect, theta = theta, cstcovar.rm = "all")
                    if (inst.method == "bms")
                        StarW2 <- starX(formula, data, rhs = 3, model = "within",
                                        effect = effect)
                }
                else W2 <- StarW2 <- NULL
                if (inst.method == "baltagi") W <- sqrt(w) * cbind(W1, W2, B1)
                if (inst.method == "am")  W <- sqrt(w) * cbind(W1, W2, B1, StarW1)
                if (inst.method == "bms") W <- sqrt(w) * cbind(W1, W2, B1, StarW1, StarW2)
            }
            if (ncol(W) < ncol(X)) stop("insufficient number of instruments")
        }
        else W <- NULL
        
        result <- mylm(y, X, W)
        df <- df.residual(result)
        vcov <- result$vcov
        aliased <- result$aliased
        
        # in case of a within estimation, correct the degrees of freedom
        if (model == "within"){
            pdim <- pdim(data)
            card.fixef <- switch(effect,
                                 "individual" = pdim$nT$n,
                                 "time"       = pdim$nT$T,
                                 "twoways"    = pdim$nT$n + pdim$nT$T - 1
                                 )
            df <- df.residual(result) - card.fixef
            vcov <- result$vcov * df.residual(result) / df
        }
        result <- list(coefficients = coef(result),
                       vcov         = vcov,
                       residuals    = resid(result),
                       weights      = w,
                       df.residual  = df,
                       formula      = formula,  #fForm
                       model        = data)
        if (is.null(model.weights(data))) result$weights <- NULL
        if (model == "random") result$ercomp <- estec
    }
    else{
        # random twoways unbalanced:
        pdim <- pdim(data)
        TS <- pdim$nT$T
        theta <- estec$theta$id
        phi2mu <- estec$sigma2["time"] / estec$sigma2["idios"]
        Dmu <- model.matrix( ~ factor(index(data)[[2]]) - 1)
        attr(Dmu, "index") <- index(data)
        Dmu <- Dmu - theta * Between(Dmu, "individual")
        X <- model.matrix(data, rhs = 1, model = "random", 
                          effect = "individual", theta = theta)
        y <- pmodel.response(data, model = "random", 
                             effect = "individual", theta = theta)
        P <- solve(diag(TS) + phi2mu * crossprod(Dmu))
        XPX <- crossprod(X)    - phi2mu * crossprod(X, Dmu) %*% P %*% crossprod(Dmu, X)
        XPy <- crossprod(X, y) - phi2mu * crossprod(X, Dmu) %*% P %*% crossprod(Dmu, y)
        gamma <- solve(XPX, XPy)[, , drop = TRUE]

        # residuals 'e' are not the residuals of a quasi-demeaned
        # model but of the 'outer' model
        e <- pmodel.response(data, model = "pooling", effect = effect) -
            as.numeric(model.matrix(data, rhs = 1, model = "pooling") %*% gamma)
        result <- list(coefficients = gamma,
                       vcov         = solve(XPX),
                       formula      = formula, #fForm
                       model        = data,
                       ercomp       = estec,
                       df.residual  = nrow(X) - ncol(X),
                       residuals    = e)
        
        # derive 'aliased' information (this is based on the assumption that
        # estimation fails anyway if singularities are present).
        aliased <- is.na(gamma)
    }
    result$assign <- attr(X, "assign")
    result$contrasts <- attr(X, "contrasts")
    result$args <- list(model = model, effect = effect)
    result$aliased <- aliased
    class(result) <- c("plm", "panelmodel")
    result
}

tss <- function(x, ...){
  UseMethod("tss")
}

tss.default <- function(x){
  var(x) * (length(x) - 1)
}

tss.plm <- function(x, model = NULL){
    if (is.null(model)) model <- describe(x, "model")
    effect <- describe(x, "effect")
    if (model == "ht") model <- "pooling"
    if (model == "random") theta <- x$ercomp$theta else theta <- NULL
    tss(pmodel.response(x, model = model, effect = effect, theta = theta))
}

#' R squared and adjusted R squared for panel models
#' 
#' This function computes R squared or adjusted R squared for plm objects.  It
#' allows to define on which transformation of the data the (adjusted) R
#' squared is to be computed and which method for calculation is used.
#' 
#' 
#' @param object an object of class \code{"plm"},
#' @param model on which transformation of the data the R-squared is to be
#' computed. If \code{NULL}, the transformation used to estimate the model is
#' also used for the computation of R squared,
#' @param type indicates method which is used to compute R squared. One of\cr
#' \code{"rss"} (residual sum of squares),\cr \code{"ess"} (explained sum of
#' squares), or\cr \code{"cor"} (coefficient of correlation between the fitted
#' values and the response),
#' @param dfcor if \code{TRUE}, the adjusted R squared is computed.
#' @return A numerical value. The R squared or adjusted R squared of the model
#' estimated on the transformed data, e. g. for the within model the so called
#' "within R squared".
#' @seealso \code{\link{plm}} for estimation of various models;
#' \code{\link{summary.plm}} which makes use of \code{r.squared}.
#' @keywords htest
#' @export
#' @examples
#' 
#' data("Grunfeld", package = "plm")
#' p <- plm(inv ~ value + capital, data = Grunfeld, model = "pooling")
#' r.squared(p)
#' r.squared(p, dfcor = TRUE)
#' 
r.squared <- function(object, model = NULL,
                      type = c("cor", "rss", "ess"), dfcor = FALSE){
    if (is.null(model)) model <- describe(object, "model")
    effect <- describe(object, "effect")
    type <- match.arg(type)
    if (type == "cor"){
        y <- pmodel.response(object, model = model, effect = effect)
        haty <- fitted(object, model = model, effect = effect)
        R2 <- cor(y, haty)^2
    }
    if (type == "rss"){
        R2 <- 1 - deviance(object, model = model) / tss(object, model = model)
    }
    if (type == "ess"){
        haty <- fitted(object, model = model)
        mhaty <- mean(haty)
        ess <- as.numeric(crossprod((haty - mhaty)))
        R2 <- ess / tss(object, model = model)
    }
    # Kevin Tappe 2015-10-19, the computation of the adjusted R2 was wrong
    if (dfcor) R2 <- 1 - (1 - R2) * (length(resid(object)) - 1) / df.residual(object)
    R2
}



# describe function: extract characteristics of plm model
describe <- function(x,
                     what = c("model", "effect", "random.method",
                              "inst.method", "transformation", "ht.method")){
  what <- match.arg(what)
  cl <- x$args
  switch(what,
         "model"          = ifelse(!is.null(cl$model), cl$model, "within"),
         "effect"         = ifelse(!is.null(cl$effect), cl$effect, "individual"),
         "random.method"  = ifelse(!is.null(cl$random.method),
                                 cl$random.method, "swar"),
         "inst.method"    = ifelse(!is.null(cl$inst.method),
                                 cl$inst.method, "bvk"),
         "transformation" = ifelse(!is.null(cl$transformation),
                                 cl$transformation, "d"),
         "ht.method"      = ifelse(!is.null(cl$ht.method), cl$ht.method, "ht")
         )
}

