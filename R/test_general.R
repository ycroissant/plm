data.name <- function(x){
  data.name <- paste(deparse(x$call$formula))
  if (length(data.name) > 1) paste(data.name[1], "...")
  else data.name
}

#' Hausman Test for Panel Models
#' 
#' Specification test for panel models.
#' 
#' The Hausman test (sometimes also called Durbin--Wu--Hausman test)
#' is based on the difference of the vectors of coefficients of two
#' different models.  The `panelmodel` method computes the original
#' version of the test based on a quadratic form
#' \insertCite{HAUS:78}{plm}. The `formula` method, if
#' `method="chisq"` (default), computes the original version of the
#' test based on a quadratic form; if `method="aux"` then the
#' auxiliary-regression-based version in Wooldridge (2010,
#' Sec. 10.7.3.) is computed instead \insertCite{@WOOL:10 Sec.10.7.3}{plm}.
#' Only the latter can be robustified by specifying a robust
#' covariance estimator as a function through the argument `vcov` (see
#' **Examples**).
#' 
#' The equivalent tests in the **one-way** case using a between
#' model (either "within vs. between" or "random vs. between")
#' \insertCite{@see @HAUS:TAYL:81 or @BALT:13 Sec.4.3}{plm} can also
#' be performed by `phtest`, but only for `test = "chisq"`, not for
#' the regression-based test. NB: These equivalent tests using the
#' between model do not extend to the two-ways case.  There are,
#' however, some other equivalent tests,
#' \insertCite{@see @KANG:85 or @BALT:13 Sec.4.3.7}{plm},
#' but those are unsupported by `phtest`.
#' 
#' @aliases phtest
#' @param x an object of class `"panelmodel"` or `"formula"`,
#' @param x2 an object of class `"panelmodel"`,
#' @param model a character vector containing the names of two models
#' (length(model) must be 2),
#' @param data a `data.frame`,
#' @param method one of `"chisq"` or `"aux"`,
#' @param index an optional vector of index variables,
#' @param vcov an optional covariance function,
#' @param \dots further arguments to be passed on. For the formula method,
#' place argument `effect` here to compare e.g. twoway models
#' (`effect = "twoways"`) Note: Argument `effect` is not respected in
#' the panelmodel method.
#' @return An object of class `"htest"`.
#' @export
#' @author Yves Croissant, Giovanni Millo
#' @references
#'
#' \insertRef{HAUS:78}{plm}
#'
#' \insertRef{HAUS:TAYL:81}{plm}
#'
#' \insertRef{KANG:85}{plm}
#'
#' \insertRef{WOOL:10}{plm}
#'
#' \insertRef{BALT:13}{plm}
#' 
#' @keywords htest
#' @examples
#' 
#' data("Gasoline", package = "plm")
#' form <- lgaspcar ~ lincomep + lrpmg + lcarpcap
#' wi <- plm(form, data = Gasoline, model = "within")
#' re <- plm(form, data = Gasoline, model = "random")
#' phtest(wi, re)
#' phtest(form, data = Gasoline)
#' phtest(form, data = Gasoline, method = "aux")
#' 
#' # robust Hausman test (regression-based)
#' phtest(form, data = Gasoline, method = "aux", vcov = vcovHC)
#' 
#' # robust Hausman test with vcov supplied as a
#' # function and additional parameters
#' phtest(form, data = Gasoline, method = "aux",
#'   vcov = function(x) vcovHC(x, method="white2", type="HC3"))
#' 
phtest <- function(x,...){
  UseMethod("phtest")
}

#' @rdname phtest
#' @export
phtest.formula <- function(x, data, model = c("within", "random"),
                            method = c("chisq", "aux"),
                            index = NULL, vcov = NULL, ...) {
  # NB: No argument 'effect' here, maybe introduce?
  #     it gets evaluated due to the eval() call for method="chisq"
  #     and since rev. 305 due to extraction from dots (...) in method="aux" as a quick fix
  #    If introduced as argument, change doc accordingly (currently, effect arg is mentioned in ...)
  
    if (length(model)!=2) stop("two models should be indicated")
    for (i in 1:2){
        model.name <- model[i]
        if(!(model.name %in% names(model.plm.list))){
            stop("model must be one of ", oneof(model.plm.list))
        }
    }
    switch(match.arg(method),
           chisq={
               cl <- match.call(expand.dots = TRUE)
               cl$model <- model[1]
               names(cl)[2] <- "formula"
               m <- match(plm.arg, names(cl), 0)
               cl <- cl[c(1,m)]
               cl[[1]] <- as.name("plm")
               plm.model.1 <- eval(cl, parent.frame())
               plm.model.2 <- update(plm.model.1, model = model[2])
               return(phtest(plm.model.1, plm.model.2))
           },
           aux={
               ## some interface checks here
               if (model[1] != "within") {
                   stop("Please supply 'within' as first model type")
               }
             
               if (!is.null(vcov) && !is.function(vcov)) stop("argument 'vcov' needs to be a function")
             
               ## set pdata.frame
               if (!inherits(data, "pdata.frame")) data <- pdata.frame(data, index = index) #, ...)

               row.names(data) <- NULL # reset rownames of original data set (->numbers rownames in clean sequence) to make rownames
                                       # comparable for later comparison to obs used in estimation of models (get rid of NA values)
                                       # [needed because pmodel.response() and model.matrix() do not retain fancy rownames, but rownames]
               
               # rev. 305: quick and dirty fix for missing effect argument in function 
               # signature for formula interface/test="aux": see if effect is in dots and extract
                  dots <- list(...)
                  # print(dots) # DEBUG printing
               if (!is.null(dots$effect)) effect <- dots$effect else effect <- NULL
               # calculatate FE and RE model

               fe_mod <- plm(formula = x, data = data, model = model[1], effect = effect)

               re_mod <- plm(formula = x, data = data, model = model[2], effect = effect)

                ## DEBUG printing:
                 # print(effect)
                 # print(model)
                 # print(paste0("mod1: ", describe(fe_mod, "effect")))
                 # print(paste0("mod2: ", describe(re_mod, "effect")))
                 # print(fe_mod)
                 # print(re_mod)
               reY <- pmodel.response(re_mod)
#               reX <- model.matrix(re_mod)[ , -1, drop = FALSE] # intercept not needed; drop=F needed to prevent matrix
#               feX <- model.matrix(fe_mod, cstcovar.rm = TRUE)                      # from degenerating to vector if only one regressor
               reX <- model.matrix(re_mod, cstcovar.rm = "intercept")
               feX <- model.matrix(fe_mod, cstcovar.rm = "all")

               dimnames(feX)[[2]] <- paste(dimnames(feX)[[2]], "tilde", sep=".")
               ## estimated models could have fewer obs (due dropping of NAs) compared to the original data
               ## => match original data and observations used in estimated models
               ## routine adapted from lmtest::bptest
               commonrownames <- intersect(intersect(intersect(row.names(data), names(reY)), row.names(reX)), row.names(feX))
               if (!(all(c(row.names(data) %in% commonrownames, commonrownames %in% row.names(data))))) {
                 data <- data[commonrownames, ]
                 reY  <- reY[commonrownames]
                 reX  <- reX[commonrownames, ]
                 feX  <- feX[commonrownames, ]
               }
               
               # Tests of correct matching of obs (just for safety ...)
               if (!all.equal(length(reY), nrow(data), nrow(reX), nrow(feX)))
                  stop("number of cases/observations do not match, most likely due to NAs in \"data\"")
                if (any(c(is.na(names(reY)), is.na(row.names(data)), is.na(row.names(reX)), is.na(row.names(feX)))))
                    stop("one (or more) rowname(s) is (are) NA")
                if (!all.equal(names(reY), row.names(data), row.names(reX), row.names(feX)))
                  stop("row.names of cases/observations do not match, most likely due to NAs in \"data\"")

               ## fetch indices here, check pdata
               ## construct data set and formula for auxiliary regression
               data <- pdata.frame(cbind(index(data), reY, reX, feX))
               auxfm <- as.formula(paste("reY~",
                                         paste(dimnames(reX)[[2]],
                                               collapse="+"), "+",
                                         paste(dimnames(feX)[[2]],
                                               collapse="+"), sep=""))
               auxmod <- plm(formula = auxfm, data = data, model = "pooling")
               nvars <- dim(feX)[[2]]
               R <- diag(1, nvars)
               r <- rep(0, nvars) # here just for clarity of illustration
               omega0 <- vcov(auxmod)[(nvars+2):(nvars*2+1),
                                      (nvars+2):(nvars*2+1)]
               Rbr <- R %*% coef(auxmod)[(nvars+2):(nvars*2+1)] - r

               h2t <- as.numeric(crossprod(Rbr, solve(omega0, Rbr)))
               ph2t <- pchisq(h2t, df = nvars, lower.tail = FALSE)

               df <- nvars
               names(df) <- "df"
               names(h2t) <- "chisq"

               if (!is.null(vcov)) {
                   vcov <- paste(", vcov: ",
                                  paste(deparse(substitute(vcov))),
                                  sep="")
               }

               haus2 <- list(statistic   = h2t,
                             p.value     = ph2t,
                             parameter   = df,
                             method      = paste("Regression-based Hausman test",
                                                  vcov, sep=""),
                             alternative = "one model is inconsistent",
                             data.name   = paste(deparse(substitute(x))))
               class(haus2) <- "htest"
               return(haus2)
           })
}

#' @rdname phtest
#' @export
phtest.panelmodel <- function(x, x2, ...){
  coef.wi <- coef(x)
  coef.re <- coef(x2)
  vcov.wi <- vcov(x)
  vcov.re <- vcov(x2)
  names.wi <- names(coef.wi)
  names.re <- names(coef.re)
  common_coef_names <- names.re[names.re %in% names.wi]
  coef.h <- common_coef_names[!(common_coef_names %in% "(Intercept)")] # drop intercept if included (relevant when between model inputed)
  if(length(coef.h) == 0) stop("no common coefficients in models")
  dbeta <- coef.wi[coef.h] - coef.re[coef.h]
  df <- length(dbeta)
  dvcov <- vcov.wi[coef.h, coef.h] - vcov.re[coef.h, coef.h]
  
  #### BEGIN cater for equivalent test within vs. between
    # Baltagi (2013), Sec. 4.3, pp. 77, 81
    modx  <- describe(x,  what = "model")
    modx2 <- describe(x2, what = "model")
    effx  <- describe(x,  what = "effect")
    effx2 <- describe(x2, what = "effect")
    
    # Tests with between model do not extend to two-ways case -> give error
    # There are, however, some equiv. tests with the individual/time between 
    # model, but let's not support them (see Kang (1985), Baltagi (2013), Sec. 4.3.7)
    if (   (modx  == "between" | modx2 == "between")
        && (effx == "twoways" | effx2 == "twoways")) stop("tests with between model in twoways case not supported")
    
    # in case of one-way within vs. between (m3 in Baltagi (2013), pp. 77, 81)
    # the variances need to be added (not subtracted like in the other cases)
    if (  (modx  == "within" && modx2 == "between")
        | (modx2 == "within" && modx  == "between")) {
      dvcov <- vcov.wi[coef.h, coef.h] + vcov.re[coef.h, coef.h]
    }
  #### END cater for equivalent tests with between model

  stat <- as.numeric(abs(t(dbeta) %*% solve(dvcov) %*% dbeta))
  pval <- pchisq(stat, df = df, lower.tail = FALSE)
  names(stat) <- "chisq"
  parameter <- df
  names(parameter) <- "df"
  alternative <- "one model is inconsistent"
#  null.value <- "both models are consistent"
  
  ## DEBUG printing:
     # print(paste0("mod1: ", describe(x,  "effect")))
     # print(paste0("mod2: ", describe(x2, "effect")))

  res <- list(statistic    = stat,
              p.value      = pval,
              parameter    = parameter,
              method       = "Hausman Test",
              data.name    = data.name(x),
 #             null.value  = null.value,
              alternative  = alternative)
  class(res) <- "htest"
  return(res)
}

############## plmtest() ############################################
# For a concise overview with original references, see
# Baltagi (2013), Econometric Analysis of Panel Data, 5th edition, pp. 68-76 (balanced), pp. 200-203 (unbalanced).
#
# balanced (original) version of Breusch-Pagan test:
#     T.S. Breusch & A.R. Pagan (1979),
#       A Simple Test for Heteroscedasticity and Random Coefficient Variation,
#       Econometrica 47, pp. 1287-1294
#
# unbalanced version:
#     Baltagi/Li (1990),
#       A lagrange multiplier test for the error components model with incomplete panels,
#       Econometric Reviews, 9, pp. 103-107,


# pchibarsq: helper function: "p-function" for mixed chisq (also called chi-bar-squared)
# used in plmtest(., type = "ghm"), see Baltagi (2013), pp. 71-72, 74, 88, 202-203, 209
#
# a reference for the distribution seems to be
# Dykstra, R./El Barmi, H., Chi-Bar-Square Distributions, in: Encyclopedia of Statistical Sciences, 
# DOI: 10.1002/0471667196.ess0265.pub2
pchibarsq <- function(q, df, weights, lower.tail = TRUE, ... ) {
  # NB: other parameters in dots (...): not checked if valid! (ncp, log, ...)
  res <- sum(weights * pchisq(q, df = df, lower.tail = lower.tail, ...))
  return(res)
}




#' Lagrange FF Multiplier Tests for Panel Models
#' 
#' Test of individual and/or time effects for panel models.
#' 
#' These Lagrange multiplier tests use only the residuals of the
#' pooling model.  The first argument of this function may be either a
#' pooling model of class `plm` or an object of class `formula`
#' describing the model. For inputted within (fixed effects) or random
#' effects models, the corresponding pooling model is calculated
#' internally first as the tests are based on the residuals of the
#' pooling model.
#' 
#' The `"bp"` test for unbalanced panels was derived in
#' \insertCite{BALT:LI:90;textual}{plm}
#' (1990), the `"kw"` test for unbalanced panels in
#' \insertCite{BALT:CHAN:LI:98;textual}{plm}.
#'
#' The `"ghm"` test and the `"kw"` test were extended to two-way
#' effects in \insertCite{BALT:CHAN:LI:92;textual}{plm}.
#' 
#' For a concise overview of all these statistics see
#' \insertCite{BALT:03;textual}{plm}, Sec. 4.2, pp. 68--76 (for balanced
#' panels) and Sec. 9.5, pp. 200--203 (for unbalanced panels).
#' 
#' @aliases plmtest
#' @param x an object of class `"plm"` or a formula of class
#'     `"formula"`,
#' @param data a `data.frame`,
#' @param effect a character string indicating which effects are
#'     tested: individual effects (`"individual"`), time effects
#'     (`"time"`) or both (`"twoways"`),
#' @param type a character string indicating the test to be performed:
#'
#' - `"honda"` (default) for \insertCite{HOND:85;textual}{plm},
#' - `"bp"` for \insertCite{BREU:PAGA:80;textual}{plm},
#' - `"kw"` for \insertCite{KING:WU:97;textual}{plm}, or
#' - `"ghm"` for \insertCite{GOUR:HOLL:MONF:82;textual}{plm} for
#'     unbalanced panel data sets, the respective unbalanced version
#'     of the tests are computed,
#' 
#' @param \dots further arguments passed to `plmtest`.
#' @return An object of class `"htest"`.
#' @note For the King-Wu statistics (`"kw"`), the oneway statistics
#'     (`"individual"` and `"time"`) coincide with the respective
#'     Honda statistics (`"honda"`); twoway statistics of `"kw"` and
#'     `"honda"` differ.
#' @export
#' @author Yves Croissant (initial implementation), Kevin Tappe
#'     (generalization to unbalanced panels)
#' @seealso [pFtest()] for individual and/or time effects tests based
#'     on the within model.
#' @references
#'
#' \insertRef{BALT:13}{plm}
#'
#' \insertRef{BALT:LI:90}{plm}
#'
#' \insertRef{BALT:CHAN:LI:92}{plm}
#'
#' \insertRef{BALT:CHAN:LI:98}{plm}
#'
#' \insertRef{BREU:PAGA:80}{plm}
#'
#' \insertRef{GOUR:HOLL:MONF:82}{plm}
#'
#' \insertRef{HOND:85}{plm}
#'
#' \insertRef{KING:WU:97}{plm}
#' 
#' @keywords htest
#' @examples
#' 
#' data("Grunfeld", package = "plm")
#' g <- plm(inv ~ value + capital, data = Grunfeld, model = "pooling")
#' plmtest(g)
#' plmtest(g, effect="time")
#' plmtest(inv ~ value + capital, data = Grunfeld, type = "honda")
#' plmtest(inv ~ value + capital, data = Grunfeld, type = "bp")
#' plmtest(inv ~ value + capital, data = Grunfeld, type = "bp",  effect = "twoways")
#' plmtest(inv ~ value + capital, data = Grunfeld, type = "ghm", effect = "twoways")
#' plmtest(inv ~ value + capital, data = Grunfeld, type = "kw",  effect = "twoways")
#' 
#' Grunfeld_unbal <- Grunfeld[1:(nrow(Grunfeld)-1), ] # create an unbalanced panel data set
#' g_unbal <- plm(inv ~ value + capital, data = Grunfeld_unbal, model = "pooling")
#' plmtest(g_unbal) # unbalanced version of test is indicated in output
#' 
plmtest <- function(x, ...){
  UseMethod("plmtest")
}

#' @rdname plmtest
#' @export
plmtest.plm <- function(x,
                        effect = c("individual", "time", "twoways"),
                        type = c("honda", "bp", "ghm", "kw"),
                        ...) {
  
  effect <- match.arg(effect)
  type <- match.arg(type)
  if (describe(x, "model") != "pooling") x <- update(x, model = "pooling")
  pdim <- pdim(x)
  n <- pdim$nT$n
  T <- pdim$nT$T
  N_obs <- pdim$nT$N
  balanced <- pdim$balanced
  index <- attr(model.frame(x), "index")
  id <- index[[1]]
  time <- index[[2]]
  T_i <- pdim$Tint$Ti
  N_t <- pdim$Tint$nt
  res <- resid(x)
  
  ### calc of parts of test statistic ##
  # calc. is done w/o using matrix calculation, see e.g. Baltagi/Li (1990), p. 106
  A1 <- as.numeric(crossprod(tapply(res, id, sum)) / sum(res ^ 2) - 1)   # == A1 <- sum(tapply(res,id,sum)^2)/sum(res^2) - 1
  A2 <- as.numeric(crossprod(tapply(res, time, sum)) / sum(res ^ 2) - 1) # == A2 <- sum(tapply(res,time,sum)^2)/sum(res^2) - 1
  
  M11 <- sum(T_i ^ 2)
  M22 <- sum(N_t ^ 2)
  
  LM1 <- N_obs * (1 / sqrt(2 * (M11 - N_obs))) * A1 # == sqrt( (((N_obs)^2) / 2) * ( A1^2 / (M11 - N_obs)) ) [except sign due to positive sqrt]
  LM2 <- N_obs * (1 / sqrt(2 * (M22 - N_obs))) * A2 # == sqrt( (((N_obs)^2) / 2) * ( A2^2 / (M22 - N_obs)) ) [except sign due to positive sqrt]
  ### END calc of parts of test statistic ##
  
  
  if (effect != "twoways"){
    # oneway
    if (!type %in% c("honda", "bp", "kw"))
      stop("type must be one of \"honda\", \"bp\" or \"kw\" for a one way model") # kw oneway coincides with honda
    
    stat <- ifelse(effect == "individual", LM1, LM2)
    stat <- switch(type,
                     honda = c(normal = stat),
                     bp    = c(chisq  = stat ^ 2),
                     kw    = c(normal = stat))
    
    parameter <- switch(type,
                          honda = NULL,
                          bp = c(df = 1), # df = 1 in the oneway case (Baltagi (2013), p. 70)
                          kw = NULL)
    
    pval <- switch(type,
                     honda = pnorm(stat, lower.tail = FALSE), # honda oneway ~ N(0,1), alternative is one-sided (Baltagi (2013), p. 71/202)
                     bp    = pchisq(stat, df = parameter, lower.tail = FALSE), # df = 1 in the one-way case, alternative is two-sided (Baltagi (2013), p. 70/201)
                     kw    = pnorm(stat, lower.tail = FALSE)) # kw oneway ~ N(0,1), alternative is one-sided (Baltagi (2013), p. 71/202)
    # END oneway
  }
  else { # twoways
    stat <- switch(type,
                   honda = c(normal = (LM1 + LM2) / sqrt(2)),
                   bp    = c(chisq = LM1 ^ 2 + LM2 ^ 2),
                   kw    = c(normal = (sqrt(M11 - N_obs) / sqrt(M11 + M22 - 2 * N_obs)) * LM1 +
                                 (sqrt(M22 - N_obs) / sqrt(M11 + M22 - 2 * N_obs)) * LM2),
                   ghm   = c(chibarsq = max(0, LM1) ^ 2 + max(0, LM2) ^ 2))
    
    parameter <- switch(type,
                          honda = NULL,
                          bp    = c(df = 2), # df = 2 in the twoway case (Baltagi (2013), p. 70/201)
                          kw    = NULL,
                          ghm   = c(df0 = 0L, df1 = 1L, df2 = 2L, w0 = 1/4, w1 = 1/2, w2 = 1/4)) # chibarsquared (mixed chisq) has several dfs and weights (Baltagi (2013), p. 72/202)
    
    pval <- switch(type,
                     honda = pnorm(stat, lower.tail = FALSE), # honda two-ways ~ N(0,1), alternative is one-sided (Baltagi (2013), p. 71/202)
                     bp    = pchisq(stat, df = parameter, lower.tail = FALSE),  # is df = 2 in the twoway case, alternative is two-sided (Baltagi (2013), p. 70/201)
                     kw    = pnorm(stat, lower.tail = FALSE), # kw twoways ~ N(0,1), alternative is one-sided (Baltagi (2013), p. 71/202)
                     ghm   = pchibarsq(stat, df = c(0L, 1L, 2L), weights = c(1/4, 1/2, 1/4), lower.tail = FALSE)) # mixed chisq (also called chi-bar-square), see Baltagi (2013), pp. 71-72, 74, 88, 202-203, 209
  } # END twoways
  
  method.type <- switch(type,
                          honda  = "Honda",
                          bp     = "Breusch-Pagan",
                          ghm    = "Gourieroux, Holly and Monfort",
                          kw     = "King and Wu")
  
  method.effect <- switch(effect,
                            id      = "individual effects",
                            time    = "time effects",
                            twoways = "two-ways effects")
  
  balanced.type <- ifelse(balanced, "balanced", "unbalanced")
  
  method <- paste("Lagrange Multiplier Test - ", method.effect,
                  " (", method.type, ") for ", balanced.type, " panels", sep="")
  
  if (type %in% c("honda", "kw")) {
    RVAL <- list(statistic = stat,
                 p.value   = pval,
                 method    = method,
                 data.name = data.name(x))
  }
  else { # bp, ghm
    RVAL <- list(statistic = stat,
                 p.value   = pval,
                 method    = method,
                 parameter = parameter,
                 data.name = data.name(x))
  }
  
  RVAL$alternative <- "significant effects" # TODO: maybe distinguish b/w one-sided and two-sided alternatives?
                                            #       (bp: two-sided alt.; all others: one-sided alt.?)
  
  class(RVAL) <- "htest"
  return(RVAL)
}

#' @rdname plmtest
#' @export
plmtest.formula <- function(x, data, ...,
                            effect = c("individual", "time", "twoways"),
                            type = c("honda", "bp", "ghm", "kw")) {
  
  cl <- match.call(expand.dots = TRUE)
  cl$model <- "pooling" # plmtest is performed on the pooling model...
  cl$effect <- NULL     # ... and pooling model has no argument effect...
  cl$type <- NULL       # ... and no argument type => see below: pass on args effect and type to plmtest.plm()
  names(cl)[2] <- "formula"
  m <- match(plm.arg, names(cl), 0)
  cl <- cl[c(1,m)]
  cl[[1]] <- as.name("plm")
  plm.model <- eval(cl, parent.frame())
  plmtest(plm.model, effect = effect, type = type) # pass on args effect and type to plmtest.plm()
}


#' F Test for Individual and/or Time Effects
#' 
#' Test of individual and/or time effects based on the comparison of the
#' `within` and the `pooling` model.
#' 
#' For the `plm` method, the argument of this function is two `plm`
#' objects, the first being a within model, the second a pooling
#' model. The effects tested are either individual, time or twoways,
#' depending on the effects introduced in the within model.
#' 
#' @aliases pFtest
#' @param x an object of class `"plm"` or of class `"formula"`,
#' @param z an object of class `"plm"`,
#' @param data a `data.frame`,
#' @param \dots further arguments.
#' @return An object of class `"htest"`.
#' @export
#' @author Yves Croissant
#' @seealso [plmtest()] for Lagrange multiplier tests of individuals
#'     and/or time effects.
#' @keywords htest
#' @examples
#' 
#' data("Grunfeld", package="plm")
#' gp <- plm(inv ~ value + capital, data = Grunfeld, model = "pooling")
#' gi <- plm(inv ~ value + capital, data = Grunfeld,
#'           effect = "individual", model = "within")
#' gt <- plm(inv ~ value + capital, data = Grunfeld,
#'           effect = "time", model = "within")
#' gd <- plm(inv ~ value + capital, data = Grunfeld,
#'           effect = "twoways", model = "within")
#' pFtest(gi, gp)
#' pFtest(gt, gp)
#' pFtest(gd, gp)
#' pFtest(inv ~ value + capital, data = Grunfeld, effect = "twoways")
#' 
pFtest <- function(x, ...){
  UseMethod("pFtest")
}

#' @rdname pFtest
#' @export
pFtest.formula <- function(x, data, ...){
  cl <- match.call(expand.dots = TRUE)
  cl$model <- "within"
  names(cl)[2] <- "formula"
  m <- match(plm.arg,names(cl),0)
  cl <- cl[c(1,m)]
  cl[[1]] <- as.name("plm")
  plm.within <- eval(cl,parent.frame())
  plm.pooling <- update(plm.within, model = "pooling")
  pFtest(plm.within, plm.pooling, ...)
}

#' @rdname pFtest
#' @export
pFtest.plm <- function(x, z, ...){
  within <- x
  pooling <- z
  ## leave this interface check commented because pkg AER (reverse dependency) has examples that
  ## use pFtest(within_twoway, within_time)
  # if (! (describe(x, "model") == "within" && describe(z, "model") == "pooling"))
  #  stop("the two arguments should be a 'within' and a 'pooling' model (in this order)")
  
  effect <- describe(x, "effect")
  df1 <- df.residual(pooling)-df.residual(within)
  df2 <- df.residual(within)
  ssrp <- as.numeric(crossprod(residuals(pooling)))
  ssrw <- as.numeric(crossprod(residuals(within)))
  stat <- (ssrp-ssrw)/ssrw/df1*df2
  names(stat) <- "F"
  parameter <- c(df1, df2)
  names(parameter) <- c("df1", "df2")
  pval <- pf(stat, df1, df2, lower.tail = FALSE)
  alternative <- "significant effects"
  res <- list(statistic   = stat,
              p.value     = pval,
              method      = paste("F test for ", effect, " effects",sep=""),
              parameter   = parameter,
              data.name   = data.name(x),
              alternative = alternative)
  class(res) <- "htest"
  res
}

############## pwaldtest() ############################################
# pwaldtest is used in summary.plm, summary.pht, summary.pgmm to compute the
# Chi-square or F statistic, but can be used as a stand-alone test of
# joint significance of all slopes
#
# Short intro (but see associated help file)
# arg 'vcov' non-NULL => the robust tests are carried out
# arg df2adj == TRUE does finite-sample/cluster adjustment for F tests's df2
# args .df1, .df2 are only there if user wants to do overwriting of dfs (user has final say)
#
# Chi-sq test for IV models as in Wooldridge (1990), A note on the Lagrange multiplier and F-statistics for two stage least
#                                                    squares regressions, Economics Letters 34: 151-155.

#' Wald-style Chi-square Test and F Test
#' 
#' Wald-style Chi-square test and F test of slope coefficients being
#' zero jointly, including robust versions of the tests.
#' 
#' 
#' `pwaldtest` can be used stand--alone with a plm object, a pvcm object,
#' and a pgmm object (for pvcm objects only the 'random' type is valid and no
#' further arguments are processed; for pgmm objects only arguments `param`
#' and `vcov` are valid). It is also used in
#' [summary.plm()] to produce the F statistic and the Chi-square
#' statistic for the joint test of coefficients and in [summary.pgmm()].
#' 
#' `pwaldtest` performs the test if the slope coefficients of a panel
#' regression are jointly zero. It does not perform general purpose
#' Wald-style tests (for those, see [lmtest::waldtest()] (from package
#' \CRANpkg{lmtest}) or [car::linearHypothesis()] (from package
#' \CRANpkg{car})).
#' 
#' If a user specified variance-covariance matrix/function is given in
#' argument `vcov`, the robust version of the tests are carried out.
#' In that case, if the F test is requested (`test = "F"`) and no
#' overwriting of the second degrees of freedom parameter is given (by
#' supplying argument (`.df2`)), the adjustment of the second degrees
#' of freedom parameter is performed by default. The second degrees of
#' freedom parameter is adjusted to be the number of unique elements
#' of the cluster variable - 1, e. g., the number of individuals minus 1.
#' For the degrees of freedom adjustment of the F test in general,
#' see e. g. \insertCite{CAME:MILL:15;textual}{plm}, section VII;
#' \insertCite{ANDR:GOLS:SCMI:13}{plm}, pp. 126, footnote 4.
#' 
#' The degrees of freedom adjustment requires the vcov object supplied
#' or created by a supplied function to carry an attribute called
#' "cluster" with a known clustering described as a character (for now
#' this could be either `"group"` or `"time"`). The vcovXX functions
#' of the package \pkg{plm} provide such an attribute for their
#' returned variance--covariance matrices. No adjustment is done for
#' unknown descriptions given in the attribute "cluster" or when the
#' attribute "cluster" is not present. Robust vcov objects/functions
#' from package \CRANpkg{clubSandwich} work as inputs to `pwaldtest`'s
#' F test because a they are translated internally to match the needs
#' described above.
#' 
#' @aliases pwaldtest
#' @param x an estimated model of which the coefficients should be
#'     tested (usually of class `"plm"`/`"pvcm"`/`"pgmm"`)`,
#' @param test a character, indicating the test to be performed, may
#'     be either `"Chisq"` or `"F"` for the Wald-style
#'     Chi-square test or F test, respectively,
#' @param vcov `NULL` by default; a `matrix` giving a
#'     variance--covariance matrix or a function which computes such;
#'     if supplied (non `NULL`), the test is carried out using
#'     the variance--covariance matrix indicated resulting in a robust
#'     test,
#' @param df2adj logical, only relevant for `test = "F"`,
#'     indicating whether the adjustment for clustered standard errors
#'     for the second degrees of freedom parameter should be performed
#'     (see **Details**, also for further requirements regarding
#'     the variance--covariance matrix in `vcov` for the
#'     adjustment to be performed),
#' @param .df1 a numeric, used if one wants to overwrite the first
#'     degrees of freedom parameter in the performed test (usually not
#'     used),
#' @param .df2 a numeric, used if one wants to overwrite the second
#'     degrees of freedom parameter for the F test (usually not used),
#' @param param (for pgmm method only): select the parameters to be tested:
#'     `"coef"`, `"time"`, or `"all"``.
#' @param \dots further arguments (currently none).
#' @return An object of class `"htest"`, except for pvcm's within model for which
#'         a data.frame with results of the Wald chi-square tests and F tests per
#'         regression is returned.
#' @export
#' @author Yves Croissant (initial implementation) and Kevin Tappe
#'     (extensions: vcov argument and F test's df2 adjustment)
#' @seealso
#' 
#' [vcovHC()] for an example of the vcovXX functions, a robust
#' estimation for the variance--covariance matrix; [summary.plm()]
#' @references
#'
#' \insertRef{WOOL:10}{plm}
#'
#' \insertRef{ANDR:GOLS:SCMI:13}{plm}
#'
#' \insertRef{CAME:MILL:15}{plm}
#' 
#' @keywords htest
#' @examples
#' 
#' data("Grunfeld", package = "plm")
#' mod_fe <- plm(inv ~ value + capital, data = Grunfeld, model = "within")
#' mod_re <- plm(inv ~ value + capital, data = Grunfeld, model = "random")
#' pwaldtest(mod_fe, test = "F")
#' pwaldtest(mod_re, test = "Chisq")
#' 
#' # with robust vcov (matrix, function)
#' pwaldtest(mod_fe, vcov = vcovHC(mod_fe))
#' pwaldtest(mod_fe, vcov = function(x) vcovHC(x, type = "HC3"))
#' 
#' pwaldtest(mod_fe, vcov = vcovHC(mod_fe), df2adj = FALSE) # w/o df2 adjustment
#' 
#' # example without attribute "cluster" in the vcov
#' vcov_mat <- vcovHC(mod_fe)
#' attr(vcov_mat, "cluster") <- NULL  # remove attribute
#' pwaldtest(mod_fe, vcov = vcov_mat) # no df2 adjustment performed
#' 
#' 
pwaldtest <- function(x, ...) {
  UseMethod("pwaldtest")
}

#' @rdname pwaldtest
#' @export
pwaldtest.plm <- function(x, test = c("Chisq", "F"), vcov = NULL,
                          df2adj = (test == "F" && !is.null(vcov) && missing(.df2)), .df1, .df2, ...) {
  model <- describe(x, "model")
  test <- match.arg(test)
  df1 <- ifelse(model == "within",
                length(coef(x)),
                length(coef(x)) - has.intercept(x))
  df2 <- df.residual(x)
  tss <- tss(x)
  ssr <- deviance(x)
  vcov_arg <- vcov
  int <- "(Intercept)"
  coefs_wo_int <- coef(x)[!(names(coef(x)) %in% int)]
  
  # sanity check
  if (df2adj == TRUE && (is.null(vcov_arg) || test != "F")) {
    stop("df2adj == TRUE sensible only for robust F test, i.e. test == \"F\" and !is.null(vcov) and missing(.df2)")
  }
  
  # if robust test: prepare robust vcov
  if (!is.null(vcov_arg)) {
    if (is.matrix(vcov_arg))   rvcov <- rvcov_orig <- vcov_arg
    if (is.function(vcov_arg)) rvcov <- rvcov_orig <- vcov_arg(x)
    
    rvcov_name <- paste0(", vcov: ", paste0(deparse(substitute(vcov)))) # save "name" for later
    
    if (int %in% names(coef(x))) { # drop intercept, if present
      rvcov <- rvcov_orig[!rownames(rvcov_orig) %in% int, !colnames(rvcov_orig) %in% int]
      attr(rvcov, which = "cluster") <- attr(rvcov_orig, which = "cluster") # restore dropped 'cluster' attribute
    }
    # if robust F test: by default, do finite-sample adjustment for df2
    if (df2adj == TRUE & test == "F") {
      # determine the variable that the clustering is done on by
      # attribute "cluster" in the vcov (matrix object)
      # if only one member in cluster: fall back to original df2
      if (!is.null(attr(rvcov, which = "cluster"))) {
        
        # if supplied vcov is from package "clubSandwich": translate attr "cluster" to fit our code
        # (use rvcov_orig here for the test as the above dropping of the intercept drops the special classes of rvcov)
        if (inherits(rvcov_orig, "vcovCR")) rvcov <- trans_clubSandwich_vcov(CSvcov = rvcov, index = attr(model.frame(x), "index"))
        
        cluster <- attr(rvcov, which = "cluster")
        pdim <- pdim(x)
        df2 <- switch(cluster,
                      group = { if(pdim$nT$n == 1L) df2 else (pdim$nT$n - 1L) },
                      time  = { if(pdim$nT$T == 1L) df2 else (pdim$nT$T - 1L) },
                      # TODO: what about double clustering? vcovDC? vcovDC identifies itself as attr(obj, "cluster")="group-time")
                      # default:
                      { # warning("unknown/not implemented clustering, no df2 adjustment for finite-samples")
                        df2}
        )
      } else {
        # no information on clustering found, do not adjust df2
        # (other options would be: assume cluster = "group", or fall-back to non robust statistics (set vcov_arg <- NULL))
        warning("no attribute 'cluster' in robust vcov found, no finite-sample adjustment for df2") # assuming cluster = \"group\"")
        # df2 <- as.integer(pdim(x)$nT$n - 1) # assume cluster = "group"
      }
    }
  }
  
  # final say: overwrite Dfs if especially supplied
  if (!missing(.df1)) df1 <- .df1
  if (!missing(.df2)) df2 <- .df2
  
  if (test == "Chisq"){
    # perform non-robust chisq test
    if (is.null(vcov_arg)) {
      stat <- if(model == "random" || length(formula(x))[2] > 1) { ## TODO: need to check F test as well?
        # random case (makes a difference for unbalanced random models! make due to our tss, ssr definitions)
        # IV case: cannot take usual TSS-SSR-way to calc. stat
        as.numeric(crossprod(solve(vcov(x)[names(coefs_wo_int), names(coefs_wo_int)], coefs_wo_int), coefs_wo_int))
      } else {
        # non-random, non-IV models
        (tss-ssr)/(ssr/df2)
      }
      
      names(stat) <- "Chisq"
      pval <- pchisq(stat, df = df1, lower.tail = FALSE)
      parameter <- c(df = df1)
      method <- "Wald test for foint significance"
    } else {
      # perform robust chisq test
      stat <- as.numeric(crossprod(solve(rvcov, coefs_wo_int), coefs_wo_int))
      names(stat) <- "Chisq"
      pval <- pchisq(stat, df = df1, lower.tail = FALSE)
      parameter <- c(df = df1)
      method <- paste0("Wald test for joint significance (robust)", rvcov_name)
    }
  }
  if (test == "F"){
    if(length(formula(x))[2] > 1) stop("test = \"F\" not sensible for IV models")
    if (is.null(vcov_arg)) {
      # perform "normal" F test
      if(model == "random") {
        stat <- as.numeric(crossprod(solve(vcov(x)[names(coefs_wo_int), names(coefs_wo_int)], coefs_wo_int), coefs_wo_int)) / df1
      } else {
        stat <- (tss-ssr)/ssr*df2/df1
      }
      names(stat) <- "F"
      pval <- pf(stat, df1 = df1, df2 = df2, lower.tail = FALSE)
      parameter <- c(df1 = df1, df2 = df2)
      method <- "F test for joint significance"
    } else {
      # perform robust F test
      stat <- as.numeric(crossprod(solve(rvcov, coefs_wo_int), coefs_wo_int) / df1)
      names(stat) <- "F"
      pval <- pf(stat, df1 = df1, df2 = df2, lower.tail = FALSE)
      parameter <- c(df1 = df1, df2 = df2) # Dfs
      method  <- paste0("F test for joint significance (robust)", rvcov_name)
    }
  }
  res <- list(data.name = data.name(x),
              statistic = stat,
              parameter = parameter,
              p.value   = pval,
              method    = method,
              alternative = "at least one coefficient is not null"
  )
  class(res) <- "htest"
  return(res)
}

#' @rdname pwaldtest
#' @export
pwaldtest.pvcm <- function(x, ...) {
  model <- describe(x, "model")
  effect <- describe(x, "effect")
  
  if(model == "within") {
    # for the within case, simply return a data.frame with all test results
    # of single estimations (per individual or per time period)
    
    ii <- switch(effect, "individual" = 1, "time" = 2)
    residl <- split(x$residuals, index(x)[[ii]])
    
    # vcocs and coefficients w/o intercept
    coefs.no.int <- !names(x$coefficients) %in% "(Intercept)"
    vcovl <- lapply(x$vcov, function(x) x[coefs.no.int, coefs.no.int])
    coefl <- as.list(data.frame(t(x$coefficients[ , coefs.no.int])))
    
    
    df1 <- ncol(x$coefficients[ , coefs.no.int]) # is same df1 for all models (as all models estimate the same coefs)
    df2 <- lengths(residl) - ncol(x$coefficients) # (any intercept is subtracted)
    
    statChisqs <- mapply(FUN = function(v, c) as.numeric(crossprod(solve(v, c), c)),
                     vcovl, coefl)
    statFs <- statChisqs / df1
    
    
    
    pstatChisqs <- pchisq(statChisqs, df = df1, lower.tail = FALSE)
    pstatFs <- pf(statFs, df1 = df1, df2 = df2, lower.tail = FALSE)
    
    stats.pvcm.within <- as.data.frame(cbind("Chisq"    = statChisqs,
                                             "p(chisq)" = pstatChisqs,
                                             "F"        = statFs,
                                             "p(F)"     = pstatFs,
                                             "df1"      = rep(df1, length(residl)),
                                             "df2"      = df2))
    # early return
    return(stats.pvcm.within)
  }
  
  ## case: model == "random"
  coefs_wo_int <- x$coefficients[setdiff(names(x$coefficients), "(Intercept)")]
  stat <- as.numeric(crossprod(solve(vcov(x)[names(coefs_wo_int), names(coefs_wo_int)], coefs_wo_int), coefs_wo_int))
  names(stat) <- "Chisq"
  df1 <- length(coefs_wo_int)
  pval <- pchisq(stat, df = df1, lower.tail = FALSE)
  parameter <- c(df = df1)
  method <- "Wald test for joint significance"
  
  res <- list(data.name = data.name(x),
              statistic = stat,
              parameter = parameter,
              p.value   = pval,
              method    = method,
              alternative = "at least one coefficient is not null"
  )
  class(res) <- "htest"
  return(res)
}


#' @rdname pwaldtest
#' @export
pwaldtest.pgmm <- function(x, param = c("coef", "time", "all"), vcov = NULL, ...) {
  param <- match.arg(param)
  vcov_supplied <- !is.null(vcov)
  myvcov <- vcov
  if (is.null(vcov)) vv <- vcov(x)
  else if (is.function(vcov)) vv <- myvcov(x)
  else vv <- myvcov
  model <- describe(x, "model")
  effect <- describe(x, "effect")
  if (param == "time" && effect == "individual") stop("no time dummies in this model")
  transformation <- describe(x, "transformation")
  if (model == "onestep") coefficients <- x$coefficients
  else coefficients <- x$coefficients[[2]]
  Ktot <- length(coefficients)
  Kt <- length(x$args$namest)
  
  switch(param,
         "time" = {
           start <- Ktot - Kt + ifelse(transformation == "ld", 2, 1)
           end <- Ktot
         },
         "coef" = {
           start <- 1
           end <- if (effect == "twoways") Ktot - Kt else Ktot
         },
         "all" = {
           start <- 1
           end <- Ktot
         })
  coef <- coefficients[start:end]
  vv <- vv[start:end, start:end]
  stat <- as.numeric(crossprod(coef, crossprod(solve(vv), coef)))
  names(stat) <- "chisq"
  parameter <- length(coef)
  names(parameter) <- "df"
  pval <- pchisq(stat, df = parameter, lower.tail = FALSE)
  method <- "Wald test for joint significance"
  if (vcov_supplied) {
    rvcov_name <- paste0(", vcov: ", paste0(deparse(substitute(vcov))))
    method <- paste0(method, " (robust)", rvcov_name)
  }
  wald.pgmm <- list(statistic = stat,
                    p.value   = pval,
                    parameter = parameter,
                    method    = method,
                    alternative = "at least one coefficient is not null",
                    data.name = data.name(x))
  class(wald.pgmm) <- "htest"
  return(wald.pgmm)
}

pwaldtest.default <- function(x, ...) {
  pwaldtest.plm(x, ...)
}


# trans_clubSandwich_vcov: helper function for pwaldtest()
# translate vcov object from package clubSandwich so it is suitable for summary.plm, plm's pwaldtest.
# Attribute "cluster" in clubSandwich's vcov objects contains the cluster variable itself.
# plm's vcov object also has attribute "cluster" but it contains a character as
# information about the cluster dimension (either "group" or "time")
#
# inputs:
#   * CSvcov: a vcov as returned by clubSandwich's vcovCR function [class c("vcovCR", "clubSandwich")]
#   * index: the index belonging to a plm object/model
# return value:
#   * modified CSvcov (substituted attribute "cluster" with suitable character or NULL)
trans_clubSandwich_vcov <- function(CSvcov, index) {
  clustervar <- attr(CSvcov, "cluster")
  if (!is.null(clustervar)) {
    if (isTRUE(all.equal(index[[1]], clustervar))) {
      attr(CSvcov, "cluster") <- "group"
      return(CSvcov)
    }
    if (isTRUE(all.equal(index[[2]], clustervar))) {
      attr(CSvcov, "cluster") <- "time"
      return(CSvcov)
    } else {
      attr(CSvcov, "cluster") <- NULL
      return(CSvcov)
    }
  }
  warning("no attribute \"cluster\" found in supplied vcov object")
  return(CSvcov)
}



#' Test of Poolability
#' 
#' A Chow test for the poolability of the data.
#' 
#' `pooltest` is a *F* test of stability (or Chow test) for the
#' coefficients of a panel model. For argument `x`, the estimated
#' `plm` object should be a `"pooling"` model or a `"within"` model
#' (the default); intercepts are assumed to be identical in the first
#' case and different in the second case.
#' 
#' @aliases pooltest
#' @param x an object of class `"plm"` for the plm method; an object of
#' class `"formula"` for the formula interface,
#' @param z an object of class `"pvcm"` obtained with
#' `model="within"`,
#' @param data a `data.frame`,
#' @param \dots further arguments passed to plm.
#' @return An object of class `"htest"`.
#' @export
#' @author Yves Croissant
#' @keywords htest
#' @examples
#' 
#' data("Gasoline", package = "plm")
#' form <- lgaspcar ~ lincomep + lrpmg + lcarpcap
#' gasw <- plm(form, data = Gasoline, model = "within")
#' gasp <- plm(form, data = Gasoline, model = "pooling")
#' gasnp <- pvcm(form, data = Gasoline, model = "within")
#' pooltest(gasw, gasnp)
#' pooltest(gasp, gasnp)
#' 
#' pooltest(form, data = Gasoline, effect = "individual", model = "within")
#' pooltest(form, data = Gasoline, effect = "individual", model = "pooling")
#' 
pooltest <- function(x,...){
  UseMethod("pooltest")
}


#' @rdname pooltest
#' @export
pooltest.plm <- function(x, z, ...){
  rss <- deviance(x)
  uss <- sum(unlist(residuals(z))^2)
  dlr <- df.residual(x)
  dlu <- df.residual(z)
  df1 <- dlr-dlu
  df2 <- dlu
  stat <- (rss-uss)/uss*df2/df1
  pval <- pf(stat, df1, df2, lower.tail = FALSE)
  parameter <- c(df1 = df1, df2 = df2)
  names(stat) <- "F"
  res <- list(statistic   = stat,
              parameter   = parameter,
              p.value     = pval,
              data.name   = data.name(x),
              alternative = "unstability",
              method      = "F statistic")
  class(res) <- "htest"
  res
}

#' @rdname pooltest
#' @export
pooltest.formula <- function(x, data, ...){
  cl <- match.call(expand.dots = TRUE)
  cl[[1]] <- as.name("plm")
  names(cl)[[2]] <- "formula"
  if (is.null(cl$effect)) cl$effect <- "individual"
  plm.model <- eval(cl,parent.frame())

  cl[[1]] <- as.name("pvcm")
  names(cl)[[2]] <- "formula"
  if (is.null(cl$effect)) cl$effect <- "individual"
  cl$model <- "within"
  pvcm.model <- eval(cl,parent.frame())
  
  pooltest(plm.model,pvcm.model)
}

