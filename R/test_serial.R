
#' Breusch--Godfrey Test for Panel Models
#' 
#' Test of serial correlation for (the idiosyncratic component of) the
#' errors in panel models.
#' 
#' This Lagrange multiplier test uses the auxiliary model on
#' (quasi-)demeaned data taken from a model of class `plm` which may
#' be a `pooling` (default for formula interface), `random` or
#' `within` model. It performs a Breusch--Godfrey test (using `bgtest`
#' from package \CRANpkg{lmtest} on the residuals of the
#' (quasi-)demeaned model, which should be serially uncorrelated under
#' the null of no serial correlation in idiosyncratic errors, as
#' illustrated in \insertCite{WOOL:10;textual}{plm}. The function
#' takes the demeaned data, estimates the model and calls `bgtest`.
#' 
#' Unlike most other tests for serial correlation in panels, this one
#' allows to choose the order of correlation to test for.
#' 
#' @aliases pbgtest
#' @importFrom lmtest bgtest
#' @param x an object of class `"panelmodel"` or of class `"formula"`,
#' @param order an integer indicating the order of serial correlation
#'     to be tested for. `NULL` (default) uses the minimum number of
#'     observations over the time dimension (see also section
#'     **Details** below),
#' @param type type of test statistic to be calculated; either
#'     `"Chisq"` (default) for the Chi-squared test statistic or `"F"`
#'     for the F test statistic,
#' @param data only relevant for formula interface: data set for which
#'     the respective panel model (see `model`) is to be evaluated,
#' @param model only relevant for formula interface: compute test
#'     statistic for model `pooling` (default), `random`, or `within`.
#'     When `model` is used, the `data` argument needs to be passed as
#'     well,
#' @param \dots further arguments (see [lmtest::bgtest()]).
#' @return An object of class `"htest"`.
#' @note The argument `order` defaults to the minimum number of
#'     observations over the time dimension, while for
#'     `lmtest::bgtest` it defaults to `1`.
#' @export
#' @author Giovanni Millo
#' @seealso For the original test in package \CRANpkg{lmtest} see
#'     [lmtest::bgtest()].  See [pdwtest()] for the analogous
#'     panel Durbin--Watson test.  See [pbltest()], [pbsytest()],
#'     [pwartest()] and [pwfdtest()] for other serial correlation
#'     tests for panel models.
#' @references
#'
#' \insertRef{BREU:78}{plm}
#'
#' \insertRef{GODF:78}{plm}
#'
#' \insertRef{WOOL:02}{plm}
#'
#' \insertRef{WOOL:10}{plm}
#'
#' \insertRef{WOOL:13}{plm}
#'  Sec. 12.2, pp. 421--422.
#' @keywords htest
#' @examples
#' 
#' data("Grunfeld", package = "plm")
#' g <- plm(inv ~ value + capital, data = Grunfeld, model = "random")
#' 
#' # panelmodel interface
#' pbgtest(g)
#' pbgtest(g, order = 4)
#' 
#' # formula interface
#' pbgtest(inv ~ value + capital, data = Grunfeld, model = "random")
#' 
#' # F test statistic (instead of default type="Chisq")
#' pbgtest(g, type="F")
#' pbgtest(inv ~ value + capital, data = Grunfeld, model = "random", type = "F")
#' 
pbgtest <- function (x, ...) {
    UseMethod("pbgtest")
}

#' @rdname pbgtest
#' @export
pbgtest.panelmodel <- function(x, order = NULL, type = c("Chisq", "F"), ...) {
    ## residual serial correlation test based on the residuals of the demeaned
    ## model (see Wooldridge (2002), p. 288) and the regular lmtest::bgtest()

    ## structure:
    ## 1: take demeaned data from 'plm' object
    ## 2: est. auxiliary model by OLS on demeaned data
    ## 3: apply lmtest::bgtest() to auxiliary model and return the result

    model <- describe(x, "model")
    effect <- describe(x, "effect")
    theta <- x$ercomp$theta

    ## retrieve demeaned data
    demX <- model.matrix(x, model = model, effect = effect, theta = theta, cstcovar.rm = "all")
    demy <- pmodel.response(model.frame(x), model = model, effect = effect, theta = theta)
    ## ...and group numerosities
    Ti <- pdim(x)$Tint$Ti
    ## set lag order to minimum group numerosity if not specified by user
    ## (check whether this is sensible)
    if(is.null(order)) order <- min(Ti)

    ## lmtest::bgtest on the demeaned model:
  
    ## check package availability and load if necessary ## not needed anymore as importFrom in NAMESPACE is now used
    #lm.ok <- require("lmtest")
    #if(!lm.ok) stop("package lmtest is needed but not available")
  
    ## pbgtest is the return value of lmtest::bgtest, exception made for the method attribute
    auxformula <- demy ~ demX - 1 #if(model == "within") demy~demX-1 else demy~demX
    lm.mod <- lm(auxformula)
    bgtest <- bgtest(lm.mod, order = order, type = type, ...)
    bgtest$method <- "Breusch-Godfrey/Wooldridge test for serial correlation in panel models"
    bgtest$alternative <- "serial correlation in idiosyncratic errors"
    bgtest$data.name <- paste(deparse(x$call$formula))
    names(bgtest$statistic) <- if(length(bgtest$parameter) == 1) "chisq" else "F"
    return(bgtest)
}

#' @rdname pbgtest
#' @export
pbgtest.formula <- function(x, order = NULL, type = c("Chisq", "F"), data, model=c("pooling", "random", "within"), ...) {
    ## formula method for pbgtest;
    ## defaults to a pooling model
    cl <- match.call(expand.dots = TRUE)
    if (names(cl)[3] == "") names(cl)[3] <- "data"
    if (is.null(cl$model)) cl$model <- "pooling"
    names(cl)[2] <- "formula"
    m <- match(plm.arg, names(cl), 0)
    cl <- cl[c(1L,m)]
    cl[[1L]] <- quote(plm)
    plm.model <- eval(cl,parent.frame())
    pbgtest(plm.model, order = order, type = type, data = data, ...)
}

#' Wooldridge's Test for Unobserved Effects in Panel Models
#' 
#' Semi-parametric test for the presence of (individual or time) unobserved
#' effects in panel models.
#' 
#' This semi-parametric test checks the null hypothesis of zero
#' correlation between errors of the same group. Therefore, it has
#' power both against individual effects and, more generally, any kind
#' of serial correlation.
#' 
#' The test relies on large-N asymptotics. It is valid under error
#' heteroskedasticity and departures from normality.
#' 
#' The above is valid if `effect="individual"`, which is the most
#' likely usage. If `effect="time"`, symmetrically, the test relies on
#' large-T asymptotics and has power against time effects and, more
#' generally, against cross-sectional correlation.
#' 
#' If the panelmodel interface is used, the inputted model must be a pooling
#' model.
#' 
#' @aliases pwtest
#' @param x an object of class `"formula"`, or an estimated model of class
#' `panelmodel`,
#' @param effect the effect to be tested for, one of `"individual"`
#' (default) or `"time"`,
#' @param data a `data.frame`,
#' @param \dots further arguments passed to `plm`.
#' @return An object of class `"htest"`.
#' @export
#' @author Giovanni Millo
#' @seealso [pbltest()], [pbgtest()],
#' [pdwtest()], [pbsytest()], [pwartest()],
#' [pwfdtest()] for tests for serial correlation in panel models.
#' [plmtest()] for tests for random effects.
#' @references
#'
#' \insertRef{WOOL:02}{plm}
#'
#' \insertRef{WOOL:10}{plm}
#' 
#' @keywords htest
#' @examples
#' 
#' data("Produc", package = "plm")
#' ## formula interface
#' pwtest(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc)
#' pwtest(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc, effect = "time")
#' 
#' ## panelmodel interface
#' # first, estimate a pooling model, than compute test statistics
#' form <- formula(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp)
#' pool_prodc <- plm(form, data = Produc, model = "pooling")
#' pwtest(pool_prodc) # == effect="individual"
#' pwtest(pool_prodc, effect="time")
#' 
pwtest <- function(x, ...){
  UseMethod("pwtest")
}

#' @rdname pwtest
#' @export
pwtest.formula <- function(x, data, effect = c("individual", "time"), ...) {
  
  effect <- match.arg(effect, choices = c("individual", "time")) # match effect to pass it on to pwtest.panelmodel

  cl <- match.call(expand.dots = TRUE)
  if (names(cl)[3] == "") names(cl)[3] <- "data"
  if (is.null(cl$model)) cl$model <- "pooling"
  if (cl$model != "pooling") stop("pwtest only relevant for pooling models")
  names(cl)[2] <- "formula"
  m <- match(plm.arg, names(cl), 0)
  cl <- cl[c(1L,m)]
  cl[[1L]] <- quote(plm)
  plm.model <- eval(cl,parent.frame())
  pwtest.panelmodel(plm.model, effect = effect, ...) # pass on desired 'effect' argument to pwtest.panelmodel
  
  ## "RE" test a la Wooldridge (2002/2010), see 10.4.4
  ## (basically the scaled and standardized estimator for sigma from REmod)
  ## does not rely on normality or homoskedasticity; 
  ## H0: composite errors uncorrelated

  ## ref. Wooldridge (2002), pp. 264-265; Wooldridge (2010), pp. 299-300

  ######### from here generic testing interface from
  ######### plm to my code
}

#' @rdname pwtest
#' @export
pwtest.panelmodel <- function(x, effect = c("individual", "time"), ...) {
  if (describe(x, "model") != "pooling") stop("pwtest only relevant for pooling models")
  effect <- match.arg(effect, choices = c("individual", "time")) # was: effect <- describe(x, "effect")
                                                                 # here we want the effect as in the call of pwtest(),
                                                                 # not of the already estimated model, because that is
                                                                 # always a pooling model
  data <- model.frame(x)
  ## extract indices

  ## if effect="individual" std., else swap
  index <- attr(data, "index")
  if (effect == "individual"){
    index  <- index[[1L]]
    tindex <- index[[2L]]
  }
  else{
    index  <- index[[2L]]
    tindex <- index[[1L]]
  }
  ## det. number of groups and df
  n <- length(unique(index))
  X <- model.matrix(x)

  k <- ncol(X)
  ## det. total number of obs. (robust vs. unbalanced panels)
  nT <- nrow(X)
  ## det. max. group numerosity
  t <- max(tapply(X[ , 1L], index, length))

  ## ref. Wooldridge (2002), p.264 / Wooldridge (2010), p.299
    
  ## extract resids
  u <- x$residuals

  ## est. random effect variance
  ## "pre-allocate" an empty list of length n
  tres <- vector("list", n)

  ## list of n "empirical omega-blocks"
  ## with averages of xproducts of t(i) residuals
  ## for each group 1..n 
  ## (possibly different sizes if unbal., thus a list
  ## and thus, unlike Wooldridge (eq.10.37), we divide 
  ## every block by *his* t(t-1)/2)
  unind <- unique(index) # ????
 
  for(i in 1:n) {
    ut <- u[index == unind[i]]
    tres[[i]] <- ut %o% ut
  }

  ## sum over all upper triangles of emp. omega blocks:
  ## define aux. function
  uptrisum <- function(x) {
    uts <- sum(x[upper.tri(x, diag = FALSE)])
    return(uts)}
  
  ## det. # of upper triangle members (n*t(t-1)/2 if balanced)
  ti <- sapply(tres, function(x) dim(x)[[1L]])
  uptrinum <- sum(ti*(ti-1)/2)  # don't need this!!

  ## ...apply to list and sum over resulting vector (df corrected)
  W <- sum(sapply(tres, uptrisum)) # /sqrt(n) simplifies out
  
  ## calculate se(Wstat) as in 10.40
  seW <- sqrt( sum( sapply(tres, uptrisum)^2 ) )
  
  ## NB should we apply a df correction here, maybe that of the standard
  ## RE estimator? (see page 261) 

  Wstat <- W/seW
  names(Wstat) <- "z"
  pW <- 2*pnorm(abs(Wstat), lower.tail = FALSE) # unlike LM, test is two-tailed!
  
  ## insert usual htest features
  dname <- paste(deparse(substitute(formula)))
  RVAL <- list(statistic = Wstat,
               parameter = NULL,
               method = paste("Wooldridge's test for unobserved",
                              effect, "effects"),
               alternative = "unobserved effect",
               p.value = pW,
               data.name = dname)
  class(RVAL) <- "htest"
  return(RVAL)
}

#' Wooldridge Test for AR(1) Errors in FE Panel Models
#' 
#' Test of serial correlation for (the idiosyncratic component of) the errors
#' in fixed--effects panel models.
#' 
#' As \insertCite{WOOL:10;textual}{plm}, Sec. 10.5.4 observes, under
#' the null of no serial correlation in the errors, the residuals of a
#' FE model must be negatively serially correlated, with
#' \eqn{cor(\hat{u}_{it}, \hat{u}_{is})=-1/(T-1)} for each
#' \eqn{t,s}. He suggests basing a test for this null hypothesis on a
#' pooled regression of FE residuals on their first lag:
#' \eqn{\hat{u}_{i,t} = \alpha + \delta \hat{u}_{i,t-1} +
#' \eta_{i,t}}. Rejecting the restriction \eqn{\delta = -1/(T-1)}
#' makes us conclude against the original null of no serial
#' correlation.
#' 
#' `pwartest` estimates the `within` model and retrieves residuals,
#' then estimates an AR(1) `pooling` model on them. The test statistic
#' is obtained by applying a F test to the latter model to test the
#' above restriction on \eqn{\delta}, setting the covariance matrix to
#' `vcovHC` with the option `method="arellano"` to control for serial
#' correlation.
#' 
#' Unlike the [pbgtest()] and [pdwtest()], this test does
#' not rely on large--T asymptotics and has therefore good properties in
#' ``short'' panels.  Furthermore, it is robust to general heteroskedasticity.
#' 
#' @aliases pwartest
#' @param x an object of class `formula` or of class `panelmodel`,
#' @param data a `data.frame`,
#' @param \dots further arguments to be passed on to `vcovHC` (see
#'     Details and Examples).
#' @return An object of class `"htest"`.
#' @export
#' @author Giovanni Millo
#' @seealso [pwfdtest()], [pdwtest()], [pbgtest()], [pbltest()],
#'     [pbsytest()].
#' @references
#'
#' \insertRef{WOOL:02}{plm}
#'
#' \insertRef{WOOL:10}{plm}
#'
#' @keywords htest
#' @examples
#' 
#' data("EmplUK", package = "plm")
#' pwartest(log(emp) ~ log(wage) + log(capital), data = EmplUK)
#' 
#' # pass argument 'type' to vcovHC used in test
#' pwartest(log(emp) ~ log(wage) + log(capital), data = EmplUK, type = "HC3")
#' 
#' 
pwartest <- function(x, ...) {
  UseMethod("pwartest")
}

#' @rdname pwartest
#' @export
pwartest.formula <- function(x, data, ...) {
  ## small-sample serial correlation test for FE models
  ## ref.: Wooldridge (2002/2010) 10.5.4 

  cl <- match.call(expand.dots = TRUE)
  if (is.null(cl$model)) cl$model <- "within"
  if (cl$model != "within") stop("pwartest only relevant for within models")
  if (names(cl)[3L] == "") names(cl)[3L] <- "data"
  names(cl)[2L] <- "formula"
  m <- match(plm.arg, names(cl), 0)
  cl <- cl[c(1L, m)]
  cl[[1L]] <- quote(plm)
  plm.model <- eval(cl, parent.frame())
  pwartest(plm.model, ...)
}

#' @rdname pwartest
#' @export
pwartest.panelmodel <- function(x, ...) {
  
  if (describe(x, "model") != "within") stop("pwartest only relevant for within models")

  FEres <- x$residuals
  data <- model.frame(x)
  
  ## this is a bug fix for incorrect naming of the "data" attr.
  ## for the pseries in pdata.frame()
  
  attr(FEres, "data") <- NULL
  N <- length(FEres)
  FEres.1 <- c(NA,FEres[1:(N-1)])
  index <- attr(data, "index")
  id   <- index[[1L]]
  time <- index[[2L]]
  lagid <- as.numeric(id) - c(NA, as.numeric(id)[1:(N-1)])
  FEres.1[lagid != 0] <- NA
  data <- data.frame(id, time, FEres = unclass(FEres), FEres.1 = unclass(FEres.1))
  names(data)[c(1L, 2L)] <- c("id", "time")
  data <- na.omit(data)
  
  # calc. auxiliary model
  auxmod <- plm(FEres ~ FEres.1, data = data, model = "pooling", index = c("id", "time"))

  ## calc. theoretical rho under H0: no serial corr. in errors
  t. <- pdim(x)$nT$T
  rho.H0 <- -1/(t.-1)
  myH0 <- paste("FEres.1 = ", as.character(rho.H0), sep="")
  
  ## test H0: rho=rho.H0 with HAC
  myvcov <- function(x) vcovHC(x, method = "arellano", ...) # more params may be passed via ellipsis
  
  # calc F stat with restriction rho.H0 and robust vcov
  FEARstat <- ((coef(auxmod)["FEres.1"] - rho.H0)/sqrt(myvcov(auxmod)["FEres.1", "FEres.1"]))^2
  names(FEARstat) <- "F"
  df1 <- c("df1" = 1)
  df2 <- c("df2" = df.residual(auxmod))
  pFEARstat <- pf(FEARstat, df1 = df1, df2 = df2, lower.tail = FALSE)
  
  ## insert usual htest features
  dname <- paste(deparse(substitute(x)))
  RVAL <- list(statistic = FEARstat,
               parameter = c(df1, df2),
               p.value   = pFEARstat,
               method = "Wooldridge's test for serial correlation in FE panels",
               alternative = "serial correlation",
               data.name = dname)
  class(RVAL) <- "htest"
  return(RVAL)
}

## Bera, Sosa-Escudero and Yoon type LM test for random effects
## under serial correlation (H0: no random effects) or the inverse;
## test="ar": serial corr. test robust vs. RE
## test="re": RE test robust vs. serial corr.
## test="j":  joint test for serial corr. and random effects

# Reference for the _balanced_ tests="ar"|"re":
#                   Bera/Sosa-Escudero/Yoon (2001), Tests for the error component model in the presence of local misspecifcation,
#                                                   Journal of Econometrics 101 (2001), pp. 1-23.
#
#           for original (balanced) test="j": Baltagi/Li (1991), A joint test for serial correlation and random individual effects,
#                                                     Statistics & Probability Letters 11 (1991), pp. 277-280.
#
# Reference for _un_balanced versions of all three tests (boil down to the balanced versions for balanced panels):
#                    Sosa-Escudero/Bera (2008), Tests for unbalanced error-components models under local misspecification,
#                                               The Stata Journal (2008), Vol. 8, Number 1, pp. 68-78.
#
# Concise treatment of only _balanced_ tests in
#                      Baltagi (2005), Econometric Analysis of Panel Data, 3rd edition, pp. 96-97
#                   or Baltagi (2013), Econometric Analysis of Panel Data, 5th edition, pp. 108.
#
#
## Implementation follows the formulae for unbalanced panels, which reduce for balanced data to the formulae for balanced panels.
##
## Notation in code largly follows Sosa-Escudero/Bera (2008) (m in Sosa-Escudero/Bera (2008) is total number of observations -> N_obs)
## NB: Baltagi's book matrix A is slightly different defined: A in Baltagi is -A in Sosa-Escudera/Bera (2008)



#' Bera, Sosa-Escudero and Yoon Locally--Robust Lagrange Multiplier
#' Tests for Panel Models and Joint Test by Baltagi and Li
#' 
#' Test for residual serial correlation (or individual random effects)
#' locally robust vs. individual random effects (serial correlation)
#' for panel models and joint test of serial correlation and the
#' random effect specification by Baltagi and Li.
#' 
#' These Lagrange multiplier tests are robust vs. local
#' misspecification of the alternative hypothesis, i.e. they test the
#' null of serially uncorrelated residuals against AR(1) residuals in
#' a pooling model, allowing for local departures from the assumption
#' of no random effects; or they test the null of no random effects
#' allowing for local departures from the assumption of no serial
#' correlation in residuals.  They use only the residuals of the
#' pooled OLS model and correct for local misspecification as outlined
#' in \insertCite{BERA:SOSA:YOON:01;textual}{plm}.
#' 
#' For `test = "re"`, the default (`re.normal = TRUE`) is to compute
#' a one-sided test which is expected to lead to a more powerful test
#' (asymptotically N(0,1) distributed).  Setting `re.normal = FALSE` gives
#' the two-sided test (asymptotically chi-squared(2) distributed). Argument
#' `re.normal` is irrelevant for all other values of `test`.
#' 
#' The joint test of serial correlation and the random effect
#' specification (`test = "j"`) is due to
#' \insertCite{BALT:LI:91;textual}{plm} (also mentioned in
#' \insertCite{BALT:LI:95;textual}{plm}, pp. 135--136) and is added
#' for convenience under this same function.
#' 
#' The unbalanced version of all tests are derived in
#' \insertCite{SOSA:BERA:08;textual}{plm}. The functions implemented
#' are suitable for balanced as well as unbalanced panel data sets.
#' 
#' A concise treatment of the statistics for only balanced panels is
#' given in \insertCite{BALT:13;textual}{plm}, p. 108.
#' 
#' Here is an overview of how the various values of the `test`
#' argument relate to the literature:
#' 
#' \itemize{ \item `test = "ar"`: \itemize{ \item \eqn{RS*_{\rho}} in Bera
#' et al. (2001), p. 9 (balanced) \item \eqn{LM*_{\rho}} in Baltagi (2013), p.
#' 108 (balanced) \item \eqn{RS*_{\lambda}} in Sosa-Escudero/Bera (2008), p. 73
#' (unbalanced) }
#' 
#' \item `test = "re", re.normal = TRUE` (default) (one-sided test,
#' asymptotically N(0,1) distributed): \itemize{ \item \eqn{RSO*_{\mu}} in Bera
#' et al. (2001), p. 11 (balanced) \item \eqn{RSO*_{\mu}} in Sosa-Escudero/Bera
#' (2008), p. 75 (unbalanced) }
#' 
#' \item `test = "re", re.normal = FALSE` (two-sided test, asymptotically
#' chi-squared(2) distributed): \itemize{ \item \eqn{RS*_{\mu}} in Bera et al.
#' (2001), p. 7 (balanced) \item \eqn{LM*_{\mu}} in Baltagi (2013), p. 108
#' (balanced) \item \eqn{RS*_{\mu}} in Sosa-Escudero/Bera (2008), p. 73
#' (unbalanced) }
#' 
#' \item `test = "j"`: \itemize{ \item \eqn{RS_{\mu\rho}} in Bera et al.
#' (2001), p. 10 (balanced) \item \eqn{LM} in Baltagi/Li (2001), p. 279
#' (balanced) \item \eqn{LM_{1}} in Baltagi and Li (1995), pp. 135--136
#' (balanced) \item \eqn{LM1} in Baltagi (2013), p. 108 (balanced) \item
#' \eqn{RS_{\lambda\rho}} in Sosa-Escudero/Bera (2008), p. 74 (unbalanced) } }
#' 
#' @aliases pbsytest
#' @param x an object of class `formula` or of class `panelmodel`,
#' @param data a `data.frame`,
#' @param test a character string indicating which test to perform:
#' first--order serial correlation (`"ar"`), random effects (`"re"`)
#' or joint test for either of them (`"j"`),
#' @param re.normal logical, only relevant for `test = "re"`: `TRUE`
#' (default) computes the one-sided `"re"` test, `FALSE` the
#' two-sided test (see also Details); not relevant for other values of
#' `test` and, thus, should be `NULL`,
#' @param \dots further arguments.
#' @return An object of class `"htest"`.
#' @export
#' @author Giovanni Millo (initial implementation) & Kevin Tappe (extension to
#' unbalanced panels)
#' @seealso [plmtest()] for individual and/or time random effects
#' tests based on a correctly specified model; [pbltest()],
#' [pbgtest()] and [pdwtest()] for serial correlation tests
#' in random effects models.
#' @references
#'
#' \insertRef{BERA:SOSA:YOON:01}{plm}
#'
#' \insertRef{BALT:13}{plm}
#'
#' \insertRef{BALT:LI:91}{plm}
#' 
#' \insertRef{BALT:LI:95}{plm}
#'
#' \insertRef{SOSA:BERA:08}{plm}
#'
#' @keywords htest
#'
#' @examples
#' 
#' ## Bera et. al (2001), p. 13, table 1 use
#' ## a subset of the original Grunfeld
#' ## data which contains three errors -> construct this subset:
#' data("Grunfeld", package = "plm")
#' Grunsubset <- rbind(Grunfeld[1:80, ], Grunfeld[141:160, ])
#' Grunsubset[Grunsubset$firm == 2 & Grunsubset$year %in% c(1940, 1952), ][["inv"]] <- c(261.6, 645.2)
#' Grunsubset[Grunsubset$firm == 2 & Grunsubset$year == 1946, ][["capital"]] <- 232.6
#' 
#' ## default is AR testing (formula interface)
#' pbsytest(inv ~ value + capital, data = Grunsubset, index = c("firm", "year"))
#' pbsytest(inv ~ value + capital, data = Grunsubset, index = c("firm", "year"), test = "re")
#' pbsytest(inv ~ value + capital, data = Grunsubset, index = c("firm", "year"), 
#'   test = "re", re.normal = FALSE)
#' pbsytest(inv ~ value + capital, data = Grunsubset, index = c("firm", "year"), test = "j")
#' 
#' ## plm interface
#' mod <- plm(inv ~ value + capital, data = Grunsubset, model = "pooling")
#' pbsytest(mod)
#' 
pbsytest <- function (x, ...) {
  UseMethod("pbsytest")
}

#' @rdname pbsytest
#' @export
pbsytest.formula <- function(x, data, ..., test = c("ar", "re", "j"), re.normal = if (test == "re") TRUE else NULL) {

  ######### from here generic testing interface from
  ######### plm to my code
  if (length(test) == 1) test <- tolower(test) # for backward compatibility: allow upper case
  test <- match.arg(test)
  
  cl <- match.call(expand.dots = TRUE)
  if (is.null(cl$model)) cl$model <- "pooling"
  if (cl$model != "pooling") stop("pbsytest only relevant for pooling models")
  names(cl)[2L] <- "formula"
  if (names(cl)[3L] == "") names(cl)[3L] <- "data"
  m <- match(plm.arg ,names(cl), 0)
  cl <- cl[c(1, m)]
  cl[[1L]] <- as.name("plm")
  plm.model <- eval(cl, parent.frame())
  pbsytest(plm.model, test = test, re.normal = re.normal, ...)
}

#' @rdname pbsytest
#' @export
pbsytest.panelmodel <- function(x, test = c("ar", "re", "j"), re.normal = if (test == "re") TRUE else NULL, ...) {
  test <- match.arg(test)
  if (describe(x, "model") != "pooling") stop("pbsytest only relevant for pooling models")

  # interface check for argument re.normal
  if (test != "re" && !is.null(re.normal)) {
    stop("argument 're.normal' only relevant for test = \"re\", set re.normal = NULL for other tests")}

  poolres <- x$residuals
  data <- model.frame(x)
  ## extract indices
  index <- attr(data, "index")
  tindex <- index[[2L]]
  iindex <- index[[1L]]
  
  
  ## till here.
  ## ordering here if needed.
  
  ## this needs ordering of obs. on time, regardless 
  ## whether before that on groups or after
  
  ## and numerosity check
  
  ## order by group, then time
  oo <- order(iindex,tindex)
  ind <- iindex[oo]
  tind <- tindex[oo]
  poolres <- poolres[oo]
  pdim <- pdim(x)
  n <- max(pdim$Tint$n) ## det. number of groups
  T_i <- pdim$Tint$Ti
  N_t <- pdim$Tint$nt
  t <- max(T_i) ## det. max. group numerosity
  N_obs <- pdim$nT$N ## det. total number of obs. (m in Sosa-Escudera/Bera (2008), p. 69)
  
  ## calc. matrices A and B:
  # Sosa-Escudera/Bera (2008), p. 74
  # Baltagi (2013), p. 108 defines A=(S1/S2)-1 and, thus, has slightly different formulae [opposite sign in Baltagi]
  S1 <- sum(tapply(poolres,ind,sum)^2)
  S2 <- sum(poolres^2)
  A <- 1 - S1/S2
  
  unind <- unique(ind)
  uu <- rep(NA, length(unind))
  uu1 <- rep(NA, length(unind))
  for(i in 1:length(unind)) {
    u.t <- poolres[ind == unind[i]]
    u.t.1 <- u.t[-length(u.t)]
    u.t <- u.t[-1]
    uu[i] <- crossprod(u.t)
    uu1[i] <- crossprod(u.t, u.t.1)
  }
  B <- sum(uu1)/sum(uu)
  
  a <- sum(T_i^2) # Sosa-Escudera/Bera (2008), p. 69
  
  switch(test,
           "ar" = {
             # RS*_lambda from Sosa-Escudero/Bera (2008), p. 73 (unbalanced formula)
             stat <- (B + (((N_obs - n)/(a - N_obs)) * A))^2 * (((a - N_obs)*N_obs^2) / ((N_obs - n)*(a - 3*N_obs + 2*n)))
             df <- c(df = 1)
             names(stat) <- "chisq"
             pstat <- pchisq(stat, df = df, lower.tail = FALSE)
             tname <- "Bera, Sosa-Escudero and Yoon locally robust test"
             myH0_alt <- "AR(1) errors sub random effects"
           },
           
           "re" = {
             if(re.normal) {
               # RSO*_mu from Sosa-Escudero/Bera (2008), p. 75 (unbalanced formula), normally distributed
               stat <- -sqrt( (N_obs^2) / (2*(a - 3*N_obs + 2*n))) * (A + 2*B)
               names(stat) <- "z"
               df <- NULL
               pstat <- pnorm(stat, lower.tail = FALSE)
               tname <- "Bera, Sosa-Escudero and Yoon locally robust test (one-sided)"
               myH0_alt <- "random effects sub AR(1) errors"
             } else {
                # RS*_mu from Sosa-Escudero/Bera (2008), p. 73 (unbalanced formula), chisq(1)
                stat <- ((N_obs^2) * (A + 2*B)^2) / (2*(a - 3*N_obs + 2*n)) 
                names(stat) <- "chisq"
                df <- c(df = 1)
                pstat <- pchisq(stat, df = df, lower.tail = FALSE)
                tname <- "Bera, Sosa-Escudero and Yoon locally robust test (two-sided)"
                myH0_alt <- "random effects sub AR(1) errors"
             }
           },
           
           "j" = {
             # RS_lambda_mu in Sosa-Escudero/Bera (2008), p. 74 (unbalanced formula)
             stat <- N_obs^2 * ( ((A^2 + 4*A*B + 4*B^2) / (2*(a - 3*N_obs + 2*n))) + (B^2/(N_obs - n)))
             # Degrees of freedom in the joint test (test="j") of Baltagi/Li (1991) are 2 (chisquare(2) distributed),
             # see Baltagi/Li (1991), p. 279 and again in Baltagi/Li (1995), p. 136
             df <- c(df = 2)
             names(stat) <- "chisq"
             pstat <- pchisq(stat, df = df, lower.tail = FALSE)
             tname <- "Baltagi and Li AR-RE joint test"
             myH0_alt <- "AR(1) errors or random effects"
           }
  ) # END switch
  
  dname <- paste(deparse(substitute(formula)))
  balanced.type <- ifelse(pdim$balanced, "balanced", "unbalanced")
  tname <- paste(tname, "-", balanced.type, "panel", collapse = " ")

  RVAL <- list(statistic   = stat,
               parameter   = df,
               method      = tname,
               alternative = myH0_alt,
               p.value     = pstat,
               data.name   = dname)
  class(RVAL) <- "htest"
  return(RVAL)
}

#' Durbin--Watson Test for Panel Models
#' 
#' Test of serial correlation for (the idiosyncratic component of) the errors
#' in panel models.
#' 
#' This Durbin--Watson test uses the auxiliary model on
#' (quasi-)demeaned data taken from a model of class `plm` which may
#' be a `pooling` (the default), `random` or `within` model. It
#' performs a Durbin--Watson test (using `dwtest` from package
#' \CRANpkg{lmtest} on the residuals of the (quasi-)demeaned model,
#' which should be serially uncorrelated under the null of no serial
#' correlation in idiosyncratic errors. The function takes the
#' demeaned data, estimates the model and calls `dwtest`. Thus, this
#' test does not take the panel structure of the residuals into
#' consideration; it shall not be confused with the generalized
#' Durbin-Watson test for panels in `pbnftest`.
#' 
#' @aliases pdwtest
#' @importFrom lmtest dwtest
#' @param x an object of class `"panelmodel"` or of class
#'     `"formula"`,
#' @param data a `data.frame`,
#' @param \dots further arguments to be passed on to `dwtest`,
#'     e.g.  `alternative`, see [lmtest::dwtest()] for
#'     further details.
#' @return An object of class `"htest"`.
#' @export
#' @author Giovanni Millo
#' @seealso [lmtest::dwtest()] for the Durbin--Watson test
#'     in \CRANpkg{lmtest}, [pbgtest()] for the analogous
#'     Breusch--Godfrey test for panel models,
#'     [lmtest::bgtest()] for the Breusch--Godfrey test for
#'     serial correlation in the linear model. [pbltest()],
#'     [pbsytest()], [pwartest()] and
#'     [pwfdtest()] for other serial correlation tests for
#'     panel models.
#' 
#' For the Durbin-Watson test generalized to panel data models see
#' [pbnftest()].
#' @references
#'
#' \insertRef{DURB:WATS:50}{plm}
#'
#' \insertRef{DURB:WATS:51}{plm}
#'
#' \insertRef{DURB:WATS:71}{plm}
#'
#' \insertRef{WOOL:02}{plm}
#'
#' \insertRef{WOOL:10}{plm}
#'
#' @keywords htest
#' @examples
#' 
#' data("Grunfeld", package = "plm")
#' g <- plm(inv ~ value + capital, data = Grunfeld, model="random")
#' pdwtest(g)
#' pdwtest(g, alternative="two.sided")
#' ## formula interface
#' pdwtest(inv ~ value + capital, data=Grunfeld, model="random")
#' 
pdwtest <- function (x, ...) {
    UseMethod("pdwtest")
}

#' @rdname pdwtest
#' @export
pdwtest.panelmodel <- function(x, ...) {
    ## does not respect panel structure: 
    ## residual serial correlation test based on the residuals of the demeaned
    ## model and passed on to lmtest::dwtest() for the original DW test
    ## approach justified in Wooldridge (2002/2010), Econometric Analysis of Cross Section and Panel Data, p. 288/328.
    ##
    ## For the Bhargava et al. (1982) generalized DW test see pbnftest()
  
    ## structure:
    ## 1: take demeaned data from 'plm' object
    ## 2: est. auxiliary model by OLS on demeaned data
    ## 3: apply lmtest::dwtest() to auxiliary model and return the result

    model <- describe(x, "model")
    effect <- describe(x, "effect")
    theta <- x$ercomp$theta

    ## retrieve demeaned data
    demX <- model.matrix(x, model = model, effect = effect, theta = theta, cstcovar.rm = "all")
    demy <- pmodel.response(model.frame(x), model = model, effect = effect, theta = theta)

    ## lmtest::dwtest on the demeaned model:

    ## ARtest is the return value of lmtest::dwtest, exception made for the method attribute
    dots <- match.call(expand.dots=FALSE)[["..."]]
    if (is.null(dots$order.by)) order.by <- NULL else order.by <- dots$order.by
    if (is.null(dots$alternative)) alternative <- "greater" else alternative <- dots$alternative
    if (is.null(dots$iterations)) iterations <- 15 else iterations <- dots$iterations
    if (is.null(dots$exact)) exact <- NULL else exact <- dots$exact
    if (is.null(dots$tol)) tol <- 1e-10 else tol <- dots$tol

    demy <- remove_pseries_features(demy) # needed as lmtest::dwtest cannot cope with pseries

    auxformula <- demy ~ demX-1
    lm.mod <- lm(auxformula)

    ARtest <- dwtest(lm.mod, order.by = order.by,
                     alternative = alternative,
                     iterations = iterations, exact = exact, tol = tol)

    # overwrite elements of the values produced by lmtest::dwtest
    ARtest$method <- "Durbin-Watson test for serial correlation in panel models"
    ARtest$alternative <- "serial correlation in idiosyncratic errors"
    ARtest$data.name <- paste(deparse(x$call$formula))
    return(ARtest)
}

#' @rdname pdwtest
#' @export
pdwtest.formula <- function(x, data, ...) {
  ## formula method for pdwtest;
  ## defaults to pooling model

  cl <- match.call(expand.dots = TRUE)
  if (is.null(cl$model)) cl$model <- "pooling"
  names(cl)[2L] <- "formula"
  if (names(cl)[3L] == "") names(cl)[3L] <- "data"
  m <- match(plm.arg, names(cl), 0)
  cl <- cl[c(1L, m)]
  cl[[1L]] <- quote(plm)
  plm.model <- eval(cl, parent.frame())
  pdwtest(plm.model, ...)
}



## references:
## * balanced and consecutive:
##    Bhargava/Franzini/Narendranathan (1982), Serial Correlation and the Fixed Effects Model, Review of Economic Studies (1982), XLIX(4), pp. 533-549.
##    (also in Baltagi (2005/2013), p. 98-99/109-110 for FE application)
## * unbalanced and/or non-consecutive: modified BNF statistic and LBI statistic
##    Baltagi/Wu (1999), Unequally spaced panel data regressions with AR(1) disturbances. Econometric Theory, 15(6), pp. 814-823.
##    (an example is also in Baltagi (2005/2013), p. 90/101)  



#' Modified BNF--Durbin--Watson Test and Baltagi--Wu's LBI Test for Panel
#' Models
#' 
#' Tests for AR(1) disturbances in panel models.
#' 
#' The default, `test = "bnf"`, gives the (modified) BNF statistic,
#' the generalised Durbin-Watson statistic for panels. For balanced
#' and consecutive panels, the reference is
#' Bhargava/Franzini/Narendranathan (1982). The modified BNF is given
#' for unbalanced and/or non-consecutive panels (d1 in formula 16 of
#' \insertCite{BALT:WU:99;textual}{plm}).
#' 
#' `test = "lbi"` yields Baltagi--Wu's LBI statistic
#' \insertCite{BALT:WU:99}{plm}, the locally best invariant test which
#' is based on the modified BNF statistic.
#' 
#' No specific variants of these tests are available for random effect models.
#' As the within estimator is consistent also under the random effects
#' assumptions, the test for random effect models is performed by taking the
#' within residuals.
#' 
#' No p-values are given for the statistics as their distribution is
#' quite difficult. \insertCite{BHAR:FRAN:NARE:82;textual}{plm} supply
#' tabulated bounds for p = 0.05 for the balanced case and consecutive
#' case.
#' 
#' For large N, \insertCite{BHAR:FRAN:NARE:82}{plm} suggest it is
#' sufficient to check whether the BNF statistic is < 2 to test
#' against positive serial correlation.
#' 
#' @aliases pbnftest
#' @param x an object of class `"panelmodel"` or of class `"formula"`,
#' @param test a character indicating the test to be performed, either
#'     `"bnf"` or `"lbi"` for the (modified) BNF statistic or
#'     Baltagi--Wu's LBI statistic, respectively,
#' @param data a `data.frame` (only relevant for formula interface),
#' @param model a character indicating on which type of model the test
#'     shall be performed (`"pooling"`, `"within"`, `"random"`, only
#'     relevant for formula interface),
#' @param \dots only relevant for formula interface: further arguments
#'     to specify the model to test (arguments passed on to plm()),
#'     e.g.  `effect`.
#' @return An object of class `"htest"`.
#' @export
#' @author Kevin Tappe
#' @seealso [pdwtest()] for the original Durbin--Watson test using
#'     (quasi-)demeaned residuals of the panel model without taking
#'     the panel structure into account. [pbltest()], [pbsytest()],
#'     [pwartest()] and [pwfdtest()] for other serial correlation
#'     tests for panel models.
#' @references
#'
#' \insertRef{BALT:13}{plm}
#'
#' \insertRef{BALT:WU:99}{plm}
#'
#' \insertRef{BHAR:FRAN:NARE:82}{plm}
#' 
#' @keywords htest
#' @examples
#' 
#' data("Grunfeld", package = "plm")
#' 
#' # formula interface, replicate Baltagi/Wu (1999), table 1, test case A:
#' data_A <- Grunfeld[!Grunfeld[["year"]] %in% c("1943", "1944"), ]
#' pbnftest(inv ~ value + capital, data = data_A, model = "within")
#' pbnftest(inv ~ value + capital, data = data_A, test = "lbi", model = "within")
#' 
#' # replicate Baltagi (2013), p. 101, table 5.1:
#' re <- plm(inv ~ value + capital, data = Grunfeld, model = "random")
#' pbnftest(re)
#' pbnftest(re, test = "lbi")
#' 
pbnftest <- function (x, ...) {
  UseMethod("pbnftest")
}

#' @rdname pbnftest
#' @export
pbnftest.panelmodel <- function(x, test = c("bnf", "lbi"), ...) {
  
  test <- match.arg(test)
  
  # no test for random effects available: take FE as also consistent (Verbeek (2004, 2nd edition), p. 358)
  model <- describe(x, "model")
  if (model == "random") x <- update(x, model = "within")
  
  consec <- all(is.pconsecutive(x))
  balanced <- is.pbalanced(x)
  
  # residuals are now class pseries, so diff.pseries is used and the
  # differences are computed within observational units (not across as
  # it would be the case if base::diff() is used and as it is done for
  # lm-objects) NAs are introduced by the differencing as one
  # observation is lost per observational unit
  if (!inherits(residuals(x), "pseries")) stop("pdwtest internal error: residuals are not of class \"pseries\"") # check to be safe: need pseries
  
  ind <- index(x)[[1L]]
  obs1 <- !duplicated(ind)                  # first ob of each individual
  obsn <- !duplicated(ind, fromLast = TRUE) # last ob of each individual
  
  #### d1, d2, d3, d4 as in Baltagi/Wu (1999), p. 819 formula (16)
  res_crossprod <- as.numeric(crossprod(residuals(x))) # denominator
  
  ## d1 consists of two parts:
  ##  d1.1: BNF statistic (sum of squared differenced residuals of consecutive time periods per individual)
  ##  d1.2: sum of squared "later" residuals (not differenced) surrounded by gaps in time periods
  ##  typo in Baltagi/Wu (1999) for d1: index j starts at j = 2, not j = 1
  res_diff <- diff(residuals(x), shift = "time")
  d1.1 <- sum(res_diff^2, na.rm = T) / res_crossprod # == BNF (1982), formula (4)
  d1.2_contrib <- as.logical(is.na(res_diff) - obs1)
  d1.2 <- sum(residuals(x)[d1.2_contrib]^2) / res_crossprod
  d1 <- d1.1 + d1.2 # == modified BNF statistic = d1 in Baltagi/Wu (1999) formula (16) [reduces to original BNF in case of balanced and consecutive data (d1.2 is zero)]
  
  if (test == "bnf") {
    stat <- d1
    names(stat) <- "DW"
    method <- "Bhargava/Franzini/Narendranathan Panel Durbin-Watson Test"
    if (!consec || !balanced) method <- paste0("modified ", method)
  }
  
  if (test == "lbi")  {
    ## d2 contains the "earlier" obs sourrounded by gaps in time periods
    d2_contrib <- as.logical(is.na(lead(residuals(x), shift = "time")) - obsn)
    d2 <- sum(residuals(x)[d2_contrib]^2) / res_crossprod
    
    ## d3, d4: sum squared residual of first/last time period for all inviduals / crossprod(residuals)
    d3 <- sum(residuals(x)[obs1]^2) / res_crossprod
    d4 <- sum(residuals(x)[obsn]^2) / res_crossprod
    
    stat <- d1 + d2 + d3 + d4
    names(stat) <- "LBI"
    method <- "Baltagi/Wu LBI Test for Serial Correlation in Panel Models"
  }
  
  result <- list(statistic = stat,
                 # p.value   = NA, # none
                 method    = method,
                 alternative = "serial correlation in idiosyncratic errors",
                 data.name = paste(deparse(x$call$formula)))
  class(result) <- "htest"
  return(result) 
}

#' @rdname pbnftest
#' @export
pbnftest.formula <- function(x, data, test = c("bnf", "lbi"), model = c("pooling", "within", "random"), ...) {
  ## formula method for pdwtest;
  ## defaults to pooling model
  
  test  <- match.arg(test)
  model <- match.arg(model)
  
  cl <- match.call(expand.dots = TRUE)
  if (is.null(model)) model <- "pooling"
  names(cl)[2L] <- "formula"
  if (names(cl)[3L] == "") names(cl)[3L] <- "data"
  m <- match(plm.arg, names(cl), 0)
  cl <- cl[c(1L, m)]
  cl[[1L]] <- quote(plm)
  plm.model <- eval(cl, parent.frame())
  pbnftest(plm.model, test = test)
}

######### Baltagi and Li's LM_rho|mu ########
## ex Baltagi and Li (1995) Testing AR(1) against MA(1)...,
## JE 68, 133-151, test statistic (one-sided) is LM_4;
## see also idem (1997), Monte Carlo results...,
## Annales d'Econometrie et Statistique 48, formula (8)

## from version 2: disposes of Kronecker products,
## thus much faster and feasible on large NT (original
## is already infeasible for NT>3000, this takes 10''
## on N=3000, T=10 and even 20000x10 (55'') is no problem;
## lme() hits the memory limit at ca. 20000x20)

#' Baltagi and Li Serial Dependence Test For Random Effects Models
#' 
#' \insertCite{BALT:LI:95;textual}{plm}'s Lagrange multiplier test for
#' AR(1) or MA(1) idiosyncratic errors in panel models with random
#' effects.
#' 
#' This is a Lagrange multiplier test for the null of no serial
#' correlation, against the alternative of either an AR(1) or an MA(1)
#' process, in the idiosyncratic component of the error term in a
#' random effects panel model (as the analytical expression of the
#' test turns out to be the same under both alternatives,
#' \insertCite{@see @BALT:LI:95 and @BALT:LI:97}{plm}. The
#' `alternative` argument, defaulting to `twosided`, allows testing
#' for positive serial correlation only, if set to `onesided`.
#' 
#' @aliases pbltest
#' @importFrom nlme lme
#' @param x a model formula or an estimated random--effects model of
#'     class `plm` ,
#' @param data for the formula interface only: a `data.frame`,
#' @param alternative one of `"twosided"`,
#'     `"onesided"`. Selects either \eqn{H_A: \rho \neq 0} or
#'     \eqn{H_A: \rho = 0} (i.e., the Normal or the Chi-squared
#'     version of the test),
#' @param index the index of the `data.frame`,
#' @param \dots further arguments.
#' @return An object of class `"htest"`.
#' @export
#' @author Giovanni Millo
#' @seealso [pdwtest()], `bgtest`,
#'     [pbsytest()], [pwartest()] and
#'     [pwfdtest()] for other serial correlation tests for
#'     panel models.
#' @references
#'
#' \insertRef{BALT:LI:95}{plm}
#'
#' \insertRef{BALT:LI:97}{plm}
#'
#' @keywords htest
#' @examples
#' 
#' data("Grunfeld", package = "plm")
#' 
#' # formula interface
#' pbltest(inv ~ value + capital, data = Grunfeld)
#' 
#' # plm interface
#' re_mod <- plm(inv ~ value + capital, data = Grunfeld, model = "random")
#' pbltest(re_mod)
#' pbltest(re_mod, alternative = "onesided")
#' 
pbltest <- function (x, ...) 
{
  UseMethod("pbltest")
}


#' @rdname pbltest
#' @export
pbltest.formula <- function(x, data, alternative = c("twosided", "onesided"), index = NULL, ...) {
 ## this version (pbltest0) based on a "formula, pdataframe" interface


  ## reduce X to model matrix value (no NAs)
    X <- model.matrix(x, data = data)
  ## reduce data accordingly
    data <- data[which(row.names(data) %in% row.names(X)), ]
    if (! inherits(data, "pdata.frame"))
        data <- pdata.frame(data, index = index)

  ## need name of individual index
  gindex <- dimnames(attr(data, "index"))[[2L]][1L]

 ## make random effects formula
  rformula <- NULL
  eval(parse(text = paste("rformula <- ~1|", gindex, sep = "")))

  ## est. MLE model
  mymod <- lme(x, data = data, random = rformula, method = "ML")

  nt. <- mymod$dims$N
  n. <- as.numeric(mymod$dims$ngrps[1])
  t. <- nt./n.
  Jt <- matrix(1, ncol = t., nrow = t.)/t.
  Et <- diag(1, t.) - Jt
  ## make 'bidiagonal' matrix (see BL, p.136)
  G <- matrix(0, ncol = t., nrow = t.)
  for(i in 2:t.) {
    G[i-1,i] <- 1
    G[i,i-1] <- 1
  }

  ## retrieve composite (=lowest level) residuals
  uhat <- residuals(mymod, level = 0)

  ## sigma2.e and sigma2.1 as in BL
  ## break up residuals by group to get rid of Kronecker prod.
  ## data have to be balanced and sorted by group/time, so this works
  uhat.i <- vector("list", n.)
  for(i in 1:n.) {
    uhat.i[[i]] <- uhat[t.*(i-1)+1:t.]
    }
  s2e <- rep(NA, n.)
  s21 <- rep(NA, n.)
  for(i in 1:n.) {
    u.i <- uhat.i[[i]]
    s2e[i] <- as.numeric(crossprod(u.i, Et) %*% u.i)
    s21[i] <- as.numeric(crossprod(u.i, Jt) %*% u.i)
    }
  sigma2.e <- sum(s2e) / (n.*(t.-1))
  sigma2.1 <- sum(s21) / n.

  ## calc. score under the null:
  star1 <- (Jt/sigma2.1 + Et/sigma2.e) %*% G %*% (Jt/sigma2.1 + Et/sigma2.e)
  star2 <- rep(NA, n.)
  ## again, do this group by group to avoid Kronecker prod.
  for(i in 1:n.) {
    star2[i] <- as.numeric(crossprod(uhat.i[[i]], star1) %*% uhat.i[[i]])
    }
  star2 <- sum(star2)
  Drho <- (n.*(t.-1)/t.) * (sigma2.1-sigma2.e)/sigma2.1 + sigma2.e/2 * star2
  ## star2 is (crossprod(uhat, kronecker(In, star1)) %*% uhat)

  ## components for the information matrix
  a <- (sigma2.e-sigma2.1)/(t.*sigma2.1)
  j.rr <- n. * (2 * a^2 * (t.-1)^2 + 2*a*(2*t.-3) + (t.-1))
  j.12 <- n.*(t.-1)*sigma2.e / sigma2.1^2
  j.13 <- n.*(t.-1)/t. * sigma2.e * (1/sigma2.1^2 - 1/sigma2.e^2)
  j.22 <- (n. * t.^2) / (2 * sigma2.1^2)
  j.23 <- (n. * t.) / (2 * sigma2.1^2)
  j.33 <- (n./2) * (1/sigma2.1^2 + (t.-1)/sigma2.e^2)

  ## build up information matrix
  Jmat <- matrix(nrow = 3, ncol = 3)
  Jmat[1,] <- c(j.rr, j.12, j.13)
  Jmat[2,] <- c(j.12, j.22, j.23)
  Jmat[3,] <- c(j.13, j.23, j.33)

  J11 <- n.^2 * t.^2 * (t.-1) / (det(Jmat) * 4*sigma2.1^2 * sigma2.e^2)
  ## this is the same as J11 <- solve(Jmat)[1,1], see BL page 73

  switch(match.arg(alternative),
         "onesided" = {
           LMr.m <- Drho * sqrt(J11)
           pval <- pnorm(LMr.m, lower.tail = FALSE)
           names(LMr.m) <- "z"
           method1 <- "one-sided"
           method2 <- "H0: rho = 0, HA: rho > 0"
           parameter <- NULL
         },
         "twosided" = {
           LMr.m <- Drho^2 * J11
           pval <- pchisq(LMr.m, df = 1, lower.tail = FALSE)
           names(LMr.m) <- "chisq"
           parameter <- c(df = 1)
           method1 <- "two-sided"
           method2 <- "H0: rho = 0, HA: rho != 0"
         }
         )
  dname <- paste(deparse(substitute(x)))
  method <- paste("Baltagi and Li", method1, "LM test")
  alternative <- "AR(1)/MA(1) errors in RE panel model"

  res <- list(statistic = LMr.m,
              p.value = pval,
              method = method,
              alternative = alternative,
              parameter = parameter,
              data.name = dname)

  class(res) <- "htest"
  res
}

#' @rdname pbltest
#' @export
pbltest.plm <- function(x, alternative = c("twosided", "onesided"), ...) {
  # only continue if random effects model
  if (describe(x, "model") != "random") stop("Test is only for random effects models.")
  
  # call pbltest.formula the right way
  pbltest.formula(formula(x$formula), data=cbind(index(x), x$model), index=names(index(x)), alternative = alternative, ...)
}

#' Wooldridge first--difference--based test for AR(1) errors in levels
#' or first--differenced panel models
#' 
#' First--differencing--based test of serial correlation for (the idiosyncratic
#' component of) the errors in either levels or first--differenced panel
#' models.
#' 
#' As \insertCite{WOOL:10;textual}{plm}, Sec. 10.6.3 observes, if the
#' idiosyncratic errors in the model in levels are uncorrelated (which
#' we label hypothesis `"fe"`), then the errors of the model in first
#' differences (FD) must be serially correlated with
#' \eqn{cor(\hat{e}_{it}, \hat{e}_{is}) = -0.5} for each \eqn{t,s}. If
#' on the contrary the levels model's errors are a random walk, then
#' there must be no serial correlation in the FD errors (hypothesis
#' `"fd"`). Both the fixed effects (FE) and the first--differenced
#' (FD) estimators remain consistent under either assumption, but the
#' relative efficiency changes: FE is more efficient under `"fe"`, FD
#' under `"fd"`.
#' 
#' Wooldridge (ibid.) suggests basing a test for either hypothesis on
#' a pooled regression of FD residuals on their first lag:
#' \eqn{\hat{e}_{i,t}=\alpha + \rho \hat{e}_{i,t-1} +
#' \eta_{i,t}}. Rejecting the restriction \eqn{\rho = -0.5} makes us
#' conclude against the null of no serial correlation in errors of the
#' levels equation (`"fe"`). The null hypothesis of no serial
#' correlation in differenced errors (`"fd"`) is tested in a similar
#' way, but based on the zero restriction on \eqn{\rho} (\eqn{\rho =
#' 0}). Rejecting `"fe"` favours the use of the first--differences
#' estimator and the contrary, although it is possible that both be
#' rejected.
#' 
#' `pwfdtest` estimates the `fd` model (or takes an `fd` model as
#' input for the panelmodel interface) and retrieves its residuals,
#' then estimates an AR(1) `pooling` model on them. The test statistic
#' is obtained by applying a F test to the latter model to test the
#' relevant restriction on \eqn{\rho}, setting the covariance matrix
#' to `vcovHC` with the option `method="arellano"` to control for
#' serial correlation.
#' 
#' Unlike the `pbgtest` and `pdwtest`, this test does not rely on
#' large--T asymptotics and has therefore good properties in ''short''
#' panels.  Furthermore, it is robust to general
#' heteroskedasticity. The `"fe"` version can be used to test for
#' error autocorrelation regardless of whether the maintained
#' specification has fixed or random effects
#' \insertCite{@see @DRUK:03}{plm}.
#' 
#' @aliases pwfdtest
#' @param x an object of class `formula` or a `"fd"`-model (plm
#' object),
#' @param data a `data.frame`,
#' @param h0 the null hypothesis: one of `"fd"`, `"fe"`,
#' @param \dots further arguments to be passed on to `vcovHC` (see Details
#' and Examples).
#' @return An object of class `"htest"`.
#' @export
#' @author Giovanni Millo
#' @seealso `pdwtest`, `pbgtest`, `pwartest`,
#' @references
#'
#' \insertRef{DRUK:03}{plm}
#'
#' \insertRef{WOOL:02}{plm}
#' Sec. 10.6.3, pp. 282--283.
#'
#' \insertRef{WOOL:10}{plm}
#' Sec. 10.6.3, pp. 319--320
#' 
#' @keywords htest
#' @examples
#' 
#' data("EmplUK" , package = "plm")
#' pwfdtest(log(emp) ~ log(wage) + log(capital), data = EmplUK)
#' pwfdtest(log(emp) ~ log(wage) + log(capital), data = EmplUK, h0 = "fe")
#' 
#' # pass argument 'type' to vcovHC used in test
#' pwfdtest(log(emp) ~ log(wage) + log(capital), data = EmplUK, type = "HC3", h0 = "fe")
#' 
#' 
#' # same with panelmodel interface
#' mod <- plm(log(emp) ~ log(wage) + log(capital), data = EmplUK, model = "fd")
#' pwfdtest(mod)
#' pwfdtest(mod, h0 = "fe")
#' pwfdtest(mod, type = "HC3", h0 = "fe")
#' 
#' 
pwfdtest <- function(x, ...) {
  UseMethod("pwfdtest")
}

#' @rdname pwfdtest
#' @export
pwfdtest.formula <- function(x, data, ..., h0 = c("fd", "fe")) {
  cl <- match.call(expand.dots = TRUE)
  if (is.null(cl$model)) cl$model <- "fd"
  names(cl)[2L] <- "formula"
  if (names(cl)[3L] == "") names(cl)[3L] <- "data"
  m <- match(plm.arg, names(cl), 0)
  cl <- cl[c(1L, m)]
  cl[[1L]] <- quote(plm)
  plm.model <- eval(cl, parent.frame())
  pwfdtest(plm.model, ..., h0 = h0)
}

#' @rdname pwfdtest
#' @export
pwfdtest.panelmodel <- function(x, ..., h0 = c("fd", "fe")) {
  ## first-difference-based serial correlation test for panel models
  ## ref.: Wooldridge (2002/2010), par. 10.6.3 
  
  # interface check
  model <- describe(x, "model")
  if (model != "fd") stop(paste0("input 'x' needs to be a \"fd\" model (first-differenced model), but is \"", model, "\""))
  
  ## fetch fd residuals
  FDres <- x$residuals
  ## indices (full length! must reduce by 1st time period)
  ## this is an ad-hoc solution for the fact that the 'fd' model
  ## carries on the full indices while losing the first time period
  index <- attr(model.frame(x), "index")
  time <- as.numeric(index[[2L]])
  id   <- as.numeric(index[[1L]])
  
  ## fetch dimensions and adapt to those of indices
  pdim <- pdim(x)
  n <- pdim$nT$n
  Ti_minus_one <- pdim$Tint$Ti-1
  
  ## generate new individual index: drop one observation per individual
  ## NB: This is based on the assumption that the estimated FD model performes
  ##     its diff-ing row-wise (it currently does so). If the diff-ing for FD
  ##     is changed to diff-ing based on time dimension, this part about index
  ##     creation needs to be re-worked because more than 1 observation per
  ##     individual can be dropped
  red_id <- integer()
  for(i in 1:n) {
    red_id <- c(red_id, rep(i, Ti_minus_one[i]))
  }
  # additional check
  # (but should error earlier already as the FD model should be nonestimable)
  if(length(red_id) == 0)
    stop("only individuals with one observation in original data: test not feasible")
  
  # make pdata.frame for auxiliary regression: time dimension is not relvant
  # as the first observation of each individual was dropped -> let time dimension
  # be created (is not related to the original times anymore)
  auxdata <- pdata.frame(as.data.frame(cbind(red_id, FDres)), index = "red_id")
  
  # lag residuals by row (as the FD model diffes by row)
  # NB: need to consoder change to shift = "time" if behaviour of FD model is changed
  auxdata[["FDres.1"]] <- lag(auxdata[["FDres"]], shift = "row")
  
  ## pooling model FDres vs. lag(FDres), with intercept (might as well do it w.o.)
  auxmod <- plm(FDres ~ FDres.1, data = auxdata, model = "pooling")
  
  switch(match.arg(h0), 
         "fd" = {h0des <- "differenced"
         ## theoretical rho under H0: no serial 
         ## corr. in differenced errors is 0
         rho.H0 <- 0},
         
         "fe" = {h0des <- "original"
         ## theoretical rho under H0: no serial 
         ## corr. in original errors is -0.5
         rho.H0 <- -0.5})
  
  myH0 <- paste("FDres.1 = ", as.character(rho.H0), sep="")
  
  ## test H0: rho=rho.H0 with HAC, more params may be passed via ellipsis
  myvcov <- function(x) vcovHC(x, method = "arellano", ...) 
  
  # calc F stat with restriction rho.H0 and robust vcov
  FDARstat <- ((coef(auxmod)["FDres.1"] - rho.H0)/sqrt(myvcov(auxmod)["FDres.1", "FDres.1"]))^2
  names(FDARstat) <- "F"
  df1 <- c(df1 = 1)
  df2 <- c(df2 = df.residual(auxmod))
  pFDARstat <- pf(FDARstat, df1 = df1, df2 = df2, lower.tail = FALSE)
  
  ## insert usual htest features
  dname <- paste(deparse(substitute(x)))
  RVAL <- list(statistic   = FDARstat, 
               parameter   = c(df1, df2),
               p.value     = pFDARstat,
               method      = "Wooldridge's first-difference test for serial correlation in panels",
               alternative = paste("serial correlation in", h0des, "errors"),
               data.name   = dname)
  class(RVAL) <- "htest"
  return(RVAL)
}
