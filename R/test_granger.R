### Panel Granger (Non-)Causality Test
##
## Reference:
##   * Dumitrescu, Elena-Ivona/Hurlin, Christophe (2012), Testing for Granger non-causality in heterogeneous panels,
##                                                        Economic Modelling, 29(4), pp. 1450-460.
##   * supplements (test data, MATLAB code): http://www.runmycode.org/companion/view/42
##
##   * Lopez, Luciano/Weber, Sylvain (2017), Testing for Granger causality in panel data,
##                                          The Stata Journal, Vol 17, Issue 4, pp. 972-984.
##      * Working paper: Testing for Granger causality in panel data, 
##                        IRENE Working paper 17-03, September 11, 2017
##      * supplements (xtgcause for Stata) https://ideas.repec.org/c/boc/bocode/s458308.html
##
##   * EViews blog with introduction to the test and a Monte Carlo study:
##     http://blog.eviews.com/2017/08/dumitrescu-hurlin-panel-granger.html
##
## TODO (if someone is willing...)
##  * Lopez/Weber (2017) also demonstrate lag selection procedure by AIC, BIC, ...
##




#' Panel Granger (Non-)Causality Test (Dumitrescu/Hurlin (2012))
#' 
#' Test for Granger (non-)causality in panel data.
#' 
#' 
#' % TODO: write about assumptions of panel Granger test: % * cross-sectional
#' independence % * convergence
#' 
#' The panel Granger (non-)causality test is a combination of Granger
#' tests \insertCite{GRAN:69}{plm} performed per individual. The test
#' is developed by \insertCite{DUMI:HURL:12;textual}{plm}, a shorter
#' exposition is given in \insertCite{LOPE:WEBE:17;textual}{plm}.
#' 
#' The formula `formula` describes the direction of the (panel) Granger
#' causation where `y ~ x` means "x (panel) Granger causes y".
#' 
#' By setting argument `test` to either `"Ztilde"` (default) or
#' `"Zbar"`, two different statistics can be requested. `"Ztilde"`
#' gives the standardised statistic recommended by Dumitrescu/Hurlin (2012) for
#' fixed T samples. If set to `"Wbar"`, the intermediate Wbar statistic
#' (average of individual Granger chi-square statistics) is given which is used
#' to derive the other two.
#' 
#' The Zbar statistic is not suitable for unbalanced panels. For the Wbar
#' statistic, no p-value is available.
#' 
#' The implementation uses `\link[lmtest]{grangertest`} from
#' package \CRANpkg{lmtest} to perform the individual Granger tests.
#' 
#' @param formula a `formula` object to describe the direction of
#'     the hypothesized Granger causation,
#' @param data a `pdata.frame` or a `data.frame`,
#' @param test a character to request the statistic to be returned,
#'     either `"Ztilde"` (default), or `"Zbar"`,
#' @param order integer(s) giving the number of lags to include in the
#'     test's auxiliary regressions, the length of order must be
#'     either 1 (same lag order for all individuals) or equal to the
#'     number of individuals (to specify a lag order per individual),
#' @param index only relevant if `data` is `data.frame` and
#'     not a `pdata.frame`; if `NULL`, the first two columns
#'     of the data.frame are assumed to be the index variables, for
#'     further details see [pdata.frame()].
#' @return An object of class \code{c("pgrangertest",
#'     "htest")}. Besides the usual elements of a `htest` object,
#'     it contains the data frame `indgranger` which carries the
#'     Granger test statistics per individual along the associated
#'     p-values, degrees of freedom and the specified lag order.
#' @export
#' @author Kevin Tappe
#' @seealso `\link[lmtest]{grangertest`} for the original
#'     (non-panel) Granger causality test in \CRANpkg{lmtest}.
#' @references
#'
#' \insertRef{DUMI:HURL:12}{plm}
#'
#' \insertRef{GRAN:69}{plm}
#'
#' \insertRef{LOPE:WEBE:17}{plm}
#' 
#' @keywords htest
#' @examples
#' 
#' ## not meaningful, just to demonstrate usage
#' ## H0: 'value' Granger causes 'inv' for all invididuals
#' 
#' data("Grunfeld", package = "plm")
#' pgrangertest(inv ~ value, data = Grunfeld)
#' pgrangertest(inv ~ value, data = Grunfeld, order = 2L)
#' pgrangertest(inv ~ value, data = Grunfeld, order = 2L, test = "Zbar")
#' 
#' # varying lag order (last individual lag order 3, others lag order 2)
#' pgrangertest(inv ~ value, data = Grunfeld, order = c(rep(2L, 9), 3L))
#' 
#' 
pgrangertest <- function(formula, data, test = c("Ztilde", "Zbar", "Wbar"), order = 1L, index = NULL) {
  # Implementation of formulae follows Lopez/Weber (2017), the formulas are slightly different
  # compared to Dumistrescu/Hurlin (2012), because "Note however that T in DH's formulas 
  # must be understood as the number of observations remaining in the estimations, that 
  # is the number of periods minus the number of lags included. In order to be consistent
  # with our notation, we therefore replaced DH's T by T - K in the follwing formulas of
  # the present paper."
  
  # y ~ x: x (panel) Granger causes y
  
  test <- match.arg(test)
  if (!inherits(data, "pdata.frame")) data <- pdata.frame(data, index = index)
  
  pdim <- pdim(data)
  balanced <- pdim$balanced
  N <- pdim$nT$n
  T. <- pdim$nT$T
  Ti <- pdim$Tint$Ti
  indi <- index(data)[[1]]
  indi_con <- is.pconsecutive(data)
  
  # some input checks
  if (!inherits(formula, "formula") || length(all.vars(formula)) > 2) {
    stop(paste0("Argument 'formula' must be of class \"formula\" and may not contain ",
                "more than 2 variables, one LHS and one RHS variable, e.g. 'y ~ x'"))
  }
  
  if (!(is.numeric(order) && all(round(order) == order) && all(order > 0))) 
    stop("Lag order 'order' must contain positive integer(s)")
  
  if (length(order) > 1 && length(order) != N) stop("'order' must have length 1 or the number of individuals")
  if (test == "Zbar" && !balanced) stop("'test = \"Zbar\"' is not suited for unbalanced panels")
  if (test == "Zbar" && length(unique(order)) != 1) stop("'test = \"Zbar\"' is not suited for varying lag order")
  
  # For statistic Ztilde, the second order moments of the individual statistics must exist
  # (formula (10) in Dumitrescu/Hurlin (2012) where T = T - K)
  if (length(order) == 1) {
    if (test == "Ztilde" && !all((Ti > (5 + 3*order)))) {
      stop(paste0("Condition for test = \"Ztilde\" not met for all individuals: length of time series ",
                  "must be larger than 5+3*order (>5+3*", order, "=", 5 + 3*order,")"))
    }
  } else {
    if (test == "Ztilde" && !all((Ti > (5 + 3*order)))) {
      stop(paste0("Condition for test = \"Ztilde\" not met for all individuals: length of time series ",
                  "must be larger than 5+3*order [where order is the order specified for the individuals]"))
    }
  }
  
  # give warning if data is not consecutive per individual
  if (!all(indi_con)) {
    indnames <- pdim[["panel.names"]][["id.names"]]
    wrn1 <- "pgrangertest: result may be unreliable due to individuals with non-consecutive time periods: "
    wrn2 <- if (sum(!indi_con) <= 5) {
      paste0(indnames[!indi_con], collapse = ", ") 
    }
    else { # cut off enumeration of individuals in warning message if more than 5
      breakpoint <- which(cumsum(!indi_con) == 5)[1]
      paste0(paste0(indnames[1:breakpoint][!indi_con[1:breakpoint]], collapse = ", "), ", ...")
    }
    wrn <- paste0(wrn1, wrn2)
    warning(wrn)
  }
  
  listdata <- split(data, indi) # split data per individual
  
  
  ## use lmtest::grangertest for the individual Granger tests
  
  # for this, if necessary, expand order argument for lmtest::grangertest to full length (N)
  # [but leave variable 'order' in its current length for later decision making]
  if (length(order) == 1) order_grangertest <- rep(order, N) else order_grangertest <- order
  
  # Dumitrescu/Hurlin (2012), p. 1453 use the Chisq definition of the Granger test
  grangertests_i <- mapply(function(data, order)
    lmtest::grangertest(formula, data = data,
                        order = order, test = "Chisq"),
    listdata, order_grangertest, SIMPLIFY = FALSE)
  
  # extract Wald/Chisq-statistics and p-values of individual Granger tests
  Wi   <- lapply(grangertests_i, function(g) g[["Chisq"]][2])
  pWi  <- lapply(grangertests_i, function(g) g[["Pr(>Chisq)"]][[2]])
  dfWi <- lapply(grangertests_i, function(g) abs(g[["Df"]][2]))
  
  Wbar <- c("Wbar" = mean(unlist(Wi)))
  
  if(test == "Zbar") {
    stat <- c(sqrt(N/(2*order)) * (Wbar - order))
    names(stat) <- "Zbar"
    pval <- 2*pnorm(abs(stat), lower.tail = F)
  }
  
  if(test == "Ztilde") {
    # Ztilde recommended for fixed T
    if (balanced && length(order) == 1L) {
      stat <- c(  sqrt( N/(2*order) * (T. - 3*order - 5) / (T. - 2*order - 3) ) 
                  * ( (T. - 3*order - 3) / (T. - 3*order - 1) * Wbar - order))
    } else {
      # unbalanced and/or varying lag order
      # unbal stat seems to reduce to the balanced case for balanced data but rather treat it seperately here
      # formula (33) in Dumitrescu/Hurlin (2012), p. 1459
      if (length(order) == 1) order <- rep(order, N) # replicate lag order for all invididuals
      stat <- c(   sqrt(N) * ( Wbar - 1/N * sum( order * (Ti - 3*order - 1) / (Ti - 3*order - 3) )) 
                   * 1/sqrt( 1/N * sum( 2* order * ((Ti - 3*order - 1)^2 * (Ti - 2*order - 3)) / 
                                          ((Ti - 3*order - 3)^2 * (Ti - 3*order - 5)) ) ) )
    }
    names(stat) <- "Ztilde"
    pval <- 2*pnorm(abs(stat), lower.tail = F)
  }
  
  if(test == "Wbar") {
    stat <- Wbar
    names(stat) <- "Wbar"
    pval <- NULL
  }
  
  # make data frame with individual Granger test results and lag order
  indgranger <- data.frame(indi[!duplicated(indi)], unlist(Wi),
                           unlist(pWi), unlist(dfWi), 
                           (if(length(order) == 1) rep(order, N) else order))
  colnames(indgranger) <- c(names(index(data))[1], "Chisq", "p-value", "df", "lag")
  
  RVAL <- list(statistic = stat,
               parameter = NULL,
               p.value   = pval,
               method = "Panel Granger (Non-)Causality Test (Dumitrescu/Hurlin (2012))",
               alternative = "Granger causality for at least one individual",
               data.name = deparse(formula),
               indgranger = indgranger)
  class(RVAL) <- c("pgrangertest", "htest")
  
  return(RVAL)
}

