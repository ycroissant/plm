
### Panel Granger Causality Test
##
## Reference:
##   * Dumitrescu, Elena-Ivona/Hurlin, Christophe (2012), Testing for Granger non-causality in heterogeneous panels,
##                                                        Economic Modelling, 29(4), pp. 1450-460.
##   * supplements (test data, MATLAB code): http://www.runmycode.org/companion/view/42
##
##   * Lopez, Luciano/Weber, Sylvain (2017), Testing for Granger causality in panel data, 
##                                           IRENE Working paper 17-03, September 11, 2017
##   * supplements (xtgcause for Stata) https://ideas.repec.org/c/boc/bocode/s458308.html
##
## TODO (?)
##  * Dumitrescu/Hurlin (2012) also give a statistic for the unbalanced case (formula (33))
##  *                          and also for individual lag orders. Take care of T = T - k there!
##  * Lopez/Weber (2017) also demonstrate lag selection procedure by AIC, BIC, ...
##


pgrangertest <- function(formula, data, test = c("Ztilde", "Zbar"), order = 1L, index = NULL) {
  # Implementation of formulas follows Lopez/Weber (2017), the formulas are slightly different
  # compared to Dumistrescu/Hurlin (2012), because "Note however that T in DH's formulas 
  # must be understood as the number of observations remaining in the estimations, that 
  # is the number of periods minus the number of lags included. In order to be consistent
  # with our notation, we therefore replaced DH's T by T - K in the follwing formulas of
  # the present paper."
  
  # y ~ x: x (panel) Granger causes y
  
  
  test <- match.arg(test)
  
  if (!(is.numeric(order) && round(order) == order && order > 0)) 
    stop("Lagging value 'order' must be a positive integer")
  
  if (!inherits(data, "pdata.frame")) data <- pdata.frame(data, index = index)
  
  pdim <- pdim(data)
  N <- pdim$nT$n
  T. <- pdim$nT$T
  
  if(!pdim$balanced) stop("data must be balanced, test for unbalanced not (yet?) implemented")
  
  # For statistic Ztilde, the second order moments of the individual statistics must exist.
  # ((10) in Dumitrescu/Hurlin (2012) where T = T - K)
  if (test == "Ztilde" && !(T. > 5 + 3*order)) {
      stop(paste0("Condition for test = \"Ztilde\" not met: length of time series ",
                  "must be larger than 5 + 3*order (", T., " > ", "5 + 3*", order, " = ", 5 + 3*order,")"))
  }
  
  listdata <- split(data, index(data)[[1]]) # split data per individual
  
  grangertests_i <- lapply(listdata, function(i)  {
    dat <- as.data.frame(i)
    lmtest::grangertest(formula, data = as.data.frame(i), order = order)
  })
  
  # extract statistics of individual Granger tests
  Wi <- lapply(grangertests_i, function(g) g["F"][[1]][2])
  
  Wbar <- mean(unlist(Wi)) * order # statistic of lmtest::grangertest must be multiplied by order - why?
  
  Zbar <- c("Zbar" = sqrt(N/(2*order)) * (Wbar - order))
  # Ztilde recommended for fixed T, formula ()
  Ztilde <- c("Ztilde" = sqrt( N/(2*order) * (T. - 3*order - 5) / (T. - 2*order -3) ) 
                         * ( (T. - 3*order - 3) / (T. - 3*order -1) * Wbar - order))
  
  pZbar   <- 2*pnorm(abs(Zbar),   lower.tail = F)
  pZtilde <- 2*pnorm(abs(Ztilde), lower.tail = F)
  
  stat <- switch(test, "Zbar" = Zbar, "Ztilde" = Ztilde)
  pval <- switch(test, "Zbar" = pZbar, "Ztilde" = pZtilde)
  
  RVAL <- list(statistic = stat,
               parameter = NULL,
               p.value   = pval,
               method = "Panel Granger Causality Test (Dumitrescu/Hurlin (2012))",
               alternative = "Granger causality for at least one individual",
               data.name = deparse(formula))
  class(RVAL) <- "htest"
  
  return(RVAL)
}

