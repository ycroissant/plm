
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
##   * EViews blog with introduction to the test and a Monte-Carlo study:
##     http://blog.eviews.com/2017/08/dumitrescu-hurlin-panel-granger.html
##
## TODO (if someone is willing...)
##  * Dumitrescu/Hurlin (2012) also give a statistic for the unbalanced case (formula (33))
##  *                          and also for individual lag orders. Take care of T = T - k there!
##  * Lopez/Weber (2017) also demonstrate lag selection procedure by AIC, BIC, ...
##


pgrangertest <- function(formula, data, test = c("Ztilde", "Zbar", "Wbar"), order = 1L, index = NULL) {
  # Implementation of formulas follows Lopez/Weber (2017), the formulas are slightly different
  # compared to Dumistrescu/Hurlin (2012), because "Note however that T in DH's formulas 
  # must be understood as the number of observations remaining in the estimations, that 
  # is the number of periods minus the number of lags included. In order to be consistent
  # with our notation, we therefore replaced DH's T by T - K in the follwing formulas of
  # the present paper."
  
  # y ~ x: x (panel) Granger causes y
  
  test <- match.arg(test)

  # some input checks
  if (!inherits(formula, "formula") || length(all.vars(formula)) > 2) {
    stop(paste0("Argument 'formula' must be of class \"formula\" and may not contain ",
                "more than 2 variables, one LHS and one RHS variable, e.g. 'y ~ x'"))
  }
    
  if (!(is.numeric(order) && round(order) == order && order > 0)) 
    stop("Lagging value 'order' must be a positive integer")
  
  if (!inherits(data, "pdata.frame")) data <- pdata.frame(data, index = index)
  
  pdim <- pdim(data)
  N <- pdim$nT$n
  T. <- pdim$nT$T
  indi <- index(data)[[1]]
  indi_con <- is.pconsecutive(data)
  
  if(!pdim$balanced) stop("data must be balanced, test for unbalanced data not (yet?) implemented")
  
  # give warning if data is not consecutive per individual
  if(!all(indi_con)) {
    indnames <- pdim[["panel.names"]][["id.names"]]
    wrn1 <- "pgrangertest: result may be unreliable due to individuals with non-consecutive time periods: "
    wrn2 <- if (sum(!indi_con) <= 5)  { 
              paste0(indnames[!indi_con], collapse = ", ") 
            }
            else { # cut off enumercation of individuals in warning message if more than 5
              breakpoint <- which(cumsum(!indi_con) == 5)[1]
              paste0(paste0(indnames[1:breakpoint][!indi_con[1:breakpoint]], collapse = ", "), ", ...")
            }
    wrn <- paste0(wrn1, wrn2)
    warning(wrn)
  }
  
  # For statistic Ztilde, the second order moments of the individual statistics must exist.
  # ((10) in Dumitrescu/Hurlin (2012) where T = T - K)
  if (test == "Ztilde" && !(T. > 5 + 3*order)) {
      stop(paste0("Condition for test = \"Ztilde\" not met: length of time series ",
                  "must be larger than 5 + 3*order (", T., " > ", "5 + 3*", order, " = ", 5 + 3*order,")"))
  }
  
  
  listdata <- split(data, indi) # split data per individual
  
  grangertests_i <- lapply(listdata, function(i)  {
    dat <- as.data.frame(i)
    # Dumitrescu/Hurlin (2012), p. 1453 use the Chisq definition of the Granger test
    lmtest::grangertest(formula, data = as.data.frame(i), order = order, test = "Chisq")
  })
  
  # extract Wald/Chisq-statistics and p-values of individual Granger tests
  Wi  <- lapply(grangertests_i, function(g) g["Chisq"][[1]][2])
  pWi <- lapply(grangertests_i, function(g) g[["Pr(>Chisq)"]][[2]])
  
  Wbar <- c("Wbar" = mean(unlist(Wi)))
  
  Zbar <- c(sqrt(N/(2*order)) * (Wbar - order))
  names(Zbar) <- "Zbar"
  
  # Ztilde recommended for fixed T
  Ztilde <- c(  sqrt( N/(2*order) * (T. - 3*order - 5) / (T. - 2*order -3) ) 
              * ( (T. - 3*order - 3) / (T. - 3*order -1) * Wbar - order))
  names(Ztilde) <- "Ztilde"
  
  pZbar   <- 2*pnorm(abs(Zbar),   lower.tail = F)
  pZtilde <- 2*pnorm(abs(Ztilde), lower.tail = F)
  
  stat <- switch(test, "Zbar" = Zbar,  "Ztilde" = Ztilde,  "Wbar" = Wbar)
  pval <- switch(test, "Zbar" = pZbar, "Ztilde" = pZtilde, "Wbar" = NULL)
  
  # save individual Granger tests in return value
  indgranger <- data.frame(indi[!duplicated(indi)], unlist(Wi), unlist(pWi))
  colnames(indgranger) <- c(names(index(data))[1], "Chisq", "p-value")
  
  RVAL <- list(statistic = stat,
               parameter = NULL,
               p.value   = pval,
               method = "Panel Granger Causality Test (Dumitrescu/Hurlin (2012))",
               alternative = "Granger causality for at least one individual",
               data.name = deparse(formula),
               indgranger = indgranger)
  class(RVAL) <- c("pgrangertest", "htest")
  
  return(RVAL)
}

