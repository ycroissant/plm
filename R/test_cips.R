## taken from pmg to estimate CIPS test statistic as "average of t's"
## since version 4: added type warning, and output single CADF
## regressions as well, use func gettvalue for speed.  estimation loop
## for single TS models is now lm(formula, data) with 'data' properly
## subsetted; this allows for decent output of individual mods.

## needed for standalone operation:
#plm <- plm:::plm
#pdim <- plm:::pdim

#model.matrix.plm <- plm:::model.matrix.plm
#pmodel.response <- plm:::pmodel.response.plm

## Reference is
## Pesaran, M.H. (2007) A simple panel unit root test in the presence of
## cross-section dependence, Journal of Applied Econometrics, 22(2), pp. 265-312




#' Cross-sectionally Augmented IPS Test for Unit Roots in Panel Models
#' 
#' Cross-sectionally augmented Im, Pesaran and Shin (IPS) test for
#' unit roots in panel models.
#' 
#' This cross-sectionally augmented version of the IPS unit root test
#' (H0: the `pseries` has a unit root) is a so-called
#' second-generation panel unit root test: it is in fact robust
#' against cross-sectional dependence, provided that the default
#' `type="cmg"` is calculated. Else one can obtain the standard
#' (`model="mg"`) or cross-sectionally demeaned (`model="dmg"`)
#' versions of the IPS test.
#' 
# TODO: maybe be more verbose here? write about type arg which corresponds
# cases III, II, I in Pesaran (2007) etc.
#' 
#' @aliases cipstest
#' @param x an object of class `"pseries"`,
#' @param lags lag order for Dickey-Fuller augmentation,
#' @param type one of `"trend"`, `"drift"`, `"none"`,
#' @param model one of `"cmg"`, `"mg"`, `"dmg"`,
#' @param truncated logical specifying whether to calculate the
#'     truncated version of the test,
#' @param \dots further arguments passed to `critvals.cips`
#' (non-exported function).
#' @return An object of class `"htest"`.
#' @author Giovanni Millo
#' @export
#' @seealso [purtest()]
#' @references
#'
#' \insertRef{pes07}{plm}
#' 
#' @keywords htest
#' @examples
#' 
#' data("Produc", package = "plm")
#' Produc <- pdata.frame(Produc, index=c("state", "year"))
#' ## check whether the gross state product (gsp) is trend-stationary
#' cipstest(Produc$gsp, type = "trend")
#' 
cipstest <- function (x, lags = 2, type = c("trend", "drift", "none"),
                      model = c("cmg", "mg", "dmg"), truncated = FALSE, ...) {

  ## type = c("trend", "drift", "none") corresponds to Case III, II, I 
  ## in Pesaran (2007), respectively.

  ## input checks
  if(!inherits(x, "pseries")) stop("Argument 'x' has to be a pseries")
  if(!is.numeric(lags)) stop("Argument 'lags' has to be an integer") # but accept numeric as well
  if(round(lags) != lags) stop("Argument 'lags' has to be an integer")
  # TODO: does 'lags' always need to be >= 1? if so, check for this, too

  dati <- pmerge(diff(x), lag(x))
  dati <- pmerge(dati, diff(lag(x)))
  ## minimal column names
  indexnames <- c("ind", "tind")
  dimnames(dati)[[2L]][1:2] <- indexnames
  clnames <- c("de", "le", "d1e")
  dimnames(dati)[[2L]][3:5] <- clnames
  ## add lags if lags > 1
  if(lags > 1L) {
      for(i in 2:lags) {
          dati <- pmerge(dati, diff(lag(x, i)))
          clnames <- c(clnames, paste("d", i, "e", sep=""))
      }
  }

  dimnames(dati)[[2]][3:(lags+4)] <- clnames

  deterministic <- switch(match.arg(type),
                          trend={"+as.numeric(tind)"},
                          drift={""},
                          none={"-1"})

  ## make formula
  adffm <- as.formula(paste("de~le+",
                            paste(clnames[3:(lags+2)], collapse="+"),
                            deterministic, sep=""))

  ## estimate preliminary pooling plm, to take care of all diffs
  ## and lags in a 'panel' way (would be lost in single TS regr.s)
    pmod <- plm(adffm, data = dati, model = "pooling")
  ## this as in pmg()
    index <- attr(model.frame(pmod), "index")
    ## group index
    ind <- index[[1L]]
    ## time index
    tind <- index[[2L]]
    ## set dimension variables
    pdim <- pdim(pmod)
    balanced <- pdim$balanced
    nt <- pdim$Tint$nt
    Ti <- pdim$Tint$Ti
    T. <- pdim$nT$T
    n <- pdim$nT$n
    N <- pdim$nT$N
    ## set index names
    time.names <- pdim$panel.names$time.names
    id.names <- pdim$panel.names$id.names
    coef.names <- names(coef(pmod))
    ## number of coefficients
    k <- length(coef.names)

    ## CIPS test needs an ADF regression with k lags
    ## so fm <- has to be like diff(e) ~ lag(e)+diff(lag(e)) etc.

    ## model data
    X <- model.matrix(pmod)
    y <- as.numeric(model.response(model.frame(pmod))) # remove pseries attribs
    
  ## det. *minimum* group numerosity
  t <- min(Ti) # == min(tapply(X[,1], ind, length))

  ## check min. t numerosity
  ## NB it is also possible to allow estimation if there *is* one group
  ## with t large enough and average on coefficients removing NAs
  ## Here we choose the explicit way: let estimation fail if we lose df
  ## but a warning would do...
  if(t < (k+1)) stop("Insufficient number of time periods")

  ## one regression for each group i in 1..n
  ## and retrieve coefficients putting them into a matrix
  ## (might be unbalanced => t1!=t2 but we don't care as long
  ## as min(t)>k+1)

  ## "pre-allocate" models' list for the n models
  tmods <- vector("list", n)

  switch(match.arg(model),
         
    "mg" = {
      ## final data as dataframe, to be subsetted for single TS models
      ## (if 'trend' fix this variable's name)
      switch(match.arg(type),
        "trend" = {
          ## make datafr. subtracting intercept and add trend
          adfdati <- data.frame(cbind(y, X[ , -1L]))
          dimnames(adfdati)[[2L]] <- c(clnames, "trend")
          adffm <- update(adffm, . ~ . -as.numeric(tind) + trend)},
        "drift" = {
          ## make df subtracting intercept
          adfdati <- data.frame(cbind(y, X[ , -1L]))
          dimnames(adfdati)[[2L]] <- clnames},
        "none" = {
          ## just make df (intercept isn't there)
          adfdati <- data.frame(cbind(y, X))
          dimnames(adfdati)[[2L]] <- clnames}
        )
      
      ## for each x-sect. i=1..n
      unind <- unique(ind)
      for(i in 1:n) {
        tdati <- adfdati[ind == unind[i], ]
        tmods[[i]] <- lm(adffm, tdati)
        }
      },
    
    "dmg" = {
      ## between-periods transformation (take means over group for each t)
      ## TODO: use general B/between() framework here
      be <- function(x,index,na.rm=TRUE) tapply(x,index,mean,na.rm=na.rm)
      Xm <- apply(X,2,FUN=be,index=tind)[tind,]
      ym <- apply(as.matrix(as.numeric(y)),2,FUN=be,index=tind)[tind]
      ## we do not care about demeaning the intercept or not as it is
      ## eliminated anyway

      demX <- X - Xm
      demy <- y - ym

      ## final data as dataframe, to be subsetted for single TS models
      ## (if 'trend' fix this variable's name)
      switch(match.arg(type),
        "trend" = {
          ## make datafr. subtracting intercept and add trend
          adfdati <- data.frame(cbind(demy, demX[ , -1L]))
          dimnames(adfdati)[[2L]] <- c(clnames, "trend")
          adffm <- update(adffm, . ~ . -as.numeric(tind) + trend)},
        "drift" = {
          ## make df subtracting intercept
          adfdati <- data.frame(cbind(demy, demX[ , -1L]))
          dimnames(adfdati)[[2L]] <- clnames},
        "none" = {
          ## just make df (intercept isn't there)
          adfdati <- data.frame(cbind(demy, demX))
          dimnames(adfdati)[[2L]] <- clnames})

      ## for each x-sect. i=1..n estimate (over t) a demeaned model
      ## (y_it-my_t) = alpha_i + beta_i*(X_it-mX_t) + err_it
      unind <- unique(ind)
      for(i in 1:n) {
        tdati <- adfdati[ind == unind[i], ]
        tmods[[i]] <- lm(adffm, tdati)
        }
    },
    
    "cmg" = {
      deterministic2 <- switch(match.arg(type),
                               trend={"+trend"},
                               drift={""},
                               none={"-1"})
      ## adjust formula
      adffm <- as.formula(paste("de~le+",
                           paste(clnames[3:(lags+2)], collapse = "+"),
                           "+", paste(paste(clnames, "bar", sep = "."),
                           collapse = "+"),
                          deterministic2, sep = ""))

      ## between-periods transformation (take means over groups for each t)
      ## TODO: use general B/between() framework here
      be <- function(x,index,na.rm=TRUE) tapply(x,index,mean,na.rm=na.rm)
      Xm <- apply(X,2,FUN=be,index=tind)[tind,]
      ym <- apply(as.matrix(as.numeric(y)),2,FUN=be,index=tind)[tind]

      ## final data as dataframe, to be subsetted for single TS models
      ## (purge intercepts etc., if 'trend' fix this variable's name)
      switch(match.arg(type),
        "trend" = {
          augX <- cbind(X[ , -1L], ym, Xm[ , -1L])
          adfdati <- data.frame(cbind(y, augX))
          ## purge intercept, averaged intercept and averaged trend
          ## (which is always last col.)
          adfdati <- adfdati[,-(dim(adfdati)[[2L]])]
          dimnames(adfdati)[[2L]] <- c(clnames, "trend",
                                      paste(clnames, "bar", sep="."))
          adffm <- update(adffm, . ~ . -as.numeric(tind) + trend)},
        
        "drift" = {
          augX <- cbind(X[ , -1L], ym, Xm[ , -1L])
          adfdati <- data.frame(cbind(y, augX))
          dimnames(adfdati)[[2]] <- c(clnames,
                                      paste(clnames, "bar", sep="."))},
        "none" = {
          ## no intercepts here
          augX <- cbind(X,ym,Xm)
          adfdati <- data.frame(cbind(y, augX))
          dimnames(adfdati)[[2L]] <- c(clnames,
                                      paste(clnames, "bar", sep="."))
          })

      ## for each x-sect. i=1..n estimate (over t) an augmented model
      ## y_it = alpha_i + beta_i*X_it + c1_i*my_t + c2_i*mX_t + err_it
      unind <- unique(ind)
      for(i in 1:n) {
        tdati <- adfdati[ind == unind[i], ]
        tmods[[i]] <- lm(adffm, tdati)
        }
  })

  
  ## CIPS statistic as an average of the t-stats on the coefficient of 'le'
  tstats <- vapply(tmods, function(mod) gettvalue(mod, "le"), FUN.VALUE = 0.0)
  
  if(truncated) {
      ## set bounds, Pesaran (2007), p. 277
        ## NB: there is a  typo in the paper (see p. 279/281 to confirm):
        ##   Case I: "with an intercept or trend" -> "with_out_ an intercept or trend"
        ## "with_out_ an intercept or trend (Case I): K1 = 6.12, K2 = 4.16"
        ## "with an intercept and no trend (Case II): K1 = 6.19, K2 = 2.61"
        ## "with a linear trend (Case III):           K1 = 6.42, K2 = 1.70"
        ## (use negative values for K1's to ease assignment if bound is reached)
      trbounds <- switch(match.arg(type),
                          none  = {c(-6.12, 4.16)},
                          drift = {c(-6.19, 2.61)},
                          trend = {c(-6.42, 1.70)})
      ## formulae (34) in Pesaran (2007):
      ## truncate at lower bound 
      tstats <- ifelse(tstats > trbounds[1L], tstats, trbounds[1L])
      ## truncate at upper bound
      tstats <- ifelse(tstats < trbounds[2L], tstats, trbounds[2L])
  }

  ## here allow for '...' to pass 'na.rm=TRUE' in case (but see what happens
  ## if unbalanced!
  cipstat <- mean(tstats, ...) #sum(tstats)/n
  pval <- critvals.cips(stat = cipstat, n= n, T. = T.,
                        type = type, truncated = truncated)

  ## if pval out of critical values' then set at boundary and issue
  ## a warning
  if(pval == "> 0.10") {
      pval <- 0.10
      warning("p-value greater than printed p-value")
  } else if(pval == "< 0.01") {
      pval <- 0.01
      warning("p-value smaller than printed p-value")
  }

  parameter <- lags
  names(parameter) <- "lag order"

  dname <- paste(deparse(substitute(x)))

  names(cipstat) <- "CIPS test"

  RVAL <- list(statistic = cipstat, parameter = parameter,
               data.name = dname, tmods = tmods,
               method = "Pesaran's CIPS test for unit roots",
               alternative = "Stationarity", p.value = pval)
  class(RVAL) <- "htest"
  return(RVAL)
}


## separate function computing critical values:

critvals.cips <- function(stat, n, T., type=c("trend", "drift", "none"),
                     truncated = FALSE) {
  ## auxiliary function for cipstest()
  ## extracts --or calculates by interpolation-- p-values for the
  ## (averaged) CIPS statistic depending on whether n and T,
  ## given the critical values of average of individual cross-sectionally
  ## augmented Dickey-Fuller distribution
  
  
  ## Non truncated version
  rnam <- c(10, 15, 20, 30, 50, 70, 100, 200)
  cnam <- rnam
  znam <- c(1, 5, 10)
  
  ## In all following tables N in rows, T in cols unlike Pesaran (2007)
  
  ## No intercept, no trend (Case I); Table II(a) Pesaran (2007), p. 279
  
  ## 1% critical values
  nvals1 <- cbind(
    c(-2.16, -2.02, -1.93, -1.85, -1.78, -1.74, -1.71, -1.70),
    c(-2.03, -1.91, -1.84, -1.77, -1.71, -1.68, -1.66, -1.63),
    c(-2.00, -1.89, -1.83, -1.76, -1.70, -1.67, -1.65, -1.62),
    c(-1.98, -1.87, -1.80, -1.74, -1.69, -1.67, -1.64, -1.61),
    c(-1.97, -1.86, -1.80, -1.74, -1.69, -1.66, -1.63, -1.61),
    c(-1.95, -1.86, -1.80, -1.74, -1.68, -1.66, -1.63, -1.61),
    c(-1.94, -1.85, -1.79, -1.74, -1.68, -1.65, -1.63, -1.61),
    c(-1.95, -1.85, -1.79, -1.73, -1.68, -1.65, -1.63, -1.61)
  )
  
  ## 5% critical values
  nvals5 <- cbind(
    c(-1.80, -1.71, -1.67, -1.61, -1.58, -1.56, -1.54, -1.53),
    c(-1.74, -1.67, -1.63, -1.58, -1.55, -1.53, -1.52, -1.51),
    c(-1.72, -1.65, -1.62, -1.58, -1.54, -1.53, -1.52, -1.50),
    c(-1.72, -1.65, -1.61, -1.57, -1.55, -1.54, -1.52, -1.50),
    c(-1.72, -1.64, -1.61, -1.57, -1.54, -1.53, -1.52, -1.51),
    c(-1.71, -1.65, -1.61, -1.57, -1.54, -1.53, -1.52, -1.51),
    c(-1.71, -1.64, -1.61, -1.57, -1.54, -1.53, -1.52, -1.51),
    c(-1.71, -1.65, -1.61, -1.57, -1.54, -1.53, -1.52, -1.51)
  )
  
  ## 10% critical values
  nvals10 <- cbind(
    c(-1.61, -1.56, -1.52, -1.49, -1.46, -1.45, -1.44, -1.43),
    c(-1.58, -1.53, -1.50, -1.48, -1.45, -1.44, -1.44, -1.43),
    c(-1.58, -1.52, -1.50, -1.47, -1.45, -1.45, -1.44, -1.43),
    c(-1.57, -1.53, -1.50, -1.47, -1.46, -1.45, -1.44, -1.43),
    c(-1.58, -1.52, -1.50, -1.47, -1.45, -1.45, -1.44, -1.43),
    c(-1.57, -1.52, -1.50, -1.47, -1.46, -1.45, -1.44, -1.43),
    c(-1.56, -1.52, -1.50, -1.48, -1.46, -1.45, -1.44, -1.43),
    c(-1.57, -1.53, -1.50, -1.47, -1.45, -1.45, -1.44, -1.43)
  )
  
  ## make critical values' cube
  nvals <- array(data = NA_real_, dim = c(8,8,3))
  nvals[,,1] <- nvals1
  nvals[,,2] <- nvals5
  nvals[,,3] <- nvals10
  dimnames(nvals) <- list(rnam, cnam, znam)
  
  ## Intercept only (Case II), Table II(b) in Pesaran (2007), p. 280
  
  ## 1% critical values
  dvals1 <- cbind(
    c(-2.97, -2.76, -2.64, -2.51, -2.41, -2.37, -2.33, -2.28),
    c(-2.66, -2.52, -2.45, -2.34, -2.26, -2.23, -2.19, -2.16),
    c(-2.60, -2.47, -2.40, -2.32, -2.25, -2.20, -2.18, -2.14),
    c(-2.57, -2.45, -2.38, -2.30, -2.23, -2.19, -2.17, -2.14),
    c(-2.55, -2.44, -2.36, -2.30, -2.23, -2.20, -2.17, -2.14),
    c(-2.54, -2.43, -2.36, -2.30, -2.23, -2.20, -2.17, -2.14),
    c(-2.53, -2.42, -2.36, -2.30, -2.23, -2.20, -2.18, -2.15),
    c(-2.53, -2.43, -2.36, -2.30, -2.23, -2.21, -2.18, -2.15)
  )
  
  ## 5% critical values
  dvals5 <- cbind(
    c(-2.52, -2.40, -2.33, -2.25, -2.19, -2.16, -2.14, -2.10),
    c(-2.37, -2.28, -2.22, -2.17, -2.11, -2.09, -2.07, -2.04),
    c(-2.34, -2.26, -2.21, -2.15, -2.11, -2.08, -2.07, -2.04),
    c(-2.33, -2.25, -2.20, -2.15, -2.11, -2.08, -2.07, -2.05),
    c(-2.33, -2.25, -2.20, -2.16, -2.11, -2.10, -2.08, -2.06),
    c(-2.33, -2.25, -2.20, -2.15, -2.12, -2.10, -2.08, -2.06),
    c(-2.32, -2.25, -2.20, -2.16, -2.12, -2.10, -2.08, -2.07),
    c(-2.32, -2.25, -2.20, -2.16, -2.12, -2.10, -2.08, -2.07)
  )
  
  ## 10% critical values
  dvals10 <- cbind(
    c(-2.31, -2.22, -2.18, -2.12, -2.07, -2.05, -2.03, -2.01),
    c(-2.22, -2.16, -2.11, -2.07, -2.03, -2.01, -2.00, -1.98),
    c(-2.21, -2.14, -2.10, -2.07, -2.03, -2.01, -2.00, -1.99),
    c(-2.21, -2.14, -2.11, -2.07, -2.04, -2.02, -2.01, -2.00),
    c(-2.21, -2.14, -2.11, -2.08, -2.05, -2.03, -2.02, -2.01),
    c(-2.21, -2.15, -2.11, -2.08, -2.05, -2.03, -2.02, -2.01),
    c(-2.21, -2.15, -2.11, -2.08, -2.05, -2.03, -2.03, -2.02),
    c(-2.21, -2.15, -2.11, -2.08, -2.05, -2.04, -2.03, -2.02)
  )
  
  ## make critical values' cube
  dvals <- array(data = NA_real_, dim = c(8,8,3))
  dvals[ , , 1] <- dvals1
  dvals[ , , 2] <- dvals5
  dvals[ , , 3] <- dvals10
  dimnames(dvals) <- list(rnam, cnam, znam)
  
  ## Intercept and trend (Case III), Table II(c) in Pesaran (2007), p. 281
  
  ## 1% critical values
  tvals1 <- cbind(
    c(-3.88, -3.61, -3.46, -3.30, -3.15, -3.10, -3.05, -2.98),
    c(-3.24, -3.09, -3.00, -2.89, -2.81, -2.77, -2.74, -2.71),
    c(-3.15, -3.01, -2.92, -2.83, -2.76, -2.72, -2.70, -2.65),
    c(-3.10, -2.96, -2.88, -2.81, -2.73, -2.69, -2.66, -2.63),
    c(-3.06, -2.93, -2.85, -2.78, -2.72, -2.68, -2.65, -2.62),
    c(-3.04, -2.93, -2.85, -2.78, -2.71, -2.68, -2.65, -2.62),
    c(-3.03, -2.92, -2.85, -2.77, -2.71, -2.68, -2.65, -2.62),
    c(-3.03, -2.91, -2.85, -2.77, -2.71, -2.67, -2.65, -2.62)
  )
  
  ## 5% critical values
  tvals5 <- cbind(
    c(-3.27, -3.11, -3.02, -2.94, -2.86, -2.82, -2.79, -2.75),
    c(-2.93, -2.83, -2.77, -2.70, -2.64, -2.62, -2.60, -2.57),
    c(-2.88, -2.78, -2.73, -2.67, -2.62, -2.59, -2.57, -2.55),
    c(-2.86, -2.76, -2.72, -2.66, -2.61, -2.58, -2.56, -2.54),
    c(-2.84, -2.76, -2.71, -2.65, -2.60, -2.58, -2.56, -2.54),
    c(-2.83, -2.76, -2.70, -2.65, -2.61, -2.58, -2.57, -2.54),
    c(-2.83, -2.75, -2.70, -2.65, -2.61, -2.59, -2.56, -2.55),
    c(-2.83, -2.75, -2.70, -2.65, -2.61, -2.59, -2.57, -2.55)
  )
  
  ## 10% critical values
  tvals10 <- cbind(
    c(-2.98, -2.89, -2.82, -2.76, -2.71, -2.68, -2.66, -2.63),
    c(-2.76, -2.69, -2.65, -2.60, -2.56, -2.54, -2.52, -2.50),
    c(-2.74, -2.67, -2.63, -2.58, -2.54, -2.53, -2.51, -2.49),
    c(-2.73, -2.66, -2.63, -2.58, -2.54, -2.52, -2.51, -2.49),
    c(-2.73, -2.66, -2.63, -2.58, -2.55, -2.53, -2.51, -2.50),
    c(-2.72, -2.66, -2.62, -2.58, -2.55, -2.53, -2.52, -2.50),
    c(-2.72, -2.66, -2.63, -2.59, -2.55, -2.53, -2.52, -2.50),
    c(-2.73, -2.66, -2.63, -2.59, -2.55, -2.54, -2.52, -2.51)
  )
  
  ## make critical values' cube
  tvals <- array(data = NA_real_, dim = c(8, 8, 3))
  tvals[ , , 1] <- tvals1
  tvals[ , , 2] <- tvals5
  tvals[ , , 3] <- tvals10
  dimnames(tvals) <- list(rnam, cnam, znam)
  
  ## if truncated substitute values according to Tables II(a), II(b), II(c)
  ## in Pesaran (2007)
  
  if(truncated) {
    # Case III (Intercept and trend)
    tvals[,1,1] <- -c(3.51, 3.31, 3.20, 3.10, 3.00, 2.96, 2.93, 2.88) # II(c),  1%
    tvals[,2,1] <- -c(3.21, 3.07, 2.98, 2.88, 2.80, 2.76, 2.74, 2.70) # II(c),  1%
    tvals[,1,2] <- -c(3.10, 2.97, 2.89, 2.82, 2.75, 2.73, 2.70, 2.67) # II(c),  5%
    tvals[,2,2] <- -c(2.92, 2.82, 2.76, 2.69, 2.64, 2.62, 2.59, 2.57) # II(c),  5%
    tvals[,1,3] <- -c(2.87, 2.78, 2.73, 2.67, 2.63, 2.60, 2.58, 2.56) # II(c), 10%
    tvals[,2,3] <- -c(2.76, 2.68, 2.64, 2.59, 2.55, 2.53, 2.51, 2.50) # II(c), 10%
    
    # Case II (Intercept only)
    dvals[,1,1] <- -c(2.85, 2.66, 2.56, 2.44, 2.36, 2.32, 2.29, 2.25) # II(b),  1%
    dvals[,1,2] <- -c(2.47, 2.35, 2.29, 2.22, 2.16, 2.13, 2.11, 2.08) # II(b),  5%
    dvals[,1,3] <- -c(2.28, 2.20, 2.15, 2.10, 2.05, 2.03, 2.01, 1.99) # II(b), 10%
    
    # Case I (No intercept, no trend)
    nvals[,1,1] <- -c(2.14, 2.00 ,1.91, 1.84, 1.77, 1.73, 1.71, 1.69) # II(a),  1%
    nvals[,1,2] <- -c(1.79, 1.71, 1.66, 1.61, 1.57, 1.55, 1.53, 1.52) # II(a),  5%
    nvals[,1,3][c(2,4,7)] <- -c(1.55, 1.48, 1.43)                     # II(a), 10%
  }
  
  ## set this according to model
  switch(match.arg(type), 
         trend = {cvals <- tvals},
         drift = {cvals <- dvals},
         none  = {cvals <- nvals})
  
  
  ## find intervals for current n and T.
  nintl <- findInterval(n, rnam)
  ninth <- nintl + 1
  nintv <- rnam[nintl:ninth]
  tintl <- findInterval(T., cnam)
  tinth <- tintl + 1
  tintv <- cnam[tintl:tinth]
  
  ## for each critical value
  cv <- numeric(3)
  for(i in 1:3) {
    
    ## on N dim
    if(n %in% rnam) {
      ## if n is exactly one of the tabulated values:
      tl <- cvals[which(rnam == n), tintl, i]
      th <- cvals[which(rnam == n), tinth, i]
      
    } else {
      ## interpolate interval of interest to get cvals(n,T.)
      tl <- approx(nintv, cvals[nintl:ninth, tintl, i],
                   n = max(nintv) - min(nintv))$y[n - min(nintv)]
      th <- approx(nintv, cvals[nintl:ninth, tinth, i],
                   n = max(nintv) - min(nintv))$y[n - min(nintv)]
    }
    
    ## on T. dim
    if(T. %in% cnam) {
      ## if T. is exactly one of the tabulated values:
      if(n %in% rnam) {
        ## ... and n too:
        cv[i] <- cvals[which(rnam == n), which(cnam == T.), i]
      } else {
        ## or if n is not, interpolate n on T.'s exact row:
        cv[i] <- approx(nintv, cvals[nintl:ninth, which(cnam == T.), i],
                        n = max(nintv) - min(nintv))$y[n - min(nintv)]
      }
    } else {
      ## idem: interpolate T.-interval to get critical value
      cv[i] <- approx(tintv, c(tl, th),
                      n = max(tintv) - min(tintv))$y[T. - min(tintv)]
    }
  }
  
  ## approximate p-values' sequence
  cvprox <- approx(cv, c(0.01, 0.05, 0.1), n = 200)
  cvseq <- cvprox$x
  pvseq <- cvprox$y
  
  if(stat < min(cv)) {
    pval <- "< 0.01"
  } else {
    if(stat > max(cv)) {
      pval <- "> 0.10"
    } else {
      if(stat %in% cv) {
        ## if exactly one of the tabulated values
        pval <- c(0.01, 0.05, 0.10)[which(cv == stat)]
      } else {
        ## find interval where true p-value lies and
        ## set p-value as the mean of bounds
        kk <- findInterval(stat, cvseq)
        pval <- mean(pvseq[kk:(kk+1)])
      }
    }
  }
  
  return(pval)
}


## gettvalue: helper function to extract one or more t value(s)
## (coef/s.e.) for a coefficient from model object useful if one wants
## to avoid the computation of a whole lot of values with summary()
gettvalue <- function(x, coefname) {
  # x: model object (usually class plm or lm) coefname: character
  # indicating name(s) of coefficient(s) for which the t value(s) is
  # (are) requested return: named numeric vector of length ==
  # length(coefname) with requested t value(s)
    beta <- coef(x)[coefname]
    se <- sqrt(diag(vcov(x))[coefname])
    tvalue <- beta / se
    return(tvalue)
}

pseries2pdataframe <- function(x, pdata.frame = TRUE, ...) {
  ## non-exported
  ## Transforms a pseries in a (p)data.frame with the indices as regular columns
  ## in positions 1, 2 and (if present) 3 (individual index, time index, group index).
  ## if pdataframe = TRUE -> return a pdata.frame, if FALSE -> return a data.frame
  ## ellipsis (dots) passed on to pdata.frame()
  if (!inherits(x, "pseries")) stop("input needs to be of class 'pseries'")
  indices <- attr(x, "index")
  class(indices) <- setdiff(class(indices), "pindex")
  vx <- remove_pseries_features(x)
  dfx <- cbind(indices, vx)
  dimnames(dfx)[[2]] <- c(names(indices), deparse(substitute(x)))
  if (pdata.frame == TRUE) {
    res <- pdata.frame(dfx, index = names(indices), ...)
   } else { res <- dfx }
  return(res)
}

pmerge <- function(x, y, ...) {
  ## non-exported
  ## Returns a data.frame, not a pdata.frame.
  ## pmerge is used to merge pseries or pdata.frames into a data.frame or
  ## to merge a pseries to a data.frame
  
  ## transf. if pseries or pdata.frame
  if(inherits(x, "pseries")) x <- pseries2pdataframe(x, pdata.frame = FALSE)
  if(inherits(y, "pseries")) y <- pseries2pdataframe(y, pdata.frame = FALSE)
  if(inherits(x, "pdata.frame")) x <- as.data.frame(x, keep.attributes = FALSE)
  if(inherits(y, "pdata.frame")) y <- as.data.frame(y, keep.attributes = FALSE)
  
  # input to merge() needs to be data.frames; not yet suitable for 3rd index (group variable)
  z <- merge(x, y,
             by.x = dimnames(x)[[2]][1:2],
             by.y = dimnames(y)[[2]][1:2], ...)
  return(z)
}
