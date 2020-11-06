
#' Driscoll and Kraay (1998) Robust Covariance Matrix Estimator
#' 
#' Nonparametric robust covariance matrix estimators *a la
#' Driscoll and Kraay* for panel models with cross-sectional
#' *and* serial correlation.
#' 
#' `vcovSCC` is a function for estimating a robust covariance matrix
#' of parameters for a panel model according to the
#' \insertCite{DRIS:KRAA:98;textual}{plm} method, which is consistent
#' with cross--sectional and serial correlation in a T-asymptotic
#' setting and irrespective of the N dimension. The use with random
#' effects models is undocumented.
#' 
#' Weighting schemes specified by `type` are analogous to those in
#' [sandwich::vcovHC()] in package \CRANpkg{sandwich} and are
#' justified theoretically (although in the context of the standard
#' linear model) by \insertCite{MACK:WHIT:85;textual}{plm} and
#' \insertCite{CRIB:04;textual}{plm} \insertCite{@see @ZEIL:04}{plm}).
#' 
#' The main use of `vcovSCC` is to be an argument to other functions,
#' e.g.  for Wald--type testing: argument `vcov.` to `coeftest()`,
#' argument `vcov` to `waldtest()` and other methods in the
#' \CRANpkg{lmtest} package; and argument `vcov.` to
#' `linearHypothesis()` in the \CRANpkg{car} package (see the
#' examples). Notice that the `vcov` and `vcov.` arguments allow to
#' supply a function (which is the safest) or a matrix
#' \insertCite{@see @ZEIL:04, 4.1-2 and examples below}{plm}.
#' 
#' @aliases vcovSCC
#' @param x an object of class `"plm"` or `"pcce"`
#' @param type the weighting scheme used, one of `"HC0"`, `"sss"`,
#'     `"HC1"`, `"HC2"`, `"HC3"`, `"HC4"`, see Details,
#' @param cluster switch for vcovG; set at `"time"` here,
#' @param maxlag either `NULL` or a positive integer specifying the
#'     maximum lag order before truncation
#' @param inner the function to be applied to the residuals inside the
#'     sandwich: `"cluster"` for SCC, `"white"` for Newey-West,
#'     (`"diagavg"` for compatibility reasons)
#' @param wj weighting function to be applied to lagged terms,
#' @param \dots further arguments
#' @return An object of class `"matrix"` containing the estimate of
#'     the covariance matrix of coefficients.
#' @export
#' @author Giovanni Millo, partially ported from Daniel Hoechle's
#'     (2007) Stata code
#' @seealso [sandwich::vcovHC()] from the \CRANpkg{sandwich}
#'     package for weighting schemes (`type` argument).
#' @references
#'
#' \insertRef{CRIB:04}{plm}
#' 
#' \insertRef{DRIS:KRAA:98}{plm}
#' 
#' \insertRef{HOEC:07}{plm}
#'
#' \insertRef{MACK:WHIT:85}{plm}
#' 
#' \insertRef{ZEIL:04}{plm}
#' 
#' @keywords regression
#' @examples
#' 
#' library(lmtest)
#' library(car)
#' data("Produc", package="plm")
#' zz <- plm(log(gsp)~log(pcap)+log(pc)+log(emp)+unemp, data=Produc, model="pooling")
#' ## standard coefficient significance test
#' coeftest(zz)
#' ## SCC robust significance test, default
#' coeftest(zz, vcov.=vcovSCC)
#' ## idem with parameters, pass vcov as a function argument
#' coeftest(zz, vcov.=function(x) vcovSCC(x, type="HC1", maxlag=4))
#' ## joint restriction test
#' waldtest(zz, update(zz, .~.-log(emp)-unemp), vcov=vcovSCC)
#' ## test of hyp.: 2*log(pc)=log(emp)
#' linearHypothesis(zz, "2*log(pc)=log(emp)", vcov.=vcovSCC)
#' 
vcovSCC <- function(x, ...){
  UseMethod("vcovSCC")
}



#' Newey and West (1987) Robust Covariance Matrix Estimator
#' 
#' Nonparametric robust covariance matrix estimators *a la Newey
#' and West* for panel models with serial correlation.
#' 
#' `vcovNW` is a function for estimating a robust covariance matrix of
#' parameters for a panel model according to the
#' \insertCite{NEWE:WEST:87;textual}{plm} method.  The function works
#' as a restriction of the \insertCite{DRIS:KRAA:98;textual}{plm} covariance (see
#' [vcovSCC()]) to no cross--sectional correlation.
#' 
#' Weighting schemes specified by `type` are analogous to those in
#' [sandwich::vcovHC()] in package \CRANpkg{sandwich} and are
#' justified theoretically (although in the context of the standard
#' linear model) by \insertCite{MACK:WHIT:85;textual}{plm} and
#' \insertCite{CRIB:04;textual}{plm} \insertCite{@see @ZEIL:04}{plm}.
#' 
#' The main use of `vcovNW` is to be an argument to other functions,
#' e.g.  for Wald--type testing: argument `vcov.` to `coeftest()`,
#' argument `vcov` to `waldtest()` and other methods in the
#' \CRANpkg{lmtest} package; and argument `vcov.` to
#' `linearHypothesis()` in the \CRANpkg{car} package (see the
#' examples). Notice that the `vcov` and `vcov.` arguments allow to
#' supply a function (which is the safest) or a matrix
#' \insertCite{@see @ZEIL:04, 4.1-2 and examples below}{plm}.
#' 
#' @aliases vcovNW
#' @param x an object of class `"plm"` or `"pcce"`
#' @param type the weighting scheme used, one of `"HC0"`, `"sss"`,
#'     `"HC1"`, `"HC2"`, `"HC3"`, `"HC4"`, see Details,
#' @param maxlag either `NULL` or a positive integer specifying the
#'     maximum lag order before truncation
#' @param wj weighting function to be applied to lagged terms,
#' @param \dots further arguments
#' @return An object of class `"matrix"` containing the estimate of
#'     the covariance matrix of coefficients.
#' @export
#' @author Giovanni Millo
#' @seealso [sandwich::vcovHC()] from the \CRANpkg{sandwich} package
#'     for weighting schemes (`type` argument).
#' @references
#'
#' \insertRef{CRIB:04}{plm}
#' 
#' \insertRef{DRIS:KRAA:98}{plm}
#'
#' \insertRef{MACK:WHIT:85}{plm}
#' 
#' \insertRef{NEWE:WEST:87}{plm}
#'
#' \insertRef{ZEIL:04}{plm}
#' 
#' @keywords regression
#' @examples
#' 
#' library(lmtest)
#' library(car)
#' data("Produc", package="plm")
#' zz <- plm(log(gsp)~log(pcap)+log(pc)+log(emp)+unemp, data=Produc, model="pooling")
#' ## standard coefficient significance test
#' coeftest(zz)
#' ## NW robust significance test, default
#' coeftest(zz, vcov.=vcovNW)
#' ## idem with parameters, pass vcov as a function argument
#' coeftest(zz, vcov.=function(x) vcovNW(x, type="HC1", maxlag=4))
#' ## joint restriction test
#' waldtest(zz, update(zz, .~.-log(emp)-unemp), vcov=vcovNW)
#' ## test of hyp.: 2*log(pc)=log(emp)
#' linearHypothesis(zz, "2*log(pc)=log(emp)", vcov.=vcovNW)
#' 
vcovNW <- function(x, ...){
  UseMethod("vcovNW")
}



#' Double-Clustering Robust Covariance Matrix Estimator
#' 
#' High-level convenience wrapper for double-clustering robust
#' covariance matrix estimators *a la*
#' \insertCite{THOM:11;textual}{plm} and
#' \insertCite{CAME:GELB:MILL:11;textual}{plm} for panel models.
#' 
#' `vcovDC` is a function for estimating a robust covariance matrix of
#' parameters for a panel model with errors clustering along both dimensions.
#' The function is a convenience wrapper simply summing a group- and a
#' time-clustered covariance matrix and subtracting a diagonal one *a la*
#' White.
#' 
#' Weighting schemes specified by `type` are analogous to those in
#' [sandwich::vcovHC()] in package \CRANpkg{sandwich} and are
#' justified theoretically (although in the context of the standard
#' linear model) by \insertCite{MACK:WHIT:85;textual}{plm} and
#' \insertCite{CRIB:04;textual}{plm} \insertCite{@see @ZEIL:04}{plm}.
#' 
#' The main use of `vcovDC` is to be an argument to other functions,
#' e.g.  for Wald-type testing: argument `vcov.` to `coeftest()`,
#' argument `vcov` to `waldtest()` and other methods in the
#' \CRANpkg{lmtest} package; and argument `vcov.` to
#' `linearHypothesis()` in the \CRANpkg{car} package (see the
#' examples). Notice that the `vcov` and `vcov.` arguments allow to
#' supply a function (which is the safest) or a matrix
#' \insertCite{@see @ZEIL:04, 4.1-2 and examples below}{plm}.
#' 
#' @aliases vcovDC
#' @param x an object of class `"plm"` or `"pcce"`
#' @param type the weighting scheme used, one of `"HC0"`, `"sss"`,
#'     `"HC1"`, `"HC2"`, `"HC3"`, `"HC4"`, see Details,
#' @param \dots further arguments
#' @return An object of class `"matrix"` containing the estimate of
#'     the covariance matrix of coefficients.
#' @export
#' @author Giovanni Millo
#' @seealso [sandwich::vcovHC()] from the \CRANpkg{sandwich}
#'     package for weighting schemes (`type` argument).
#' @references
#'
#' \insertRef{CAME:GELB:MILL:11}{plm}
#' 
#' \insertRef{CRIB:04}{plm}
#'
#' \insertRef{MACK:WHIT:85}{plm}
#' 
#' \insertRef{THOM:11}{plm}
#' 
#' \insertRef{ZEIL:04}{plm}
#' 
#' @keywords regression
#' @examples
#' 
#' library(lmtest)
#' library(car)
#' data("Produc", package="plm")
#' zz <- plm(log(gsp)~log(pcap)+log(pc)+log(emp)+unemp, data=Produc, model="pooling")
#' ## standard coefficient significance test
#' coeftest(zz)
#' ## DC robust significance test, default
#' coeftest(zz, vcov.=vcovDC)
#' ## idem with parameters, pass vcov as a function argument
#' coeftest(zz, vcov.=function(x) vcovDC(x, type="HC1", maxlag=4))
#' ## joint restriction test
#' waldtest(zz, update(zz, .~.-log(emp)-unemp), vcov=vcovDC)
#' ## test of hyp.: 2*log(pc)=log(emp)
#' linearHypothesis(zz, "2*log(pc)=log(emp)", vcov.=vcovDC)
#' 
vcovDC <- function(x, ...){
  UseMethod("vcovDC")
}



#' Generic Lego building block for Robust Covariance Matrix Estimators
#' 
#' Generic Lego building block for robust covariance matrix estimators
#' of the vcovXX kind for panel models.
#' 
#' `vcovG` is the generic building block for use by higher--level
#' wrappers [vcovHC()], [vcovSCC()], [vcovDC()], and [vcovNW()]. The
#' main use of `vcovG` is to be used internally by the former, but it
#' is made available in the user space for use in non--standard
#' combinations. For more documentation, see see wrapper functions
#' mentioned.
#' 
#' @aliases vcovG
#' @param x an object of class `"plm"` or `"pcce"`
#' @param type the weighting scheme used, one of `"HC0"`,
#'     `"sss"`, `"HC1"`, `"HC2"`, `"HC3"`,
#'     `"HC4"`,
#' @param cluster one of `"group"`, `"time"`,
#' @param l lagging order, defaulting to zero
#' @param inner the function to be applied to the residuals inside the
#'     sandwich: one of `"cluster"` or `"white"` or
#'     `"diagavg"`,
#' @param \dots further arguments
#' @return An object of class `"matrix"` containing the estimate
#'     of the covariance matrix of coefficients.
#' @export
#' @author Giovanni Millo
#' @seealso [vcovHC()], [vcovSCC()],
#'     [vcovDC()], [vcovNW()], and
#'     [vcovBK()] albeit the latter does not make use of
#'     vcovG.
#' @references
#'
#' \insertRef{mil17b}{plm}
#'
#' @keywords regression
#' @examples
#' 
#' data("Produc", package="plm")
#' zz <- plm(log(gsp)~log(pcap)+log(pc)+log(emp)+unemp, data=Produc,
#' model="pooling")
#' ## reproduce Arellano's covariance matrix
#' vcovG(zz, cluster="group", inner="cluster", l=0)
#' ## use in coefficient significance test
#' library(lmtest)
#' ## define custom covariance function
#' ## (in this example, same as vcovHC)
#' myvcov <- function(x) vcovG(x, cluster="group", inner="cluster", l=0)
#' ## robust significance test
#' coeftest(zz, vcov.=myvcov)
#' 
vcovG <- function(x, ...) {
    UseMethod("vcovG")
}


#' @rdname vcovG
#' @export
vcovG.plm <- function(x, type = c("HC0", "sss", "HC1", "HC2", "HC3", "HC4"),
                      cluster = c("group", "time"),
                      l = 0,
                      inner = c("cluster", "white", "diagavg"),
                     ...) {

  ## general building block for vcov
  ## for panel models (pooling, random, within or fd type plm obj.)
  ##
  ## * (7/11/2016): compliant with IV models
  
    # stopping control for weighted regressions
    if (!is.null(x$weights)) stop("vcovXX functions not implemented for weighted panel regressions")
  

    type <- match.arg(type)
    model <- describe(x, "model")
    if (!model %in% c("random", "within", "pooling", "fd")) {
        stop("Model has to be either \"random\", \"within\", \"pooling\", or \"fd\" model")
    }

  ## extract demeaned data
    demy <- pmodel.response(x, model = model)
    demX <- model.matrix(x, model = model, rhs = 1, cstcovar.rm = "all")
    ## drop any linear dependent columns (corresponding to aliased coefficients)
    ## from model matrix X
    ## na.rm = TRUE because currently, RE tw unbalanced models set aliased simply to NA
    if (!is.null(x$aliased) && any(x$aliased, na.rm = TRUE)) demX <- demX[, !x$aliased, drop = FALSE]

    ## control: IV or not (two- or one-part formula)
    if(length(formula(x))[2] > 1) {
        demZ <- model.matrix(x, model = model, rhs = 2, cstcovar.rm = "all")
        ## substitute (transformed) X with projection of X on Z
        ## any linear dependence in Z (demZ) is appropriately taken care of by lm.fit()
        demX <- fitted(lm.fit(demZ, demX))
    }

    pdim <- pdim(x)
    nT <- pdim$nT$N
    Ti <- pdim$Tint$Ti
    k <- dim(demX)[[2]]
    n0 <- pdim$nT$n
    t0 <- pdim$nT$T

  ## extract residuals
    uhat <- x$residuals

  ## define residuals weighting function omega(res)
  ## (code taken from meatHC and modified)
  ## (the weighting is defined "in sqrt" relative to the literature)
  ## 
  ## (see the theoretical comments in pvcovHC)

    ## diaghat function for matrices
    dhat <- function(x) {tx <- t(x)
                         diag(crossprod(tx, solve(crossprod(x), tx)))}

    ## this is computationally heavy, do only if needed
    switch(match.arg(type), HC0 = {diaghat <- NULL},
                            sss = {diaghat <- NULL},
                            HC1 = {diaghat <- NULL},
                            HC2 = {diaghat <- try(dhat(demX), silent = TRUE)},
                            HC3 = {diaghat <- try(dhat(demX), silent = TRUE)},
                            HC4 = {diaghat <- try(dhat(demX), silent = TRUE)})
    df <- nT - k
    switch(match.arg(type), 
           HC0 = {
            omega <- function(residuals, diaghat, df, g) residuals
        }, sss = {
            omega <- function(residuals, diaghat, df, g) residuals *
                                sqrt(g/(g-1)*((nT-1)/(nT-k)))
        }, HC1 = {
            omega <- function(residuals, diaghat, df, g) residuals *
                                sqrt(length(residuals)/df)
        }, HC2 = {
            omega <- function(residuals, diaghat, df, g) residuals /
                                sqrt(1 - diaghat)
        }, HC3 = {
            omega <- function(residuals, diaghat, df, g) residuals /
                                (1 - diaghat)
        }, HC4 = {
            omega <- function(residuals, diaghat, df, g) {
                residuals/sqrt(1 - diaghat)^
                 pmin(4, length(residuals) *
                      diaghat/as.integer(round(sum(diaghat),
                digits = 0)))
            }
        })

   ## Definition module for E(u,v)
    if(is.function(inner)) {
        E <- inner
    } else {
      ## outer for clustering/arellano, diag(diag(inner)) for white
      switch(match.arg(inner), 
         cluster={
          E <- function(u, v) outer(u, v)
      }, white={
          E <- function(u, v) { # was simply: diag(diag(outer(u,v)))
              # but unfortunately we have to manage unbalanced panels
              # in the case l!=0 (the residual vectors are different)
              # by producing a "pseudo-diagonal" with all those obs.
              # common to both vectors
 
              if(isTRUE(all.equal(names(u), names(v)))) {
                  ## ..then keep it simple! (halves time on EmplUK ex.)
                  n <- length(u)
                  euv <- diag(u*v, n)
              } else {
                  ## calculate outer product
                  efull <- outer(u, v)
                  ## make matrix of zeros with same dims and names
                  eres <- array(0, dim = dim(efull))
                  dimnames(eres) <- dimnames(efull)
                  ## populate "pseudo-diagonal" with values from efull
                  for(i in 1:length(names(u))) {
                      for(j in 1:length(names(v))) {
                          if(names(u)[i] == names(v)[j]) {
                              eres[i,j] <- efull[i,j]
                          }
                      }
                  }
                  euv <- eres
              }
              return(euv)
          }
      }, diagavg={
          E <- function(u,v) {
              ## this is the averaged version for 'white2'
              if(isTRUE(all.equal(names(u), names(v)))) {
                  ## ..then keep it simple
                  n <- length(u)
                  euv <- diag(x = sum(u*v)/n, n)
              } else {
                  ## do just as for 'white' and then average nonzeros:
                  ## calculate outer product
                  efull <- outer(u,v)
                  ## make matrix of zeros with same dims and names
                  eres <- array(0, dim = dim(efull))
                  dimnames(eres) <- dimnames(efull)
                  ## populate "pseudo-diagonal" with values from efull
                  for(i in 1:length(names(u))) {
                      for(j in 1:length(names(v))) {
                          if(names(u)[i] == names(v)[j]) {
                              eres[i,j] <- efull[i,j]
                          }
                      }
                  }
                  euv <- eres
                  ## substitute nonzeros with average thereof
                  euv[euv != 0] <- mean(euv[euv != 0])
              }
              return(euv)
          }
      })
  } ## END: Definition module for E(u,v)
 

    ## try passing: function (a or b) or matrix (unconditional) to vcovG

  ## robustifying against either serial or xs intragroup dependence:
  ## if 'group' then keep current indexing, if 'time' then swap i<->t
  ## so that residuals get 'clustered' by time period instead of by
  ## group (i.e. the vcov estimator is robust vs. xsectional dependence)

  ## extract indices
    groupind <- as.numeric(attr(x$model, "index")[,1])
    timeind  <- as.numeric(attr(x$model, "index")[,2])

  ## adjust for 'fd' model (losing first time period)
    if(model == "fd") {
      groupi <- as.numeric(groupind)
      ## make vector =1 on first obs in each group, 0 elsewhere
      selector <- groupi - c(0, groupi[-length(groupi)])
      selector[1] <- 1 # the first must always be 1
      ## eliminate first obs in time for each group
      groupind <- groupind[!selector]
      timeind <- timeind[!selector]
      nT <- nT-n0
      Ti <- Ti-1
      t0 <- t0-1
    }

  ## set grouping indexes
    switch(match.arg(cluster),
            group = {
              n <- n0
              t <- t0
              relevant.ind <- groupind
              lab <- timeind}, 
            time = {
              n <- t0
              t <- n0
              relevant.ind <- timeind
              lab <- groupind})
    
    tind <- vector("list", n)
    tlab <- vector("list", n)
    
    for (i in 1:length(unique(relevant.ind))) {
        tind[[i]] <- which(relevant.ind == i)
        tlab[[i]] <- lab[which(relevant.ind == i)]
    }
  
  ## lab were the 'labels' (a numeric, actually) for the relevant index;
  ## in use again from the need to make pseudo-diagonals for
  ## calc. the lagged White terms on unbalanced panels

  ## transform residuals by weights (here because type='sss' needs to
  ## know who the grouping index 'g' is

  ## set number of clusters for Stata-like small sample correction
  ## (if clustering, i.e. inner="cluster", then G is the cardinality of
  ## the grouping index; if inner="white" it is simply the sample size)
    ## find some more elegant solution for this!
    ## (perhaps if white then sss->HC1 but check...)
  G <- if(match.arg(inner) == "cluster") n else nT
  uhat <- omega(uhat, diaghat, df, G)

  ## compute basic block: X'_t u_t u'_(t-l) X_(t-l) foreach t,
  ## then calculate Sl_t and sum over t (here i in place of t)

    ## here the benchmark case is time-clustering, but beware
    ## that group-clustering is the default

    ## preallocate k x k x (T-l) array for 'pile' of kxk matrices
    ## holding the X' E(u,ul) X elements
    Sl <- array(dim = c(k, k, n-l))
    
    ## (l=0 gives the special contemporaneous case where Xi=Xil, ui=uil
    ## for computing W, CX, CT)
    for(i in (1+l):n) {
      X <- demX[tind[[i]], , drop = FALSE]
      Xl <- demX[tind[[i-l]], , drop = FALSE]
      u <- uhat[tind[[i]]]
      names(u) <- tlab[[i]]
      ul <- uhat[tind[[(i-l)]]]
      names(ul) <- tlab[[(i-l)]]
      ## calculate V_yy
      Sl[, , i-l] <- crossprod(X, E(u, ul)) %*% Xl
    }
    
    ## in order to sum on available observations two things can be done:
    ## a) apply sum(..., na.rm=TRUE) over the third dim
    ## b) apply mean(..., na.rm=TRUE) idem and multiply by n-l
    ## In case a) averaging is then done dividing each covariance point
    ## by (n-l), regardless of whether there are NAs in the "vertical"
    ## vector Sl[p,q, ]
    ## In case b) each mean is calculated correctly on the right number
    ## of observations, excluding missing data. 'salame' has to be
    ## multiplied by (n-l)
    ## But notice, here there should be none left! Each Sl_i is k x k.
    ## Hence use sum().

    ## meat
    ## salame <- apply(Sl, 1:2, mean, na.rm=TRUE) * (n-l)
    salame <- apply(Sl, 1:2, sum)

    ## bread by standard method
    pane <- solve(crossprod(demX))

    ## sandwich
    mycov <- pane %*% salame %*% pane
    
    # save information about cluster variable in matrix (needed for e.g. robust F test)
    attr(mycov, which = "cluster") <- match.arg(cluster)
    return(mycov)
}

#' Robust Covariance Matrix Estimators
#' 
#' Robust covariance matrix estimators *a la White* for panel
#' models.
#' 
#' `vcovHC` is a function for estimating a robust covariance matrix of
#' parameters for a fixed effects or random effects panel model
#' according to the White method
#' \insertCite{WHIT:80,WHIT:84b,AREL:87}{plm}. Observations may be
#' clustered by `"group"` (`"time"`) to account for serial
#' (cross-sectional) correlation.
#' 
#' All types assume no intragroup (serial) correlation between errors
#' and allow for heteroskedasticity across groups (time periods). As
#' for the error covariance matrix of every single group of
#' observations, `"white1"` allows for general heteroskedasticity but
#' no serial (cross--sectional) correlation; `"white2"` is `"white1"`
#' restricted to a common variance inside every group (time period)
#' \insertCite{@see @GREE:03, Sec. 13.7.1-2, @GREE:12, Sec. 11.6.1-2
#' and @WOOL:02, Sec. 10.7.2}{plm}; `"arellano"` \insertCite{@see
#' ibid. and the original ref. @AREL:87}{plm} allows a fully general
#' structure w.r.t. heteroskedasticity and serial (cross--sectional)
#' correlation.
#' 
#' Weighting schemes specified by `type` are analogous to those in
#' [sandwich::vcovHC()] in package \CRANpkg{sandwich} and are
#' justified theoretically (although in the context of the standard
#' linear model) by \insertCite{MACK:WHIT:85;textual}{plm} and
#' \insertCite{CRIB:04;textual}{plm}
#' \insertCite{ZEIL:04}{plm}. `type = "sss"` employs the small sample
#' correction as used by Stata.
#' 
# % TODO: give formula for "sss";
# elaborate why different result for FE models (intercept)
#' 
#' The main use of `vcovHC` is to be an argument to other functions,
#' e.g.  for Wald--type testing: argument `vcov.` to `coeftest()`,
#' argument `vcov` to `waldtest()` and other methods in the
#' \CRANpkg{lmtest} package; and argument `vcov.` to
#' `linearHypothesis()` in the \CRANpkg{car} package (see the
#' examples). Notice that the `vcov` and `vcov.` arguments allow to
#' supply a function (which is the safest) or a matrix
#' \insertCite{@ZEIL:04, 4.1-2 and examples below}{plm}.
#' 
#' A special procedure for `pgmm` objects, proposed by
#' \insertCite{WIND:05;textual}{plm}, is also provided.
#' 
#' @name vcovHC.plm
#' @param x an object of class `"plm"` which should be the result of a
#'     random effects or a within model or a model of class `"pgmm"`
#'     or an object of class `"pcce"`,
#' @param method one of `"arellano"`, `"white1"`, `"white2"`,
#' @param type the weighting scheme used, one of `"HC0"`, `"sss"`,
#'     `"HC1"`, `"HC2"`, `"HC3"`, `"HC4"`, see Details,
#' @param cluster one of `"group"`, `"time"`,
#' @param \dots further arguments.
#' @return An object of class `"matrix"` containing the estimate of
#'     the asymptotic covariance matrix of coefficients.
#' @note The function `pvcovHC` is deprecated. Use `vcovHC` for the
#'     same functionality.
#' @export
#' @author Giovanni Millo & Yves Croissant
#' @seealso [sandwich::vcovHC()] from the \CRANpkg{sandwich}
#'     package for weighting schemes (`type` argument).
#' @references
#'
#' \insertRef{AREL:87}{plm}
#' 
#' \insertRef{CRIB:04}{plm}
#'
#' \insertRef{GREE:03}{plm}
#'
#' \insertRef{GREE:12}{plm}
#'
#' \insertRef{MACK:WHIT:85}{plm}
#'
#' \insertRef{WIND:05}{plm}
#' 
#' \insertRef{WHIT:84b}{plm}
#' chap. 6
#'
#' \insertRef{WHIT:80}{plm}
#' 
#' \insertRef{WOOL:02}{plm}
#'
#' \insertRef{ZEIL:04}{plm}
#' 
#' @keywords regression
#' @examples
#' 
#' library(lmtest)
#' library(car)
#' data("Produc", package = "plm")
#' zz <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
#'           data = Produc, model = "random")
#' ## standard coefficient significance test
#' coeftest(zz)
#' ## robust significance test, cluster by group
#' ## (robust vs. serial correlation)
#' coeftest(zz, vcov.=vcovHC)
#' ## idem with parameters, pass vcov as a function argument
#' coeftest(zz, vcov.=function(x) vcovHC(x, method="arellano", type="HC1"))
#' ## idem, cluster by time period
#' ## (robust vs. cross-sectional correlation)
#' coeftest(zz, vcov.=function(x) vcovHC(x, method="arellano",
#'  type="HC1", cluster="group"))
#' ## idem with parameters, pass vcov as a matrix argument
#' coeftest(zz, vcov.=vcovHC(zz, method="arellano", type="HC1"))
#' ## joint restriction test
#' waldtest(zz, update(zz, .~.-log(emp)-unemp), vcov=vcovHC)
#' ## test of hyp.: 2*log(pc)=log(emp)
#' linearHypothesis(zz, "2*log(pc)=log(emp)", vcov.=vcovHC)
#' 
#' ## Robust inference for CCE models
#' data("Produc", package = "plm")
#' ccepmod <- pcce(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc, model="p")
#' ## IGNORE_RDIFF_BEGIN
#' summary(ccepmod, vcov = vcovHC)
#' ## IGNORE_RDIFF_END
#' 
#' ## Robust inference for GMM models
#' data("EmplUK", package="plm")
#' ar <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
#'            + log(capital) + lag(log(capital), 2) + log(output)
#'            + lag(log(output),2) | lag(log(emp), 2:99),
#'             data = EmplUK, effect = "twoways", model = "twosteps")
#' rv <- vcovHC(ar)
#' mtest(ar, order = 2, vcov = rv)
NULL

#' @rdname vcovHC.plm
#' @export
vcovHC.plm <- function(x, method=c("arellano", "white1", "white2"),
                       type=c("HC0", "sss", "HC1", "HC2", "HC3", "HC4"),
                       cluster=c("group", "time"), ...) {
    ## user-level wrapper for White-Arellano covariances

    ## translate arguments
    inner <- switch(match.arg(method),
                    arellano = "cluster",
                    white1 = "white",
                    white2 = "diagavg")

    return(vcovG(x, type=type, cluster=cluster,
                        l=0, inner=inner, ...))
}

#' @rdname vcovNW
#' @export
vcovNW.plm <- function(x, type=c("HC0", "sss", "HC1", "HC2", "HC3", "HC4"),
                       maxlag=NULL,
                       wj=function(j, maxlag) 1-j/(maxlag+1),
                       ...) {
    ## user-level wrapper for panel Newey-West estimator

    ## set default lag order
    if(is.null(maxlag)) maxlag <- floor((max(pdim(x)$Tint$Ti))^(1/4))

    return(vcovSCC(x, type=type, maxlag=maxlag, inner="white", wj=wj, ...))
}

#' @rdname vcovDC
#' @export
vcovDC.plm <- function(x, type=c("HC0", "sss", "HC1", "HC2", "HC3", "HC4"),
                       ...) {
    ## user-level wrapper for double-clustering (no persistence)

    Vcx <- vcovG(x, type=type, cluster="group",
                        l=0, inner="cluster", ...)
    Vct <- vcovG(x, type=type, cluster="time",
                        l=0, inner="cluster", ...)
    Vw <- vcovG(x, type=type, l=0, inner="white", ...)

    res <- Vcx + Vct - Vw
    
    # save information about cluster variable in matrix (needed for e.g. robust F test)
    attr(res, which = "cluster") <- "group-time"
    return(res)
}

#' @rdname vcovSCC
#' @export
vcovSCC.plm <- function(x, type=c("HC0", "sss", "HC1", "HC2", "HC3", "HC4"),
                        cluster="time",
                        maxlag=NULL,
                        inner=c("cluster", "white", "diagavg"),
                        wj=function(j, maxlag) 1-j/(maxlag+1),
                        ...) {

    ## replicates vcovSCC

    ## set default lag order
    if(is.null(maxlag)) maxlag <- floor((max(pdim(x)$Tint$Ti))^(1/4))

    ## def. Bartlett kernel
    ## wj <- function(j, maxlag) 1-j/(maxlag+1)
    ## has been passed as argument

    S0 <- vcovG(x, type=type, cluster=cluster, l=0, inner=inner)

    if(maxlag > 0) {
        for(i in 1:maxlag) {
            Vctl <- vcovG(x, type=type, cluster=cluster,
                             l=i, inner=inner)
            S0 <- S0 + wj(i, maxlag) * (Vctl + t(Vctl))
        }
    }

    return(S0)
}

                                        # ICI

##############################################################

## separate function for BK (PCSE) covariance



#' Beck and Katz Robust Covariance Matrix Estimators
#' 
#' Unconditional Robust covariance matrix estimators *a la Beck
#' and Katz* for panel models (a.k.a. Panel Corrected Standard Errors
#' (PCSE)).
#' 
#' `vcovBK` is a function for estimating a robust covariance matrix of
#' parameters for a panel model according to the
#' \insertCite{BECK:KATZ:95;textual}{plm} method, a.k.a. Panel
#' Corrected Standard Errors (PCSE), which uses an unconditional
#' estimate of the error covariance across time periods (groups)
#' inside the standard formula for coefficient
#' covariance. Observations may be clustered either by `"group"` to
#' account for timewise heteroskedasticity and serial correlation or
#' by `"time"` to account for cross-sectional heteroskedasticity and
#' correlation. It must be borne in mind that the Beck and Katz
#' formula is based on N- (T-) asymptotics and will not be appropriate
#' elsewhere.
#' 
#' The `diagonal` logical argument can be used, if set to
#' `TRUE`, to force to zero all nondiagonal elements in the
#' estimated error covariances; this is appropriate if both serial and
#' cross--sectional correlation are assumed out, and yields a
#' timewise- (groupwise-) heteroskedasticity--consistent estimator.
#' 
#' Weighting schemes specified by `type` are analogous to those in
#' [sandwich::vcovHC()] in package \CRANpkg{sandwich} and are
#' justified theoretically (although in the context of the standard
#' linear model) by \insertCite{MACK:WHIT:85;textual}{plm} and
#' \insertCite{CRIB:04;textual}{plm} \insertCite{@see @ZEIL:04}{plm}.
#' 
# % TODO: once "sss" has been added: `type = "sss"` employs the small
# % sample correction as used by Stata. give formula for "sss";
# % elaborate why different result for FE models (intercept)
#' 
#' The main use of `vcovBK` is to be an argument to other functions,
#' e.g.  for Wald--type testing: argument `vcov.` to `coeftest()`,
#' argument `vcov` to `waldtest()` and other methods in the
#' \CRANpkg{lmtest} package; and argument `vcov.` to
#' `linearHypothesis()` in the \CRANpkg{car} package (see the
#' examples). Notice that the `vcov` and `vcov.` arguments allow to
#' supply a function (which is the safest) or a matrix
#' \insertCite{@see @ZEIL:04, 4.1-2 and examples below}{plm}.
#' 
#' @param x an object of class `"plm"`,
#' @param type the weighting scheme used, one of `"HC0"`, `"HC1"`,
#'     `"HC2"`, `"HC3"`, `"HC4"`, see Details,
#' @param cluster one of `"group"`, `"time"`,
#' @param diagonal a logical value specifying whether to force
#'     nondiagonal elements to zero,
#' @param \dots further arguments.
#' @export
#' @return An object of class `"matrix"` containing the estimate of
#'     the covariance matrix of coefficients.
#' @author Giovanni Millo
#' @seealso [sandwich::vcovHC()] from the \CRANpkg{sandwich}
#'     package for weighting schemes (`type` argument).
#' @references
#'
#'
#' \insertRef{BECK:KATZ:95}{plm}
#'
#' \insertRef{CRIB:04}{plm}
#'
#' \insertRef{GREE:03}{plm}
#'
#' \insertRef{MACK:WHIT:85}{plm}
#'
#' \insertRef{ZEIL:04}{plm}
#' 
#' @keywords regression
#' @examples
#' 
#' library(lmtest)
#' library(car)
#' data("Produc", package="plm")
#' zz <- plm(log(gsp)~log(pcap)+log(pc)+log(emp)+unemp, data=Produc, model="random")
#' ## standard coefficient significance test
#' coeftest(zz)
#' ## robust significance test, cluster by group
#' ## (robust vs. serial correlation), default arguments
#' coeftest(zz, vcov.=vcovBK)
#' ## idem with parameters, pass vcov as a function argument
#' coeftest(zz, vcov.=function(x) vcovBK(x, type="HC1"))
#' ## idem, cluster by time period
#' ## (robust vs. cross-sectional correlation)
#' coeftest(zz, vcov.=function(x) vcovBK(x,
#'  type="HC1", cluster="time"))
#' ## idem with parameters, pass vcov as a matrix argument
#' coeftest(zz, vcov.=vcovBK(zz, type="HC1"))
#' ## joint restriction test
#' waldtest(zz, update(zz, .~.-log(emp)-unemp), vcov=vcovBK)
#' ## test of hyp.: 2*log(pc)=log(emp)
#' linearHypothesis(zz, "2*log(pc)=log(emp)", vcov.=vcovBK)
#' 
vcovBK <- function(x, ...) {
    UseMethod("vcovBK")
}


# TODO: add type "sss" for vcovBK

#' @rdname vcovBK
#' @export
vcovBK.plm <- function(x, type = c("HC0", "HC1", "HC2", "HC3", "HC4"),
                       cluster = c("group", "time"),
                       diagonal = FALSE, ...) {

  ## Robust vcov a la Beck and Katz (1995; AKA 'pcse')
  ## for panel models (pooling, random, within or fd type plm obj.)
  ##
  ## This version: October 20th, 2009; allows choosing the clustering dimension
  ## so as to have serial- or x-sectional-correlation robustness;
  ##
  ## This function takes the demeaned data from the
  ## plm object, then estimates an *unconditional* error covariance by
  ## averaging the empirical covariance blocks by group (time period);
  ## this average block (say, OmegaM in EViews notation) is then put into
  ## White's formula instead of each Omega_i.
  ##
  ## The clustering defaults to "group" for consistency with pvcovHC;
  ## nevertheless the most likely usage is cluster="time" for robustness vs.
  ## cross-sectional dependence, as in the original Beck and Katz paper (where
  ## it is applied to "pooling" models).
  ##
  ## This version: compliant with plm 1.2-0; lmtest.
  ## Code is identical to pvcovHC until mark.
  ##
  ## Usage:
  ## myplm <- plm(<model>,<data>, ...)
  ## # default (cluster by group = robust vs. serial correlation):
  ## coeftest(myplm, vcov=vcovBK)
  ## # cluster by time period (robust vs. XS correlation):
  ## coeftest(myplm, vcov=function(x) vcovBK(x, cluster="time"))
  ## # idem, HC3 weighting:
  ## coeftest(myplm, vcov=function(x) vcovBK(x,cluster="time",type="HC3"))
  ## waldtest(myplm,update(myplm,<new formula>),vcov=vcovBK)
  ##
  ## This weighted version implements a system of weights as
  ## in vcovHC/meatHC. Sure this makes sense for white1, but it
  ## is open to question for white2 and arellano. We'll see.
  ##
  ## Results OK vs. EViews, vcov=PCSE. Unbal. case not exactly the
  ## same (but then, who knows what EViews does!)
  
    # stopping control for weighted regressions
    if (!is.null(x$weights)) stop("vcovXX functions not implemented for weighted panel regressions")
  
    type <- match.arg(type)
    model <- describe(x, "model")
    if (!model %in% c("random", "within", "pooling", "fd")) {
        stop("Model has to be either \"random\", \"within\", \"pooling\", or \"fd\" model")
    }
    
  ## extract demeaned data
    demy <- pmodel.response(x, model = model)
    demX <- model.matrix(x, model = model, rhs = 1, cstcovar.rm = "all")
    ## drop any linear dependent columns (corresponding to aliased coefficients)
    ## from model matrix X
    ##  na.rm = TRUE because currently, RE tw unbalanced models set aliased simply to NA
    if (!is.null(x$aliased) && any(x$aliased, na.rm = TRUE)) demX <- demX[, !x$aliased, drop = FALSE]
    
    ## control: IV or not (two- or one-part formula)
    if(length(formula(x))[2] > 1) {
        demZ <- model.matrix(x, model = model, rhs = 2, cstcovar.rm = "all")
        ## substitute (transformed) X with projection of X on Z
        ## any linear dependence in Z (demZ) is appropriately taken care of by lm.fit()
        demX <- fitted(lm.fit(demZ, demX))
    }

    pdim <- pdim(x)
    nT <- pdim$nT$N
    Ti <- pdim$Tint$Ti
    k <- dim(demX)[[2]]
    n0 <- pdim$nT$n 
    t0 <- pdim$nT$T
    
  ## extract residuals
    uhat <- x$residuals

  ## robustifying against either serial or xs intragroup dependence:
  ## if 'group' then keep current indexing, if 'time' then swap i<->t
  ## so that residuals get 'clustered' by time period instead of by
  ## group (i.e. the vcov estimator is robust vs. xsectional dependence)

  ## extract indices
    groupind <- as.numeric(attr(x$model, "index")[,1])
    timeind  <- as.numeric(attr(x$model, "index")[,2])

  ## Achim's fix for 'fd' model (losing first time period)
    if(model == "fd") {
      groupind <- groupind[timeind > 1]
      timeind <- timeind[timeind > 1]
      nT <- nT-n0
      Ti <- Ti-1
      t0 <- t0-1
    }

  ## set grouping indexes
    switch(match.arg(cluster),
            group = {
              n <- n0 # this is needed only for 'pcse'
              t <- t0 # this is needed only for 'pcse'
              relevant.ind <- groupind
              lab <- timeind },
            time = {
              n <- t0 # this is needed only for 'pcse'
              t <- n0 # this is needed only for 'pcse'
              relevant.ind <- timeind
              lab <- groupind
            })
    
    tind <- vector("list", n)
    tlab <- vector("list", n)
    
    for (i in 1:length(unique(relevant.ind))) {
        tind[[i]] <- which(relevant.ind == i)
        tlab[[i]] <- lab[which(relevant.ind == i)]
    }

  ## define residuals weighting function omega(res)
  ## (code taken from meatHC and modified)
  ## (the weighting is defined "in sqrt" relative to the literature)
  ##
  ## (see the theoretical comments in pvcovHC)

    ## diaghat function for matrices
    dhat <- function(x) {tx <- t(x)
                         diag(crossprod(tx, solve(crossprod(x), tx)))}

    ## this is computationally heavy, do only if needed
    switch(match.arg(type), HC0 = {diaghat <- NULL},
                            HC1 = {diaghat <- NULL},
                            HC2 = {diaghat <- try(dhat(demX), silent = TRUE)},
                            HC3 = {diaghat <- try(dhat(demX), silent = TRUE)},
                            HC4 = {diaghat <- try(dhat(demX), silent = TRUE)})
    df <- nT - k
    switch(match.arg(type), 
           HC0 = {
            omega <- function(residuals, diaghat, df) residuals
        }, HC1 = {
            omega <- function(residuals, diaghat, df) residuals *
                                sqrt(length(residuals)/df)
        }, HC2 = {
            omega <- function(residuals, diaghat, df) residuals /
                                sqrt(1 - diaghat)
        }, HC3 = {
            omega <- function(residuals, diaghat, df) residuals /
                                (1 - diaghat)
        }, HC4 = {
            omega <- function(residuals, diaghat, df) residuals/sqrt(1 -
                diaghat)^pmin(4, length(residuals) * diaghat/as.integer(round(sum(diaghat),
                digits = 0)))
        })

  ## transform residuals by weights
  uhat <- omega(uhat, diaghat, df)

  ## CODE TAKEN FROM pvcovHC() UNTIL HERE except for ind/time labeling ##

  ## the PCSE covariance estimator is based on the unconditional estimate
  ## of the intragroup (intraperiod) covariance of errors, OmegaT or OmegaM
  ## in the EViews help.
  ## we calculate this based on code from pggls().
  ## the Omegai function is then:
  ## - constant if the panel is balanced
  ## - depending only on the intragroup (intraperiod) position index
  ##   if the panel is unbalanced.

  ## (code for estimating OmegaM/OmegaT partly taken from pggls)

    ## est. omega submatrix
    ## "pre-allocate" an empty array
    tres <- array(dim = c(t, t, n))

    ## array of n "empirical omega-blocks"
    ## with outer product of t(i) residuals
    ## for each group 1..n
    ## (use subscripting from condition 'label in labels' set',
    ## the rest stays NA if any)
    for(i in 1:n) {
      ut <- uhat[tind[[i]]]
      tpos <- (1:t)[unique(lab) %in% tlab[[i]]]
      ## put nondiag elements to 0 if diagonal=TRUE
      if(diagonal) {
        tres[tpos, tpos, i] <- diag(diag(ut %o% ut))
      } else {
        tres[tpos, tpos, i] <- ut %o% ut
      }
    }

    ## average over all omega blocks, removing NAs (apply preserving
    ## *two* dimensions, i.e. over the third) to get the unconditional
    ## covariance matrix of errors for a group (viz. time period):
    OmegaT <- apply(tres, 1:2, mean, na.rm = TRUE)

  ## end of PCSE covariance calculation.

  ## fetch (all, unique) values of the relevant labels
  unlabs <- unique(lab)

  salame <- array(dim = c(k, k, n))
  for(i in 1:n) {
    groupinds <- tind[[i]]
    grouplabs <- tlab[[i]]
    xi <- demX[groupinds, , drop = FALSE]
    ## for every group, take relevant positions
    tpos <- unlabs %in% grouplabs
    OmegaTi <- OmegaT[tpos, tpos, drop = FALSE]
    salame[,,i] <- crossprod(xi, OmegaTi) %*% xi
  }

  ## meat
  salame <- apply(salame, 1:2, sum)

  ## bread
  pane <- solve(crossprod(demX))

  ## sandwich
  mycov <- pane %*% salame %*% pane
  
  # save information about cluster variable in matrix (needed for e.g. robust F test)
  attr(mycov, which = "cluster") <- match.arg(cluster)
  return(mycov)
}

#######################################################

#####################################
## vcovXX methods for pcce objects ##
#####################################

## pcce is compliant with plm so vcovXX.pcce <- vcovXX.plm
## for any vcov that makes sense computed on the transformed
## data from model.matrix.pcce and pmodel.response.pcce

## TODO: vcovBK.pcce missing? Or not valid?

#' @rdname vcovG
#' @export
vcovG.pcce   <- vcovG.plm

#' @rdname vcovHC.plm
#' @export
vcovHC.pcce  <- vcovHC.plm

#' @rdname vcovNW
#' @export
vcovNW.pcce  <- vcovNW.plm

#' @rdname vcovSCC
#' @export
vcovSCC.pcce <- vcovSCC.plm


####################################
## vcovHC method for pgmm objects ##
####################################

#' @rdname vcovHC.plm
#' @importFrom MASS ginv
#' @export
vcovHC.pgmm <- function(x, ...){
  model <- describe(x, "model")
  transformation <- describe(x, "transformation")
  A1 <- x$A1
  A2 <- x$A2

  if (transformation=="ld"){
##     yX <- lapply(x$model,function(x) rbind(diff(x),x))
##     residuals <-lapply(x$residuals,function(x) c(diff(x),x))
    yX <- x$model
    residuals <- x$residuals
  }
  else{
    yX <- x$model
    residuals <- x$residuals
  }    
  minevA2 <- min(abs(Re(eigen(A2)$values)))
  eps <- 1E-9
  if (minevA2 < eps){
    SA2 <- ginv(A2)
    warning("a general inverse is used")
  }
  else SA2 <- solve(A2)
  
  if (model=="twosteps"){
    coef1s <- x$coefficients[[1]]
    res1s <- lapply(yX, function(x) x[,1] - crossprod(t(x[, -1, drop=FALSE]), coef1s))
    K <- ncol(yX[[1]])
    D <- c()
    WX <- Reduce("+",
                 mapply(function(x, y) crossprod(x, y[,-1, drop=FALSE]), x$W, yX, SIMPLIFY = FALSE))
    We <- Reduce("+", mapply(function(x, y) crossprod(x, y), x$W, residuals, SIMPLIFY = FALSE))
    B1 <- solve(t(WX) %*% A1 %*% WX)
    B2 <- vcov(x)

    vcov1s <- B1 %*% (t(WX) %*% A1 %*% SA2 %*% A1 %*% WX) %*% B1
    for (k in 2:K){
      exk <- mapply(
                    function(x,y){
                      z <- crossprod(t(x[,k, drop=FALSE]),t(y))
                      - z - t(z)
                    },
                    yX, res1s, SIMPLIFY = FALSE)
      wexkw <- Reduce("+",
                      mapply(
                             function(x, y)
                             crossprod(x, crossprod(y, x)),
                             x$W, exk, SIMPLIFY = FALSE))
      Dk <- -B2 %*% t(WX) %*% A2 %*% wexkw %*% A2 %*% We
      D <- cbind(D,Dk)
    }
    vcovr <- B2 + crossprod(t(D), B2) + t(crossprod(t(D), B2)) + D %*% vcov1s %*% t(D)
  }
  else{
    res1s <- lapply(yX, function(z) z[,1] - crossprod(t(z[, -1, drop=FALSE]), x$coefficients))
    K <- ncol(yX[[1]])
    WX <- Reduce("+", mapply(function(z, y) crossprod(z[,-1, drop=FALSE], y), yX, x$W, SIMPLIFY = FALSE))
    B1 <- vcov(x)
    vcovr <- B1 %*% (WX %*% A1 %*% SA2 %*% A1 %*% t(WX)) %*% B1
  }
  vcovr
}
