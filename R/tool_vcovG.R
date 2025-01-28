
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
#' The main use of `vcovSCC` (and the other variance-covariance estimators 
#' provided in the package `vcovHC`, `vcovBK`, `vcovNW`, `vcovDC`) is to pass 
#' it to plm's own functions like `summary`, `pwaldtest`, and `phtest` or 
#' together with testing functions from the `lmtest` and `car` packages. All of 
#' these typically allow passing the `vcov` or `vcov.` parameter either as a 
#' matrix or as a function, e.g., for Wald--type testing: argument `vcov.` to
#' `coeftest()`, argument `vcov` to `waldtest()` and other methods in the
#' \CRANpkg{lmtest} package; and argument `vcov.` to
#' `linearHypothesis()` in the \CRANpkg{car} package (see the
#' examples), \insertCite{@see also @ZEIL:04}{plm}, 4.1-2, and examples below.
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
#' data("Produc", package="plm")
#' zz <- plm(log(gsp)~log(pcap)+log(pc)+log(emp)+unemp, data=Produc, model="pooling")
#' ## as function input to plm's summary method (with and without additional arguments):
#' summary(zz, vcov = vcovSCC)
#' summary(zz, vcov = function(x) vcovSCC(x, method="arellano", type="HC1"))
#' ## standard coefficient significance test
#' library(lmtest)
#' coeftest(zz)
#' ## SCC robust significance test, default
#' coeftest(zz, vcov.=vcovSCC)
#' ## idem with parameters, pass vcov as a function argument
#' coeftest(zz, vcov.=function(x) vcovSCC(x, type="HC1", maxlag=4))
#' ## joint restriction test
#' waldtest(zz, update(zz, .~.-log(emp)-unemp), vcov=vcovSCC)
#' \dontrun{
#' ## test of hyp.: 2*log(pc)=log(emp)
#' library(car)
#' linearHypothesis(zz, "2*log(pc)=log(emp)", vcov.=vcovSCC)
#' }
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
#' The main use of `vcovNW` (and the other variance-covariance estimators 
#' provided in the package `vcovHC`, `vcovBK`, `vcovDC`, `vcovSCC`) is to pass 
#' it to plm's own functions like `summary`, `pwaldtest`, and `phtest` or 
#' together with testing functions from the `lmtest` and `car` packages. All of 
#' these typically allow passing the `vcov` or `vcov.` parameter either as a 
#' matrix or as a function, e.g., for Wald--type testing: argument `vcov.` to
#' `coeftest()`, argument `vcov` to `waldtest()` and other methods in the
#' \CRANpkg{lmtest} package; and argument `vcov.` to
#' `linearHypothesis()` in the \CRANpkg{car} package (see the
#' examples), see \insertCite{@see also @ZEIL:04}{plm}, 4.1-2, and examples below.
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
#' data("Produc", package="plm")
#' zz <- plm(log(gsp)~log(pcap)+log(pc)+log(emp)+unemp, data=Produc, model="pooling")
#' ## as function input to plm's summary method (with and without additional arguments):
#' summary(zz, vcov = vcovNW)
#' summary(zz, vcov = function(x) vcovNW(x, method="arellano", type="HC1"))
#' ## standard coefficient significance test
#' library(lmtest)
#' coeftest(zz)
#' ## NW robust significance test, default
#' coeftest(zz, vcov.=vcovNW)
#' ## idem with parameters, pass vcov as a function argument
#' coeftest(zz, vcov.=function(x) vcovNW(x, type="HC1", maxlag=4))
#' ## joint restriction test
#' waldtest(zz, update(zz, .~.-log(emp)-unemp), vcov=vcovNW)
#' \dontrun{
#' ## test of hyp.: 2*log(pc)=log(emp)
#' library(car)
#' linearHypothesis(zz, "2*log(pc)=log(emp)", vcov.=vcovNW)
#' }
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
#' The main use of `vcovDC` (and the other variance-covariance estimators 
#' provided in the package `vcovHC`, `vcovBK`, `vcovNW`, `vcovSCC`) is to pass 
#' it to plm's own functions like `summary`, `pwaldtest`, and `phtest` or 
#' together with testing functions from the `lmtest` and `car` packages. All of 
#' these typically allow passing the `vcov` or `vcov.` parameter either as a 
#' matrix or as a function, e.g., for Wald--type testing: argument `vcov.` to
#' `coeftest()`, argument `vcov` to `waldtest()` and other methods in the
#' \CRANpkg{lmtest} package; and argument `vcov.` to
#' `linearHypothesis()` in the \CRANpkg{car} package (see the
#' examples), see \insertCite{@see also @ZEIL:04}{plm}, 4.1-2, and examples below.
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
#' data("Produc", package="plm")
#' zz <- plm(log(gsp)~log(pcap)+log(pc)+log(emp)+unemp, data=Produc, model="pooling")
#' ## as function input to plm's summary method (with and without additional arguments):
#' summary(zz, vcov = vcovDC)
#' summary(zz, vcov = function(x) vcovDC(x, type="HC1", maxlag=4))
#' ## standard coefficient significance test
#' library(lmtest)
#' coeftest(zz)
#' ## DC robust significance test, default
#' coeftest(zz, vcov.=vcovDC)
#' ## idem with parameters, pass vcov as a function argument
#' coeftest(zz, vcov.=function(x) vcovDC(x, type="HC1", maxlag=4))
#' ## joint restriction test
#' waldtest(zz, update(zz, .~.-log(emp)-unemp), vcov=vcovDC)
#' \dontrun{
#' ## test of hyp.: 2*log(pc)=log(emp)
#' library(car)
#' linearHypothesis(zz, "2*log(pc)=log(emp)", vcov.=vcovDC)
#' }
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
#'     `"sss"`, `"HC1"`, `"HC2"`, `"HC3"`, `"HC4"`,
#' @param cluster one of `"group"`, `"time"`,
#' @param l lagging order, defaulting to zero
#' @param inner the function to be applied to the residuals inside the
#'     sandwich: one of `"cluster"` or `"white"` or
#'     `"diagavg"`, or a user specified R function,
#' @param \dots further arguments
#' @return An object of class `"matrix"` containing the estimate
#'     of the covariance matrix of coefficients.
#' @export
#' @author Giovanni Millo
#' @seealso [vcovHC()], [vcovSCC()],
#'     [vcovDC()], [vcovNW()], and
#'     [vcovBK()] albeit the latter does not make use of vcovG.
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
#' ## define custom covariance function
#' ## (in this example, same as vcovHC)
#' myvcov <- function(x) vcovG(x, cluster="group", inner="cluster", l=0)
#' summary(zz, vcov = myvcov)
#' ## use in coefficient significance test
#' library(lmtest)
#' ## robust significance test
#' coeftest(zz, vcov. = myvcov)
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
    demX <- model.matrix(x, model = model, rhs = 1, cstcovar.rm = "all")
    ## drop any linear dependent columns (corresponding to aliased coefficients)
    ## from model matrix X
    ## na.rm = TRUE because currently, RE tw unbalanced models set aliased simply to NA
    if (!is.null(x$aliased) && any(x$aliased, na.rm = TRUE)) demX <- demX[ , !x$aliased, drop = FALSE]

    ## control: IV or not (two- or one-part formula)
    if(length(formula(x))[2L] > 1L) {
        demZ <- model.matrix(x, model = model, rhs = 2, cstcovar.rm = "all")
        ## substitute (transformed) X with projection of X on Z
        ## any linear dependence in Z (demZ) is appropriately taken care of by lm.fit()
        nms <- colnames(demX)
        demX <- lm.fit(demZ, demX)$fitted.values
        # catches case with only one regressor -> need to convert numeric 
        # returned from lm.fit()$fitted.values to matrix:
        if(!is.matrix(demX)) demX <- matrix(demX, dimnames = list(NULL, nms[1L]))
    }

    pdim <- pdim(x)
    nT <- pdim$nT$N
    Ti <- pdim$Tint$Ti
    k <- dim(demX)[[2L]]
    n0 <- pdim$nT$n # no. of groups
    t0 <- pdim$nT$T # max time periods over groups

  ## extract residuals
    uhat <- x$residuals

    diaghat <- switch(type, "HC0" = NULL,
                            "sss" = NULL,
                            "HC1" = NULL,
                            "HC2" = try(dhat(demX), silent = TRUE),
                            "HC3" = try(dhat(demX), silent = TRUE),
                            "HC4" = try(dhat(demX), silent = TRUE))
    

   ## Definition module for E(u,v)
    if(is.function(inner)) {
      # case of user-specified function
      # (only if user calls workhorse vcovG directly (i.e., not possible via wrappers vcovXX)
        E <- inner
    } else {
      ## outer for clustering/arellano, diag(diag(inner)) for white
      switch(match.arg(inner), 
         "cluster" = {
            E <- function(u, v) outer(u, v)
          },
          "white" = {
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
                    for(i in seq_along(names(u))) {
                        for(j in seq_along(names(v))) {
                            if(names(u)[i] == names(v)[j]) {
                                eres[i, j] <- efull[i, j]
                            }
                        }
                    }
                    euv <- eres
                }
                return(euv)
            }
          },
          "diagavg" = {
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
                    for(i in seq_along(names(u))) {
                        for(j in seq_along(names(v))) {
                            if(names(u)[i] == names(v)[j]) {
                                eres[i, j] <- efull[i, j]
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
  ## group (i.e., the vcov estimator is robust vs. xsectional dependence)

  ## extract indices
    xindex <- unclass(attr(x$model, "index")) # unclass for speed
    groupind <- as.numeric(xindex[[1L]])
    timeind  <- as.numeric(xindex[[2L]])


  ## adjust for 'fd' model (losing first time period)
    if(model == "fd") {
      ## debug printing:
      #print("before FD adj:")
      #print(paste0("nT = ", nT))
      #print(paste0("Ti = ", paste0(Ti, collapse = ", ")))
      #print(paste0("t0 = ", t0))
      #cat("\n")
   
      groupi <- as.numeric(groupind)
      ## make vector =1 on first obs in each group, 0 elsewhere
      selector <- groupi - c(0, groupi[-length(groupi)])
      selector[1L] <- 1 # the first must always be 1
      ## eliminate first obs in time for each group
      groupind <- groupind[!selector]
      timeind  <- timeind[!selector]
      nT <- nT - n0
      Ti <- Ti - 1
      if(any(drop <- Ti == 0L)) {
        # drop groups in Ti that are now empty (group had 1 observation before first-differencing, hence 0 after)
        # and adjust n0 due to same reason
        Ti <- Ti[!drop]
        n0 <- n0 - sum(drop)
      }
      t0 <- t0 - 1
      
      ## debug printing:
      #print("after FD adj:")
      #print(paste0("nT = ", nT))
      #print(paste0("Ti = ", paste0(Ti, collapse = ", ")))
      #print(paste0("t0 = ", t0))
    }

  ## set grouping indexes
    cluster <- match.arg(cluster)
    switch(cluster,
            "group" = {
              n <- n0
              t <- t0
              relevant.ind <- groupind
              lab <- timeind}, 
            "time" = {
              n <- t0
              t <- n0
              relevant.ind <- timeind
              lab <- groupind})
    
    relevant.ind.GRP <- collapse::GRP(relevant.ind)
    tind <- collapse::gsplit(seq_along(relevant.ind), relevant.ind.GRP)
    tlab <- collapse::gsplit(lab, relevant.ind.GRP)

  ## lab were the 'labels' (a numeric, actually) for the relevant index;
  ## in use again from the need to make pseudo-diagonals for
  ## calc. the lagged White terms on unbalanced panels

  ## transform residuals by weights (here because type='sss' needs to
  ## know who the grouping index 'g' is in helper function omega())

  ## set number of clusters for Stata-like small sample correction
  ## (if clustering, i.e., inner="cluster", then G is the cardinality of
  ## the grouping index; if inner="white" it is simply the sample size)
    ## TODO: find some more elegant solution for this!
    ## (perhaps if white then sss -> HC1 but check...)
  G <- if(match.arg(inner) == "cluster") n else nT
  
  # transform residuals by weights
  uhat <- omega(residuals = uhat, diaghat = diaghat, g = G, nT = nT, k = k, type = type)

  ## compute basic block: X'_t u_t u'_(t-l) X_(t-l) foreach t,
  ## then calculate Sl_t and sum over t (here i in place of t)

    ## here the benchmark case is time-clustering, but beware
    ## that group-clustering is the default

    ## preallocate k x k x (T-l) array for 'pile' of kxk matrices
    ## holding the X' E(u,ul) X elements
    Sl <- array(NA_real_, dim = c(k, k, n-l))
    
    ## (l=0 gives the special contemporaneous case where Xi=Xil, ui=uil
    ## for computing W, CX, CT)

    for(i in (1+l):n) {
      X  <- demX[tind[[i]], ,   drop = FALSE]
      Xl <- demX[tind[[i-l]], , drop = FALSE]
      u  <- uhat[tind[[i]]]
      ul <- uhat[tind[[(i-l)]]]
      names(u)  <- tlab[[i]]      # names needed in E()
      names(ul) <- tlab[[(i-l)]]  #   --- " ---
      ## calculate V_yy
      Sl[ , , i-l] <- tcrossprod(crossprod(X, E(u, ul)), t(Xl))
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
    salame <- rowSums(Sl, dims = 2L) # == apply(Sl, 1:2, sum) but faster

    ## bread by standard method
    pane <- solve(crossprod(demX))
    ## sandwich
    mycov <- tcrossprod(tcrossprod(pane, salame), t(pane)) # == pane %*% salame %*% pane
    
    # save information about cluster variable in matrix (needed for e.g.,
    # robust F test)
    attr(mycov, which = "cluster") <- cluster
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
#' The main use of `vcovHC` (and the other variance-covariance estimators 
#' provided in the package `vcovBK`, `vcovNW`, `vcovDC`, `vcovSCC`) is to pass 
#' it to plm's own functions like `summary`, `pwaldtest`, and `phtest` or 
#' together with testing functions from the `lmtest` and `car` packages. All of 
#' these typically allow passing the `vcov` or `vcov.` parameter either as a 
#' matrix or as a function, e.g., for Wald--type testing: argument `vcov.` to
#' `coeftest()`, argument `vcov` to `waldtest()` and other methods in the
#' \CRANpkg{lmtest} package; and argument `vcov.` to
#' `linearHypothesis()` in the \CRANpkg{car} package (see the
#' examples), see \insertCite{@see also @ZEIL:04}{plm}, 4.1-2, and examples below.
#' 
#' A method for `pgmm` objects, `vcovHC.pgmm`, is also provided and gives the robust
#' variance-covariances matrix, in case of a two-steps panel GMM model with the 
#' small-sample correction proposed by \insertCite{WIND:05;textual}{plm}.
#' 
#' @name vcovHC.plm
#' @aliases vcovHC
#' @importFrom sandwich vcovHC
#' @export vcovHC
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
#' data("Produc", package = "plm")
#' zz <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
#'           data = Produc, model = "random")
#' ## as function input to plm's summary method (with and without additional arguments):
#' summary(zz, vcov = vcovHC)
#' summary(zz, vcov = function(x) vcovHC(x, method="arellano", type="HC1"))
#' 
#' ## standard coefficient significance test
#' library(lmtest)
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
#' \dontrun{
#' ## test of hyp.: 2*log(pc)=log(emp)
#' library(car)
#' linearHypothesis(zz, "2*log(pc)=log(emp)", vcov.=vcovHC)
#' }
#' ## Robust inference for CCE models
#' data("Produc", package = "plm")
#' ccepmod <- pcce(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc, model="p")
#' summary(ccepmod, vcov = vcovHC)
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
vcovHC.plm <- function(x, method = c("arellano", "white1", "white2"),
                       type = c("HC0", "sss", "HC1", "HC2", "HC3", "HC4"),
                       cluster = c("group", "time"), ...) {
    ## user-level wrapper for White-Arellano covariances

    ## translate arguments
    inner <- switch(match.arg(method),
                    "arellano" = "cluster",
                    "white1"   = "white",
                    "white2"   = "diagavg")

    return(vcovG(x, type = type, cluster = cluster, l = 0, inner = inner, ...))
}

#' @rdname vcovNW
#' @export
vcovNW.plm <- function(x, type = c("HC0", "sss", "HC1", "HC2", "HC3", "HC4"),
                       maxlag = NULL,
                       wj = function(j, maxlag) 1-j/(maxlag+1),
                       ...) {
    ## user-level wrapper for panel Newey-West estimator

    ## set default lag order
    if(is.null(maxlag)) maxlag <- floor((max(pdim(x)$Tint$Ti))^(1/4))

    return(vcovSCC(x, type = type, maxlag = maxlag, inner = "white", wj = wj, ...))
}

#' @rdname vcovDC
#' @export
vcovDC.plm <- function(x, type = c("HC0", "sss", "HC1", "HC2", "HC3", "HC4"),
                       ...) {
    ## user-level wrapper for double-clustering (no persistence)

    Vcx <- vcovG(x, type = type, cluster = "group", l = 0, inner = "cluster", ...)
    Vct <- vcovG(x, type = type, cluster = "time",  l = 0, inner = "cluster", ...)
    Vw  <- vcovG(x, type = type,                    l = 0, inner = "white", ...)

    res <- Vcx + Vct - Vw
    
    # save information about cluster variable in matrix (needed for e.g.,
    # robust F test)
    attr(res, which = "cluster") <- "group-time"
    return(res)
}

#' @rdname vcovSCC
#' @export
vcovSCC.plm <- function(x, type = c("HC0", "sss", "HC1", "HC2", "HC3", "HC4"),
                        cluster = "time",
                        maxlag = NULL,
                        inner = c("cluster", "white", "diagavg"),
                        wj = function(j, maxlag) 1-j/(maxlag+1),
                        ...) {

    ## set default lag order
    if(is.null(maxlag)) maxlag <- floor((max(pdim(x)$Tint$Ti))^(1/4))

    ## def. Bartlett kernel
    ## wj <- function(j, maxlag) 1-j/(maxlag+1)
    ## has been passed as argument

    S0 <- vcovG(x, type = type, cluster = cluster, l = 0, inner = inner)

    if(maxlag > 0) {
        for(i in seq_len(maxlag)) {
            Vctl <- vcovG(x, type = type, cluster = cluster, l = i, inner = inner)
            S0 <- S0 + wj(i, maxlag) * (Vctl + t(Vctl))
        }
    }

    return(S0)
}


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
#' `TRUE`, to force to zero all non-diagonal elements in the
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
#' The main use of `vcovBK` (and the other variance-covariance estimators 
#' provided in the package `vcovHC`, `vcovNW`, `vcovDC`, `vcovSCC`) is to pass 
#' it to plm's own functions like `summary`, `pwaldtest`, and `phtest` or 
#' together with testing functions from the `lmtest` and `car` packages. All of 
#' these typically allow passing the `vcov` or `vcov.` parameter either as a 
#' matrix or as a function, e.g., for Wald--type testing: argument `vcov.` to
#' `coeftest()`, argument `vcov` to `waldtest()` and other methods in the
#' \CRANpkg{lmtest} package; and argument `vcov.` to
#' `linearHypothesis()` in the \CRANpkg{car} package (see the
#' examples), see \insertCite{@see also @ZEIL:04}{plm}, 4.1-2, and examples below.
#' 
#' @param x an object of class `"plm"`,
#' @param type the weighting scheme used, one of `"HC0"`, `"sss"`, `"HC1"`,
#'     `"HC2"`, `"HC3"`, `"HC4"`, see Details,
#' @param cluster one of `"group"`, `"time"`,
#' @param diagonal a logical value specifying whether to force
#'     non-diagonal elements to zero,
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

#' data("Produc", package="plm")
#' zz <- plm(log(gsp)~log(pcap)+log(pc)+log(emp)+unemp, data=Produc, model="random")
#' summary(zz, vcov = vcovBK)
#' summary(zz, vcov = function(x) vcovBK(x, type="HC1"))
#' 
#' ## standard coefficient significance test
#' library(lmtest)
#' coeftest(zz)
#' ## robust significance test, cluster by group
#' ## (robust vs. serial correlation), default arguments
#' coeftest(zz, vcov.=vcovBK)
#' ## idem with parameters, pass vcov as a function argument
#' coeftest(zz, vcov.=function(x) vcovBK(x, type="HC1"))
#' ## idem, cluster by time period
#' ## (robust vs. cross-sectional correlation)
#' coeftest(zz, vcov.=function(x) vcovBK(x, type="HC1", cluster="time"))
#' ## idem with parameters, pass vcov as a matrix argument
#' coeftest(zz, vcov.=vcovBK(zz, type="HC1"))
#' ## joint restriction test
#' waldtest(zz, update(zz, .~.-log(emp)-unemp), vcov=vcovBK)
#' \dontrun{
#' ## test of hyp.: 2*log(pc)=log(emp)
#' library(car)
#' linearHypothesis(zz, "2*log(pc)=log(emp)", vcov.=vcovBK)
#' }
vcovBK <- function(x, ...) {
    UseMethod("vcovBK")
}

#' @rdname vcovBK
#' @export
vcovBK.plm <- function(x, type = c("HC0", "sss" , "HC1", "HC2", "HC3", "HC4"),
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
  ## The clustering defaults to "group" for consistency with vcovHC;
  ## nevertheless the most likely usage is cluster="time" for robustness vs.
  ## cross-sectional dependence, as in the original Beck and Katz paper (where
  ## it is applied to "pooling" models).
  ##
  ## This version: compliant with plm 1.2-0; lmtest.
  ## Code is identical to vcovHC until mark.
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
    demX <- model.matrix(x, model = model, rhs = 1, cstcovar.rm = "all")
    ## drop any linear dependent columns (corresponding to aliased coefficients)
    ## from model matrix X
    ##  na.rm = TRUE because currently, RE tw unbalanced models set aliased simply to NA
    if (!is.null(x$aliased) && any(x$aliased, na.rm = TRUE)) demX <- demX[ , !x$aliased, drop = FALSE]
    
    ## control: IV or not (two- or one-part formula)
    if(length(formula(x))[2L] > 1L) {
        demZ <- model.matrix(x, model = model, rhs = 2, cstcovar.rm = "all")
        ## substitute (transformed) X with projection of X on Z
        ## any linear dependence in Z (demZ) is appropriately taken care of by lm.fit()
        nms <- colnames(demX)
        demX <- lm.fit(demZ, demX)$fitted.values
        # catches case with only one regressor -> need to convert numeric 
        # returned from lm.fit()fitted.values to matrix:
        if(!is.matrix(demX)) demX <- matrix(demX, dimnames = list(NULL, nms[1L]))
    }

    pdim <- pdim(x)
    nT <- pdim$nT$N
    Ti <- pdim$Tint$Ti
    k <- dim(demX)[[2L]]
    n0 <- pdim$nT$n 
    t0 <- pdim$nT$T
    
  ## extract residuals
    uhat <- x$residuals

  ## robustifying against either serial or xs intragroup dependence:
  ## if 'group' then keep current indexing, if 'time' then swap i<->t
  ## so that residuals get 'clustered' by time period instead of by
  ## group (i.e., the vcov estimator is robust vs. xsectional dependence)

  ## extract indices
    xindex <- unclass(attr(x$model, "index")) # unclass for speed
    groupind <- as.numeric(xindex[[1L]])
    timeind  <- as.numeric(xindex[[2L]])

    ## adjust for 'fd' model (losing first time period) [same code as in vcovG.plm]
    if(model == "fd") {
      ## debug printing:
      #print("before FD adj:")
      #print(paste0("nT = ", nT))
      #print(paste0("Ti = ", paste0(Ti, collapse = ", ")))
      #print(paste0("t0 = ", t0))
      #cat("\n")
      
      groupi <- as.numeric(groupind)
      ## make vector =1 on first obs in each group, 0 elsewhere
      selector <- groupi - c(0, groupi[-length(groupi)])
      selector[1L] <- 1 # the first must always be 1
      ## eliminate first obs in time for each group
      groupind <- groupind[!selector]
      timeind  <- timeind[!selector]
      nT <- nT - n0
      Ti <- Ti - 1
      if(any(drop <- Ti == 0L)) {
        # drop groups in Ti that are now empty (group had 1 observation before first-differencing, hence 0 after)
        # and adjust n0 due to same reason
        Ti <- Ti[!drop]
        n0 <- n0 - sum(drop)
      }
      t0 <- t0 - 1
      
      ## debug printing:
      #print("after FD adj:")
      #print(paste0("nT = ", nT))
      #print(paste0("Ti = ", paste0(Ti, collapse = ", ")))
      #print(paste0("t0 = ", t0))
    }
    
  ## set grouping indexes
    cluster <- match.arg(cluster)
    switch(cluster,
            "group" = {
              n <- n0 # this is needed only for 'pcse'
              t <- t0 # this is needed only for 'pcse'
              relevant.ind <- groupind
              lab <- timeind },
            "time" = {
              n <- t0 # this is needed only for 'pcse'
              t <- n0 # this is needed only for 'pcse'
              relevant.ind <- timeind
              lab <- groupind
            })
    
    relevant.ind.GRP <- collapse::GRP(relevant.ind)
    tind <- collapse::gsplit(seq_along(relevant.ind), relevant.ind.GRP)
    tlab <- collapse::gsplit(lab, relevant.ind.GRP)
    
    diaghat <- switch(type, "HC0" = NULL,
                            "sss" = NULL,
                            "HC1" = NULL,
                            "HC2" = try(dhat(demX), silent = TRUE),
                            "HC3" = try(dhat(demX), silent = TRUE),
                            "HC4" = try(dhat(demX), silent = TRUE))
  
  ## transform residuals by weights
  uhat <- omega(residuals = uhat, diaghat = diaghat,
                g = n, nT = nT, k = k, type = type)

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
    tres <- array(NA_real_, dim = c(t, t, n))

    ## array of n "empirical omega-blocks"
    ## with outer product of t(i) residuals
    ## for each group 1..n
    ## (use subscripting from condition 'label in labels' set',
    ## the rest stays NA if any)
    
    unlabs <- unique(lab) # fetch (all, unique) values of the relevant labels
    seq.len.t <- seq_len(t)
        
    for(i in seq_len(n)) {
      ut <- uhat[tind[[i]]]
      tpos <- seq.len.t[unlabs %in% tlab[[i]]]
      ## put non-diag elements to 0 if diagonal=TRUE
      tres[tpos, tpos, i] <- if(diagonal) diag(diag(tcrossprod(ut))) else tcrossprod(ut)
    }

    ## average over all omega blocks, removing NAs (apply preserving
    ## *two* dimensions, i.e., over the third) to get the unconditional
    ## covariance matrix of errors for a group (viz. time period):
    OmegaT <- rowMeans(tres, dims = 2L, na.rm = TRUE) # == apply(tres, 1:2, mean, na.rm = TRUE) but faster
  ## end of PCSE covariance calculation.

  salame <- array(NA_real_, dim = c(k, k, n))
  for(i in seq_len(n)) {
    groupinds <- tind[[i]]
    grouplabs <- tlab[[i]]
    xi <- demX[groupinds, , drop = FALSE]
    ## for every group, take relevant positions
    tpos <- unlabs %in% grouplabs
    OmegaTi <- OmegaT[tpos, tpos, drop = FALSE]
    salame[ , , i] <- tcrossprod(crossprod(xi, OmegaTi), t(xi))
  }
  ## meat
  salame <- rowSums(salame, dims = 2L) # == apply(salame, 1:2, sum) but faster

  ## bread
  pane <- solve(crossprod(demX))

  ## sandwich
  mycov <- tcrossprod(tcrossprod(pane, salame), t(pane)) # == pane %*% salame %*% pane
  
  # save information about cluster variable in matrix (needed for e.g.,
  # robust F test)
  attr(mycov, which = "cluster") <- cluster
  return(mycov)
}

#######################################################

#####################################
## vcovXX methods for pcce objects ##
#####################################

## pcce is compliant with plm so vcovXX.pcce <- vcovXX.plm
## for any vcov that makes sense computed on the transformed
## data from model.matrix.pcce and pmodel.response.pcce

## TODO: vcovBK.pcce, vcovDC.pcce missing? Or not valid?
##       Have a stopping break for now w/ informative error

#' @export
vcovBK.pcce <- function(x, ...) stop("plm::vcovBK not implemented for 'pcce' models")

#' @export
vcovDC.pcce <- function(x, ...) stop("plm::vcovDC not implemented for 'pcce' models")

#' @rdname vcovG
#' @export
vcovG.pcce <- vcovG.plm

#' @rdname vcovHC.plm
#' @export
vcovHC.pcce <- vcovHC.plm

#' @rdname vcovNW
#' @export
vcovNW.pcce <- vcovNW.plm

#' @rdname vcovSCC
#' @export
vcovSCC.pcce <- vcovSCC.plm

########################################################
## vcovXX methods for pggls objects as stopping break ##
########################################################
vcovXX.pggls.error <- "not implemented as not sensible for 'pggls' models"
#' @export
vcovG.pggls <- function(x, ...) stop(paste0("plm::vcovG ", vcovXX.pggls.error))

#' @export
vcovHC.pggls <- function(x, ...) stop(paste0("plm::vcovHC ", vcovXX.pggls.error))

#' @export
vcovNW.pggls <- function(x, ...) stop(paste0("plm::vcovNW ", vcovXX.pggls.error))

#' @export
vcovSCC.pggls <- function(x, ...) stop(paste0("plm::vcovSCC ", vcovXX.pggls.error))

#' @export
vcovBK.pggls <- function(x, ...) stop(paste0("plm::vcovBK ", vcovXX.pggls.error))

#' @export
vcovDC.pggls <- function(x, ...) stop(paste0("plm::vcovDC ", vcovXX.pggls.error))


####################################
## vcovHC method for pgmm objects ##
####################################

#' @rdname vcovHC.plm
#' @importFrom MASS ginv
#' @export
vcovHC.pgmm <- function(x, ...) {
  model <- describe(x, "model")
  transformation <- describe(x, "transformation")
  A1 <- x$A1
  A2 <- x$A2
  B1 <- x$B1 # needs to be B1 (from one-step model)

  if(transformation == "ld") {
##     yX <- lapply(x$model,function(x) rbind(diff(x),x))
##     residuals <-lapply(x$residuals,function(x) c(diff(x),x))
    yX <- x$model
    residuals <- x$residuals
  }
  else {
    # transformation = "d"
    yX <- x$model
    residuals <- x$residuals
  }
  
  minevA2 <- min(abs(Re(eigen(A2)$values)))
  eps <- 1E-9
  
  SA2 <- if(minevA2 < eps){
    warning("a general inverse is used")
    ginv(A2)
  } else solve(A2)
  
  WX <- Reduce("+", mapply(function(w, y) crossprod(w, y[ , -1L, drop = FALSE]), x$W, yX, SIMPLIFY = FALSE))
  
  # robust vcov for one-step GMM, see Roodman (2009), formula (15)
  vcovr1s <- B1 %*% (t(WX) %*% A1 %*% SA2 %*% A1 %*% WX) %*% B1
  
  if(model == "twosteps") {
    coef1s <- x$coefficients[[1L]]
    res1s <- lapply(yX, function(x) x[ , 1L] - crossprod(t(x[ , -1L, drop = FALSE]), coef1s))
    K <- ncol(yX[[1L]])
    D <- c()
    We <- Reduce("+", mapply(function(x, y) crossprod(x, y), x$W, residuals, SIMPLIFY = FALSE))
    for (k in 2:K) {
      exk <- mapply(function(x, y){
                      z <- crossprod(t(x[ , k, drop = FALSE]), t(y))
                      return(- z - t(z))
                    },
                    yX, res1s, SIMPLIFY = FALSE)
      
      wexkw <- Reduce("+", mapply(function(x, y) 
                                    crossprod(x, crossprod(y, x)),
                                  x$W, exk, SIMPLIFY = FALSE))
      B2 <- x$vcov # is "B2" for a two-step model
      Dk <- -B2 %*% t(WX) %*% A2 %*% wexkw %*% A2 %*% We
      D <- cbind(D, Dk)
    }
    # Windmeijer (2005) small-sample bias correction for twosteps GMM model, 
    # see Windmeijer (2005), p. 33 formula (3.3); Roodman (2019) form. (18)
    vcovr2s <- B2 + crossprod(t(D), B2) + t(crossprod(t(D), B2)) + D %*% vcovr1s %*% t(D)
  }
  if(model == "twosteps") vcovr2s else vcovr1s
}


## dhat: diaghat function for matrices
dhat <- function(x) {
  rowSums(crossprod(t(x), solve(crossprod(x))) * x) # == (old) diag(crossprod(t(x), solve(crossprod(x), t(x)))
}

## omega: weighting function, used in vcvG and vcovBK
## define residuals weighting function omega(res)
## (code taken from meatHC and modified)
## (the weighting is defined "in sqrt" relative to the literature)
omega <- function(residuals, diaghat, g, nT, k, type = c("HC0", "sss", "HC1", "HC2", "HC3", "HC4")) {
  type <- match.arg(type)
  switch(type,
         "HC0" = { residuals },
         "sss" = { residuals * sqrt(g/(g-1) * ((nT-1) / (nT-k))) },
         "HC1" = { residuals * sqrt(length(residuals) / (nT-k))  },
         "HC2" = { residuals / sqrt(1 - diaghat) },
         "HC3" = { residuals /     (1 - diaghat) },
         "HC4" = { residuals / sqrt(1 - diaghat)^pmin(4, length(residuals) *
                                                        diaghat/as.integer(round(sum(diaghat), digits = 0))) }
  )
}