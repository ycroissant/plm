
############## Pesaran's CD test and Breusch/Pagan LM Test (also scaled) ###############

  ## Pesaran's CD test for cross-sectional dependence in panel data models
  ## (and Breusch and Pagan's LM and scaled LM)
  ## ref. Pesaran, General diagnostic tests..., CESifo WP 1229, 2004

  ## In case K+1>T the group-specific model is not estimable;
  ## as in Greene 11.7.2, formula (11.23) we use the group-specific residuals
  ## of a consistent estimator. This may be pooled OLS, RE, FE. Here the
  ## default is set to FE.

  ## Note that the test can be performed on the results of plm objects with
  ## any kind of effects: having "time" effects means checking for
  ## xs-dependence *after* introducing time dummies.

  ## In principle, the test can be performed on the results of *any*
  ## panelmodel object. Some issues remain regarding standardization of
  ## model output: some missing pieces are, e.g., the 'model$indexes'
  ## in ggls. ''fd'' models are also not compatible because of indexes
  ## keeping the original timespan, while data lose the first period.

## production version, generic and based on plm

## version 11: added test = "bcsclm"
##
## version 10:
## substantial optimization for speed, now fast (few seconds) on N=3000
## all methods pass on a pseries to pcdres()

## make toy example
#dati <- data.frame(ind=rep(1:7, 4), time=rep(1:4, each=7), x=rnorm(28),
#                   group=rep(c(1,1,2,2,2,3,3), 4))
#pdati <- pdata.frame(dati)

#' Tests of cross-section dependence for panel models
#' 
#' Pesaran's CD or Breusch--Pagan's LM (local or global) tests for cross
#' sectional dependence in panel models
#' 
#' These tests are originally meant to use the residuals of separate
#' estimation of one time--series regression for each cross-sectional
#' unit in order to check for cross--sectional dependence. If a
#' different model specification (`within`, `random`, \ldots{}) is
#' assumed consistent, one can resort to its residuals for testing
#' (which is common, e.g., when the time dimension's length is
#' insufficient for estimating the heterogeneous model).  If the time
#' dimension is insufficient and `model=NULL`, the function defaults
#' to estimation of a `within` model and issues a warning. The main
#' argument of this function may be either a model of class
#' `panelmodel` or a `formula` and `dataframe`; in the second case,
#' unless `model` is set to `NULL`, all usual parameters relative to
#' the estimation of a `plm` model may be passed on. The test is
#' compatible with any consistent `panelmodel` for the data at hand,
#' with any specification of `effect`. E.g., specifying
#' `effect="time"` or `effect="twoways"` allows to test for residual
#' cross-sectional dependence after the introduction of time fixed
#' effects to account for common shocks.
#' 
#' A **local** version of either test can be computed by supplying a
#' proximity matrix (elements coercible to `logical`) with argument
#' `w` which provides information on whether any pair of individuals
#' are neighbours or not. If `w` is supplied, only neighbouring pairs
#' will be used in computing the test; else, `w` will default to
#' `NULL` and all observations will be used. The matrix need not be
#' binary, so commonly used "row--standardized" matrices can be
#' employed as well. `nb` objects from \CRANpkg{spdep} must instead be
#' transformed into matrices by \CRANpkg{spdep}'s function `nb2mat`
#' before using.
#' 
#' The methods implemented are suitable also for unbalanced panels.
#' 
#' Pesaran's CD test (`test="cd"`), Breusch and Pagan's LM test
#' (`test="lm"`), and its scaled version (`test="sclm"`) are all
#' described in \insertCite{PESA:04;textual}{plm} (and complemented by
#' Pesaran (2005)). The bias-corrected scaled test (`test="bcsclm"`)
#' is due to \insertCite{BALT:FENG:KAO:12}{plm} and only valid for
#' within models including the individual effect (it's unbalanced
#' version uses max(Tij) for T) in the bias-correction term).
#' \insertCite{BREU:PAGA:80;textual}{plm} is the original source for
#' the LM test.
#' 
#' The test on a `pseries` is the same as a test on a pooled
#' regression model of that variable on a constant, i.e.,
#' `pcdtest(some_pseries)` is equivalent to `pcdtest(plm(some_var ~ 1,
#' data = some_pdata.frame, model = "pooling")` and also equivalent to
#' `pcdtest(some_var ~ 1, data = some_data)`, where `some_var` is
#' the variable name in the data which corresponds to `some_pseries`.
#' 
#' @aliases pcdtest
#' @param x an object of class `formula`, `panelmodel`, or `pseries`
#'     (depending on the respective interface) describing the model to
#'     be tested,
#' @param data a `data.frame`,
#' @param index an optional numerical index, if `NULL`, the first two
#'     columns of the data.frame provided in argument `data` are
#'     assumed to be the index variables; for further details see
#'     [pdata.frame()],
#' @param model an optional character string indicating which type of
#'     model to estimate; if left to `NULL`, the original
#'     heterogeneous specification of Pesaran is used,
#' @param test the type of test statistic to be returned. One of
#'     \itemize{ \item `"cd"` for Pesaran's CD statistic, \item `"lm"`
#'     for Breusch and Pagan's original LM statistic, \item `"sclm"`
#'     for the scaled version of Breusch and Pagan's LM statistic,
#'     \item `"bcsclm"` for the bias-corrected scaled version of
#'     Breusch and Pagan's LM statistic, \item `"rho"` for the average
#'     correlation coefficient, \item `"absrho"` for the average
#'     absolute correlation coefficient,}
#' @param w either `NULL` (default) for the global tests or -- for the
#'     local versions of the statistics -- a `n x n` `matrix`
#'     describing proximity between individuals, with \eqn{w_ij = a}
#'     where \eqn{a} is any number such that `as.logical(a)==TRUE`, if
#'     \eqn{i,j} are neighbours, \eqn{0} or any number \eqn{b} such
#'     that `as.logical(b)==FALSE` elsewhere. Only the lower
#'     triangular part (without diagonal) of `w` after coercing by
#'     `as.logical()` is evaluated for neighbouring information (but
#'     `w` can be symmetric). See also **Details** and
#'     **Examples**,
#' @param \dots further arguments to be passed on to `plm`, such as
#'     `effect` or `random.method`.
#' @return An object of class `"htest"`.
#' @export
#' @references
#'
#' \insertRef{BALT:FENG:KAO:12}{plm}
#' 
#' \insertRef{BREU:PAGA:80}{plm}
#' 
#' \insertRef{PESA:04}{plm}
#'
#' \insertRef{PESA:15}{plm}
#' 
#' @keywords htest
#' @examples
#' 
#' data("Grunfeld", package = "plm")
#' ## test on heterogeneous model (separate time series regressions)
#' pcdtest(inv ~ value + capital, data = Grunfeld,
#'         index = c("firm", "year"))
#' 
#' ## test on two-way fixed effects homogeneous model
#' pcdtest(inv ~ value + capital, data = Grunfeld, model = "within",
#'         effect = "twoways", index = c("firm", "year"))
#' 
#' ## test on panelmodel object
#' g <- plm(inv ~ value + capital, data = Grunfeld, index = c("firm", "year"))
#' pcdtest(g)
#' 
#' ## scaled LM test
#' pcdtest(g, test = "sclm")
#' 
#' ## test on pseries
#' pGrunfeld <- pdata.frame(Grunfeld)
#' pcdtest(pGrunfeld$value)
#' 
#' ## local test
#' ## define neighbours for individual 2: 1, 3, 4, 5 in lower triangular matrix
#' w <- matrix(0, ncol= 10, nrow=10)
#' w[2,1] <- w[3,2] <- w[4,2] <- w[5,2] <- 1
#' pcdtest(g, w = w)
#' 
pcdtest <- function(x, ...)
{
    UseMethod("pcdtest")
}

## this formula method here only for adding "rho" and "absrho"
## arguments

#' @rdname pcdtest
#' @export
pcdtest.formula <- function(x, data, index = NULL, model = NULL, 
                            test = c("cd", "sclm", "bcsclm", "lm", "rho", "absrho"),
                            w = NULL, ...) {
    #data <- pdata.frame(data, index = index)
    test <- match.arg(test)
    if (is.null(model) && test == "bcsclm") stop("for test = 'bcsclm', set argument model = 'within'")
    mymod <- if (test != "bcsclm") plm(x, data = data, index = index, model = "pooling", ...)
              else plm(x, data = data, index = index, model = "within", ...)
    if(is.null(model) && min(pdim(mymod)$Tint$Ti) < length(mymod$coefficients)+1) 
      {
        warning("Insufficient number of observations in time to estimate heterogeneous model: using within residuals",
            call. = FALSE)
        model <- "within"
    }
    
    ind0 <- attr(model.frame(mymod), "index")
    tind <- as.numeric(ind0[[2]])
    ind <- as.numeric(ind0[[1]])
    if (is.null(model)) {
        ## estimate individual regressions one by one
        X <- model.matrix(mymod)
        y <- model.response(model.frame(mymod))
        unind <- unique(ind)
        n <- length(unind)
        ti.res <- vector("list", n)
        ind.res <- vector("list", n)
        tind.res <- vector("list", n)
        for (i in 1:n) {
            tX <- X[ind == unind[i], , drop = FALSE]
            ty <- y[ind == unind[i]]
            res.i <- lm.fit(tX, ty)$residuals
            ti.res[[i]] <- res.i
            names(ti.res[[i]]) <- tind[ind == unind[i]]
            ind.res[[i]] <- rep(i, length(res.i))
            tind.res[[i]] <- tind[ind == unind[i]]
        }
        ## make pseries of (all) residuals
        resdata <- data.frame(ee = unlist(ti.res),
                              ind = unlist(ind.res),
                              tind = unlist(tind.res))
        pee <- pdata.frame(resdata, index = c("ind", "tind"))
        tres <- pee$ee
    }
    else {
        mymod <- plm(x, data, index = index, model = model, ...)
        tres <- resid(mymod)
        unind <- unique(ind)
        n <- length(unind)
        t <- min(pdim(mymod)$Tint$Ti)
        nT <- length(ind)
        k <- length(mymod$coefficients)
        }

    return(pcdres(tres = tres, n = n, w = w,
                  form = paste(deparse(x)),
                  test = test))
}


## panelmodel method: just fetch resid (as a pseries) and hand over to pcdres
 
#' @rdname pcdtest
#' @export
pcdtest.panelmodel <- function(x, test = c("cd", "sclm", "bcsclm", "lm", "rho", "absrho"),
                               w = NULL, ...) {
    
    test <- match.arg(test)
    model <- describe(x, "model")
    effect <- describe(x, "effect")
    eff <- (effect == "individual" || effect == "twoways")
    if (test == "bcsclm")
      if (model != "within" || !eff) stop("for test = 'bcsclm', model x must be a within individual or twoways model")
  
    tres <- resid(x)
    index <- attr(model.frame(x), "index")
    #tind <- as.numeric(index[[2]])
    ind <- as.numeric(index[[1]])
    unind <- unique(ind)
    n <- length(unind)
    #t <- pdim(x)$Tint$Ti
    #nT <- length(ind)
    #k <- length(x$coefficients)
    return(pcdres(tres = tres, n = n, w = w,
                  form = paste(deparse(x$formula)),
                  test = test))
}

#' @rdname pcdtest
#' @export
pcdtest.pseries <- function(x, test = c("cd", "sclm", "bcsclm", "lm", "rho", "absrho"),
                             w = NULL, ...) {
  
    ## calculates local or global CD test on a pseries 'x' just as it
    ## would on model residuals
    ## important difference here: a pseries _can_ have NAs
  
    # input check
    if (!inherits(x, "pseries")) stop("input 'x' needs to be of class \"pseries\"")
    form <- paste(deparse(substitute(x)))
  
    pos.na <- is.na(x)
    if (any(pos.na)) {
      x <- subset_pseries(x, !pos.na)
      warning("NA values encountered in input and removed")
      if (length(x) == 0L) stop("input is empty after removal of NA values")
    }
  
    ## get indices
    tind <- as.numeric(attr(x, "index")[[2]])
    ind <- as.numeric(attr(x, "index")[[1]])

    ## det. number of groups and df
    unind <- unique(ind)
    n <- length(unind)

    tres <- x
    
    ## "pre-allocate" an empty list of length n
    #tres <- vector("list", n)

    ## use model residuals, group by group
    ## list of n:
    ## t_i residuals for each x-sect. 1..n
    #for(i in 1:n) {
    #          # remove NAs
    #          xnonna <- !is.na(x[ind==unind[i]])
    #          tres[[i]] <- x[ind==unind[i]][xnonna]
    #          ## name resids after the time index
    #          names(tres[[i]]) <- tind[ind==unind[i]][xnonna]
    #          }

    return(pcdres(tres = tres, n = n, w = w,
                  form = form,
                  test = match.arg(test)))
}

pcdres <- function(tres, n, w, form, test) {
  # 'form' is a character describing the formula (not a formula object!)
  # and goes into htest_object$data.name

  ## Take model residuals as pseries, and calc. test
  ## (from here on, what's needed for rho_ij is ok)
    
  ## this function is the modulus calculating the test,
  ## to be called from pcdtest.formula,
  ## pcdtest.panelmodel or pcdtest.pseries

  ## now (since v10) tres is the pseries of model residuals
    
    ## calc matrix of all possible pairwise corr.
    ## coeffs. (200x speedup from using cor())
    wideres <- t(preshape(tres, na.rm = FALSE))
    rho <- cor(wideres, use = "pairwise.complete.obs")
    
    ## find length of intersecting pairs
    ## fast method, times down 200x
    data.res <- data.frame(time = attr(tres, "index")[[2]],
                           indiv = attr(tres, "index")[[1]])
    ## tabulate which obs in time for each ind are !na
    presence.tab <- table(data.res)
    ## calculate t.ij
    t.ij <- crossprod(presence.tab)
    
  # input check
  if (!is.null(w)) {
    dims.w <- dim(w)
    if(dims.w[1] != n || dims.w[2] != n)
      stop(paste0("matrix 'w' describing proximity of individuals has wrong dimensions: ",
           "should be ", n, " x ", n, " (no. of individuals) but is ", dims.w[1], " x ", dims.w[2]))
  }
  

  ## begin features for local test ####################
  ## higher orders omitted for now, use wlag() explicitly

  ## if global test, set all elements in w to 1
  if(is.null(w)) {
    w <- matrix(1, ncol = n, nrow = n)
    dep <- ""
  } else { dep <- "local" }

  ## make (binary) selector matrix based on the contiguity matrix w
  ## and extracting elements corresponding to ones in the lower triangle
  ## excluding the diagonal

  ## transform in logicals (0=FALSE, else=TRUE: no need to worry
  ## about row-std. matrices)
  selector.mat <- matrix(as.logical(w), ncol = n)
  
  ## some santiy checks for 'w' (not perfect sanity, but helps)
  if (sum(selector.mat[lower.tri(selector.mat, diag = FALSE)]) == 0) {
    stop(paste0("no neighbouring individuals defined in proximity matrix 'w'; ",
                "only lower triangular part of 'w' (w/o diagonal) is evaluated"))
  } else {
    if (sum(selector.mat[upper.tri(selector.mat, diag = FALSE)]) != 0) {
      if (!isSymmetric((unname(selector.mat)))) { # unname needed to ignore rownames and colnames
        stop(paste0("proximity matrix 'w' is ambiguous: upper and lower triangular part ",
                    "define different neighbours (it is sufficient to provide information ",
                    "about neighbours only in the lower triangluar part of 'w'"))
      }
    }
  }
  
  ## if no intersection or only 1 shared period of e_it and e_jt
  ## => exclude from calculation and issue a warning.
  ## In general, length(m.ij) gives the number of shared periods by indiviudals i, j
  ## Thus, non intersecting pairs are indicated by length(m.ij) == 0 (t.ij[i,j] == 0)
  no.one.intersect <- (t.ij <= 1)
  if (any(no.one.intersect, na.rm = TRUE)) {
    # t.ij is a lower triangular matrix: do not divide by 2 to get the number of non-intersecting pairs!
    number.of.non.one.intersecting.pairs <- sum(no.one.intersect, na.rm = TRUE)
    number.of.total.pairs <- (n*(n-1))/2
    share.on.one.intersect.pairs <- number.of.non.one.intersecting.pairs / number.of.total.pairs * 100
    warning(paste("Some pairs of individuals (",
                  signif(share.on.one.intersect.pairs, digits = 2),
                  " percent) do not have any or just one time period in common and have been omitted from calculation", sep=""))
    selector.mat[no.one.intersect] <- FALSE
  }

  ## set upper tri and diagonal to FALSE
  selector.mat[upper.tri(selector.mat, diag = TRUE)] <- FALSE

  ## number of elements in selector.mat
  ## elem.num = 2*(N*(N-1)) in Pesaran (2004), formulae (6), (7), (31), ...
  elem.num <- sum(selector.mat)

  ## end features for local test ######################

  ## Breusch-Pagan or Pesaran statistic for cross-sectional dependence,
  ## robust vs. unbalanced panels:

  switch(test,
   lm = {
    CDstat        <- sum((t.ij*rho^2)[selector.mat])
    pCD           <- pchisq(CDstat, df = elem.num, lower.tail = FALSE)
    names(CDstat) <- "chisq"
    parm          <- elem.num
    names(parm)   <- "df"
    testname      <- "Breusch-Pagan LM test"
   },
   sclm = {
    CDstat        <- sqrt(1/(2*elem.num))*sum((t.ij*rho^2-1)[selector.mat])
    pCD           <- 2*pnorm(abs(CDstat), lower.tail = FALSE)
    names(CDstat) <- "z"
    parm          <- NULL
    testname      <- "Scaled LM test"
   },
   bcsclm = {
      CDstat        <- sqrt(1/(2*elem.num))*sum((t.ij*rho^2-1)[selector.mat]) - (n/(2*(max(t.ij)-1))) # Baltagi/Feng/Kao (2012), formula (11) (unbalanced case as sclm + in bias correction as EViews: max(T_ij) instead of T)
      pCD           <- 2*pnorm(abs(CDstat), lower.tail = FALSE)
      names(CDstat) <- "z"
      parm          <- NULL
      testname      <- "Bias-corrected Scaled LM test"
   },
   cd = {
    CDstat        <- sqrt(1/elem.num)*sum((sqrt(t.ij)*rho)[selector.mat]) # (Pesaran (2004), formula (31))
    pCD           <- 2*pnorm(abs(CDstat), lower.tail = FALSE)
    names(CDstat) <- "z"
    parm          <- NULL
    testname      <- "Pesaran CD test"
   },
   rho = {
    CDstat        <- sum(rho[selector.mat])/elem.num
    pCD           <- NULL
    names(CDstat) <- "rho"
    parm          <- NULL
    testname      <- "Average correlation coefficient"
   },
   absrho = {
    CDstat        <- sum(abs(rho)[selector.mat])/elem.num
    pCD           <- NULL
    names(CDstat) <- "|rho|"
    parm          <- NULL
    testname      <- "Average absolute correlation coefficient"
   })

  ##(insert usual htest features)
  RVAL <- list(statistic = CDstat,
               parameter = parm,
               method    = paste(testname, "for", dep,
                            "cross-sectional dependence in panels"),
               alternative = "cross-sectional dependence",
               p.value     = pCD,
               data.name   = form)
  class(RVAL) <- "htest"
  return(RVAL)
}

preshape <- function(x, na.rm = TRUE, ...) {
    ## reshapes pseries,
    ## e.g., of residuals from a panelmodel,
    ## in wide form
    inames <- names(attr(x, "index"))
    mres <- reshape(cbind(as.vector(x), attr(x, "index")),
                    direction = "wide",
                    timevar = inames[2],
                    idvar = inames[1])
    ## drop ind in first column
    mres <- mres[ , -1]
    ## reorder columns (may be scrambled depending on first
    ## available obs in unbalanced panels)
    mres <- mres[ , order(dimnames(mres)[[2]])]
    ## if requested, drop columns (time periods) with NAs
    if(na.rm) {
        na.cols <- vapply(mres, FUN = anyNA, FUN.VALUE = TRUE)
        if(sum(na.cols) > 0) mres <- mres[, !na.cols]
    }
    return(mres)
}




#' Cross--sectional correlation matrix
#' 
#' Computes the cross--sectional correlation matrix
#' 
#' 
#' @param x an object of class `pseries`
#' @param grouping grouping variable,
#' @param groupnames a character vector of group names,
#' @param value to complete
#' @param \dots further arguments
#' @return A matrix
#' @export
#' @keywords htest
cortab <- function(x, grouping, groupnames = NULL,
                   value = "statistic", ...) {
    ## makes table of within and between correlation
    ## needs a pseries and a groupings vector of **same length**

    ## would use a better naming, and also passing a char or factor as
    ## grouping index

    ## x must be a pseries
    if(!inherits(x, "pseries")) stop("First argument must be a pseries")
    if(length(x) != length(grouping)) stop("Incompatible lengths")

    fullind <- as.numeric(attr(x, "index")[,1])
    ids <- unique(fullind)
    n <- length(ids)
    regs <- 1:length(unique(grouping))

    if(!(is.numeric(grouping))) grouping <- as.numeric(as.factor(grouping))
    
    idnames <- as.character(ids)
    if(is.null(groupnames)) {
        groupnames <- as.character(unique(grouping))
    }

    ## make matrices of between-regions correlations
    ## (includes within correlation on diagonal)
    ## for each pair of regions (nb: no duplicates, e.g., 3.1 but not 1.3)

    ## make w<1.n>:
    for(h in 1:length(regs)) {
      for(k in 1:h) {
        statew <- matrix(0, ncol=n, nrow=n)
        ## make statew for cor. between h and k
        for(i in 1:n) {
          ## get first region (all values equal, so take first one)
          ireg <- grouping[fullind==ids[i]][1]
          if(ireg==h) {
            for(j in 1:n) {
                jreg <- grouping[fullind==ids[j]][1]
                if(jreg==k) statew[i,j] <- 1
            }
          }
        }
        if(h!=k) statew <- statew + t(statew)
        ## just for debugging reasons:
        dimnames(statew) <- list(idnames, idnames)
        ## eliminate self.correlation of states if i=j
        diag(statew) <- 0
        ## not needed: pcdtest seems to do this by construction
        eval(parse(text=paste("w", h, ".", k, " <- statew", sep="")))
      }
     }

     ## notice: without the line
     ## '' if(i!=j) statew <- statew + t(statew) ''
     ## all wn.n matrices would have values only on one half (upper
     ## or lower triangle)

     ## make generic table of regions' within and between correlation
     ## argument: a pseries
    #YC regnames is undefined, so is myw
    tab.g <- function(x, regs, regnames, test="rho", value) {
        myw <- 0
         tabg <- matrix(NA, ncol=length(regs), nrow=length(regs))
         for(i in 1:length(regs)) {
             for(j in 1:i) {
                 ## take appropriate w matrix
                 eval(parse(text=paste("myw<-w", i, ".", j, sep="")))
                 tabg[i,j] <- pcdtest(x, test="rho", w=myw)[[value]]
             }
         }
         dimnames(tabg) <- list(groupnames, groupnames)
         return(tabg)
    }
    regnames <- ""
    mytab <- tab.g(x, regs=regs, regnames=regnames, test="rho", value=value)
    return(mytab)
}




