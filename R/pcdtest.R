
############## Pesaran's CD test and Breusch/Pagan LM Test (also scaled) ###############

  ## Pesaran's CD test for cross-sectional dependence in panel data models
  ## (and Breusch and Pagan's LM and scaled LM)
  ## ref. Pesaran, General diagnostic tests..., CESifo WP 1229, 2004

  ## In case K+1>T the group-specific model is not estimable;
  ## as in Greene 11.7.2, formula (11.23) we use the group-specific residuals
  ## of a consistent estimator. This may be pooled OLS, RE, FE. Here the
  ## default is set to FE.

  ## Note that the test can be performed on the results of plm's with
  ## any kind of effects: having "time" effects means checking for
  ## xs-dependence *after* introducing time dummies.

  ## In principle, the test can be performed on the results of *any*
  ## panelmodel object. Some issues remain regarding standardization of
  ## model output: some missing pieces are e.g. the 'model$indexes'
  ## in ggls. ''fd'' models are also not compatible because of indexes
  ## keeping the original timespan, while data lose the first period.

## production version, generic and based on plm

## this version 10:
## substantial optimization for speed, now fast (few seconds) on N=3000
## all methods pass on a pseries to pcdres()

pcdtest <- function(x, ...)
{
    UseMethod("pcdtest")
}

## this formula method here only for adding "rho" and "absrho"
## arguments

pcdtest.formula <- function(x, data, index = NULL, model = NULL, 
                            test = c("cd", "sclm", "lm", "rho", "absrho"),
                            w = NULL, ...) {
    #data <- pdata.frame(data, index = index)
    mymod <- plm(x, data, index = index, model = "pooling", ...)
    if (is.null(model) & min(pdim(mymod)$Tint$Ti) < length(mymod$coefficients) +
        1) {
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
            res.i <- lm.fit(tX, ty)$resid
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
                  test = match.arg(test)))
}


## panelmodel method: just fetch resid (as a pseries)
 
pcdtest.panelmodel <- function(x, test = c("cd", "sclm", "lm", "rho", "absrho"),
                               w = NULL, ...) {
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
                  test = match.arg(test)))
}

pcdtest.pseries <- function(x, test = c("cd", "sclm", "lm", "rho", "absrho"),
                             w = NULL, ...) {
  
    ## calculates local or global CD test on a pseries 'x' just as it
    ## would on model residuals

    ## important difference here: a pseries _can_ have NAs

    # input check
    if (!inherits(x, "pseries")) stop("input 'x' needs to be of class \"pseries\"")
  
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
                  form = paste(deparse(substitute(x))),
                  test = match.arg(test)))
}

pcdres <- function(tres, n, w, form, test) {
  
  # 'form' is a character describing the formula (not a formula object!)
  # and goes into htest_object$data.name

  ## Take model residuals as pseries, and calc. test
  ## (from here on, what's needed for rho_ij is ok)
    
  ## this function is the modulus calculating the test,
  ## to be called from either pcdtest.formula or
  ## pcdtest.panelmodel or pcdtest.pseries

  ## now (since v10) tres is the pseries of model residuals
    
    ## calc matrix of all possible pairwise corr.
    ## coeffs. (200x speedup from using cor())
    wideres <- t(preshape(tres, na.rm=FALSE))
    rho <- cor(wideres, use="pairwise.complete.obs")
    
    ## find length of intersecting pairs
    ## fast method, times down 200x
    data.res <- data.frame(time=attr(tres, "index")[[2]],
                           indiv=attr(tres, "index")[[1]])
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
  ## => exclude from calculation and issue warning as information
  ## in general, length(m.ij) gives the number of shared periods by indiviudals i, j
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
    pCD           <- pchisq(CDstat, df=elem.num, lower.tail=F)
    names(CDstat) <- "chisq"
    parm          <- elem.num
    names(parm)   <- "df"
    testname      <- "Breusch-Pagan LM test"
   },
   sclm = {
    CDstat        <- sqrt(1/(2*elem.num))*sum((t.ij*rho^2-1)[selector.mat])
    pCD           <- 2*pnorm(abs(CDstat), lower.tail=F) # was until rev. 293: pnorm(CDstat, lower.tail=F)
    names(CDstat) <- "z"
    parm          <- NULL
    testname      <- "Scaled LM test"
   },
   cd = {
    CDstat        <- sqrt(1/elem.num)*sum((sqrt(t.ij)*rho)[selector.mat]) # (Pesaran (2004), formula (31))
    pCD           <- 2*pnorm(abs(CDstat), lower.tail=F)
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

preshape <- function(x, na.rm=TRUE, ...) {
    ## reshapes pseries,
    ## e.g. of residuals from a panelmodel,
    ## in wide form
    inames <- names(attr(x, "index"))
    mres <- reshape(cbind(as.vector(x), attr(x, "index")),
                    direction="wide",
                    timevar=inames[2], idvar=inames[1])
    ## drop ind in first column
    mres <- mres[,-1]
    ## reorder columns (may be scrambled depending on first
    ## available obs in unbalanced panels)
    mres <- mres[, order(dimnames(mres)[[2]])]
    ## if requested, drop columns (time periods) with NAs
    if(na.rm) {
        rmc <- which(is.na(apply(mres, 2, sum)))
        if(sum(rmc)>0) mres <- mres[,-rmc]
    }
    return(mres)
}

