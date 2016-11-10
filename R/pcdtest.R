
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

## this version 9:
## allows for calc. average rho and average absolute rho
## fixed: 2016-11-10: 'data.name' in htest object now correct.

## only adds pseries method and modifies pcdres(), the rest from
## "production" version (notice diff to pcdtest8() here)

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
    index <- attr(model.frame(mymod), "index")
    tind <- as.numeric(index[[2]])
    ind <- as.numeric(index[[1]])
    if (is.null(model)) {
        X <- model.matrix(mymod)
        y <- model.response(model.frame(mymod))
        unind <- unique(ind)
        n <- length(unind)
        tres <- vector("list", n)
        for (i in 1:n) {
            tX <- X[ind == unind[i], , drop = FALSE]
            ty <- y[ind == unind[i]]
            tres[[i]] <- lm.fit(tX, ty)$resid
            names(tres[[i]]) <- tind[ind == unind[i]]
        }
    }
    else {
        mymod <- plm(x, data, model = model, ...)
        myres <- mymod$residuals
        unind <- unique(ind)
        n <- length(unind)
        t <- min(pdim(mymod)$Tint$Ti)
        nT <- length(ind)
        k <- length(mymod$coefficients)
        tres <- vector("list", n)
        for (i in 1:n) {
            tres[[i]] <- myres[ind == unind[i]]
            names(tres[[i]]) <- tind[ind == unind[i]]
        }
    }
    return(pcdres(tres = tres, n = n, w = w,
                  form = paste(deparse(x)),
                  test = match.arg(test)))
}


## this panelmodel method here only for adding "rho" and
## "absrho" arguments
 
pcdtest.panelmodel <- function(x, test = c("cd", "sclm", "lm", "rho", "absrho"),
                               w = NULL, ...) {
    myres <- resid(x)
    index <- attr(model.frame(x), "index")
    tind <- as.numeric(index[[2]])
    ind <- as.numeric(index[[1]])
    unind <- unique(ind)
    n <- length(unind)
    t <- pdim(x)$Tint$Ti
    nT <- length(ind)
    k <- length(x$coefficients)
    tres <- vector("list", n)
    for (i in 1:n) {
        tres[[i]] <- myres[ind == unind[i]]
        names(tres[[i]]) <- tind[ind == unind[i]]
    }
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

    ## "pre-allocate" an empty list of length n
    tres <- vector("list", n)

    ## use model residuals, group by group
    ## list of n:
    ## t_i residuals for each x-sect. 1..n
    for(i in 1:n) {
              # remove NAs
              xnonna <- !is.na(x[ind==unind[i]])
              tres[[i]] <- x[ind==unind[i]][xnonna]
              ## name resids after the time index
              names(tres[[i]]) <- tind[ind==unind[i]][xnonna]
              }

    return(pcdres(tres = tres, n = n, w = w,
                  form = paste(deparse(substitute(x))),
                  test = match.arg(test)))
}

pcdres <- function(tres, n, w, form, test) {
  
  # 'form' is a character describing the formula (not a formula object!)
  # and goes into htest_object$data.name

  ## Take list of model residuals, group by group, and calc. test
  ## (from here on, what's needed for rho_ij is ok)
  ## this function is the modulus calculating the test,
  ## to be called from either pcdtest.formula or
  ## pcdtest.panelmodel or pcdtest.pseries

  ## rho_ij matrix
  rho <- matrix(NA,ncol=n,nrow=n)
  ## T_ij matrix
  t.ij <- matrix(NA,ncol=n,nrow=n)

  ## calc. only for lower tri, rest is nullified later anyway
  for(i in 2:n) {
    for(j in 1:(i-1)) {

      ## Pesaran (2004), p. 18: for unbalanced data sets:
      ## "compute the pair-wise correlations of eit
      ##  and ejt using the common set of data points"
      ##
      ## determination of joint range m_i | m_j
      ## m_ij=m_i|m_j, working on names of the residuals' vectors
      m.ij <- intersect(names(tres[[i]]), names(tres[[j]]))

      ## for this ij do me_i=mean_t(e_it[m_ij]), idem j
      ## and rho and T_ij as in Pesaran, page 18
      ## (as mean(ei)=0 doesn't necessarily hold any more)

      ei <- tres[[i]][m.ij]
      ej <- tres[[j]][m.ij]
      dei <- ei - mean(ei)
      dej <- ej - mean(ej)
      rho[i,j] <- ( dei%*%dej )/( sqrt(dei%*%dei) * sqrt(dej%*%dej) )

      ## put this here inside summations, as for unbalanced panels
      ## "common obs. numerosity" T_ij may vary on i,j
      t.ij[i,j] <- length(m.ij)

      }
    }

  ## begin features for local test ####################
  ## higher orders omitted for now, use wlag() explicitly

  ## if global test, set all elements in w to 1
  if(is.null(w)) {
    w <- matrix(1,ncol=n,nrow=n)
    dep <- ""
  } else { dep <- "local" }

  ## make (binary) selector matrix based on the contiguity matrix w
  ## and extracting elements corresponding to ones in the lower triangle
  ## excluding the diagonal

  ## transform in logicals (0=FALSE, else=TRUE: no need to worry
  ## about row-std. matrices)
  selector.mat <- matrix(as.logical(w), ncol=n)
  
  ## if no intersection of e_it and e_jt => exclude from calculation and issue warning as information
  ## non intersecting pairs are indicated by length(m.ij) == 0 and, hence, t.ij[i,j] == 0 
  non.intersecting <- t.ij == 0
  if (any(non.intersecting, na.rm = TRUE)) {
    # t.ij is a lower triangular matrix: do not divide by 2 to get the number of non-intersecting pairs!
    number.of.non.intersecting.pairs <- sum(non.intersecting, na.rm = TRUE)
    number.of.total.pairs <- (n*(n-1))/2
    share.non.intersecting.pairs <- number.of.non.intersecting.pairs / number.of.total.pairs * 100
    warning(paste("Some pairs of individuals (",
                  signif(share.non.intersecting.pairs, digits = 2),
                  " percent) do not have any time period in common and have been omitted from calculation", sep=""))
    selector.mat[non.intersecting] <- FALSE
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

