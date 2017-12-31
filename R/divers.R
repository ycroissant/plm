bdiag <- function(...){
  if (nargs() == 1)
    x <- as.list(...)
  else
    x <- list(...)
  n <- length(x)
  if(n==0) return(NULL)
  x <- lapply(x, function(y) if(length(y)) as.matrix(y) else
              stop("Zero-length component in x"))
  d <- array(unlist(lapply(x, dim)), c(2, n))
  rr <- d[1,]
  cc <- d[2,]
  rsum <- sum(rr)
  csum <- sum(cc)
  out <- array(0, c(rsum, csum))
  ind <- array(0, c(4, n))
  rcum <- cumsum(rr)
  ccum <- cumsum(cc)
  ind[1,-1] <- rcum[-n]
  ind[2,] <- rcum
  ind[3,-1] <- ccum[-n]
  ind[4,] <- ccum
  imat <- array(1:(rsum * csum), c(rsum, csum))
  iuse <- apply(ind, 2, function(y, imat) imat[(y[1]+1):y[2],
                                               (y[3]+1):y[4]], imat=imat)
  iuse <- as.vector(unlist(iuse))
  out[iuse] <- unlist(x)
  return(out)
} 


twosls <- function(y, X, W, intercept = FALSE){
  Xhat <- lm(X ~ W)$fitted.values
  if(!is.matrix(Xhat)){
    Xhat <- matrix(Xhat, ncol = 1)
    colnames(Xhat) <- colnames(X)
  }
  if(intercept){
    model <- lm(y ~ Xhat)
    yhat <- as.vector(crossprod(t(cbind(1, X)), coef(model)))
  }
  else{
    model <- lm(y ~ Xhat - 1)
    yhat <- as.vector(crossprod(t(X), coef(model)))
  }
  model$residuals <- y - yhat
  model
}


sumres <- function(x){
  sr <- summary(unclass(resid(x)))
  srm <- sr["Mean"]
  if (abs(srm)<1e-10){
    sr <- sr[c(1:3,5:6)]
  }
  sr
}

sumsq <- function(x, ...){
    xb <- mean(x, na.rm = TRUE)
    sum((na.omit(x)-xb)^2)
}



# suml(x) is replaced by Reduce("+", x)
## suml <- function(x){
##   n <- length(x)
##   if (!is.null(dim(x[[1]]))){
##     d <- dim(x[[1]])
##     s <- matrix(0,d[1],d[2])
##     for (i in 1:n){
##       s <- s+x[[i]]
##     }
##   }
##   else{
##     s <- rep(0,length(x[[n]]))
##     for (i in 1:n){
##       s <- s+x[[i]]
##     }
##   }
##   s
## }

oppl <- function(x,y,func){
  n <- length(x)
  z <- list()
  if (!is.list(y)){
    for (i in 1:n){
      t <- paste("\"",func,"\"","(x[[i]],y)",sep="")
      z[[i]] <- eval(parse(text=t))
    }
  }
  else{
    for (i in 1:n){
      t <- paste("\"",func,"\"","(x[[i]],y[[i]])",sep="")
      z[[i]] <- eval(parse(text=t))
    }
  }
  z
}

rbindl <- function(x){
  n <- length(x)
  d <- dim(x[[1]])
  s <- c()
  for (i in 1:n){
    s <- rbind(s,x[[i]])
  }
}

expand.formula <- function(x){
  oclass <- class(x)
  if (! any(class(x) == "Formula")) stop("not a Formula object")
  if (length(x)[2] != 2) stop("not a two part formula")
  xs <- structure(x, class = "formula")
  has.response <- attr(terms(xs),"response") == 1
  if (has.response){
    y <- x[[2]]
    rhs <- x[[3]]
  }
  else{
    y <- NULL
    rhs <- x[[2]]
  }
  firstpart <- rhs[[2]]
  secondpart <- rhs[[3]]
  if (has.response){
    one <- do.call("~", list(y,firstpart))
    two <- do.call("~", list(y,secondpart))
  }
  else{
    one <- do.call("~", list(firstpart))
    two <- do.call("~", list(secondpart))
  }
  two <- update(one, two)
  one <- paste(deparse(one), collapse = "")
  two <- paste(deparse(two[[3]]), collapse = "")
  result <- as.formula(paste(one, "|", two, collapse = ""));
  result <- as.Formula(result)
  #YC  class(result) <- c("pFormula", class(result))
  structure(result, class = oclass)
}


lev2var <- function(x, ...){
  # takes a data.frame and returns a vector of variable names, the
  # names of the vector being the names of the effect
  
  is.fact <- sapply(x, is.factor)
  if (sum(is.fact) > 0){
    not.fact <- names(x)[!is.fact]
    names(not.fact) <- not.fact
    x <- x[is.fact]
    wl <- lapply(x,levels)
    # nl is the number of levels for each factor
    nl <- sapply(wl,length)
    # nf is a vector of length equal to the total number of levels
    # containing the name of the factor
    nf <- rep(names(nl),nl)
    result <- unlist(wl)
    names(result) <- nf
    result <- paste(names(result),result,sep="")
    names(nf) <- result
    c(nf, not.fact)
  }
  else{
    z <- names(x)
    names(z) <- z
    z
  }
}

##### has.intercept methods #####
has.intercept <- function(object, ...) {
  UseMethod("has.intercept")
}

has.intercept.default <- function(object, ...) {
  has.intercept(formula(object), ...)
}

has.intercept.formula <- function(object, ...) {
  attr(terms(object), "intercept") == 1L
}

has.intercept.Formula <- function(object, rhs = NULL, ...) {
  ## NOTE: returns a logical vector of the necessary length
  ## (which might be > 1)
  if(is.null(rhs)) rhs <- 1:length(attr(object, "rhs"))
  sapply(rhs, function(x) has.intercept(formula(object, lhs = 0, rhs = x)))
}

has.intercept.panelmodel <- function(object, ...){
  object <- attr(model.frame(object),"formula")
  has.intercept(object)
}

has.intercept.plm <- function(object, part = "first", ...){
  has.intercept(formula(object), part = part)
}



pres <- function(x) {  # pres.panelmodel
  ## extracts model residuals as pseries

  ## extract indices
  groupind <-attr(x$model, "index")[,1]
  timeind  <-attr(x$model, "index")[,2]
  
  # fix to allow operation with pggls, pmg (NB: pmg? meant: plm?)
  # [TODO: one day, make this cleaner; with the describe framework?]
  if (!is.null(x$args$model))                 maybe_fd <- x$args$model
  if (!is.null(attr(x, "pmodel")$model.name)) maybe_fd <- attr(x, "pmodel")$model.name # this line is currently needed to detect pggls models
  
  ## Achim's fix: reduce id and time index to accomodate first-differences model's number of observations
  if(exists("maybe_fd") && maybe_fd == "fd") {
    groupi <- as.numeric(groupind)
    ## make vector =1 on first obs in each group, 0 elsewhere
    selector <- groupi - c(0, groupi[-length(groupi)])
    selector[1] <- 1 # the first must always be 1
    ## eliminate first obs in time for each group
    groupind <- groupind[!selector]
    timeind <- timeind[!selector]
  }

  resdata <- data.frame(ee = x$residuals, ind = groupind, tind = timeind)
  pee <- pdata.frame(resdata, index = c("ind", "tind"))
  pres <- pee$ee
  return(pres)
}


# nobs() function to extract total number of observations used for estimating the panelmodel
# like stats::nobs for lm objects
# NB: here, use object$residuals rather than residuals(object)
#     [b/c the latter could do NA padding once NA padding works for plm objects.
#      NA padded residuals would yield wrong result for nobs!]
nobs.panelmodel <- function(object, ...) {
  if (inherits(object, "plm") | inherits(object, "panelmodel")) return(length(object$residuals))
    else stop("Input 'object' needs to be of class 'plm' or 'panelmodel'")
}

# No of obs calculated as in print.summary.pgmm [code copied from there]
nobs.pgmm <- function(object, ...) {
  if (inherits(object, "pgmm")) return(sum(unlist(object$residuals) != 0))
    else stop("Input 'object' needs to be of class 'pgmm', i. e., a GMM estimation with panel data estimated by pgmm()")
}



# punbalancedness: measures for unbalancedness of a pandel data set
# as defined in Ahrens/Pincus (1981), p. 228 (gamma and nu)
# and for nested panel structures as in Baltagi/Song/Jung (2001), pp. 368-369
punbalancedness.default <- function(x, ...) {

  ii <- index(x)
  
  if (ncol(ii) == 2) {
   ## original Ahrens/Pincus (1981)
    pdim <- pdim(x, ...)
    N <- pdim$nT$n # no. of individuals
    Totalobs <- pdim$nT$N # no. of total observations
    Ti <- pdim$Tint$Ti
    Tavg <- sum(Ti)/N
    
    r1 <- N / (Tavg * sum(1/Ti))
    r2 <- 1 / (N * (sum( (Ti/Totalobs)^2)))
    result <- c(gamma = r1, nu = r2)
  } else {
    if (ncol(ii) == 3) {
     ## extension to nested model with additional group variable
     ## Baltagi/Song/Jung (2001), pp. 368-369
      ids <- ii[[1]]
      tss <- ii[[2]]
      gps <- ii[[3]]
      Tis <- unique(data.frame(tss, gps))
      Tis <- table(Tis$gps)               # no of max time periods per group
      Nis <- unique(data.frame(ids, gps))
      Nis <- table(Nis$gps)               # no of individuals per group
      M <- length(unique(gps))            # no of unique groups
      Nbar <- sum(Nis)/M
      Tbar <- sum(Tis)/M
      
      c1 <- M / (Nbar * sum(1/Nis))
      c2 <- M / (Tbar * sum(1/Tis))
      c3 <- M / (sum(Nis * Tis)/M * sum(1/(Nis*Tis)))
      result <- (c(c1 = c1, c2 = c2, c3 = c3))
    } else stop(paste0("unsupported number of dimensions: ", ncol(ii)))
  }
  return(result)
}

punbalancedness.pdata.frame <- function(x, ...) {
  punbalancedness.default(x, ...)
}

punbalancedness.data.frame <- function(x, index = NULL, ...) {
  x <- pdata.frame(x, index = index, ...)
  punbalancedness.default(x, ...)
}

punbalancedness.panelmodel <- function(x, ...) {
  punbalancedness.default(x, ...)
}

punbalancedness <- function(x, ...) {
  UseMethod("punbalancedness")
}


# helper function, used in pdata.frame()
fancy.row.names <- function(index, sep = "-") {
  if (length(index) == 2) {result <- paste(index[[1]], index[[2]], sep = sep)}
  # this in the order also used for sorting (group, id, time):
  if (length(index) == 3) {result <- paste(index[[3]], index[[1]], index[[2]], sep = sep)}
  return(result)
}

# trans_clubSandwich_vcov: helper function for pwaldtest()
# translate vcov object from package clubSandwich so it is suitable for summary.plm, plm's pwaldtest.
# Attribute "cluster" in clubSandwich's vcov objects contains the cluster variable itself.
# plm's vcov object also has attribute "cluster" but it contains a character as
# information about the cluster dimension (either "group" or "time")
#
# inputs:
#   * CSvcov: a vcov as returned by clubSandwich's vcovCR function [class c("vcovCR", "clubSandwich")]
#   * index: the index belonging to a plm object/model
# return value:
#   * modified CSvcov (substituted attribute "cluster" with suitable character or NULL)
trans_clubSandwich_vcov <- function(CSvcov, index) {
  clustervar <- attr(CSvcov, "cluster")
  if (!is.null(clustervar)) {
      if (isTRUE(all.equal(index[[1]], clustervar))) {
        attr(CSvcov, "cluster") <- "group"
        return(CSvcov)
      }
      if (isTRUE(all.equal(index[[2]], clustervar))) {
        attr(CSvcov, "cluster") <- "time"
        return(CSvcov)
      } else {
        attr(CSvcov, "cluster") <- NULL
        return(CSvcov)
      }
  }
  warning("no attribute \"cluster\" found in supplied vcov object")
  return(CSvcov)
}

## gettvalue: helper function to extract one or more t value(s) (coef/s.e.) for a coefficient from model object
## useful if one wants to avoid the computation of a whole lot of values with summary()
gettvalue <- function(x, coefname) {
  # x: model object (usually class plm or lm)
  # coefname: character indicating name(s) of coefficient(s) for which the t value(s) is (are) requested
  # return: named numeric vector of length == length(coefname) with requested t value(s)
  beta <- coef(x)[coefname]
  se <- sqrt(diag(vcov(x))[coefname])
  tvalue <- beta / se
  return(tvalue)
}

# pchibarsq: helper function: "p-function" for mixed chisq (also called chi-bar-squared)
# used in plmtest(., type = "ghm"), see Baltagi (2013), pp. 71-72, 74, 88, 202-203, 209
#
# a reference for the distribution seems to be
# Dykstra, R./El Barmi, H., Chi-Bar-Square Distributions, in: Encyclopedia of Statistical Sciences, 
# DOI: 10.1002/0471667196.ess0265.pub2
pchibarsq <- function(q, df, weights, lower.tail = TRUE, ... ) {
  # NB: other parameters in dots (...): not checked if valid! (ncp, log, ...)
  res <- sum(weights * pchisq(q, df = df, lower.tail = lower.tail, ...))
  return(res)
}

## TODO: is print.form actually used? - delete?
print.form <- function(x, length.line){
  x <- deparse(x,width.cutoff=length.line)
  n <- length(x)
  cat(paste(x[1],"\n",sep=""))
  if (n>1){
    for (i in 2:n){
      cat(paste(x[i],"\n",sep=""))
    }
  }
}

# check_propagation_correct_class: helper function
# Function checks if the class and storage mode (type) of an object match 
# and corrects its class attribute if not
#
# A mismatch can occur if a pseries of lower class and type logical or integer
# are propagated to higher type by an arithmetic operation as R's arithmetic
# operations do not change the first value of class attribute for
# c("pseries", "logical/integer"). However, using groupGenerics as wrapper around
# pseries objects, this does not happen anymore.
# E.g.
#  x <- c(1L, 2L, 3L)
#  x + 1.5
# results in class propagation from class "integer" to "numeric" 
# but not if x is of class c("myclass", "integer")
check_propagation_correct_class <- function(x) {
  # x: a pseries object (usually)
  if (any((pos <- inherits(x, c("logical" ,"integer", "numeric"), which = TRUE)) > 0)) {
    pos <- pos[pos > 0] # non-matches in inherits(..., which = TRUE) results in 0
    switch(typeof(x),
           "double"  = { attr(x, "class")[pos] <- "numeric" },
           "integer" = { attr(x, "class")[pos] <- "integer" },
           "complex" = { attr(x, "class")[pos] <- "complex" }
    )
  }
  return(x)
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

is.pseries <- function(object) {
 # checks if an object has the necessary features to qualify as a 'pseries'
  res <- TRUE
  if (!inherits(object, "pseries")) res <- FALSE
  # class 'pseries' is always on top of basic class: min 2 classes needed, if 2 classes "pseries" needs to be first entry
  if (!length(class(object)) >= 2L) res <- FALSE
  if (length(class(object)) == 2L & class(object)[1] != "pseries") res <- FALSE
  if (!has.index(object)) res <- FALSE
  if (!any(c(is.numeric(object), is.factor(object), is.logical(object), is.character(object)))) check <- FALSE
  return(res)
}
