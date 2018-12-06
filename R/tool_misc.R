## Function that are used in more that on place in plm. 

## - bdiag : takes matrices as argument and returns the block-diagonal matrix (used in pgmm and plm.list)
## - twosls : computes the 2SLS estimator (used in plm and ercomp)
## - has.intercept : tests the presence of an intercept
## - pres : extract model residuals as pseries (used in several estimation functions)

bdiag <- function(...){
  if (nargs() == 1)
    x <- as.list(...)
  else
    x <- list(...)
  n <- length(x)
  if(n == 0) return(NULL)
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

## expand.formula <- function(x){
##   oclass <- class(x)
##   if (! any(class(x) == "Formula")) stop("not a Formula object")
##   if (length(x)[2] != 2) stop("not a two part formula")
##   xs <- structure(x, class = "formula")
##   has.response <- attr(terms(xs),"response") == 1
##   if (has.response){
##     y <- x[[2]]
##     rhs <- x[[3]]
##   }
##   else{
##     y <- NULL
##     rhs <- x[[2]]
##   }
##   firstpart <- rhs[[2]]
##   secondpart <- rhs[[3]]
##   if (has.response){
##     one <- do.call("~", list(y,firstpart))
##     two <- do.call("~", list(y,secondpart))
##   }
##   else{
##     one <- do.call("~", list(firstpart))
##     two <- do.call("~", list(secondpart))
##   }
##   two <- update(one, two)
##   one <- paste(deparse(one), collapse = "")
##   two <- paste(deparse(two[[3]]), collapse = "")
##   result <- as.formula(paste(one, "|", two, collapse = ""));
##   result <- as.Formula(result)
##   #YC  class(result) <- c("pFormula", class(result))
##   structure(result, class = oclass)
## }



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
    sapply(rhs, function(x){
        aform <- formula(object, lhs = 0, rhs = x)
        # expand the dot if any in all the parts except the first
        if (x > 1) aform <- update(formula(object, lhs = 0, rhs = 1), aform)
        has.intercept(aform)
    }
    )
}

has.intercept.panelmodel <- function(object, ...) {
  object <- attr(model.frame(object), "formula")
  has.intercept(object)
}

has.intercept.plm <- function(object, part = "first", ...) {
  has.intercept(formula(object), part = part)
}

pres <- function(x) {  # pres.panelmodel
  ## extracts model residuals as pseries
  ## not necessary for plm models as residuals.plm returns a pseries,
  ## but used in residuals.pggls, residuals.pcce, residuals.pmg
  
  ## extract indices
  groupind <-attr(x$model, "index")[,1]
  timeind  <-attr(x$model, "index")[,2]
  
  # fix to allow operation with pggls, pmg
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

