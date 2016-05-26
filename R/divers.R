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
  Xhat <- lm(X ~ W)$fit
  if(!is.matrix(Xhat)){
    Xhat <- matrix(Xhat, ncol = 1)
    colnames(Xhat) <- colnames(X)
  }
  if(intercept){
    model <- lm(y~Xhat)
    yhat <- as.vector(crossprod(t(cbind(1,X)),coef(model)))
  }
  else{
    model <- lm(y~Xhat-1)
    yhat <- as.vector(crossprod(t(X),coef(model)))
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
  if (!any(class(x) == "Formula")) stop("not a Formula object")
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
    one <- do.call("~",list(y,firstpart))
    two <- do.call("~",list(y,secondpart))
  }
  else{
    one <- do.call("~",list(firstpart))
    two <- do.call("~",list(secondpart))
  }
  two <- update(one,two)
  one <- paste(deparse(one), collapse = "")
  two <- paste(deparse(two[[3]]), collapse = "")
  result <- as.formula(paste(one, "|", two, collapse = ""));
  result <- as.Formula(result)
  class(result) <- c("pFormula", class(result))
  result
}


lev2var <- function(x, ...){
  # take a data.frame and returns a vector of variable names, the
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
  ## NOTE: return a logical vector of the necessary length
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
  groupind <-attr(x$model, "index")[,1]   #attr(model.frame(x), "index")[[1]]
  timeind  <-attr(x$model, "index")[,2]   #attr(model.frame(x), "index")[[2]]
  
  # fix to allow operation with pggls, pmg (NB: pmg? meant: plm?)
  # [NB: one day, make this cleaner; with the describe framework?]
  if (!is.null(x$args$model))                 maybe_fd <- x$args$model
  if (!is.null(attr(x, "pmodel")$model.name)) maybe_fd <- attr(x, "pmodel")$model.name # this line is currently needed to detect pggls models
  
  ## Achim's fix: reduce id and time index to accomodate first-differences model's number of observations
  if(exists("maybe_fd") && maybe_fd == "fd") {
    groupi <- as.numeric(groupind)
    ## make vector =1 on first obs in each group, 0 elsewhere
    selector <-groupi-c(0,groupi[-length(groupi)])
    selector[1] <- 1 # the first must always be 1
    ## eliminate first obs in time for each group
    groupind <- groupind[!selector]
    timeind <- timeind[!selector]
  }

  resdata <- data.frame(ee=x$residuals, ind=groupind, tind=timeind)
  pee<-pdata.frame(resdata, index=c("ind","tind"))

  pres <- pee$ee
  return(pres)
}


# nobs() function to extract total number of observations used for estimating the panelmodel
# like stats::nobs for lm objects
nobs.panelmodel <- function(object, ...) {
  if (inherits(object, "plm") | inherits(object, "panelmodel")) return(length(residuals(object)))
    else stop("Input 'object' needs to be of class 'plm' or 'panelmodel'")
}

# No of obs calculated as in print.summary.pgmm [code copied from there]
nobs.pgmm <- function(object, ...) {
  if (inherits(object, "pgmm")) return(sum(unlist(object$residuals) != 0))
    else stop("Input 'object' needs to be of class 'pgmm', i. e. a GMM estimation with panel data estimated by pgmm()")
}



# punbalancedness: measures for unbalancedness of a pandel data set
# as defined in Ahrens/Pincus (1981), p. 228 (gamma and nu)
punbalancedness.default <- function(x, ...) {
  pdim <- pdim(x, ...)
  
  N <- pdim$nT$n # no. of individuals
  Totalobs <- pdim$nT$N # no. of total observations
  Ti <- pdim$Tint$Ti
  Tavg <- sum(Ti/N)
  
  r1 <- N / (Tavg * sum(1/Ti))
  r2 <- 1 / (N * (sum( (Ti/Totalobs)^2)))
  
  return(c(gamma = r1, nu = r2))
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


# helper function
fancy.row.names <- function(index, sep = "-") {
  return(paste(index[[1]], index[[2]], sep = sep))
}