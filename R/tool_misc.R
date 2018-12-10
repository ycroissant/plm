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

myvar <- function(x){
  x.na <- is.na(x)
  if(anyNA(x.na)) x <- x[!x.na]
  n <- length(x)
  
  z <- switch(as.character(n),
              "0" = NA,
              "1" = 0,
              ifelse(!(is.factor(x) || is.character(x)), var(x), !all(duplicated(x)[-1L]))) # [var() on factors is deprecated as of R 3.2.3]
  z
}

pvar <- function(x, ...){
  UseMethod("pvar")
}

pvar.default <- function(x, id, time, ...){
  name.var <- names(x)
  len <- length(x)
  time.variation <- rep(TRUE, len)
  id.variation   <- rep(TRUE, len)
  time.variation_anyNA <- rep(FALSE, len)
  id.variation_anyNA   <- rep(FALSE, len)
  lid <- split(x, id)
  ltime <- split(x, time)
  if (is.list(x)){
    if (len == 1){
      # time variation
      temp_time.var          <- sapply(lid, function(x) sapply(x, myvar))
      temp_time.var_sumNoVar <- sum(temp_time.var == 0, na.rm = TRUE) # number of non-varying id-time comb. (without all NA groups)
      temp_time.var_sumNA    <- sum(is.na(temp_time.var))             # number of all-NA groups
      temp_time.varResult    <- temp_time.var_sumNoVar + temp_time.var_sumNA
      time.variation         <- temp_time.varResult != length(lid) # no variation if (no. non-varying + no. all-NA) == number of groups 
      time.variation_anyNA   <- temp_time.var_sumNA > 0            # indicates if at least one id-time comb is all NA
      
      # id variation
      temp_id.var          <- sapply(ltime, function(x) sapply(x, myvar))
      temp_id.var_sumNoVar <- sum(temp_id.var == 0, na.rm = TRUE)
      temp_id.var_sumNA    <- sum(is.na(temp_id.var))
      temp_id.varResult    <- temp_id.var_sumNoVar + temp_id.var_sumNA
      id.variation         <- temp_id.varResult != length(ltime)
      id.variation_anyNA   <- temp_id.var_sumNA > 0
    }
    else{
      # time variation
      temp_time.var          <- sapply(lid, function(x) sapply(x, myvar))
      temp_time.var_sumNoVar <- apply(temp_time.var == 0, 1, sum, na.rm = TRUE)
      temp_time.var_sumNA    <- apply(is.na(temp_time.var), 1, sum)
      temp_time.varResult    <- temp_time.var_sumNoVar + temp_time.var_sumNA
      time.variation         <- temp_time.varResult != length(lid)
      time.variation_anyNA   <- temp_time.var_sumNA > 0
      
      # id variation
      temp_id.var          <- sapply(ltime, function(x) sapply(x, myvar))
      temp_id.var_sumNoVar <- apply(temp_id.var == 0, 1, sum, na.rm = TRUE)
      temp_id.var_sumNA    <- apply(is.na(temp_id.var), 1, sum)
      temp_id.varResult    <- temp_id.var_sumNoVar + temp_id.var_sumNA
      id.variation         <- temp_id.varResult != length(ltime)
      id.variation_anyNA   <- temp_id.var_sumNA > 0
    }
  }
  else{ # not a list (not a data.frame, pdata.frame) - try our best for that unknown data structure
    # time variation
    temp_time.var          <- sapply(lid, function(x) sapply(x, myvar))
    temp_time.var_sumNoVar <- sum(temp_time.var == 0, na.rm = TRUE)
    temp_time.var_sumNA    <- sum(is.na(temp_time.var))
    temp_time.varResult    <- temp_time.var_sumNoVar + temp_time.var_sumNA
    time.variation         <- temp_time.varResult != length(lid)
    time.variation_anyNA   <- temp_time.var_sumNA > 0
    
    # id variation
    temp_id.var          <- sapply(ltime, function(x) sapply(x, myvar))
    temp_id.var_sumNoVar <- sum(temp_id.var == 0, na.rm = TRUE)
    temp_id.var_sumNA    <- sum(is.na(temp_id.var))
    temp_id.varResult    <- temp_id.var_sumNoVar + temp_id.var_sumNA
    id.variation         <- temp_id.varResult != length(ltime)
    id.variation_anyNA   <- temp_id.var_sumNA > 0
  }
  
  # make 'pvar' object
  names(id.variation) <- names(time.variation) <- names(id.variation_anyNA) <- names(time.variation_anyNA) <- name.var
  dim.var <- list(id.variation         = id.variation,
                  time.variation       = time.variation,
                  id.variation_anyNA   = id.variation_anyNA,
                  time.variation_anyNA = time.variation_anyNA)
  class(dim.var) <- "pvar"
  return(dim.var)
}

pvar.matrix <- function(x, index = NULL, ...){
  x <- pdata.frame(as.data.frame(x), index, ...)
  pvar(x)
}

pvar.data.frame <- function(x, index = NULL, ...){
  x <- pdata.frame(x, index, ...)
  pvar(x)
}

pvar.pdata.frame <- function(x, ...){
  index <- attr(x, "index")
  id <- index[[1]]
  time <- index[[2]]
  pvar.default(x, id, time)
}

pvar.pseries <- function(x, ...){
  # use drop.index = TRUE so that the index columns' 
  # variations are not evaluated:
  pdfx <- pseries2pdataframe(x, drop.index = TRUE)
  pvar.pdata.frame(pdfx)
}

print.pvar <- function(x, ...){
  varnames <- names(x$time.variation)
  if(any(!x$time.variation)){
    var <- varnames[x$time.variation==FALSE]
    #    if (!is.null(y)) var <- var[-which(var==y$id)]
    if (length(var)!=0) cat(paste("no time variation:      ", paste(var,collapse=" "),"\n"))
  }
  if(any(!x$id.variation)){
    var <- varnames[x$id.variation==FALSE]
    #    if (!is.null(y)) var <- var[-which(var==y$time)]
    if(length(var)!=0) cat(paste("no individual variation:", paste(var,collapse=" "),"\n"))
  }
  
  # any individual-time combinations all NA?
  if(any(x$time.variation_anyNA)){
    var_anyNA <- varnames[x$time.variation_anyNA]
    if (length(var_anyNA)!=0) cat(paste("all NA in time dimension for at least one individual: ", paste(var_anyNA,collapse=" "),"\n"))
  }
  if(any(x$id.variation_anyNA)){
    var_anyNA <- varnames[x$id.variation_anyNA]
    if (length(var_anyNA)!=0) cat(paste("all NA in ind. dimension for at least one time period:", paste(var_anyNA,collapse=" "),"\n"))
  }
}