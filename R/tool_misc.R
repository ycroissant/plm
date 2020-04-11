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

#' Check for the presence of an intercept in a formula or in a fitted
#' model
#'
#' The presence of an intercept is checked using the formula which is
#' either provided as the argument of the function or extracted from
#' a fitted model
#'
#' @param object a `formula`, a `Formula` or a fitted model (of class
#'     `plm` or `panelmodel`),
#' @param rhs,part the index of the right hand sides part of the
#'     formula for which one wants to check the presence of an
#'     intercept (relevant for the `Formula` and the `plm` methods),
#' @param \dots further arguments.
#'
#' @return a boolean
#' @export
has.intercept <- function(object, ...) {
  UseMethod("has.intercept")
}

#' @rdname has.intercept
#' @export
has.intercept.default <- function(object, ...) {
  has.intercept(formula(object), ...)
}

#' @rdname has.intercept
#' @export
has.intercept.formula <- function(object, ...) {
  attr(terms(object), "intercept") == 1L
}

#' @rdname has.intercept
#' @export
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

#' @rdname has.intercept
#' @export
has.intercept.panelmodel <- function(object, ...) {
  object <- attr(model.frame(object), "formula")
  has.intercept(object)
}

#' @rdname has.intercept
#' @export
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


# punbalancedness: measures for unbalancedness of a pandel data set as
# defined in \insertCite{AHRE:PINC:81;textual}{plm}, p. 228 (gamma and
# nu) and for nested panel structures as in
# \insertCite{BALT:SONG:JUNG:01;textual}{plm}, pp. 368-369


#' Measures for Unbalancedness of Panel Data
#' 
#' This function reports unbalancedness measures for panel data as
#' defined in \insertCite{AHRE:PINC:81;textual}{plm} and
#' \insertCite{BALT:SONG:JUNG:01;textual}{plm}.
#' 
#' `punbalancedness` returns measures for the unbalancedness of a
#' panel data set.
#' 
#' - For two-dimensional data:\cr The two measures of
#' \insertCite{AHRE:PINC:81;textual}{plm} are calculated, called
#' "gamma" (\eqn{\gamma}) and "nu" (\eqn{\nu}).
#' 
#' If the panel data are balanced, both measures equal 1. The more
#' "unbalanced" the panel data, the lower the measures (but > 0). The
#' upper and lower bounds as given in \insertCite{AHRE:PINC:81;textual}{plm}
#' are:\cr
#' \eqn{0 < \gamma, \nu \le 1}, and for \eqn{\nu} more precisely
#' \eqn{\frac{1}{n} < \nu \le 1}{1/n < \nu \le 1}, with \eqn{n} being
#' the number of individuals (as in `pdim(x)$nT$n`).
#' 
#' - For nested panel data (meaning including a grouping variable):\cr
#' The extension of the above measures by
#' \insertCite{BALT:SONG:JUNG:01;textual}{plm}, p. 368, are
#' calculated:\cr
#'
#'   - c1: measure of subgroup (individual) unbalancedness,
#'   - c2: measure of time unbalancedness,
#'   - c3: measure of group unbalancedness due to each group size.
#'
#' Values are 1 if the data are balanced and become smaller as the
#' data become more unbalanced.
#' 
#'  
#' An application of the measure "gamma" is found in e. g.
#' \insertCite{BALT:SONG:JUNG:01;textual}{plm}, pp. 488-491, and
#' \insertCite{BALT:CHAN:94;textual}{plm}, pp. 78--87, where it is
#' used to measure the unbalancedness of various unbalanced data sets
#' used for Monte Carlo simulation studies. Measures c1, c2, c3 are
#' used for similar purposes in
#' \insertCite{BALT:SONG:JUNG:01;textual}{plm}.
#' 
#' In the two-dimensional case, `punbalancedness` uses output of
#' [pdim()] to calculate the two unbalancedness measures, so inputs to
#' `punbalancedness` can be whatever `pdim` works on. `pdim` returns
#' detailed information about the number of individuals and time
#' observations (see [pdim()]).
#' 
#' @param x a `panelmodel`, a `data.frame`, or a `pdata.frame` object,
#' @param index only relevant for `data.frame` interface, for details
#'     see [pdata.frame()],
#' @param \dots further arguments.
#' @return A named numeric containing either two or three entries,
#'     depending on the panel structure inputted:
#' 
#' - For the two-dimensional panel structure, the entries are called
#' `gamma` and `nu`,
#'
#' - For a nested panel structure, the entries are called `c1`, `c2`,
#' `c3`.
#' 
#' @note Calling `punbalancedness` on an estimated `panelmodel` object
#'     and on the corresponding `(p)data.frame` used for this
#'     estimation does not necessarily yield the same result (true
#'     also for `pdim`). When called on an estimated `panelmodel`, the
#'     number of observations (individual, time) actually used for
#'     model estimation are taken into account. When called on a
#'     `(p)data.frame`, the rows in the `(p)data.frame` are
#'     considered, disregarding any NA values in the dependent or
#'     independent variable(s) which would be dropped during model
#'     estimation.
#' @export
#' @author Kevin Tappe
#' @seealso [nobs()], [pdim()], [pdata.frame()]
#' @references
#'
#' \insertRef{AHRE:PINC:81}{plm}
#'
#' \insertRef{BALT:CHAN:94}{plm}
#'
#' \insertRef{BALT:SONG:JUNG:01}{plm}
#' 
#' \insertRef{BALT:SONG:JUNG:02}{plm}
#'
#' @keywords attribute
#' @examples
#' 
#' # Grunfeld is a balanced panel, Hedonic is an unbalanced panel
#' data(list=c("Grunfeld", "Hedonic"), package="plm")
#' 
#' # Grunfeld has individual and time index in first two columns
#' punbalancedness(Grunfeld) # c(1,1) indicates balanced panel
#' pdim(Grunfeld)$balanced   # TRUE
#' 
#' # Hedonic has individual index in column "townid" (in last column)
#' punbalancedness(Hedonic, index="townid") # c(0.472, 0.519)
#' pdim(Hedonic, index="townid")$balanced   # FALSE
#' 
#' # punbalancedness on estimated models
#' plm_mod_pool <- plm(inv ~ value + capital, data = Grunfeld)
#' punbalancedness(plm_mod_pool)
#' 
#' plm_mod_fe <- plm(inv ~ value + capital, data = Grunfeld[1:99, ], model = "within")
#' punbalancedness(plm_mod_fe)
#' 
#' # replicate results for panel data design no. 1 in Ahrens/Pincus (1981), p. 234
#' ind_d1  <- c(1,1,1,2,2,2,3,3,3,3,3,4,4,4,4,4,4,4,5,5,5,5,5,5,5)
#' time_d1 <- c(1,2,3,1,2,3,1,2,3,4,5,1,2,3,4,5,6,7,1,2,3,4,5,6,7)
#' df_d1 <- data.frame(individual = ind_d1, time = time_d1)
#' punbalancedness(df_d1) # c(0.868, 0.887)
#' 
#' # example for a nested panel structure with a third index variable
#' # specifying a group (states are grouped by region) and without grouping
#' data("Produc", package = "plm")
#' punbalancedness(Produc, index = c("state", "year", "region"))
#' punbalancedness(Produc, index = c("state", "year")) 
#' 
#' @rdname punbalancedness
#' @export
punbalancedness <- function(x, ...) {
  UseMethod("punbalancedness")
}


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

#' @rdname punbalancedness
#' @export
punbalancedness.pdata.frame <- function(x, ...) {
  punbalancedness.default(x, ...)
}

#' @rdname punbalancedness
#' @export
punbalancedness.data.frame <- function(x, index = NULL, ...) {
  x <- pdata.frame(x, index = index, ...)
  punbalancedness.default(x, ...)
}

#' @rdname punbalancedness
#' @export
punbalancedness.panelmodel <- function(x, ...) {
  punbalancedness.default(x, ...)
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



#' Check for Cross-Sectional and Time Variation
#' 
#' This function checks for each variable of a panel if it varies
#' cross-sectionally and over time.
#' 
#' For (p)data.frame and matrix interface: All-NA columns are removed
#' prior to calculation of variation due to coercing to pdata.frame
#' first.
#' 
#' @aliases pvar
#' @param x a `(p)data.frame` or a `matrix`,
#' @param index see [pdata.frame()],
#' @param \dots further arguments.
#' @return An object of class `pvar` containing the following
#'     elements:
#' 
#' \item{id.variation}{a logical vector with `TRUE` values if the
#' variable has individual variation, `FALSE` if not,}
#'
#' \item{time.variation}{a logical vector with `TRUE` values if the
#' variable has time variation, `FALSE` if not,}
#'
#' \item{id.variation_anyNA}{a logical vector with `TRUE` values if
#' the variable has at least one individual-time combination with all
#' NA values in the individual dimension for at least one time period,
#' `FALSE` if not,}
#'
#' \item{time.variation_anyNA}{a logical vector with `TRUE` values if
#' the variable has at least one individual-time combination with all
#' NA values in the time dimension for at least one individual,
#' `FALSE` if not.}
#' 
#' @note `pvar` can be time consuming for ``big'' panels.
#' @export
#' @author Yves Croissant
#' @seealso [pdim()] to check the dimensions of a 'pdata.frame' (and
#'     other objects),
#' @keywords attribute
#' @examples
#' 
#' 
#' # Gasoline contains two variables which are individual and time
#' # indexes and are the first two variables
#' data("Gasoline", package = "plm")
#' pvar(Gasoline)
#' 
#' # Hedonic is an unbalanced panel, townid is the individual index;
#' # the drop.index argument is passed to pdata.frame
#' data("Hedonic", package = "plm")
#' pvar(Hedonic, "townid", drop.index = TRUE)
#' 
#' # same using pdata.frame
#' Hed <- pdata.frame(Hedonic, "townid", drop.index = TRUE)
#' pvar(Hed)
#' 
#' # Gasoline with pvar's matrix interface
#' Gasoline_mat <- as.matrix(Gasoline)
#' pvar(Gasoline_mat)
#' pvar(Gasoline_mat, index=c("country", "year"))
#' 
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

#' @rdname pvar
#' @export
pvar.matrix <- function(x, index = NULL, ...){
  x <- pdata.frame(as.data.frame(x), index, ...)
  pvar(x)
}

#' @rdname pvar
#' @export
pvar.data.frame <- function(x, index = NULL, ...){
  x <- pdata.frame(x, index, ...)
  pvar(x)
}

#' @rdname pvar
#' @export
pvar.pdata.frame <- function(x, ...){
  index <- attr(x, "index")
  id <- index[[1]]
  time <- index[[2]]
  pvar.default(x, id, time)
}

#' @rdname pvar
#' @export
pvar.pseries <- function(x, ...){
  # use drop.index = TRUE so that the index columns' 
  # variations are not evaluated:
  pdfx <- pseries2pdataframe(x, drop.index = TRUE)
  pvar.pdata.frame(pdfx)
}

#' @rdname pvar
#' @export
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
