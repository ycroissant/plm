## Function that are used in more than on place in plm (or likely to be used in more than one place in the future)

## - trace : calculate trace of a matrix (used in ercomp())
## - is.constant : check if a numeric vector or columns of a matrix is constant
## - sumres : gives summary for residuals (used in several print.summary.* methods)
## - bdiag : takes matrices as argument and returns the block-diagonal matrix (used in pgmm and plm.list)
## - mylm : inner fitting func based on stats::lm with matrix inputs (used in plm.fit)
## - my.lm.fit : like the barebone stats::lm.fit but with some extra information (e.g., SEs, sigma) used in purtest
## - twosls : computes the 2SLS estimator (used in plm and ercomp)
## - data.name : used in a lot tests to generate the 'data.name' entry for htest objects from the model object's call$formula
## - has.intercept : tests the presence of an intercept
## - pres : extract model residuals as pseries (used in several estimation functions)
## - punbalancedness : measures for the unbalancedness of panel data
## - myvar : calculates variance with NA removal, checks if input is constant (also for factor and character)
## - pvar : checks if input varies in individual / time dimension
## - make.dummies : create a contrast-coded dummy matrix from a factor
## - describe : extract characteristics of plm and other model objects

trace <- function(x, names = FALSE) sum(diag(x, names = names)) # opposite default for names for speed

is.constant <- function(x) (max(x) - min(x)) < sqrt(.Machine$double.eps)

sumres <- function(x){
  sr <- summary(unclass(resid(x)))
  if(abs(sr["Mean"]) < 1e-10) sr[c(1:3, 5:6)] else sr
}

bdiag <- function(...){
  ## non-exported
  x <- if(nargs() == 1L) as.list(...) else list(...)
  n <- length(x)
  if(n == 0L) return(NULL)
  x <- lapply(x, function(y) if(length(y)) as.matrix(y) else
              stop("Zero-length component in x"))
  d <- array(unlist(lapply(x, dim)), c(2, n))
  rr <- d[1L, ]
  cc <- d[2L, ]
  rsum <- sum(rr)
  csum <- sum(cc)
  out <- array(0, c(rsum, csum))
  ind <- array(0, c(4, n))
  rcum <- cumsum(rr)
  ccum <- cumsum(cc)
  ind[1, -1] <- rcum[-n]
  ind[2,   ] <- rcum
  ind[3, -1] <- ccum[-n]
  ind[4,   ] <- ccum
  imat <- array(seq_len(rsum * csum), c(rsum, csum))
  iuse <- apply(ind, 2, function(y, imat) imat[(y[1L]+1):y[2L],
                                               (y[3L]+1):y[4L]], imat = imat)
  iuse <- as.vector(unlist(iuse))
  out[iuse] <- unlist(x)
  out
}

# mylm is used in plm.fit()
mylm <- function(y, X, W = NULL) {
  ## non-exported
  names.X <- colnames(X)
  result <- if(is.null(W)) lm(y ~ X - 1) else twosls(y, X, W)
  if(any(na.coef <- is.na(result$coefficients))) {
    ## for debug purpose:
    # warning("Coefficient(s) '", paste((names.X)[na.coef], collapse = ", "), 
    #"' could not be estimated and is (are) dropped.")
    X <- X[ , !na.coef, drop = FALSE]
    if(dim(X)[2L] == 0L) stop(paste("estimation not possible: all coefficients",
                                    "omitted from estimation due to aliasing"))
    
    ## re-estimate without the columns which resulted previously in NA-coefficients
    result <- if(is.null(W)) lm(y ~ X - 1) else twosls(y, X, W)
  }
  result$vcov <- vcov(result)
  result$X <- X
  result$y <- y
  result$W <- W
  # aliased is an element of summary.lm-objects:
  # since plm drops aliased coefs, store this info in plm object
  # NB: this only sets coefs to NA that are detected/set to NA by mylm()/lm.fit();
  #     covariates dropped earlier by model.matrix( , cstcovar.rm) are not included here anymore
  result$aliased <- na.coef
  names(result$aliased) <- names.X
  names(result$coefficients) <- colnames(result$vcov) <- 
    rownames(result$vcov) <- colnames(X)
  result
}

# my.lm.fit is used in purtest()
my.lm.fit <- function(X, y, dfcor = TRUE, ...){
  reg <- lm.fit(X, y)
  ## 'as' summary method for lm.fit
  p <- reg$rank
  Qr <- reg$qr
  n <- NROW(Qr$qr)
  rdf <- n - p
  p1 <- seq_len(p)
  r <- reg$residuals
  rss <- as.numeric(crossprod(r))
  resvar <- if (dfcor) rss/rdf else rss/n
  sigma <- sqrt(resvar)
  R <- chol2inv(Qr$qr[p1, p1, drop = FALSE])
  thecoef <- reg$coefficients[Qr$pivot[p1]] #[lags+1]
  these <- sigma * sqrt(diag(R)) #[lags+1])
  list(coef = thecoef, se = these, sigma = sigma,
       rss = rss, n = n, K = p, rdf = rdf)
}

#' @importFrom stats .lm.fit
twosls <- function(y, X, W, intercept = FALSE, lm.type = "lm"){
  ## non-exported
  # Return value can be controlled by argument lm.type. Often, a full lm model
  # is needed for further processing but can select one of the fast but less
  # rich objects produced by lm.fit or .lm.fit (the latter does not contain, e.g.,
  # fitted.values and is to be used very carefully (e.g., coefs not in input order)).

  # As NA/NaN/(+/-)Inf-freeness needs to be guaranteed when functions call
  # twosls(), so can use lm.fit to calc. Xhat.
   Xhat <- lm.fit(W, X)$fitted.values

  
  if(!is.matrix(Xhat)) {
    # ensure Xhat is a matrix
    Xhat <- matrix(Xhat, ncol = 1L)
    colnames(Xhat) <- colnames(X)
  }
  
  if(intercept) {
    model <- switch(lm.type,
                    "lm"      =  lm(y ~ Xhat),
                    "lm.fit"  =  lm.fit(cbind(1, Xhat), y),
                    ".lm.fit" = .lm.fit(cbind(1, Xhat), y))
    yhat <- as.vector(crossprod(t(cbind(1, X)), coef(model)))
  }
  else{
    model <- switch(lm.type,
                    "lm"      =  lm(y ~ Xhat - 1),
                    "lm.fit"  =  lm.fit(Xhat, y),
                    ".lm.fit" = .lm.fit(Xhat, y))
    yhat <- as.vector(crossprod(t(X), coef(model)))
  }
  model$residuals <- y - yhat
  model
}

data.name <- function(x) {
  ## non-exported, used in various tests
  data.name <- paste(deparse(x$call$formula))
  if (length(data.name) > 1L) paste(data.name[1L], "...")
  else data.name
}

##### has.intercept methods #####

#' Check for the presence of an intercept in a formula or in a fitted
#' model
#'
#' The presence of an intercept is checked using the formula which is
#' either provided as the argument of the function or extracted from
#' a fitted model.
#'
#' @param object a `formula`, a `Formula` or a fitted model (of class
#'     `plm` or `panelmodel`),
#' @param rhs an integer (length > 1 is possible), indicating the parts of right
#'      hand sides of the formula to be evaluated for the presence of an
#'      intercept or `NULL` for all parts of the right hand side
#'      (relevant for the `Formula` and the `plm` methods),
#' @param data default is `NULL` and only needs to be changes to a data set if
#'             the formula contains a dot (`.`) to allow evaluation of the dot,
#' @param \dots further arguments.
#'
#' @return a logical
#' @export
has.intercept <- function(object, ...) {
  UseMethod("has.intercept")
}

#' @rdname has.intercept
#' @export
has.intercept.default <- function(object, data = NULL, ...) {
  has.intercept(formula(object), data = data, ...)
}

#' @rdname has.intercept
#' @export
has.intercept.formula <- function(object, data = NULL, ...) {
  attr(terms(object, data = data, ...), "intercept") == 1L
}

#' @rdname has.intercept
#' @export
has.intercept.Formula <- function(object, rhs = NULL, data = NULL, ...) {
  ## NOTE: returns a logical vector of the necessary length
  ## (which might be > 1)
  if (is.null(rhs)) rhs <- seq_along(attr(object, "rhs"))
  res <- sapply(rhs, function(x) {
    aform <- formula(object, lhs = 0, rhs = x)
    # expand the dot if any in all the parts except the first
    if (x > 1L) aform <- update(formula(object, lhs = 0, rhs = 1), aform, ...)
    has.intercept(aform, data = data)
  })
  return(res)
}

#' @rdname has.intercept
#' @export
has.intercept.panelmodel <- function(object, ...) {
  object <- attr(model.frame(object), "formula")
  has.intercept(object)
}

#' @rdname has.intercept
#' @export
has.intercept.plm <- function(object, rhs = 1L, ...) {
  has.intercept(formula(object), rhs = rhs, ...)
}


pres <- function(x) {  # pres.panelmodel
  ## extracts model residuals as pseries
  ## not necessary for plm models as residuals.plm returns a pseries,
  ## but used in residuals.pggls, residuals.pcce, residuals.pmg
  
  ## extract indices
  xindex <- unclass(attr(x$model, "index")) # unclass for speed
  groupind <- xindex[[1L]]
  timeind  <- xindex[[2L]]
  
  # fix to allow operation with pggls, pmg
  # [TODO: one day, make this cleaner; with the describe framework?]
  if (!is.null(x$args$model))                 maybe_fd <- x$args$model
  if (!is.null(attr(x, "pmodel")$model.name)) maybe_fd <- attr(x, "pmodel")$model.name # this line is currently needed to detect pggls models
  
  ## Achim's fix: reduce id and time index to accommodate first-differences model's number of observations
  if(exists("maybe_fd") && maybe_fd == "fd") {
    groupi <- as.numeric(groupind)
    ## make vector =1 on first obs in each group, 0 elsewhere
    selector <- groupi - c(0, groupi[-length(groupi)])
    selector[1L] <- 1 # the first must always be 1
    ## eliminate first obs in time for each group
    groupind <- groupind[!selector]
    timeind <- timeind[!selector]
  }

  resdata <- data.frame(ee = x$residuals, ind = groupind, tind = timeind)
  pee <- pdata.frame(resdata, index = c("ind", "tind"))
  pres <- pee$ee
  return(pres)
}


# punbalancedness: measures for unbalancedness of a panel data set as
# defined in Ahrens/Pincus (1981), p. 228 (gamma and
# nu) and for nested panel structures as in Baltagi/Song/Jung (2001), pp. 368-369 .
#
# Ahrens/Pincus (1981), On Two Measures of Unbalancedness in a One-Way Model
#  and Their Relation to Efficiency, Biometrical Journal, Vol. 23, pp. 227-235.
# 
# Baltagi/Song/Jung (2001), The unbalanced nested error component regression model,
#  Journal of Econometrics, Vol. 101, pp. 357-381


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
#'     considered, disregarding any `NA` values in the dependent or
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

#' @export
punbalancedness.default <- function(x, ...) {
  ii <- index(x)
  if(!is.index(ii)) stop("no valid index found for input object 'x'")
  
  if (ncol(ii) == 2L) {
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
    if (ncol(ii) == 3L) {
     ## extension to nested model with additional group variable
     ## Baltagi/Song/Jung (2001), pp. 368-369
      ii <- unclass(ii) # unclass for speed
      ids <- ii[[1L]]
      tss <- ii[[2L]]
      gps <- ii[[3L]]
      Tis <- unique(data.frame(tss, gps))
      Tis <- collapse::qtable(Tis$gps)    # no of max time periods per group
      Nis <- unique(data.frame(ids, gps))
      Nis <- collapse::qtable(Nis$gps)    # no of individuals per group
      M <- length(unique(gps))            # no of unique groups
      Nbar <- sum(Nis)/M
      Tbar <- sum(Tis)/M
      
      c1 <- M / (Nbar * sum(1/Nis))
      c2 <- M / (Tbar * sum(1/Tis))
      c3 <- M / (sum(Nis * Tis)/M * sum(1/(Nis*Tis)))
      result <- (c(c1 = c1, c2 = c2, c3 = c3))
    } else stop(paste0("unsupported number of dimensions: ", ncol(ii)))
  }
  result
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
  ## non-exported
  x.na <- is.na(x)
  if(anyNA(x.na)) x <- x[!x.na]
  n <- length(x)

  if(n <= 1L) {
    if(n == 0L) z <- NA
    if(n == 1L) z <- 0
  } else {
    z <- if(!(is.factor(x) || is.character(x))) var(x)
         else !all(collapse::fduplicated(x, all = FALSE)[-1L])
  }
  z
}



#' Check for Cross-Sectional and Time Variation
#' 
#' This function checks for each variable of a panel if it varies
#' cross-sectionally and over time.
#' 
#' For (p)data.frame and matrix interface: All-`NA` columns are removed
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
#' `NA` values in the individual dimension for at least one time period,
#' `FALSE` if not,}
#'
#' \item{time.variation_anyNA}{a logical vector with `TRUE` values if
#' the variable has at least one individual-time combination with all
#' `NA` values in the time dimension for at least one individual,
#' `FALSE` if not.}
#' 
#' @note `pvar` can be time consuming for ``big'' panels. As a fast alternative
#' [collapse::varying()] from package \CRANpkg{collapse} could be used. 
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

#' @export
pvar.default <- function(x, id, time, ...){
  name.var <- names(x)
  len <- length(x)
  time.variation <- rep(TRUE, len)
  id.variation   <- rep(TRUE, len)
  time.variation_anyNA <- rep(FALSE, len)
  id.variation_anyNA   <- rep(FALSE, len)
  lid   <- split(x, id)   # these split() functions seem particularly slow
  ltime <- split(x, time)
  if(is.list(x)){
    if(len == 1L){
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
  index <- unclass(attr(x, "index")) # unclass for speed
  pvar.default(x, index[[1L]], index[[2L]])
}

#' @rdname pvar
#' @export
pvar.pseries <- function(x, ...){
  # use drop.index = TRUE so that the index columns' 
  # variations are not evaluated:
  pdfx <- pseries2pdataframe(x, drop.index = TRUE)

  # give the variable in pdata.frame the original name, so it prints nicely
  substx <- substitute(x)
  len <- length(substx)
  names(pdfx) <- paste(substx)[len]

  pvar.pdata.frame(pdfx)
}

#' @rdname pvar
#' @export
print.pvar <- function(x, ...){
  varnames <- names(x$time.variation)
  if(any(!x$time.variation)){
    var <- varnames[x$time.variation == FALSE]
    #    if (!is.null(y)) var <- var[-which(var==y$id)]
    if(length(var)!=0) cat(paste("no time variation:      ", paste(var,collapse=" "),"\n"))
  }
  if(any(!x$id.variation)){
    var <- varnames[x$id.variation == FALSE]
    #    if (!is.null(y)) var <- var[-which(var==y$time)]
    if(length(var)!=0) cat(paste("no individual variation:", paste(var,collapse=" "),"\n"))
  }
  
  # any individual-time combinations all NA?
  if(any(x$time.variation_anyNA)){
    var_anyNA <- varnames[x$time.variation_anyNA]
    if(length(var_anyNA)!=0) cat(paste("all NA in time dimension for at least one individual: ", paste(var_anyNA,collapse=" "),"\n"))
  }
  if(any(x$id.variation_anyNA)){
    var_anyNA <- varnames[x$id.variation_anyNA]
    if(length(var_anyNA)!=0) cat(paste("all NA in ind. dimension for at least one time period:", paste(var_anyNA,collapse=" "),"\n"))
  }
  invisible(x)
}


#' Create a Dummy Matrix
#' 
#' Contrast-coded dummy matrix (treatment coding) created from a factor
#' 
#' This function creates a matrix of dummies from the levels of a factor in 
#' treatment coding. In model estimations, it is usually preferable to not 
#' create the dummy matrix prior to estimation but to simply specify a factor 
#' in the formula and let the estimation function handle the creation of the 
#' dummies.
#' 
#' This function is merely a convenience wrapper around `stats::contr.treatment`
#' to ease the dummy matrix creation process shall the dummy matrix be explicitly
#' required. See Examples for a use case in LSDV (least squares dummy variable)
#' model estimation.
#' 
#' The default method uses a factor as main input (or something coercible to a 
#' factor) to derive the dummy matrix from. Methods for data frame and pdata.frame 
#' are available as well and have the additional argument `col` to specify the
#' the column from which the dummies are created; both methods merge the dummy 
#' matrix to the data frame/pdata.frame yielding a ready-to-use data set.
#' See also Examples for use cases.
#' 
#' @param x a factor from which the dummies are created (x is coerced to 
#'          factor if not yet a factor) for the default method or a data 
#'          data frame/pdata.frame for the respective method.
#' @param base integer or character, specifies the reference level (base), if 
#'             integer it refers to position in `levels(x)`, if character the name 
#'             of a level,
#' @param base.add logical, if `TRUE` the reference level (base) is added 
#'                 to the return value as first column, if `FALSE` the reference
#'                 level is not included.
#' @param col character (only for the data frame and pdata.frame methods), to
#'            specify the column which is used to derive the dummies from,
#' @param \dots further arguments.
#'
#' @return For the default method, a matrix containing the contrast-coded 
#'         dummies (treatment coding), 
#'         dimensions are n x n where `n = length(levels(x))` if argument  
#'        `base.add = TRUE` or `n = length(levels(x)-1)` if `base.add = FALSE`;
#'         for the data frame and pdata.frame method, a data frame or pdata.frame,
#'         respectively, with the dummies appropriately merged to the input as 
#'         last columns (column names are derived from the name of the column 
#'         used to create the dummies and its levels).
#' @author Kevin Tappe
#' @importFrom stats contr.treatment
#' @export
#' @seealso [stats::contr.treatment()], [stats::contrasts()]
#' @keywords manip
#' @examples
#' library(plm)
#' data("Grunfeld", package = "plm")
#' Grunfeld <- Grunfeld[1:100, ] # reduce data set (down to 5 firms)
#'
#' ## default method
#' make.dummies(Grunfeld$firm) # gives 5 x 5 matrix (5 firms, base level incl.)
#' make.dummies(Grunfeld$firm, base = 2L, base.add = FALSE) # gives 5 x 4 matrix
#'
#' ## data frame method
#' Grun.dummies <- make.dummies(Grunfeld, col = "firm")
#' 
#' ## pdata.frame method
#' pGrun <- pdata.frame(Grunfeld)
#' pGrun.dummies <- make.dummies(pGrun, col = "firm")
#' 
#' ## Model estimation:
#' ## estimate within model (individual/firm effects) and LSDV models (firm dummies)
#' # within model:
#' plm(inv ~ value + capital, data = pGrun, model = "within")
#' 
#' ## LSDV with user-created dummies by make.dummies:
#' form_dummies <- paste0("firm", c(1:5), collapse = "+")
#' form_dummies <- formula(paste0("inv ~ value + capital + ", form_dummies))
#' plm(form_dummies, data = pGrun.dummies, model = "pooling") # last dummy is dropped
#'
#' # LSDV via factor(year) -> let estimation function generate dummies:
#' plm(inv ~ value + capital + factor(firm), data = pGrun, model = "pooling")
make.dummies <- function(x, ...){
  UseMethod("make.dummies")
}

#' @rdname make.dummies
#' @export
make.dummies.default <- function(x, base = 1L, base.add = TRUE, ...) {
  
  stopifnot(is.numeric(base) || is.character(base))
  if(is.numeric(base)) if(round(base) != base) stop("Argument 'base' specified as numeric but is not integer")
  if(!is.factor(x)) x <- factor(x)
  
  lvl <- levels(x)
  
  if(is.character(base)) {
    pos <- match(base, lvl)
    if(is.na(pos)) stop(paste0("argument 'base' specified as character but value \"", 
                               base, "\", is not in levels(x)"))
    base <- pos
  }
  
  dummies <- contr.treatment(lvl, base = base)
  
  # if requested, add reference level to dummy matrix in 1st position
  if(base.add) {
    lvl.base <- levels(x)[base]
    dummies <- cbind(c(1, rep(0, NROW(dummies)-1)), dummies)
    colnames(dummies) <- c(lvl.base, colnames(dummies)[-1L])  
  }
  dummies # is a matrix
}

#' @rdname make.dummies
#' @export
make.dummies.data.frame <- function(x, col, base = 1L, base.add = TRUE, ...) {

  stopifnot(inherits(col, "character"))
  dum.mat <- make.dummies.default(x[ , col], base, base.add) # dummy matrix
  colnames(dum.mat) <- paste0(col, colnames(dum.mat))
  dum.df <- data.frame(cbind("merge.col" = rownames(dum.mat), dum.mat))
 
  merge(x, dum.df, by.x = col, by.y = "merge.col", sort = FALSE)
}

#' @rdname make.dummies
#' @export
make.dummies.pdata.frame <- function(x, col, base = 1L, base.add = TRUE, ...) {
  
  stopifnot(inherits(col, "character"))
#  idx.pos <- pos.index(x)
#  drop.idx <- anyNA(idx.pos)
  idx <- attr(x, "index")
  res <- make.dummies.data.frame(x, col, base, base.add)
  # add back pdata.frame features (assumption is: merge did not change order of original data.frame)
  attr(res, "index") <- idx
  class(res) <- c("pdata.frame", class(res))
  res
}


gettvalue <- function(x, coefname) {
  ## non-exported
  ## helper function to extract one or more t value(s)
  ## (coef/s.e.) for a coefficient from model object useful if one wants
  ## to avoid the computation of a whole lot of values with summary()
  
  # x: model object (usually class plm or lm) coefname: character
  # indicating name(s) of coefficient(s) for which the t value(s) is
  # (are) requested
  # return value: named numeric vector of length == length(coefname)
  # with requested t value(s)
  beta <- coef(x)[coefname]
  se <- sqrt(diag(vcov(x))[coefname])
  tvalue <- beta / se
  return(tvalue)
}

# describe function: extract characteristics of plm and other model objects
describe <- function(x, what = c("model", "effect", "random.method",
                                 "inst.method", "transformation", "ht.method")){
  what <- match.arg(what)
  cl <- x$args
  switch(what,
         "model"          = if(!is.null(cl$model))          cl$model          else "within",
         "effect"         = if(!is.null(cl$effect))         cl$effect         else "individual",
         "random.method"  = if(!is.null(cl$random.method))  cl$random.method  else "swar",
         "inst.method"    = if(!is.null(cl$inst.method))    cl$inst.method    else "bvk",
         "transformation" = if(!is.null(cl$transformation)) cl$transformation else "d",
         "ht.method"      = if(!is.null(cl$ht.method))      cl$ht.method      else "ht"
  )
}
