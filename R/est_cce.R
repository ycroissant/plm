## Common Correlated Effects Pooled/MG estimators
  ## ref. Holly, Pesaran and Yamagata JoE 158 (2010)
  ## (also Kapetanios, Pesaran and Yamagata JoE 2011)
  ## CCEP and CCEMG together in the same SW framework
  ## based on generalized FEs

  ## this version 6: includes both defactored (cce) and raw (standard) residuals,
  ## leaving to a special residuals.pcce method the choice of which to retrieve

  ## NB the effect of including a trend is exactly the same as for
  ## including as.numeric(<timeindex>) in the model specification
  ## If the panel is unbalanced, though, then for some i the trend becomes
  ## (3,4,5, ...) instead of (1,2,3, ...); the difference is absorbed by
  ## the individual intercept, and *the group intercept* changes.

## needed for standalone operation:
#plm <- plm:::plm
#pdim <- plm:::pdim

#model.matrix.plm <- plm:::model.matrix.plm
#pmodel.response.plm <- plm:::pmodel.response.plm

#tss <- plm:::tss


#' Common Correlated Effects estimators
#' 
#' Common Correlated Effects Mean Groups (CCEMG) and Pooled (CCEP)
#' estimators for panel data with common factors (balanced or
#' unbalanced)
#' 
#' `pcce` is a function for the estimation of linear panel models by
#' the Common Correlated Effects Mean Groups or Pooled estimator,
#' consistent under the hypothesis of unobserved common factors and
#' idiosyncratic factor loadings. The CCE estimator works by
#' augmenting the model by cross-sectional averages of the dependent
#' variable and regressors in order to account for the common factors,
#' and adding individual intercepts and possibly trends.
#' 
#' @aliases pcce
#' @param formula a symbolic description of the model to be estimated,
#' @param object,x an object of class `"pcce"`,
#' @param data a `data.frame`,
#' @param subset see `lm`,
#' @param na.action see `lm`,
#' @param model one of `"mg"`, `"p"`, selects Mean Groups vs. Pooled
#'     CCE model,
#' @param index the indexes, see [pdata.frame()],
#' @param trend logical specifying whether an individual-specific
#'     trend has to be included,
#' @param digits digits,
#' @param width the maximum length of the lines in the print output,
#' @param type one of `"defactored"` or `"standard"`,
#' @param vcov a variance-covariance matrix furnished by the user or a function to calculate one,
#' @param \dots further arguments.
#' @return An object of class `c("pcce", "panelmodel")` containing:
#'     \item{coefficients}{the vector of coefficients,}
#'     \item{residuals}{the vector of (defactored) residuals,}
#'     \item{stdres}{the vector of (raw) residuals,}
#'     \item{tr.model}{the transformed data after projection on H,}
#'     \item{fitted.values}{the vector of fitted values,}
#'     \item{vcov}{the covariance matrix of the coefficients,}
#'     \item{df.residual}{degrees of freedom of the residuals,}
#'     \item{model}{a data.frame containing the variables used for the
#'     estimation,}
#'     \item{call}{the call,}
#'     \item{indcoef}{the matrix of individual coefficients from
#'     separate time series regressions,}
#'     \item{r.squared}{numeric, the R squared.}
#' @export
#' @importFrom MASS ginv
#' @importFrom collapse rsplit gsplit GRP GRPN
#' @author Giovanni Millo
#' @references
#'
#' \insertRef{kappesyam11}{plm}
#' 
#' \insertRef{HOLL:PESA:YAMA:10}{plm}
#' 
#' @keywords regression
#' @examples
#' 
#' data("Produc", package = "plm")
#' ccepmod <- pcce(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc, model="p")
#' summary(ccepmod)
#' summary(ccepmod, vcov = vcovHC) # use argument vcov for robust std. errors
#' 
#' ccemgmod <- pcce(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc, model="mg")
#' summary(ccemgmod)
#' 
pcce <- function (formula, data, subset, na.action,
                  model = c("mg", "p"),
                   #residuals = c("defactored", "standard"),
                  index = NULL, trend = FALSE, ...) {
  
  ## TODO: in general: 
  ##    * consider parallel execution via mclapply/mcmapply (aligns with the 
  ##      split-only-once aspect mentioned above).
  
  ## Create a Formula object if necessary (from plm)
  if (!inherits(formula, "Formula")) formula <- as.Formula(formula)

  ## same as pggls but for effect, fixed at "individual" for compatibility
  ## ind for id, tind for time, k for K, coefnam for coef.names
  effect <- "individual"

  ## record call etc.
  model <- match.arg(model)
  model.name <- paste("cce", model, sep="")
  data.name <- paste(deparse(substitute(data)))
  cl <- match.call()
  plm.model <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "na.action", "effect",
      "model", "index"), names(plm.model), 0)
  plm.model <- plm.model[c(1L, m)]
  plm.model[[1L]] <- as.name("plm")
  ## change the 'model' in call
  plm.model$model <- "pooling"
  ## evaluates the call, modified with model == "pooling", inside the
  ## parent frame resulting in the pooling model on formula, data
  plm.model <- eval(plm.model, parent.frame())
  mf <- model.frame(plm.model)
  index <- unclass(attr(mf, "index")) # unclass for speed
  ind  <- index[[1L]] ## individual index
  tind <- index[[2L]] ## time index
  ## set dimension variables
  pdim <- pdim(plm.model)
  nt <- pdim$Tint$nt
  Ti <- pdim$Tint$Ti
  T. <- pdim$nT$T
  n <- pdim$nT$n
  N <- pdim$nT$N
  ## set index names
  time.names <- pdim$panel.names$time.names
  id.names   <- pdim$panel.names$id.names
  coef.names <- names(coef(plm.model))
  ## number of coefficients
  k <- length(coef.names)

  ## model data
  X <- model.matrix(plm.model)
  y <- model.response(mf)

  ## det. *minimum* group numerosity
  t <- min(Ti)

  ## check min. t numerosity
    ## NB it is also possible to allow estimation if there *is* one group
    ## with t large enough and average on coefficients removing NAs
    ## Here we choose the explicit way: let estimation fail if we lose df
    ## but a warning would do...
  if(t < (k+1)) stop("Insufficient number of time periods")

  ## one regression for each group i in 1..n
  ## and retrieve coefficients putting them into a matrix
  ## (might be unbalanced => t1 != t2 but we don't care as long
  ## as min(t) > k+1)

  has.int <- attr(terms(plm.model), "intercept")
  if(has.int) {
    ## subtract intercept from parms number and names
      k <- k - 1
      coef.names <- coef.names[-1L]
      
    ## must put the intercept into the group-invariant part!!
    ## so first drop it from X
      X <- X[ , -1L, drop = FALSE]
  }

  ## "pre-allocate" coefficients matrix for the n models
  ## (dimensions are known in advance/by now)
  tcoef <- matrix(NA_real_, nrow = k, ncol = n)

  ## pre-allocate residuals lists for individual regressions
  ## (lists allow for unbalanced panels)
  cceres <- vector("list", n)
  stdres <- vector("list", n)
  
  ## CCE by-group estimation

  ## group-invariant part, goes in Hhat
    ## between-periods transformation (take means over groups for each t)
    Xm <- Between(X, effect = tind, na.rm = TRUE)
    ym <- as.numeric(Between(y, effect = "time", na.rm = TRUE))

    Hhat <- if(has.int) cbind(ym, Xm, 1L) else cbind(ym, Xm)

    ## pre-allocate XMX, XMy arrays
    ## (dimensions are known in advance/by now)
    XMX <- array(data = NA_real_, dim = c(k, k, n))
    XMy <- array(data = NA_real_, dim = c(k, 1L, n))
    
    ## pre-allocate MX, My for list of transformed data, 
    ## later reduced to matrix and numeric, respectively
    ## (dimensions of n matrices/numerics to be hold by MX/My are not known in
    ## advance, depend on time periods per individual -> hence use list)
    MX <- vector("list", length = n)
    My <- vector("list", length = n)

    ## hence calc. beta_i anyway because of vcov

    ## for each x-sect. i=1..n estimate (over t) the CCE for every TS
    ## as in KPY, eq. 15
    
    # split X, y, Hhat by individual and store in lists
    ind.GRP <- collapse::GRP(ind)
    tX.list <- collapse::rsplit(X, ind.GRP, use.names = FALSE)
    
    ty.list <- collapse::gsplit(y, ind.GRP)
    
    tHhat.list <- collapse::rsplit(Hhat, ind.GRP, use.names = FALSE)
    tMhat.list <- vector("list", length = n) # pre-allocate
 
    for(i in seq_len(n)) {
      tX <- tX.list[[i]]
      ty <- ty.list[[i]]
      tHhat <- tHhat.list[[i]]

      ## if 'trend' then augment the xs-invariant component 
      if(trend) tHhat <- cbind(tHhat, seq_len(dim(tHhat)[[1L]]))
      
      ## NB tHHat, tMhat should be i-invariant (but for the
      ## group size if unbalanced)
      tMhat <- diag(1, length(ty)) -
                crossprod(t(tHhat), solve(crossprod(tHhat), t(tHhat)))
      
      ## tMhat is needed again later, so save in list
      tMhat.list[[i]] <- tMhat
 
      CP.tXtMhat <- crossprod(tX, tMhat)
      tXMX <- tcrossprod(CP.tXtMhat, t(tX))
      tXMy <- tcrossprod(CP.tXtMhat, t(ty))
      
      ## XMX_i, XMy_i
      XMX[ , , i] <- tXMX
      XMy[ , , i] <- tXMy
      
      ## save transformed data My, MX for vcovHC use
      ## (NB M is symmetric)
      MX[[i]] <- t(CP.tXtMhat)
      My[[i]] <- crossprod(tMhat, ty)

      ## single CCE coefficients
      tcoef[ , i] <- crossprod(ginv(tXMX), tXMy) # solve(tXMX, tXMy)
        ## USED A GENERALIZED INVERSE HERE BECAUSE OF PBs WITH ECM SPECS
        ## Notice remark in Pesaran (2006, p.977, between (27) and (28))
        ## that XMX.i is invariant to the choice of a g-inverse for H'H
    }

    # Reduce transformed data to matrix and numeric, respectively
    MX <- Reduce(rbind, MX)
    My <- Reduce(c, My)
    
    # set names lost in processing above
    dimnames(MX) <- list(rownames(X), colnames(X))
    names(My) <- names(y)

  ## end data module

    ## CCEMG coefs are averages across individual regressions
    ## (here: coefs of xs-variants only!)
    coefmg <- rowMeans(tcoef)

    ## make demeaned coefficients: b_i - b_CCEMG
    demcoef <- tcoef - coefmg # coefmg gets recycled n times by column
    
    ## pre-allocate matrix of cross-products of demeaned individual coefficients
    Rmat <- array(data = NA_real_, dim = c(k, k, n))

    ## calc. coef, vcov, and residuals according to model
    switch(model,
        "mg" = {
            ## assign beta CCEMG
            coef <- coefmg

            ## calc CCEMG covariance:
            ## (HPY 2010, p. 163, between (3.10) and (3.11) / KPY 2011, p. 330 (38))
            for(i in seq_len(n)) Rmat[ , , i] <- outer(demcoef[ , i], demcoef[ , i])
            vcov <- 1/(n*(n-1)) * rowSums(Rmat, dims = 2L) # == 1/(n*(n-1)) * apply(Rmat, 1:2, sum), but rowSums(., dims = 2L)-construct is way faster

            ## calc CCEMG residuals, both defactored and raw
            for(i in seq_len(n)) {
              ## must redo all this because needs b_CCEP, which is
              ## not known at by-groups step
              tX <- tX.list[[i]]
              ty <- ty.list[[i]]
              tMhat <- tMhat.list[[i]]
              tb <- tcoef[ , i]
              
              ## cce (defactored) residuals as M_i(y_i - X_i * bCCEMG_i)
              tytXtb      <- ty - tcrossprod(tX, t(tb))
              cceres[[i]] <- tcrossprod(tMhat, t(tytXtb))
              ## std. (raw) residuals as y_i - X_i * bCCEMG_i - a_i
              ta <- mean(ty - tX)
              stdres[[i]] <- tytXtb - ta
            }
        },
           
        "p" = {
            ## calc beta_CCEP
            sXMX <- rowSums(XMX, dims = 2L) # == apply(XMX, 1:2, sum), but rowSums(., dims = 2L)-construct is way faster
            sXMy <- rowSums(XMy, dims = 2L) # == apply(XMy, 1:2, sum), but rowSums(., dims = 2L)-construct is way faster
            coef <- solve(sXMX, sXMy) # bCCEP in HPY

            ## calc CCEP covariance:
            ## (HPY 2010, p. 163-4, (3.12, 3.13)
            for(i in seq_len(n)) {
              Rmat[ , , i] <- crossprod(XMX[ , , i], 
                                        crossprod(outer(demcoef[ , i],
                                                        demcoef[ , i]), 
                                                  XMX[ , , i]))
            }
            
            ## summing over the n-dimension of the array we get the
            ## covariance matrix of coefs
            R.star <- 1/(n-1) * rowSums(Rmat, dims = 2L) * 1/(t^2) # rowSums(Rmat, dims = 2L) faster than == apply(Rmat, 1:2, sum)
    
            psi.star <- 1/N * sXMX
            Sigmap.star <-  tcrossprod(solve(psi.star, R.star), solve(psi.star))
            vcov <- Sigmap.star/n

            ## calc CCEP residuals, both defactored and raw
            for(i in seq_len(n)) {
                tX <- tX.list[[i]]
                ty <- ty.list[[i]]
                tMhat <- tMhat.list[[i]]
    
                ## cce residuals as M_i(y_i - X_i * bCCEP)
                tytXcoef    <- ty - tcrossprod(tX, t(coef))
                cceres[[i]] <- tcrossprod(tMhat, t(tytXcoef))
                
                ## std. (raw) residuals as y_i - X_i * bCCEP - a_i
                # (HPY, p. 165 (left column at the bottom))
                ta <- mean(ty - tX)
                stdres[[i]] <- tytXcoef - ta
            }
    })

    ## calc. measures of fit according to model type
    switch(model,
        "mg" = {
            ## R2 as in HPY 2010: sigma2ccemg = average (over n) of variances
            ## of defactored residuals
            ## (for unbalanced panels, each variance is correctly normalized
            ## by group dimension T.i)
            ##
            ## If balanced, would simply be
            ## sum(unlist(cceres)^2)/(n*(T.-2*k-2))
    
            ## average variance of defactored residuals sigma2ccemg as in
            ## Holly, Pesaran and Yamagata, (3.14)
            sigma2cce.i <- vapply(cceres,
                                  function(cceres.i)
                                    crossprod(cceres.i) * 1/(length(cceres.i)-2*k-2),
                                  FUN.VALUE = 0.0, USE.NAMES = FALSE)
            sigma2cce <- 1/n*sum(sigma2cce.i)
        },
           
        "p" = {
            ## variance of defactored residuals sigma2ccep as in Holly,
            ## Pesaran and Yamagata, (3.15)
            sigma2cce <- 1/(n*(T.-k-2)-k)*
                sum(vapply(cceres, crossprod, FUN.VALUE = 0.0, USE.NAMES = FALSE))
            ## is the same as sum(unlist(cceres)^2)
    })

    ## calc. overall R2, CCEMG or CCEP depending on 'model'
    sigma2.i <- collapse::gsplit(y, ind.GRP)
    sigma2.i <- lapply(sigma2.i, function(y.i) {
                    as.numeric(crossprod(y.i - mean(y.i)))/(length(y.i)-1)})
    sigma2y <- mean(unlist(sigma2.i, use.names = FALSE))
    r2cce <- 1 - sigma2cce/sigma2y

    ## allow outputting different types of residuals, defactored residuals are
    ## default/go into slot 'residuals'
    stdres    <- unlist(stdres)
    residuals <- unlist(cceres)

    ## add transformed data (for now a simple list)
    tr.model <- list(y = My, X = MX)
    ## so that if the model is ccepmod,
    ## > lm(ccepmod$tr.model[["y"]] ~ ccepmod$tr.model[["X"]]-1)
    ## reproduces the model results

    ## Final model object:
    ## code as in pggls, differences:
    ## - here there is no 'sigma'
    ## - there are two types of residuals
    ## - transformed data My, MX are included for vcovHC usage
    df.residual <- nrow(X) - ncol(X)
    fitted.values <- y - residuals
    coef <- as.numeric(coef)
    names(coef) <- rownames(vcov) <- colnames(vcov) <- coef.names
    dimnames(tcoef) <- list(coef.names, id.names)
    pmodel <- list(model.name = model.name)
    pccemod <- list(coefficients  = coef,
                    residuals     = residuals,
                    stdres        = stdres,
                    tr.model      = tr.model,
                    fitted.values = fitted.values,
                    vcov          = vcov,
                    df.residual   = df.residual,
                    model         = mf,
                    indcoef       = tcoef,
                    r.squared     = r2cce,
                    #cceres   = as.vector(cceres),
                    #ccemgres = as.vector(ccemgres),
                    formula       = formula,
                    call          = cl)
    pccemod <- structure(pccemod, pdim = pdim, pmodel = pmodel)
    class(pccemod) <- c("pcce", "panelmodel")
    pccemod
}

#' @rdname pcce
#' @export
summary.pcce <- function(object, vcov = NULL, ...){
  vcov_arg <- vcov
  std.err <- if (!is.null(vcov_arg)) {
    if (is.matrix(vcov_arg))   rvcov <- vcov_arg
    if (is.function(vcov_arg)) rvcov <- vcov_arg(object)
    sqrt(diag(rvcov))
  } else {
    sqrt(diag(stats::vcov(object)))
  }
  b <- object$coefficients
  z <- b/std.err
  p <- 2*pnorm(abs(z), lower.tail = FALSE)
  CoefTable <- cbind(b, std.err, z, p)
  colnames(CoefTable) <- c("Estimate", "Std. Error", "z-value", "Pr(>|z|)")
  object$CoefTable <- CoefTable
  y <- object$model[[1L]]
  object$tss <- tss(y)
  object$ssr <- as.numeric(crossprod(residuals(object)))
  object$rsqr <- object$r.squared #1-object$ssr/object$tss
  ## add some info to summary.pcce object 
  # robust vcov (next to "normal" vcov)
  if (!is.null(vcov_arg)) {
    object$rvcov <- rvcov
    rvcov.name <- paste0(deparse(substitute(vcov)))
    attr(object$rvcov, which = "rvcov.name") <- rvcov.name 
  }
  class(object) <- c("summary.pcce")
  return(object)
}

#' @rdname pcce
#' @export
print.summary.pcce <- function(x, digits = max(3, getOption("digits") - 2), width = getOption("width"), ...){
  pmodel <- attr(x, "pmodel")
  pdim   <- attr(x, "pdim")
  cat("Common Correlated Effects ")
  cat(paste(model.pcce.list[pmodel$model.name], "\n", sep = ""))
  if (!is.null(x$rvcov)) {
    cat("\nNote: Coefficient variance-covariance matrix supplied: ", attr(x$rvcov, which = "rvcov.name"), "\n", sep = "")
  }
  cat("\nCall:\n")
  print(x$call)
  cat("\n")
  print(pdim)
  cat("\nResiduals:\n")
  print(sumres(x))
  cat("\nCoefficients:\n")
  printCoefmat(x$CoefTable, digits = digits)
  cat(paste("Total Sum of Squares: ",    signif(x$tss,  digits), "\n", sep=""))
  cat(paste("Residual Sum of Squares: ", signif(x$ssr,  digits), "\n", sep=""))
  cat(paste("HPY R-squared: ",           signif(x$rsqr, digits), "\n", sep=""))
  invisible(x)
}

#' @rdname pcce
#' @export
residuals.pcce <- function(object,
                           type = c("defactored", "standard"),
                           ...) {
    ## special resid() method for pcce: allows to extract either
    ## defactored residuals (default) or raw residuals
    defres <- pres(object)
    switch(match.arg(type),
           "standard" = {
               ## add panel features and names from 'defres'
               residuals <- add_pseries_features(object$stdres, index(defres))
               names(residuals) <- names(defres)
              },
           "defactored" = { residuals <- defres }
           )
    return(residuals)
}

#' @rdname pcce
#' @export
model.matrix.pcce <- function(object, ...) {
    object$tr.model$X
}

#' @rdname pcce
#' @export
pmodel.response.pcce <- function(object, ...) {
    object$tr.model$y
}
