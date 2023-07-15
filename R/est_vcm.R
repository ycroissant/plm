#' Variable Coefficients Models for Panel Data
#' 
#' Estimators for random and fixed effects models with variable coefficients.
#' 
#' `pvcm` estimates variable coefficients models. Individual or time
#' effects are introduced, respectively, if `effect = "individual"` 
#' (default) or `effect = "time"`.
#' 
#' Coefficients are assumed to be fixed if `model = "within"`, i.e., separate
#' pooled OLS models are estimated per individual (`effect = "individual"`)
#' or per time period (`effect = "time"`). Coefficients are assumed to be
#' random if `model = "random"` and the model by 
#' \insertCite{SWAM:70;textual}{plm} is estimated; it is a generalized least
#' squares model which uses the results of the OLS models estimated per 
#' individual/time dimension. The corresponding unbiased single coefficients, 
#' variance-covariance matrices, and standard errors of the random coefficients 
#' model are available in the returned object (see *Value*).
#' 
#' A test for parameter stability (homogeneous coefficients) of the random 
#' coefficients model is printed in the model's summary and is available in the
#' returned object (see *Value*).
#' 
#' `pvcm` objects have `print`, `summary` and `print.summary` methods.
#' 
#' @aliases pvcm
#' @param formula a symbolic description for the model to be estimated,
#' @param object,x an object of class `"pvcm"`,
#' @param data a `data.frame`,
#' @param subset see `lm`,
#' @param na.action see `lm`,
#' @param effect the effects introduced in the model: one of
#' `"individual"`, `"time"`,
#' @param model one of `"within"`, `"random"`,
#' @param index the indexes, see [pdata.frame()],
#' @param digits digits,
#' @param width the maximum length of the lines in the print output,
#' @param \dots further arguments.
#' @return An object of class `c("pvcm", "panelmodel")`, which has the
#' following elements:
#' 
#' \item{coefficients}{the vector (numeric) of coefficients (or data frame for 
#' fixed effects),}
#'
#' \item{residuals}{the vector (numeric) of residuals,}
#'
#' \item{fitted.values}{the vector of fitted values,}
#' 
#' \item{vcov}{the covariance matrix of the coefficients (a list for
#' fixed effects model (`model = "within"`)),}
#'
#' \item{df.residual}{degrees of freedom of the residuals,}
#'
#' \item{model}{a data frame containing the variables used for the
#' estimation,}
#'
#' \item{call}{the call,}
#' 
#' \item{args}{the arguments of the call,}
#' 
#' random coefficients model only (`model = "random"`)
#' \item{Delta}{the estimation of the covariance matrix of the coefficients,}
#' \item{single.coefs}{matrix of unbiased coefficients of single estimations,}
#' \item{single.var}{list of variance-covariance matrices for `single.coefs`,}
#' \item{single.std.error}{matrix of standard errors of `single.coefs`,} 
#' \item{chisq.test}{htest object: parameter stability test (homogeneous 
#'                   coefficients),} 
#' 
#' separate OLS estimations only (`model = "within"`):
#' \item{std.error}{a data frame containing standard errors for all
#' coefficients for each individual (within models only).}
#' 
#' @export
#' @author Yves Croissant, Kevin Tappe
#' @references \insertAllCited{}
#' @references \insertRef{SWAM:71}{plm}
#' @references \insertRef{GREE:18}{plm}
#' @references \insertRef{POI:03}{plm}
#'
#' @keywords regression
#' @examples
#' 
#' data("Produc", package = "plm")
#' zw <- pvcm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc, model = "within")
#' zr <- pvcm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc, model = "random")
#' 
#' ## replicate Greene (2018), p. 452, table 11.22/(2012), p. 419, table 11.14
#' summary(pvcm(log(gsp) ~ log(pc) + log(hwy) + log(water) + log(util) + log(emp) + unemp, 
#'              data = Produc, model = "random"))
#'              
#' ## replicate Poi (2003) (need data adjustment, remaining tiny diffs are due 
#' ## Poi's data set having more digits, not justified by the original Grunfeld data)
#' data(Grunfeld) # need firm = 1, 4, 3, 8, 2
#' Gr.Poi.2003 <- Grunfeld[c(1:20, 61:80, 41:60, 141:160, 21:40), ]
#' Gr.Poi.2003$firm <- rep(1:5, each = 20)
#' Gr.Poi.2003[c(86, 98), "inv"] <- c(261.6, 645.2)
#' Gr.Poi.2003[c(92), "capital"] <- c(232.6)
#' 
#' mod.poi <- pvcm(inv ~ value + capital, data = Gr.Poi.2003, model = "random")
#' summary(mod.poi)
#' print(mod.poi$single.coefs)
#' print(mod.poi$single.std.err)
#'
#' \dontrun{
#' # replicate Swamy (1971), p. 166, table 5.2
#' data(Grunfeld, package = "AER") # 11 firm Grunfeld data needed from package AER
#' gw <- pvcm(invest ~ value + capital, data = Grunfeld, index = c("firm", "year"))
#' }
#' 
pvcm <- function(formula, data, subset, na.action, effect = c("individual", "time"),
                 model = c("within", "random"), index = NULL, ...){

    effect <- match.arg(effect)
    model.name <- match.arg(model)
    data.name <- paste(deparse(substitute(data)))

    cl <- match.call(expand.dots = TRUE)
    mf <- match.call()
    mf[[1L]] <- as.name("plm")
    mf$model <- NA
    data <- eval(mf, parent.frame()) # make model.frame
    result <- switch(model.name,
                     "within" = pvcm.within(formula, data, effect),
                     "random" = pvcm.random(formula, data, effect))
    class(result) <- c("pvcm", "panelmodel")
    result$call <- cl
    result$args <- list(model = model.name, effect = effect)
    result
}

pvcm.within <- function(formula, data, effect){
    index <- attr(data, "index")
    id <- index[[1L]]
    time <- index[[2L]]
    pdim <- pdim(data)
    
    if (effect == "time"){
        cond <- time
        other <- id
        card.cond <- pdim$nT$T
    }
    else{
        cond <- id
        other <- time
        card.cond <- pdim$nT$n
    }

    # estimate single OLS regressions and save in a list
    ols <- est.ols(data, cond, effect, "within")
    
    # extract coefficients:
    coef <- matrix(unlist(lapply(ols, coef)), nrow = length(ols), byrow = TRUE)
    dimnames(coef)[1:2] <- list(names(ols), names(coef(ols[[1L]])))
    coef <- as.data.frame(coef)
    
    # extract residuals and make pseries:
    residuals <- unlist(lapply(ols, residuals))
    residuals <- add_pseries_features(residuals, index)
    
    # extract standard errors:
    vcov <- lapply(ols, vcov)
    std <- matrix(unlist(lapply(vcov, function(x) sqrt(diag(x)))), nrow = length(ols), byrow = TRUE)
    dimnames(std)[1:2] <- list(names(vcov), colnames(vcov[[1L]]))
    std <- as.data.frame(std)
    # ssr <- as.numeric(crossprod(residuals)) # not used here, so commented
    y <- unlist(split(model.response(data), cond))
    fitted.values <- y - residuals
    tss <- tss(y)
    df.resid <- pdim$nT$N - card.cond * ncol(coef)
    nopool <- list(coefficients  = coef,
                   residuals     = residuals,
                   fitted.values = fitted.values,
                   vcov          = vcov,
                   df.residual   = df.resid,
                   model         = data,
                   std.error     = std)
    nopool
}

pvcm.random <- function(formula, data, effect){
  ## Swamy (1970)
  ## see also Poi (2003), The Stata Journal: 
  ## https://www.stata-journal.com/sjpdf.html?articlenum=st0046
  ## and Stata's xtxtrc command, section Method and formulas:
  ## https://www.stata.com/manuals/xtxtrc.pdf
  index <- index(data)
  id <- index[[1L]]
  time <- index[[2L]]
  pdim <- pdim(data)
  N <- nrow(data)
  if (effect == "time"){
    cond <- time
    other <- id
    card.cond <- pdim$nT$T
  }
  else{
    cond <- id
    other <- time
    card.cond <- pdim$nT$n
  }
  
  # stopping control: later we have to calc. D1 with division by (card.cond - 1), 
  # so check here early if model is estimable
  if(!(card.cond - 1L)) {
    error.msg <- paste0("Swarmy (1970) model non-estimable due to only 1 ",
                        "group in ", effect, " dimension, but need > 1")
    stop(error.msg)
  }
  
  # estimate single OLS regressions and save in a list
  ols <- est.ols(data, cond, effect, "random")
  
  # matrix of coefficients
  coefm <- matrix(unlist(lapply(ols, coef)), nrow = length(ols), byrow = TRUE)
  dimnames(coefm)[1:2] <- list(names(ols), names(coef(ols[[1]])))
  
  # number of covariates
  K <- ncol(coefm) - has.intercept(formula)
  # check for NA coefficients
  coefna <- is.na(coefm)
  # list of model matrices
  X <- lapply(ols, model.matrix)
  # same without the covariates with NA coefficients
  # Xna <- lapply(seq_len(nrow(coefm)), function(i) X[[i]][ , !coefna[i, ], drop = FALSE])
  
  # list of model responses
  y <- lapply(ols, function(x) model.response(model.frame(x)))
  # compute a list of XpX^-1 matrices, with 0 for lines/columns with
  # NA coefficients
  xpxm1 <- lapply(seq_len(card.cond), function(i){
    z <- matrix(0, ncol(coefm), ncol(coefm),
                dimnames = list(colnames(coefm), colnames(coefm)))
    ii <- !coefna[i, ]
    z[ii, ii] <- solve(crossprod(X[[i]][ii, ii, drop = FALSE]))
    z
  })
  
  # coef.mb: compute demeaned coefficients
  coef.mb <- if(!any(coefna)) {
    t(collapse::fwithin(coefm))
  } else {
    # NA handling: in coefm, insert the mean values in place of NA coefficients (if any)
    coefm <- apply(coefm, 2, function(x){x[is.na(x)] <- mean(x, na.rm = TRUE); x})
    coefb <- colMeans(coefm, na.rm = TRUE)
    t(coefm) - coefb
  }
  
  # D1: compute the first part of the variance matrix
  D1 <- tcrossprod(coef.mb) / (card.cond - 1)
  # D2: compute the second part of the variance matrix
  sigi <- vapply(ols, function(x) deviance(x) / df.residual(x), FUN.VALUE = 0.0)
  D2 <- Reduce("+", lapply(seq_len(card.cond),
                           function(i) sigi[i] * xpxm1[[i]])) / card.cond
  # if D1-D2 semi-definite positive, use it, otherwise use D1
  # (practical solution, e.g., advertised in Poi (2003), Greene (2018), p. 452)
  ## TODO: Poi (2003) and Greene (2018) only write about positive definite (not semi-def.)
  ##       Hsiao (2014), p. 174 writes about D1-D2 possibly being not "non-negative definite"
  eig <- all(eigen(D1 - D2)$values >= 0) 
  Delta <- if(eig) { D1 - D2 } else D1
  
  # compute the Omega matrix for each individual
  Omegan <- lapply(seq_len(card.cond), function(i) {
    Xi <- X[[i]]
    diag(sigi[i], nrow = nrow(Xi)) + crossprod(t(Xi), tcrossprod(Delta, Xi))
  })
  
  # compute X'Omega X and X'Omega y for each individual
  XyOmXy <- lapply(seq_len(card.cond), function(i){
    ii <- !coefna[i, ]
    Xn <- X[[i]][ , ii, drop = FALSE]
    yn <- y[[i]]
    
    # pre-allocate matrices
    XnXn <- matrix(0, ncol(coefm), ncol(coefm), dimnames = list(colnames(coefm), colnames(coefm)))
    Xnyn <- matrix(0, ncol(coefm), 1L,          dimnames = list(colnames(coefm), "y"))
    
    solve_Omegan_i <- solve(Omegan[[i]])
    CP.tXn.solve_Omegan_i <- crossprod(Xn, solve_Omegan_i)
    XnXn[ii, ii] <- CP.tXn.solve_Omegan_i %*% Xn # == t(Xn) %*% solve(Omegan[[i]]) %*% Xn
    Xnyn[ii, ]   <- CP.tXn.solve_Omegan_i %*% yn # == t(Xn) %*% solve(Omegan[[i]]) %*% yn
    list("XnXn" = XnXn, "Xnyn" = Xnyn)
  })
  
  # Compute coefficients
  # extract and reduce XnXn (pos 1 in list's element) and Xnyn (pos 2)
  # (position-wise extraction is faster than name-based extraction)
  XpXm1 <-     solve(Reduce("+", vapply(XyOmXy, "[", 1L, FUN.VALUE = list(length(XyOmXy)))))
  beta  <- XpXm1 %*% Reduce("+", vapply(XyOmXy, "[", 2L, FUN.VALUE = list(length(XyOmXy))))
  
  beta.names <- rownames(beta)
  beta <- as.numeric(beta)
  names(beta) <- beta.names
  
## Here was start of debug control (always TRUE), removed December 6th, 2018 which has the double calc.
## was this commit: https://github.com/ycroissant/plm/commit/af5d895ccd6309448fd0ba7f5574a04a1d515550#diff-c4395861f56386ea1dc3f81a026ead695951d2de7b40293d4509ade929e5c830
  
  ## notation here follows Hsiao (2014), p. 173
  weightsn <- lapply(seq_len(card.cond),
                     function(i){
                       # YC2019/30/08
                       #old
                       #                           vcovn <- vcov(ols[[i]])
                       #                           Deltan <- Delta[! coefna[i,], ! coefna[i,]]
                       #                           wn <- solve(vcovn + Deltan)
                       #new
                       vcovn <- vcov(ols[[i]])
                       ii <- !coefna[i, ]
                       wn <- solve((vcovn + Delta)[ii, ii, drop = FALSE])
                       z <- matrix(0, nrow = ncol(coefm), ncol = ncol(coefm),
                                   dimnames = list(colnames(coefm), colnames(coefm)))
                       z[ii, ii] <- wn
                       z
                     })
  
  V <- solve(Reduce("+", weightsn)) # V: left part of W_i  (in Hsiao)
  weightsn <- lapply(weightsn, function(x) V %*% x) # full W_i (in Hsiao)
  Beta <- Reduce("+", lapply(seq_len(card.cond), function(i) weightsn[[i]] %*% coefm[i, ]))
  Beta.names <- rownames(Beta)
  Beta <- as.numeric(Beta)
  names(Beta) <- Beta.names
  XpXm1 <- V
  
  ## TODO:
  ##   * "Beta" vs "beta" - seem to be the same - so calculated twice?
  ##   * XpXm1 vs. V, seem to be the same - so calculated twice?
  ##    -> Beta/beta, XpXm1(1st calc further up) and V (XpXm1 <- V) have same result for 
  ##       balanced and unbalanced data, so seems to be superfluous calculations here
  
  ## calc. single unbiased coefficients and variance:
  solve.Delta <- solve(Delta)
  b.hat.coef.var <- lapply(seq_len(card.cond), function(i) {
    Xi <- X[[i]]
    cp.Xi <- crossprod(Xi)
    sigi.inv <- 1/sigi[i]
    # (Poi (2003), p. 304, 2nd line) (1st line in Poi (2003) is Hsiao (2014), p. 175)
    sigi.inv.cp.Xi <- sigi.inv * cp.Xi
    left<- solve(solve.Delta + sigi.inv.cp.Xi)
    right <- crossprod(sigi.inv.cp.Xi, coefm[i, ]) + crossprod(solve.Delta, Beta)
    b.hat.i <- crossprod(left, right)
    
    ## beta.hat.i: same result via formula following Hsiao (2014), p. 175 (need residuals first)
    # Beta + Delta %*% t(Xi) %*% solve(Xi %*% Delta %*% t(Xi) + sigi_i * diag(1, nrow(Xi))) %*% resid[[i]]
    
    ## Variance-Covariance matrix (asymp. formula in Poi (2003))
    # V = Var(beta-hat)
    # vcov(ols[[i]]) # = Var(bi)
    A <- crossprod(left, solve.Delta)
    IA <- diag(1, nrow(A)) - A 
    var.b.hat.i <- V + tcrossprod(IA, crossprod(vcov(ols[[i]]) - V, t(IA)))
    
    list(b.hat.i, var.b.hat.i)
  })
  
  # extract single coeffs, variance, std. error
  b.hat.i     <- t(collapse::qM(lapply(b.hat.coef.var, "[[", 1L)))
  var.b.hat.i <-                lapply(b.hat.coef.var, "[[", 2L)
  rm(b.hat.coef.var)
  std.err.b.hat.i <- lapply(var.b.hat.i, function(i) sqrt(diag(i)))
  std.err.b.hat.i <- t(collapse::qM(std.err.b.hat.i))
  rownames(std.err.b.hat.i) <- rownames(b.hat.i) <- names(var.b.hat.i) <- names(sigi)
  
## Here was end of debug control (always TRUE), removed December 6th, 2018 leading to the double calc.
  
  
  y <- pmodel.response(data)
  X <- model.matrix(data)
  fit <- as.numeric(tcrossprod(beta, X))
  res <- y - fit
  df.resid <- N - ncol(coefm)
  
  ## Chi-sq test for homogeneous parameters (all panel-effect-specific coefficients are the same)
  #  notation resembles Greene (2018), ch. 11, p. 452
  # TODO: 
  #       * this is a crude but correct implementation, improve one day
  #       * need to have it in the model estimation due to the many inputs or can be a separate function?
  V.t <- lapply(seq_len(card.cond), function(i) sigi[i] * xpxm1[[i]])
  V.t.inv <- lapply(V.t, function(i) solve(i))
  b <- collapse::mrtl(coefm) # create list based on matrix rows
  b.left <- solve(Reduce("+", V.t.inv))
  b.right <- Reduce("+", lapply(seq_len(card.cond), function(i) crossprod(V.t.inv[[i]], b[[i]])))
  b.star <- as.numeric(crossprod(b.left, b.right))
  chi.sq.stat <- as.numeric(Reduce("+", mapply(function(coefs, mat) {
    bi.bstar <- coefs - b.star
    tcrossprod(crossprod(bi.bstar, mat), bi.bstar)}, # == t(b[[i]] - b.star) %*% V.t.inv[[i]] %*% (b[[i]] - b.star)
    b, V.t.inv)))
  
  # pchisq(chi.sq.stat, df = 12,  lower.tail = FALSE) # Poi (2003): stat: 603.9944, df = 12
  # pchisq(chi.sq.stat, df = 329, lower.tail = FALSE) # Greene (2018): stat: 25556,26, df = 329 (=7*47)
  
  chi.sq.df <- ncol(coefm) * (pdim$nT$n - 1L)
  chi.sq.p  <- pchisq(chi.sq.stat, df = ncol(coefm) * (pdim$nT$n - 1L), lower.tail = FALSE)
  
  chi.sq.test <- list(statistic   = c("chisq" = chi.sq.stat),
                      parameter   = c("df"    = chi.sq.df),
                      method      = "Test for parameter homogeniety",
                      p.value     = chi.sq.p,
                      alternative = "Heterogeneous parameters (panel-effect-specific coefficients differ)",
                      data.name   = paste(deparse(formula)))
  
  list(coefficients   = beta,
       residuals      = res,
       fitted.values  = fit,
       vcov           = XpXm1,
       df.residual    = df.resid,
       model          = data,
       Delta          = Delta,
       single.coefs   = b.hat.i,
       single.var     = var.b.hat.i,
       single.std.err = std.err.b.hat.i,
       chisq.test     = structure(chi.sq.test, class = "htest"))
}

#' @rdname pvcm
#' @export
summary.pvcm <- function(object, ...) {
  model <- describe(object, "model")
  if (model == "random") {
    coef_wo_int <- object$coefficients[!(names(coef(object)) %in% "(Intercept)")]
    int.only <- !length(coef_wo_int)
    object$waldstatistic <- if(!int.only) pwaldtest(object) else NULL
    std.err <- sqrt(diag(vcov(object)))
    b <- object$coefficients
    z <- b / std.err
    p <- 2 * pnorm(abs(z), lower.tail = FALSE)
    coef <- cbind(b, std.err, z, p)
    colnames(coef) <- c("Estimate", "Std. Error", "z-value", "Pr(>|z|)")
    object$coefficients <- coef
  }
  object$ssr <- deviance(object)
  object$tss <- tss(unlist(model.frame(object)))
  object$rsqr <- 1 - object$ssr / object$tss
  class(object) <- c("summary.pvcm", "pvcm")
  return(object)
}

#' @rdname pvcm
#' @export
print.summary.pvcm <- function(x, digits = max(3, getOption("digits") - 2),
                               width = getOption("width"), ...) {
    effect <- describe(x, "effect")
    formula <- formula(x)
    model <- describe(x, "model")
    cat(paste(effect.pvcm.list[effect], " ", sep = ""))
    cat(paste(model.pvcm.list[model], "\n", sep = ""))
    cat("\nCall:\n")
    print(x$call)
    cat("\n")
    print(pdim(model.frame(x)))
    cat("\nResiduals:\n")
    print(sumres(x))
    if(model == "random") {
      cat("\nEstimated mean of the coefficients:\n")
      printCoefmat(x$coefficients, digits = digits)
      cat("\nEstimated variance of the coefficients:\n")
      print(x$Delta, digits = digits)
    }
    if(model == "within") {
      cat("\nCoefficients:\n")
      print(summary(x$coefficients))
    }
    cat("\n")
    cat(paste0("Total Sum of Squares: ",    signif(x$tss, digits), "\n"))
    cat(paste0("Residual Sum of Squares: ", signif(x$ssr, digits), "\n"))
    cat(paste0("Multiple R-Squared: ",      signif(x$rsqr, digits), "\n"))
    if (model == "random" && !is.null(waldstat <- x$waldstatistic)) {
      cat(paste0("Chisq: ", signif(waldstat$statistic), " on ",
          waldstat$parameter, " DF, p-value: ",
          format.pval(waldstat$p.value, digits = digits), "\n"))
    }
    if(model == "random") {
      cat("Test for parameter homogeneity: ")
      cat(paste("Chisq = ", signif(x$chisq.test$statistic),
                " on ", x$chisq.test$parameter,
                " DF, p-value: ", format.pval(x$chisq.test$p.value, digits = digits), "\n", sep=""))
    }
    invisible(x)
  }


est.ols <- function(mf, cond, effect, model) {
## helper function: estimate the single OLS regressions (used for pvcm's model = "random" as well as "within" )
  ml <- split(mf, cond)
  #ml <- collapse::rsplit(mf, cond) # does not yet work - TODO: check why (comment stemming from random model)
  ols <- lapply(ml, function(x) {
      X <- model.matrix(x)
      if(    (nrow(X) <  ncol(X)) && model == "within"
          || (nrow(X) <= ncol(X)) && model == "random") {
        ## catch non-estimable model
        ## equality NROW NCOL: can estimate coefficients but not variance
        ##             -> for "within" model: this is ok (output has variance NA/NaN)
        #              -> or "random" model:variance is needed for D2, chi-sq test, so rather be strict
        error.msg <- paste0("insufficient number of observations for at least ",
                            "one group in ", effect, " dimension, so defined ",
                            "model is non-estimable")
        stop(error.msg)
      }
      y <- pmodel.response(x)
      r <- lm(y ~ X - 1, model = FALSE)
      names(r$coefficients) <- colnames(X)
      r})
  ols
}
