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
#' individual/time dimension (coefficient estimates are weighted averages of the
#' single OLS estimates with weights inversely proportional to the 
#' variance-covariance matrices). The corresponding unbiased single coefficients, 
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
#' random coefficients model only (`model = "random"`):
#' \item{Delta}{the estimation of the covariance matrix of the coefficients,}
#' \item{single.coefs}{matrix of unbiased coefficients of single estimations,}
#' \item{single.vcov}{list of variance-covariance matrices for `single.coefs`,}
#' \item{single.std.error}{matrix of standard errors of `single.coefs`,} 
#' \item{chisq.test}{htest object: parameter stability test (homogeneous 
#'                   coefficients),} 
#' 
#' separate OLS estimations only (`model = "within"`):
#' \item{std.error}{a data frame containing standard errors for all
#' coefficients for each single regression.}
#' 
#' @export
#' @author Yves Croissant, Kevin Tappe
#' @references \insertAllCited{}
#' @references \insertRef{SWAM:71}{plm}
#' @references \insertRef{GREE:18}{plm}
#' @references \insertRef{POI:03}{plm}
#' @references \insertRef{KLEI:ZEIL:10}{plm}
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
#' # close replication of Swamy (1970), (7.4) [remaining diffs likely due to less 
#' # precise numerical methods in the 1970, as supposed in Kleiber/Zeileis (2010), p. 9]
#' gr <- pvcm(invest ~ value + capital, data = Grunfeld, index = c("firm", "year"), model = "random")
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
    residuals <- unlist(sapply(ols, residuals, simplify = FALSE, USE.NAMES = FALSE), use.names = TRUE)
    residuals <- add_pseries_features(residuals, index)
    
    # extract standard errors:
    vcov <- lapply(ols, vcov)
    std <- matrix(unlist(lapply(vcov, function(x) sqrt(diag(x)))), nrow = length(ols), byrow = TRUE)
    dimnames(std)[1:2] <- list(names(vcov), colnames(vcov[[1L]]))
    std <- as.data.frame(std)
    y <- unlist(rsplit(model.response(data), cond), use.names = FALSE)
    fitted.values <- y - residuals
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
    error.msg <- paste0("Swamy (1970) model non-estimable due to only 1 ",
                        "group in ", effect, " dimension, but need > 1")
    stop(error.msg)
  }
  
  
  # estimate single OLS regressions and save in a list
  ols <- est.ols(data, cond, effect, "random")
  
  # matrix of coefficients
  coefm <- matrix(unlist(lapply(ols, coef)), nrow = length(ols), byrow = TRUE)
  dimnames(coefm)[1:2] <- list(names(ols), names(coef(ols[[1]])))
  
  # save these as used quite often
  seq_len.card.cond <- seq_len(card.cond)
  colnms.coefm <- colnames(coefm)
  nrcols.coefm <- ncol(coefm)
  
  # number of covariates
  K <- nrcols.coefm - has.intercept(formula)
  # check for NA coefficients
  coefna <- is.na(coefm)
  # list of model matrices
  X <- lapply(ols, model.matrix)
  # same without the covariates with NA coefficients
  # Xna <- lapply(seq_len(nrow(coefm)), function(i) X[[i]][ , !coefna[i, ], drop = FALSE])
  
  # compute a list of XpX and XpX^-1 matrices, with 0 for lines/columns 
  # corresponding to the NA coefficients of coefm
  xpx <- lapply(seq_len.card.cond, function(i){
    cp <- matrix(0, nrow = nrcols.coefm, ncol = nrcols.coefm,
                    dimnames = list(colnms.coefm, colnms.coefm))
    ii <- !coefna[i, ]
    cp[ii, ii] <- crossprod(X[[i]][ii, ii, drop = FALSE])
    cp
  })
  
  xpxm1 <- lapply(seq_len.card.cond, function(i){
    inv <- matrix(0, nrow = nrcols.coefm, ncol = nrcols.coefm,
                   dimnames = list(colnms.coefm, colnms.coefm))
    ii <- !coefna[i, ]
    inv[ii, ii] <- solve(xpx[[i]][ii, ii])
    inv
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
  sigi.xpxm1 <- lapply(seq_len.card.cond, function(i) sigi[i] * xpxm1[[i]])
  D2 <- Reduce("+", sigi.xpxm1) / card.cond
  # if D1-D2 semi-definite positive, use it, otherwise use D1
  # (practical solution, e.g., advertised in Poi (2003), Greene (2018), p. 452)
  ## Poi (2003) and Greene (2018) only write about positive definite (not semi-def.)
  ## Hsiao (2014), p. 174 writes about D1-D2 possibly being not "non-negative definite"
  ## -> stick with pos. semi-def.
  Delta <- D1 - D2
  eig <- all(eigen(Delta)$values >= 0)
  if(!eig) Delta <- D1

  ### these calculations are superfluous because down below Beta is calculated
  ### via weightsn etc.
  # # compute the Omega matrix for each individual
  # Omegan <- lapply(seq_len(card.cond), function(i) {
  #   #    Xi <- X[[i]] ## this Xi has a column for the NA coef
  #   ### adding these three lines leads to matching the "weightsn" approach
  #   Xi <- matrix(0, nrow(X[[i]]), ncol(X[[i]]))
  #   ii <- !coefna[i, ]
  #   Xi[ , ii] <- X[[i]][, ii]
  #   
  #   diag(sigi[i], nrow = nrow(Xi)) + crossprod(t(Xi), tcrossprod(Delta, Xi))
  # })
  # 
  # # list of model responses
  # y <- lapply(ols, function(x) model.response(model.frame(x)))
  # 
  # # compute X'Omega X and X'Omega y for each individual
  # XyOmXy <- lapply(seq_len(card.cond), function(i){
  #   ii <- !coefna[i, ]
  #   Xn <- X[[i]][ , ii, drop = FALSE]
  #   yn <- y[[i]]
  # 
  #   # pre-allocate matrices
  #   XnXn <- matrix(0, ncol(coefm), ncol(coefm), dimnames = list(colnames(coefm), colnames(coefm)))
  #   Xnyn <- matrix(0, ncol(coefm), 1L,          dimnames = list(colnames(coefm), "y"))
  # 
  #   solve_Omegan_i <- solve(Omegan[[i]])
  #   CP.tXn.solve_Omegan_i <- crossprod(Xn, solve_Omegan_i)
  #   XnXn[ii, ii] <- CP.tXn.solve_Omegan_i %*% Xn # == t(Xn) %*% solve(Omegan[[i]]) %*% Xn
  #   Xnyn[ii, ]   <- CP.tXn.solve_Omegan_i %*% yn # == t(Xn) %*% solve(Omegan[[i]]) %*% yn
  #   list("XnXn" = XnXn, "Xnyn" = Xnyn)
  # })
  # 
  # # Compute coefficients
  # # extract and reduce XnXn (pos 1 in list's element) and Xnyn (pos 2)
  # # (position-wise extraction is faster than name-based extraction)
  #  XpXm1 <-     solve(Reduce("+", vapply(XyOmXy, "[", 1L, FUN.VALUE = list(length(XyOmXy)))))
  #  beta  <- XpXm1 %*% Reduce("+", vapply(XyOmXy, "[", 2L, FUN.VALUE = list(length(XyOmXy))))
  # 
  # beta.names <- rownames(beta)
  # beta <- as.numeric(beta)
  # names(beta) <- beta.names

  ## notation here follows Hsiao (2014), p. 173
  weightsn <- lapply(seq_len.card.cond,
                     function(i){
                       vcovn <- vcov(ols[[i]])
                       ii <- !coefna[i, ]
                       wn <- solve((vcovn + Delta)[ii, ii, drop = FALSE])
                       z <- matrix(0, nrow = nrcols.coefm, ncol = nrcols.coefm)
                       z[ii, ii] <- wn
                       z
                     })
  
  V <- solve(Reduce("+", weightsn)) # V = var(Beta-hat): left part of W_i in Hsiao (6.2.9)
  weightsn <- lapply(weightsn, function(x) crossprod(V, x)) # full W_i in Hsiao (6.2.9)
  Beta <- as.numeric(Reduce("+", lapply(seq_len.card.cond, function(i) tcrossprod(weightsn[[i]], t(coefm[i, ])))))
  names(Beta) <- colnames(coefm)
  
  ## calc. single unbiased coefficients and variance:
  solve.Delta <- solve(Delta)
  b.hat.coef.var <- lapply(seq_len.card.cond, function(i) {
    # (Poi (2003), p. 304, 2nd line) (1st line in Poi (2003) is Hsiao (2014), p. 175)
    sigi.inv.cp.Xi <- 1/sigi[i] * xpx[[i]]
    left <- solve(solve.Delta + sigi.inv.cp.Xi)
    right <- crossprod(sigi.inv.cp.Xi, coefm[i, ]) + crossprod(solve.Delta, Beta)
    b.hat.i <- crossprod(left, right)
    
    ## beta.hat.i: same result via formula following Hsiao (2014), p. 175 (need residuals first)
    # Beta + Delta %*% t(Xi) %*% solve(Xi %*% Delta %*% t(Xi) + sigi_i * diag(1, nrow(Xi))) %*% resid[[i]]
    
    ## Variance-Covariance matrix (asymp. formula in Poi (2003))
    # V = Var(beta-hat)
    # vcov(ols[[i]]) # = Var(bi)
    A <- crossprod(left, solve.Delta)
    IA <- diag(1, nrow(A)) - A
    var.b.hat.i <- V + crossprod(t(IA), crossprod((vcov(ols[[i]]) - V), t(IA)))
    
    list(b.hat.i, var.b.hat.i)
  })
  
  # extract single coeffs, variance, std. error
  b.hat.i     <- t(collapse::qM(lapply(b.hat.coef.var, "[[", 1L)))
  var.b.hat.i <-                lapply(b.hat.coef.var, "[[", 2L)
  rm(b.hat.coef.var) # not needed anymore (rm to save space)
  std.err.b.hat.i <- lapply(var.b.hat.i, function(i) sqrt(diag(i)))
  std.err.b.hat.i <- t(collapse::qM(std.err.b.hat.i))
  rownames(std.err.b.hat.i) <- rownames(b.hat.i) <- names(var.b.hat.i) <- names(sigi)
  
  fit <- as.numeric(tcrossprod(Beta, model.matrix(data)))
  resid <- pmodel.response(data) - fit
  df.resid <- pdim$nT$N - nrcols.coefm

  
  ## Chi-sq test for homogeneous parameters (all panel-effect-specific coefficients are the same)
  #  notation resembles Greene (2018), ch. 11, p. 452
  dims <- dim(sigi.xpxm1[[1L]])
  
  V.t.inv <- vapply(seq_len.card.cond, function(i) {
                V.t.inv.i <- matrix(0, nrow = nrcols.coefm, ncol = nrcols.coefm)
                ii <- !coefna[i, ]
                V.t.inv.i[ii, ii] <- solve(sigi.xpxm1[[i]][ii, ii])
                V.t.inv.i},
              FUN.VALUE = matrix(0, nrow = dims[1L], ncol = dims[2L])) # V.t = sigi.xpxm1
  
  if(inherits(V.t.inv, "array")) {
    b.star.left <- solve(rowSums(V.t.inv, dims = 2L)) # == solve(Reduce("+", V.t.inv))
    b.star.right <- rowSums(vapply(seq_len.card.cond,
                                   function(i) crossprod(V.t.inv[ , , i], coefm[i , ]), 
                                   FUN.VALUE = numeric(nrcols.coefm)))
    b.star <- as.numeric(crossprod(b.star.left, b.star.right))
    bi.bstar <- t(coefm) - b.star
    chi.sq.stat <- sum(vapply(seq_len.card.cond, function(i) {
                              tcrossprod(crossprod(bi.bstar[ , i], V.t.inv[ , , i]), bi.bstar[ , i])
                              }, FUN.VALUE = numeric(1L)))
  }  else {
    ## need to special-case the intercept-only case
    b.star.left <- solve(sum(V.t.inv))
    b.star.right <- sum(vapply(seq_len.card.cond,
                                   function(i) crossprod(V.t.inv[i], coefm[i , ]), 
                                   FUN.VALUE = numeric(nrcols.coefm))) 
    b.star <- as.numeric(crossprod(b.star.left, b.star.right))
    bi.bstar <- t(coefm) - b.star
    chi.sq.stat <- sum(vapply(seq_len.card.cond, function(i) {
                                  tcrossprod(crossprod(bi.bstar[ , i], V.t.inv[i]), bi.bstar[ , i])
                              }, FUN.VALUE = numeric(1L)))
  }
  
  chi.sq.df <- nrcols.coefm * (pdim$nT$n - 1L)
  chi.sq.p  <- pchisq(chi.sq.stat, df = chi.sq.df, lower.tail = FALSE)
  
  chi.sq.test <- list(statistic   = c("chisq" = chi.sq.stat),
                      parameter   = c("df"    = chi.sq.df),
                      method      = "Test for parameter homogeniety",
                      p.value     = chi.sq.p,
                      alternative = "Heterogeneous parameters (panel-effect-specific coefficients differ)",
                      data.name   = paste(deparse(formula)))
  
  # return object: estimations incl. variance and chi-sq test
  list(coefficients     = Beta,
       residuals        = resid,
       fitted.values    = fit,
       vcov             = V,
       df.residual      = df.resid,
       model            = data,
       Delta            = Delta,
       single.coefs     = b.hat.i,
       single.vcov      = var.b.hat.i,
       single.std.error = std.err.b.hat.i,
       chisq.test       = structure(chi.sq.test, class = "htest"))
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
  mm <- model.matrix(mf)
  
  ## catch non-estimable model:
  ## check for obs. per individual vs. nr. of variables. equality: can estimate coefficients but not variance
  ##    -> for "within" model: this is ok (output has variance NA/NaN)
  #     -> or "random" model: variance is needed for D2, chi-sq test, so rather be strict
    error.msg <- paste0("insufficient number of observations for at least ",
                        "one group in ", effect, " dimension, so defined ",
                        "model is non-estimable")
     cond <- GRP(cond)
     Ti <- collapse::GRPN(cond, expand = FALSE)
     ncolumns <- ncol(mm)
     if(model == "within" && any(Ti <  ncolumns)) stop(error.msg)
     if(model == "random" && any(Ti <= ncolumns)) stop(error.msg)

     # split data by group
     X <- collapse::rsplit(mm,                 cond)
     y <- collapse::rsplit(model.response(mf), cond)
     
     # estimate OLS per group
     ols <- mapply(function(X_i, y_i) {
       r <- lm(y_i ~ X_i - 1, model = FALSE)
       names(r$coefficients) <- colnames(X_i) # need to plug-in original coef names due to lm(., model = FALSE) losing them
       r
     },  X, y, SIMPLIFY = FALSE)
     
  ols
}

