
#' Angrist and Newey's version of Chamberlain test for fixed effects
#' 
#' Angrist and Newey's version of the Chamberlain test
#' 
#' Angrist and Newey's test is based on the results of the artifactual
#' regression of the within residuals on the covariates for all the
#' periods.
#' 
#' @aliases aneweytest
#' @param formula a symbolic description for the model to be estimated,
#' @param data a `data.frame`,
#' @param subset see [lm()],
#' @param na.action see [lm()],
#' @param index the indexes,
#' @param \dots further arguments.
#' @return An object of class `"htest"`.
#' @export
#' @author Yves Croissant
#' @references
#' \insertRef{ANGR:NEWE:91}{plm}
#' 
#' @seealso [piest()] for Chamberlain's test
#' @keywords htest
#' @examples
#' 
#' data("RiceFarms", package = "plm")
#' aneweytest(log(goutput) ~ log(seed) + log(totlabor) + log(size), RiceFarms, index = "id")
#' 
aneweytest <- function(formula, data, subset, na.action, index = NULL,  ...){
  # NB: code fails for unbalanced data -> is Angrist and Newey's test only for balanced data?
  #     unbalanced case is currently caught and a message is printed
  
    mf <- match.call()
    # compute the model.frame using plm with model = NA
    mf[[1L]] <- as.name("plm")
    mf$model <- NA
    data <- eval(mf, parent.frame())
    # estimate the within model without instrument and extract the fixed
    # effects
    formula <- as.Formula(formula)
    mf$formula <- formula(formula, rhs = 1)
    index <- index(data)
    id <- index[[1L]]
    time <- index[[2L]]
    periods <- unique(time)
    pdim <- pdim(data)
    T <- pdim$nT$T
    n <- pdim$nT$n
    N <- pdim$nT$N
    Ti <- pdim$Tint$Ti
    balanced <- pdim$balanced
    
    if(!balanced) stop("'aneweytest' not implemented for unbalanced data")
    
    ht <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "na.action",
                 "effect", "model", "inst.method", "restrict.matrix",
                 "restrict.rhs", "index"), names(ht), 0)
    ht <- ht[c(1L, m)]
    ht[[1L]] <- as.name("plm")
    ht$model <- "within"
    ht$effect <- "individual"
    ht <- eval(ht, parent.frame())
    
    time.GRP <- collapse::GRP(time)
    .resid <- collapse::gsplit(resid(ht), time.GRP)

    # extract the covariates (no intercept), and isolate time-invariant covariates
    X <- model.matrix(data, model = "pooling", rhs = 1, lhs = 1)[ , -1, drop = FALSE]
    cst <- attr(model.matrix(data, model = "within", rhs = 1, lhs = 1), "constant")

    # get constant columns and remove the intercept
    if (length(cst) > 0L) cst <- cst[- match("(Intercept)", cst)]
    if (length(cst) > 0L){
        vr <- colnames(X)[!(colnames(X) %in% cst)]
        Z <- X[ , cst, drop = FALSE]
        X <- X[ , vr,  drop = FALSE]
        Kz <- ncol(Z)
        namesZ <- colnames(Z)
    } else {
        Z <- NULL
        Kz <- 0
        namesZ <- NULL
    }

    Kx <- ncol(X)
    
    # time-demean and split by period:
    attr(X, "index") <- index
    X <- Within(X, effect ="time")
    namesX <- colnames(X)
    
    X <- collapse::rsplit(X, time.GRP, use.names = FALSE)
    
    # put column names for split matrices in X:
    for (i in seq_along(periods)){
      colnames(X[[i]]) <- paste(namesX, periods[i], sep = ".")
    }
    
    if (!is.null(Z)){
        Z <- Z[time == periods[1], , drop = FALSE]
        Z <- t(t(Z) - .colMeans(Z, nrow(Z), ncol(Z))) # TODO: could use Within() framework
    }

    XX <- cbind(Reduce("cbind", X), Z)

    # compute the unconstrained estimates
    # NA-freeness guaranteed by model frame construction, so can use lm.fit
    # (non-collinearity is not catered for but code errors anywayif collinearity 
    # is present a bit later)
    #   was:   LMS <- lapply(.resid, function(x) lm(x ~ XX - 1))
    LMS <- lapply(.resid, function(x) lm.fit(XX, x))
    
    YTOT <- vapply(.resid, function(x) crossprod(x),           FUN.VALUE = 0.0, USE.NAMES = FALSE)
    DEV  <- vapply(LMS,    function(x) crossprod(x$residuals), FUN.VALUE = 0.0, USE.NAMES = FALSE)
    
    stat <- c("chisq" = sum(1 - DEV / YTOT) * (n - ncol(XX)))
    df <- c("df" = (T ^ 2 - T - 1) * Kx)
    aneweytest <- structure(list(statistic   = stat,
                                 parameter   = df,
                                 method      = "Angrist and Newey's test of within model",
                                 p.value     = pchisq(stat, df = df, lower.tail = FALSE),
                                 alternative = "within specification does not apply",
                                 data.name   = paste(deparse(formula))),
                            class = "htest")
    aneweytest
}



#' Chamberlain estimator and test for fixed effects
#' 
#' General estimator useful for testing the within specification
#' 
#' The Chamberlain method consists in using the covariates of all the
#' periods as regressors. It allows to test the within specification.
#' 
#' @aliases piest
#' @param formula a symbolic description for the model to be estimated,
#' @param object,x an object of class `"piest"` and of class `"summary.piest"` 
#'                  for the print method of summary for piest objects,
#' @param data a `data.frame`,
#' @param subset see [lm()],
#' @param na.action see [lm()],
#' @param index the indexes,
#' @param robust logical, if `FALSE`, the error is assumed to be spherical,
#' if `TRUE`, a robust estimation of the covariance matrix is computed,
#' @param \dots further arguments.
#' @return An object of class `"piest"`.
#' @export
#' @author Yves Croissant
#' @references
#'
#' \insertRef{CHAM:82}{plm}
#'
#' @seealso [aneweytest()]
#' @keywords htest
#' @examples
#' 
#' data("RiceFarms", package = "plm")
#' pirice <- piest(log(goutput) ~ log(seed) + log(totlabor) + log(size), RiceFarms, index = "id")
#' summary(pirice)
#' 
piest <- function(formula, data, subset, na.action, index = NULL, robust = TRUE,  ...){
  # NB: code fails for unbalanced data -> is Chamberlain's test only for balanced data?
  #     unbalanced case is currently caught and a message is printed
    cl <- match.call(expand.dots = TRUE)
    mf <- match.call()
    # compute the model.frame using plm with model = NA
    mf[[1L]] <- as.name("plm")
    mf$model <- NA
    data <- eval(mf, parent.frame())
    # estimate the within model without instrument and extract the fixed
    # effects
    formula <- as.Formula(formula)
    mf$formula <- formula(formula, rhs = 1)
    index <- index(data)
    id   <- index[[1L]]
    time <- index[[2L]]
    pdim <- pdim(data)
    balanced <- pdim$balanced
    T  <- pdim$nT$T
    n  <- pdim$nT$n
    N  <- pdim$nT$N
    Ti <- pdim$Tint$Ti

    if(!balanced) stop("'piest' not implemented for unbalanced data")
    
    # extract the response, time-demean and split by period
    y <- pmodel.response(data, model = "pooling", effect = "individual")
    Y <- Within(y, "time")
    time.GRP <- collapse::GRP(time)
    Y <- collapse::gsplit(Y, time.GRP)
    
    # extract the covariates, and isolate time-invariant covariates
    X <- model.matrix(data, model = "pooling", rhs = 1, lhs = 1)[ , -1, drop = FALSE]
    cst <- attr(model.matrix(data, model = "within", rhs = 1, lhs = 1), "constant")
    
    # get constant columns and remove the intercept
    if (length(cst) > 0L) cst <- cst[- match("(Intercept)", cst)]
    if (length(cst) > 0L){
        vr <- colnames(X)[!(colnames(X) %in% cst)]
        Z <- X[ , cst, drop = FALSE]
        X <- X[ , vr, drop = FALSE]
        Kz <- ncol(Z)
        namesZ <- colnames(Z)
    } else {
        Z <- NULL
        Kz <- 0
        namesZ <- NULL
    }
    
    Kx <- ncol(X)
    namesX <- colnames(X)

    # time-demean X and split by period:
    attr(X, "index") <- index
    X <- Within(X, effect ="time")
    periods <- unique(time)
    
    X <- collapse::rsplit(X, time.GRP, use.names = FALSE)

    # put column names for split matrices in X:
    for (i in seq_along(periods)){
      colnames(X[[i]]) <- paste(namesX, periods[i], sep = ".")
    }
    
    if (!is.null(Z)){
        Z <- Z[time == periods[1L], , drop = FALSE]
        Z <- t(t(Z) - .colMeans(Z, nrow(Z), ncol(Z))) # TODO: can use Within() framework
    }

    XX <- cbind(Reduce("cbind", X), Z)
    
    # compute the unconstrained estimates
      # NA-freeness guaranteed by model frame construction, so can use lm.fit
      # (non-collinearity is not cared for but code errors if collinearity is 
      # present anyway a bit later)
      #   was:   LMS <- lapply(Y, function(x) lm(x ~ XX - 1))
    LMS <- lapply(Y, function(x) lm.fit(XX, x))
    
    # compute the empirical covariance of the covariates
    Sxxm1 <- solve(crossprod(XX) / n)
    # compute the residuals matrix
    .resid <- sapply(LMS, resid)
    # extract the pi vector of unconstrained estimates
    pi <- unlist(lapply(LMS, coef), use.names = FALSE)
    
    if(robust) {
        Omega <- lapply(seq_len(n),
                        function(i)
                            tcrossprod(.resid[i, ]) %x%
                            (Sxxm1 %*% tcrossprod(XX[i, ]) %*% Sxxm1))
        Omega <- Reduce("+", Omega) / n
    } else {
        Omega <- (crossprod(.resid) / n) %x% Sxxm1
    }
    
    # construct the matrix of linear restrictions R | R x theta = pi
    R <- matrix(0, T * (T * Kx + Kz), (T + 1) * Kx + Kz)
    for (i in seq_len(Kx)){
        R[ ((1:T) - 1) * (Kx * T + Kz) + (Kx * (1:T - 1)) + i , i] <- 1
    }
    if (Kz > 0){
        for (i in seq_len(Kz)){
            R[ (Kx * T) + (1:T - 1) * (Kx * T + Kz) + i, Kx + i] <- 1
        }
    }
    for (i in seq_len(Kx * T)){
        R[((1:T) - 1) * (Kx * T + Kz) + i , Kx + Kz + i] <- 1
    }
    
    solve_Omega <- solve(Omega)
    A <- solve(t(R) %*% solve_Omega %*% R)
    .coef <- as.numeric(A %*% t(R) %*% solve_Omega %*% as.numeric(pi))
    #  .coef <- as.numeric(solve(t(R) %*% R) %*% t(R) %*% as.numeric(pi))
    namescoef <- if(Kz > 0)  c(namesX, namesZ, colnames(XX)[- c(ncol(XX) - 0:(Kz-1))])
                    else     c(namesX, namesZ, colnames(XX))
    names(.coef) <- rownames(A) <- colnames(A) <- namescoef
    resb <- as.numeric(R %*% .coef) - as.numeric(pi)
    piconst <- matrix(R %*% .coef, ncol = T)
    OOmega <- Omega                                       ## TODO: OOmega is never used
    .resid <- matrix(unlist(Y, use.names = FALSE), ncol = length(Y)) - XX %*% piconst
    
    if(TRUE){                                             ## TODO: this is always TRUE...!
        if(robust) {                                      ## and Omega is calc. again, with a
                                                          ## new .resid input but with same lapply-construct
            Omega <- lapply(seq_len(n),
                            function(i)
                                tcrossprod(.resid[i, ]) %x%
                                (Sxxm1 %*% tcrossprod(XX[i, ]) %*% Sxxm1))
            Omega <- Reduce("+", Omega) / n
        } else {
            Omega <- (crossprod(.resid) / n) %x% Sxxm1
        }
    }
    
    A <- solve(t(R) %*% solve(Omega) %*% R)
    stat <- c("chisq" = n * resb %*% solve(Omega) %*% resb)
    df <- c("df" = Kx * (T ^ 2 - T - 1))    ## TODO: df is overwritten in next line...?!
    df <- c("df" = length(pi) - length(.coef))
    
    pitest <- list(statistic   = stat,
                   parameter   = df,
                   method      = "Chamberlain's pi test",
                   p.value     = pchisq(stat, df = df, lower.tail = FALSE),
                   alternative = "within specification does not apply",
                   data.name   = paste(deparse(formula))
                   )
    
    structure(list(coefficients = .coef,
                   pi           = pi,
                   daub         = resb,
                   vcov         = A / n,
                   formula      = formula,
                   R            = R,
                   model        = data,
                   pitest       = structure(pitest, class = "htest"),
                   Omega        = Omega,
                   moments      = resb,
                   call         = cl),
              class = c("piest", "panelmodel"))
}

#' @rdname piest
#' @export
print.piest <- function(x, ...) print(x$pitest, ...)

#' @rdname piest
#' @export
summary.piest <- function(object,...){
  # construct the table of coefficients
  std.err <- sqrt(diag(vcov(object)))
  b <- coefficients(object)
  z <- b / std.err
  p <- 2 * pnorm(abs(z), lower.tail = FALSE)
  object$coefficients <- cbind("Estimate"   = b,
                               "Std. Error" = std.err,
                               "z-value"    = z,
                               "Pr(>|z|)"   = p)
  class(object) <- c("summary.piest", "piest", "panelmodel")
  object
}

#' @rdname piest
#' @param digits number of digits for printed output,
#' @param width the maximum length of the lines in the printed output,
#' @export
print.summary.piest <- function(x, digits = max(3, getOption("digits") - 2),
                                width = getOption("width"), subset = NULL, ...){
  if(is.null(subset)) printCoefmat(coef(x), digits = digits, ...)
  else printCoefmat(coef(x)[subset, , drop = FALSE], digits = digits, ...)
  print(x$pitest, ...)
  invisible(x)
}

