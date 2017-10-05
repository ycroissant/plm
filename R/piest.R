aneweytest <-  function(formula, data, subset, na.action, index = NULL,  ...){
    cl <- match.call(expand.dots = TRUE)
    mf <- match.call()
    # compute the model.frame using plm with model = NA
    mf[[1]] <- as.name("plm")
    mf$model <- NA
    data <- eval(mf, parent.frame())
    # estimate the within model without instrument and extract the fixed
    # effects
    formula <- pFormula(formula)
    mf$formula <- formula(formula, rhs = 1)
    id <- index(data, "id")
    time <- index(data, "time")
    years <- unique(time)
    pdim <- pdim(data)
    balanced <- pdim$balanced
    T <- pdim$nT$T
    n <- pdim$nT$n
    N <- pdim$nT$N
    Ti <- pdim$Tint$Ti
    
    ht <- match.call(expand.dots=FALSE)
    m <- match(c("formula", "data", "subset", "na.action",
                 "effect", "model", "inst.method", "restict.matrix",
                 "restrict.rhs", "index"),names(ht), 0)
    ht <- ht[c(1,m)]
    ht[[1]] <- as.name("plm")
    ht$model <- "within"
    #  ht$effect <- "twoways"       ## TODO: ht$effect is overwritten in next line...?!
    ht$effect <- "individual"    ## TODO
    ht <- eval(ht, parent.frame())
    .resid <- split(resid(ht), time)
  
    # extract the covariates, and isolate time-invariant covariates
    X <- model.matrix(formula, data, model = "pooling", rhs = 1, lhs = 1)[, - 1, drop = FALSE]
    cst <- attr(model.matrix(formula, data, model = "within", rhs = 1, lhs = 1), "constant")
    # get constant columns and remove the intercept
    if (length(cst) > 0) cst <- cst[- match("(Intercept)", cst)]
    if (length(cst) > 0){
        vr <- colnames(X)[!(colnames(X) %in% cst)]
        Z <- X[, cst, drop = FALSE]
        X <- X[, vr, drop = FALSE]
        Kz <- ncol(Z)
        namesZ <- colnames(Z)
    }
    else{
        Z <- NULL
        Kz <- 0
        namesZ <- NULL
    }
    Kx <- ncol(X)
    namesX <- colnames(X)
    # split by time period and remove the mean
    X <- lapply(as.list(years), function(x) X[time == x, , drop = FALSE])
    X <- lapply(X, function(x) t(t(x) - .colMeans(x, nrow(x), ncol(x)))) # was: t(t(x) - apply(x, 2, mean))
    if (!is.null(Z)){
        Z <- Z[time == years[1], , drop = FALSE]
        Z <- t(t(Z) - .colMeans(Z, nrow(Z), ncol(Z))) # was: t(t(Z) - apply(Z, 2, mean))
    }
    for (i in 1:(length(years))){
        colnames(X[[i]]) <- paste(colnames(X[[i]]), years[i], sep = ".")
    }
    XX <- cbind(Reduce("cbind", X), Z)
    # compute the unconstrained estimates
    LMS <- lapply(.resid, function(x) lm(x ~ XX - 1))
    YTOT <- sapply(.resid, function(x) sum(x^2))
    DEV <- sapply(LMS, deviance)
    stat <- c("chisq" = sum(1 - DEV / YTOT) * (n - ncol(XX)))
    df <- c("df" = (T ^ 2 - T - 1) * Kx)
    aneweytest <- structure(list(statistic = stat,
                                 parameter = df,
                                 method    = "Angrist and Newey's test of within model",
                                 p.value   = pchisq(stat, df = df, lower.tail = FALSE),
                                 data.name = paste(deparse(formula))),
                            class = "htest")
    aneweytest
}

piest <- function(formula, data, subset, na.action, index = NULL, robust = TRUE,  ...){
    cl <- match.call(expand.dots = TRUE)
    mf <- match.call()
    # compute the model.frame using plm with model = NA
    mf[[1]] <- as.name("plm")
    mf$model <- NA
    data <- eval(mf, parent.frame())
    # estimate the within model without instrument and extract the fixed
    # effects
    formula <- pFormula(formula)
    mf$formula <- formula(formula, rhs = 1)
    id <- index(data, "id")
    time <- index(data, "time")
    pdim <- pdim(data)
    balanced <- pdim$balanced
    T <- pdim$nT$T
    n <- pdim$nT$n
    N <- pdim$nT$N
    Ti <- pdim$Tint$Ti
    # extract the response, split by period and remove the mean
    y <- pmodel.response(data, model = "pooling")
    Y <- split(y, time)
    Y <- lapply(Y, function(x) x - mean(x))
    # extract the covariates, and isolate time-invariant covariates
    X <- model.matrix(formula, data, model = "pooling", rhs = 1, lhs = 1)[, -1, drop = FALSE]
    cst <- attr(model.matrix(formula, data, model = "within", rhs = 1, lhs = 1), "constant")
    # get constant columns and remove the intercept
    if (length(cst) > 0) cst <- cst[- match("(Intercept)", cst)]
    if (length(cst) > 0){
        vr <- colnames(X)[!(colnames(X) %in% cst)]
        Z <- X[, cst, drop = FALSE]
        X <- X[, vr, drop = FALSE]
        Kz <- ncol(Z)
        namesZ <- colnames(Z)
    }
    else{
        Z <- NULL
        Kz <- 0
        namesZ <- NULL
    }
    Kx <- ncol(X)
    namesX <- colnames(X)
    # split by time period and remove the mean
    years <- unique(index(data, "time"));
    X <- lapply(as.list(years), function(x) X[time == x, , drop = FALSE])
    X <- lapply(X, function(x) t(t(x) - .colMeans(x, nrow(x), ncol(x)))) # was: t(t(x) - apply(x, 2, mean)))
    if (!is.null(Z)){
        Z <- Z[time == years[1], , drop = FALSE]
        Z <- t(t(Z) - .colMeans(Z, nrow(Z), ncol(Z))) #was: t(t(Z) - apply(Z, 2, mean))
    }
    for (i in 1:(length(years))){
        colnames(X[[i]]) <- paste(colnames(X[[i]]), years[i], sep = ".")
    }
    XX <- cbind(Reduce("cbind", X), Z)
    # compute the unconstrained estimates
    LMS <- lapply(Y, function(x) lm(x ~ XX - 1))
    # compute the empirical covariance of the covariates
    Sxxm1 <- solve(crossprod(XX) / n)
    # compute the residuals matrix
    .resid <- sapply(LMS, resid)
    # extract the pi vector of unconstrained estimates
    pi <- Reduce("c", lapply(LMS, coef))
    if (robust){
        Omega <- lapply(seq_len(n),
                        function(i)
                            tcrossprod(.resid[i, ]) %x%
                            (Sxxm1 %*% tcrossprod(XX[i, ]) %*% Sxxm1))
        Omega <- Reduce("+", Omega) / n;
    }
    else{
        Omega <- (crossprod(.resid) / n) %x% Sxxm1
    }
    
    # construct the matrix of linear restrictions R | R x theta = pi
    R <- matrix(0, T * (T * Kx + Kz), (T + 1) * Kx + Kz)
    for (i in 1:Kx){
        R[ ((1:T) - 1) * (Kx * T + Kz) + (Kx * (1:T - 1)) + i , i] <- 1
    }
    if (Kz > 0){
        for (i in 1:Kz){
            R[ (Kx * T) + (1:T - 1) * (Kx * T + Kz) + i, Kx + i] <- 1
        }
    }
    for (i in 1:(Kx * T)){
        R[((1:T) - 1) * (Kx * T + Kz) + i , Kx + Kz + i] <- 1
    }
    A <- solve(t(R) %*% solve(Omega) %*% R)
    .coef <- as.numeric(A %*% t(R) %*% solve(Omega) %*% as.numeric(pi))
    #  .coef <- as.numeric(solve(t(R) %*% R) %*% t(R) %*% as.numeric(pi))
    if (Kz > 0) namescoef <- c(namesX, namesZ, colnames(XX)[- c(ncol(XX) - 0:(Kz-1))])
    else namescoef <- c(namesX, namesZ, colnames(XX))
    names(.coef) <- rownames(A) <- colnames(A) <- namescoef
    resb <- as.numeric(R %*% .coef) - as.numeric(pi)
    piconst <- matrix(R %*% .coef, ncol = T)
    OOmega <- Omega
    .resid <- as.matrix(as.data.frame(Y)) - XX %*% piconst
    if(TRUE){                                             ## TODO: this is always TRUE...?!
        if (robust){
            Omega <- lapply(seq_len(n),
                            function(i)
                                tcrossprod(.resid[i, ]) %x%
                                (Sxxm1 %*% tcrossprod(XX[i, ]) %*% Sxxm1))
            Omega <- Reduce("+", Omega) / n;
        }
        else{
            Omega <- (crossprod(.resid) / n) %x% Sxxm1
        }
    }
    A <- solve(t(R) %*% solve(Omega) %*% R)
    stat <- c("chisq" = n * resb %*% solve(Omega) %*% resb)
    df <- c("df" = Kx * (T ^ 2 - T - 1))                  ## TODO: df is overwritten in next line...?!
    df <- c("df" = length(pi) - length(.coef))
    pitest <- list(statistic = stat,
                   parameter = df,
                   method = "Chamberlain's pi test",
                   p.value = pchisq(stat, df = df, lower.tail = FALSE),
                   data.name = paste(deparse(formula))
                   )
    
    structure(list(coefficients = .coef,
                   pi = pi,
                   daub = resb,
                   vcov =  A / n,
                   formula = formula,
                   R = R,
                   model = data,
                   pitest = structure(pitest, class = "htest"),
                   Omega = Omega,
                   moments = resb,
                   call = cl),
              class = c("piest", "panelmodel"))
}

print.piest <- function(x, ...) print(x$pitest)   

summary.piest <- function(object,...){
#  object$fstatistic <- Ftest(object, test = "F")
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

print.summary.piest <- function(x, ...){
  print(x$coefficients)
  print(x$pitest)
}

## data(Produc, package="plm")
## Produc <- subset(Produc, year >= 1970 & year < 1974)
## T <- 3
## N <- 8
## K <- 2
## data(Wages, package="plm")
## Wages$time <- rep(1:7, 595)
## Wages$id <- rep(1:595, each = 7)
## Wages <- subset(Wages, time %in% c(1:T))
## Wages <- pdata.frame(Wages, index=c("id", "time"))
## #l <- piest(lwage ~  smsa+married, Wages)

## data(Crime, package="plm")
## plm(log(crmrte) ~ log(prbarr) + log(prbconv) + log(prbpris) + log(polpc) + log(density) + log(pctmin) + region + log(wtuc) + log(wmfg), Crime, effect = "twoways")
## otime <- proc.time();za <- piest(log(crmrte) ~ log(prbarr) + log(prbconv) + log(prbpris) + log(polpc) + log(density) + log(pctmin) + region + log(wtuc) + log(wmfg), Crime);print(proc.time()-otime)
