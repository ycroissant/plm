#' Panel estimators for limited dependent variables
#' 
#' Fixed and random effects estimators for truncated or censored
#' limited dependent variable
#' 
#' `pldv` computes two kinds of models : maximum likelihood estimator
#' with an assumed normal distribution for the individual effects and
#' a LSQ/LAD estimator for the first-difference model.
#' @aliases pldv
#' @param formula a symbolic description for the model to be
#'     estimated,
#' @param data a `data.frame`,
#' @param subset see `lm`,
#' @param weights see `lm`,
#' @param na.action see `lm`,
#' @param model one of `"fd"`, `"random"` or `"pooling"`,
#' @param index the indexes, see [pdata.frame()],
#' @param R the number of points for the gaussian quadrature,
#' @param start a vector of starting values,
#' @param lower the lower bound for the censored/truncated dependent
#'     variable,
#' @param upper the upper bound for the censored/truncated dependent
#'     variable,
#' @param objfun the objective function for the fixed effect model,
#'     one of `"lsq"` for least squares and `"lad"` for least absolute
#'     deviations,
#' @param sample `"cens"` for a censored (tobit-like) sample,
#'     `"trunc"` for a truncated sample,
#' @param \dots further arguments.
#' @return An object of class `c("plm","panelmodel")`.
#' @export
#' @importFrom maxLik maxLik
#' @author Yves Croissant
#' @references
#'
#' \insertRef{HONO:92}{plm}
#' 
#' @keywords regression
pldv <- function(formula, data, subset, weights, na.action,
                 model = c("fd", "random", "pooling"), index = NULL,
                 R = 20, start = NULL, lower = 0, upper = + Inf,
                 objfun = c("lsq", "lad"), sample = c("cens", "trunc"), ...){
  
## Due to the eval() construct with maxLik::maxLik we import maxLik::maxLik
## and re-export it via NAMESPACE as plm::maxLik with a minimal documentation 
## pointing to the original documentation.
## This way, we can keep the flexibility of eval() ## [evalutate in parent frame] 
## and can lessen the dependency burden by palacing pkg maxLik in 'Imports'
## rather than 'Depends' in DESCRIPTION.
  
    # use the plm interface to compute the model.frame
    sample <- match.arg(sample)
    model <- match.arg(model)
    cl <- match.call()
    mf <- match.call(expand.dots = FALSE)
    mf <- cl
    m <- match(c("formula", "data", "subset", "weights", "na.action", "index"), names(mf), 0)
    mf <- mf[c(1, m)]
    mf$model <- NA
    mf[[1]] <- as.name("plm")
    mf <- eval(mf, parent.frame())
    formula <- attr(mf, "formula")
    # extract the relevant arguments for maxLik
    maxl <- cl
    m <- match(c("print.level", "ftol", "tol", "reltol",
                 "gradtol", "steptol", "lambdatol", "qrtol",
                 "iterlim", "fixed", "activePar", "method"), names(maxl), 0)
    maxl <- maxl[c(1, m)]
    maxl[[1]] <- as.name("maxLik")
    
    # The within model -> Bo Honore (1992)
    if (model == "fd"){
        objfun <- match.arg(objfun)
        # create a data.frame containing y_t and y_{t-1}
        y <- as.character(formula[[2]])
        y <- mf[[y]]
        ly <- c(NA, y[1:(length(y) - 1)])
        id <- as.integer(index(mf, "id"))
        lid <- c(NA, id[1:(nrow(mf) - 1)])
        keep <- id == lid
        keep[1] <- FALSE
        Y <- data.frame(y, ly)
        Y <- Y[keep, ]
        yt <- Y$y
        ytm1 <- Y$ly
        # create the matrix of first differenced covariates
        X <- model.matrix(mf, model = "fd")
        start <- rep(.1, ncol(X))
        names(start) <- colnames(X)
        if (sample == "trunc"){
            if (objfun == "lad") fm <- function(x) abs(x)
            if (objfun == "lsq") fm <- function(x) x ^ 2
            psi <- function(a1, a2, b){
                fm( (a2 <= b) * a1 + 
                    (b > - a1 & b < a2) * (a2 - a1 - b) + 
                    (a1 <= - b) * a2
                   )
            }
        }
        if (sample == "cens"){
            if (objfun == "lad"){
                psi <- function(a1, a2, b){
                    (a1 <= pmax(0, - b) & a2 <= pmax(0, b)) * 0 +
                        (! (a1 <= pmax(0, - b) & a2 <= pmax(0, b)) ) * abs(a2 - a1 - b)
                }
            }
            if (objfun == "lsq"){
                psi <- function(a1, a2, b){
                    (a2 <= b) * (a1 ^ 2 - 2 * a1 * (a2 - b)) + 
                        (b > - a1 & b < a2) * (a2 - a1 - b) ^ 2 +
                        (a1 <= - b) * (a2 ^ 2 - 2 * a2 * (b + a1))
                }
                
            }
        }
        BO <- function(param){
            bdx <- as.numeric(X %*% param)
            lnl <- - psi(ytm1, yt, bdx)
            selobs <- (bdx > - ytm1 & bdx < yt)
            if (objfun == "lsq" && sample == "cens"){
                attr(lnl, "gradient") <- -
                    ( (ytm1 > - bdx & yt > bdx) * (- 2 * (yt - ytm1 - bdx)) +
                      (ytm1 > - bdx & yt < bdx) * (  2 * ytm1) +
                      (ytm1 < - bdx & yt > bdx) * (- 2 * yt) ) * X
                attr(lnl, "hessian") <-  - crossprod( (ytm1 > - bdx & yt > bdx) * X)
            }
            lnl
        }
        maxl[c("logLik", "start")] <- list(BO, start)
        result <- eval(maxl, parent.frame())
        if (objfun == "lsq" && sample == "cens"){
            bdx <- as.numeric(X %*% coef(result))
            V4 <- yt ^ 2 * (bdx <= - ytm1) + ytm1 ^ 2 * (yt <= bdx) +
                (yt - ytm1 - bdx) ^ 2 * (bdx > - ytm1 & bdx < yt)
            V4 <- crossprod(X, V4 * X) / length(V4)
            T4 <- crossprod((bdx > - ytm1 & bdx < yt) * X, X) / length(V4)
            vcov <- solve(T4) %*% V4 %*% solve(T4)
            result$vcov <- V4
            result
        }
        if (is.null(result$vcov)) result$vcov <- solve(- result$hessian)
        resid <- yt - as.numeric(X %*% coef(result))
        result <- list(coefficients = coef(result),
                       vcov         = result$vcov,
                       formula      = formula,
                       model        = mf,
                       df.residual  = nrow(X) - ncol(X),
                       residuals    = resid,
                       args         = list(model = "fd", effect = "individual"),
                       call         = cl)
        class(result) <- c("plm", "panelmodel")
        result
    }
    else{
        # old pglm stuff for the pooling and the random model, with
        # update to allow upper and lower bonds
        X <- model.matrix(mf, rhs = 1, model = "pooling", effect = "individual")
        
        if (ncol(X) == 0) stop("empty model")
        y <- pmodel.response(mf, model = "pooling", effect = "individual")
        id <- attr(mf, "index")[[1]]
        
          # The following is the only instance of statmod::gauss.quad, so check for 
          # the package's availability. (We placed 'statmod' in 'Suggests' rather
          # than 'Imports' so that it is not an absolutely required depedency.)
          ## Procedure for pkg check for pkg in 'Suggests' as recommended in 
          ## Wickham, R packages (http://r-pkgs.had.co.nz/description.html).
          if (!requireNamespace("statmod", quietly = TRUE)) {
            stop(paste("Function 'gauss.quad' from package 'statmod' needed for this function to work.",
                       "Please install it, e.g., with 'install.packages(\"statmod\")"),
                 call. = FALSE)
          }
        # compute the nodes and the weights for the gaussian quadrature
        rn <- statmod::gauss.quad(R, kind = 'hermite')
        # compute the starting values
        ls <- length(start)
        if (model == "pooling"){
            K <- ncol(X)
            if (! ls %in% c(0, K + 1)) stop("irrelevant length for the start vector")
            if (ls == 0){
                m <- match(c("formula", "data", "subset", "na.action"), names(cl), 0)
                lmcl <- cl[c(1,m)]
                lmcl[[1]] <- as.name("lm")
                lmcl <- eval(lmcl, parent.frame())
                sig2 <- deviance(lmcl) / df.residual(lmcl)
                sigma <- sqrt(sig2)
                start <- c(coef(lmcl), sd.nu = sigma)
            }
        }
        else{
            if (ls <= 1){
                startcl <- cl
                startcl$model <- "pooling"
                startcl$method <- "bfgs"
                pglmest <- eval(startcl, parent.frame())
                thestart <- coef(pglmest)
                if (ls == 1){
                    start <- c(thestart, start)
                }
                else{
                    resid <- y - as.numeric(X %*% coef(pglmest)[1:ncol(X)])
                    eta <- tapply(resid, id, mean)[as.character(id)]
                    nu <- resid - eta
                    start <- c(thestart[1:ncol(X)], sd.nu = sd(nu), sd.eta = sd(eta))
                    
                }
            }
        }
        # call to maxLik with the relevant arguments
        argschar <- function(args){
            paste(as.character(names(args)), as.character(args),
                  sep= "=", collapse= ",")
        }
        args <- list(param = "start",
                     y = "y", X = "X", id = "id", model = "model",
                     rn = "rn", lower = lower, upper = upper)
        thefunc <- paste("function(start) lnl.tobit", "(", argschar(args), ")", sep = "")
        maxl$logLik <- eval(parse(text = thefunc))
        maxl$start <- start
        result <- eval(maxl, parent.frame())
        result[c('call', 'args', 'model')] <- list(cl, args, data)
    }
    result
}


lnl.tobit <- function(param, y, X, id, lower = 0, upper = +Inf, model = "pooling", rn = NULL){
    compute.gradient <- TRUE
    compute.hessian <- FALSE
    mills <- function(x) exp(dnorm(x, log = TRUE) - pnorm(x, log.p = TRUE))
    O <- length(y)
    K <- ncol(X)
    beta <- param[1L:K]
    sigma <- param[K + 1L]
    Xb <- as.numeric(crossprod(t(X), beta))
    YLO <- (y == lower)
    YUT <- (y > lower) & (y < upper)
    YUP <- y == upper
    if (model == "random"){
        R <- length(rn$nodes)
        seta <- param[K + 2L]
    }
    else seta <- 0

    f <- function(i = NA){
        result <- numeric(length = length(y))
        if (is.na(i)) z <- 0 else z <- rn$nodes[i]
        e <- (y - Xb - sqrt(2) * seta * z) / sigma
        result[YLO] <- pnorm(  e[YLO], log.p = TRUE)
        result[YUT] <- dnorm(  e[YUT], log = TRUE) - log(sigma)
        result[YUP] <- pnorm(- e[YUP], log.p = TRUE)
        result
    }

    g <- function(i = NA){
        if (is.na(i)) z <- 0 else z <- rn$nodes[i]
        e <- (y - Xb - sqrt(2) * seta * z) / sigma
        mz <-  mills(e)
        mmz <- mills(- e)
        gradi <- matrix(0, nrow = nrow(X), ncol = ncol(X) + 1L)
        gradi[YLO, 1L:K]   <- - mz[YLO] * X[YLO, , drop = FALSE]
        gradi[YLO, K + 1L] <- -  e[YLO] * mz[YLO]
        gradi[YUT, 1L:K]   <-    e[YUT] * X[YUT, , drop = FALSE]
        gradi[YUT, K + 1L] <- - (1 - e[YUT] ^ 2)
        gradi[YUP, 1L:K]   <-  mmz[YUP] *  X[YUP, , drop = FALSE]
        gradi[YUP, K + 1L] <-    e[YUP] * mmz[YUP]
        if (! is.na(i)){
            gradi <- cbind(gradi, NA)
            gradi[YLO, K + 2L] <- - mz[YLO] * sqrt(2) * z 
            gradi[YUT, K + 2L] <- e[YUT] * sqrt(2) * z 
            gradi[YUP, K + 2L] <- mmz[YUP] * sqrt(2) * z
        }
        gradi / sigma
    }

    h <- function(i = NA, pwnt = NULL){
        if (is.na(i)){
            z <- 0
            seta <- 0
            pw <- 1
        }
        else{
            z <- rn$nodes[i]
            pw <- pwnt[[i]]
        }
        e <- (y - Xb - sqrt(2) * seta * z) / sigma
        mz <-  mills(e)
        mmz <- mills(- e)
        hbb <- hbs <- hss <- numeric(length = nrow(X))
        hbb[YLO] <- - (e[YLO] + mz[YLO]) * mz[YLO]
        hbs[YLO] <-     mz[YLO] * (1 - (e[YLO] + mz[YLO]) * e[YLO])
        hss[YLO] <-    e[YLO] * mz[YLO] * (2 - (e[YLO] + mz[YLO]) * e[YLO])
        hbb[YUT] <- - 1
        hbs[YUT] <- - 2 * e[YUT]
        hss[YUT] <- (1 - 3 * e[YUT] ^ 2)
        hbb[YUP] <- - (- e[YUP] + mmz[YUP]) * mmz[YUP]
        hbs[YUP] <- - mmz[YUP] * (1 + (mmz[YUP] - e[YUP]) * e[YUP])
        hss[YUP] <- - e[YUP] * mmz[YUP] * (2 + (mmz[YUP] - e[YUP]) * e[YUP])
        hbb <- crossprod(hbb * X * pw, X)
        hbs <- apply(hbs * X * pw, 2, sum)
        hss <- sum(hss * pw)
        H <- rbind(cbind(hbb, hbs), c(hbs, hss))
        if (! is.na(i)){
            hba <- hsa <- haa <- numeric(length = nrow(X))
            hba[YLO] <- - (e[YLO] + mz[YLO]) * mz[YLO] * sqrt(2) * z
            hsa[YLO] <-   mz[YLO] * sqrt(2) * z * (1 - (e[YLO] + mz[YLO]) * e[YLO])
            haa[YLO] <- - (e[YLO] + mz[YLO]) * mz[YLO] * 2 * z ^ 2
            hba[YUT] <- - sqrt(2) * z
            hsa[YUT] <- - 2 * sqrt(2) * z * e[YUT]
            haa[YUT] <- - 2 * z ^ 2
            hba[YUP] <- - (- e[YUP] + mmz[YUP]) * mmz[YUP] * sqrt(2) * z
            hsa[YUP] <- - mmz[YUP] * sqrt(2) * z * (1 + (- e[YUP] + mmz[YUP]) * e[YUP])
            haa[YUP] <- - (- e[YUP] + mmz[YUP]) * mmz[YUP] * 2 * z ^ 2
            hba <- apply(hba * X * pw, 2, sum)
            haa <- sum(haa * pw)
            hsa <- sum(hsa * pw)
            H <- rbind(cbind(H, c(hba, hsa)), c(hba, hsa, haa))
        }
        H / sigma ^ 2
    }
    
    if (model == "pooling"){
        lnL <- sum(f(i = NA))
        if (compute.gradient) attr(lnL, "gradient") <- g(i = NA)
        if (compute.hessian) attr(lnL, "hessian") <- h(i = NA)
    }
    if (model == "random"){
        lnPntr <- lapply(1:R, function(i)  f(i = i))
        lnPnr <- lapply(lnPntr, function(x){
            result <- tapply(x, id, sum)
            ids <- names(result)
            result <- as.numeric(result)
            names(result) <- ids
            result
        }
        )
        lnPn <- lapply(1:R, function(i) rn$weights[i] * exp(lnPnr[[i]]))
        lnPn <- log(Reduce("+", lnPn)) - 0.5 * log(pi)
        lnL <- sum(lnPn)
        if (compute.gradient || compute.hessian){
            glnPnr <- lapply(1:R, function(i) g(i = i))
            pwn <- lapply(1:R, function(i) exp(lnPnr[[i]] - lnPn))
            pwnt <- lapply(1:R, function(i) pwn[[i]][as.character(id)])
            glnPnr2 <- lapply(1:R, function(i) rn$weights[i] * pwnt[[i]]  * glnPnr[[i]])
            gradi <- Reduce("+", glnPnr2) / sqrt(pi)
            attr(lnL, "gradient") <- gradi
        }
        if (compute.hessian){
            hlnPnr <- lapply(1:R, function(i) h(i = i, pwnt = pwnt))
            daub <- lapply(1:R, function(i) apply(glnPnr[[i]], 2, tapply, id, sum) * pwn[[i]] * rn$weights[i])
            daub <- Reduce("+", daub) / sqrt(pi)
            DD1 <- - crossprod(daub)
            DD2 <- lapply(1:R, function(i) rn$weights[i] * hlnPnr[[i]])
            DD2 <- Reduce("+", DD2) / sqrt(pi)
            DD3 <- lapply(1:R, function(i) rn$weights[i] * crossprod(sqrt(pwn[[i]]) * apply(glnPnr[[i]], 2, tapply, id, sum)))
            DD3 <- Reduce("+", DD3) / sqrt(pi)
            H <- (DD1+ DD2 + DD3) 
            attr(lnL, "hessian") <- H
        }
    }
    lnL
}


