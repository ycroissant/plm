trace <- function(x) sum(diag(x))

## pdim.default is now in file R/attributes.R
## pdim.default <- function(x, y, ...){
##   if (length(x) != length(y)) stop("The length of the two vectors differs\n")
##   x <- x[drop = TRUE]
##   y <- y[drop = TRUE]
##   z <- table(x, y)
##   cards <- list(id = nrow(z), time = ncol(z), sample = length(x))
##   panel.names <- list(id = rownames(z), time = colnames(z))
##   if (any(as.vector(z) == 0)) balanced <- FALSE else balanced <- TRUE
##   if (any(as.vector(z) > 1)) stop("duplicate couples (id-time)\n")
##   margins <- list(id = apply(z, 1, sum), time = apply(z, 2, sum))
##   z <- list(cards = cards, margins = margins, balanced = balanced, panel.names = panel.names)
##   class(z) <- "pdim"
##   z
## }  

### ercomp(formula, data, random.method, effect)

ercomp <- function(object, ...){
  UseMethod("ercomp")
}

ercomp.plm <- function(object, ...){
  model <- describe(object, "model")
  if (model != "random") stop("ercomp only relevant for random models")
  object$ercomp
}

ercomp.formula <- function(object, data, 
                           effect = c('individual', 'time', 'twoways'),
                           method = c('swar', 'walhus', 'amemiya', 'nerlove'),
                           dfcor = NULL,                           
                           index = NULL, ...){

    effect <- match.arg(effect)
    method <- match.arg(method)

    # Nerlove (0, 1)
    # Walhus (1, 0)
    # Amemyia (1, 0)
    # Swar 2
    if (is.null(dfcor)){
        dfcor <- switch(method,
                       'swar' = c(2, 2),
                       'walhus' = c(1, 0),
                       'amemiya' = c(1, 0),
                       'nerlove' = c(0, 1))
    }
    else{
        if (length(dfcor) > 2) stop("dfcor length should be at least 2")
        if (length(dfcor) == 1) dfcor <- rep(dfcor, 2)
        if (any(dfcor == 2) & any(dfcor != 2)) stop("if any dfcor element is 2, both of them must be")
    }
    # if the data argument is not a pdata.frame, create it using plm
    if (!inherits(data, "pdata.frame")) data <- plm(object, data, model = NA, index = index)

    # if formula is not a pFormula object, coerce it
    if (!inherits(object, "pFormula")) object <- pFormula(object)
    balanced <- pdim(data)$balanced
    Z <- model.matrix(object, data, model = "pooling")
    K <- ncol(Z) - 1
    if (effect != 'twoways'){
        if (effect == 'time'){
            ids <- index(data, "time")
            T <- table(index(data, "time"))
            N <- length(T)
        }
        else{
            ids <- index(data, "id")
            T <- table(index(data, "id"))
            N <- length(T)
        }
        nT <- names(T)
        T <- as.numeric(T)
        names(T) <- nT
        O <- sum(T)
        if (method == 'walhus'){
            model.pooling <- plm.fit(object, data, model = 'pooling', effect = effect)
            hateps <- resid(model.pooling, model = "pooling", effect = effect)
            qw <- crossprod(Within(hateps, effect = effect))
            qb <- crossprod(Between(hateps, effect = effect))
            df1 <- switch(as.character(dfcor[1]),
                          "0" = O,
                          "1" = O - N)
            df2 <- switch(as.character(dfcor[2]),
                         "0" = N,
                         "1" = N - K - 1)
            if (dfcor[1] == 2){
                ZI <- crossprod(Z)
                ZW <- crossprod(cbind(0, model.matrix(model.pooling, model = "within", effect = effect)))
                ZB <- crossprod(model.matrix(model.pooling, model = "Between", effect = effect))
                ZU <- crossprod(apply(model.matrix(model.pooling, model = "pooling"), 2, tapply, ids, sum)[as.character(ids), ], Z)
                M <- matrix(c(O - N - trace(ZW %*% solve(ZI)),
                              N - trace(solve(ZI) %*% ZB),
                              trace(ZW %*% solve(ZI) %*% ZU %*% solve(ZI)),
                              O + trace(ZU %*% solve(ZI) %*% ZB %*% solve(ZI)) - 2 * trace(ZU %*% solve(ZI))
                              ), 2)
            }
        }
        if (method == 'swar'){
            model.within <- plm.fit(object, data, model = 'within', effect = effect)
            model.between <- plm.fit(object, data, model = 'Between', effect = effect)
            qw <- crossprod(resid(model.within))
            qb <- crossprod(resid(model.between))
            df1 <- switch(as.character(dfcor[1]),
                          "0" = O,
                          "1" = O - N)
            df2 <- switch(as.character(dfcor[2]),
                          "0" = N,
                          "1" = N - K - 1)
            if (dfcor[1] == 2){
                ZB <- crossprod(model.matrix(model.between, model = "Between", effect = effect))
                ZU <- crossprod(apply(model.matrix(model.between, model = "pooling"), 2,
                                      tapply, ids, sum)[as.character(ids), ], Z)
                M <- matrix(c(O - N - K,
                              N - K - 1,
                              0,
                              O - trace(ginv(ZB) %*% ZU)
                              ), 2)
            }
        }
        if (method == "amemiya"){
            model.within <- plm.fit(object, data, model = 'within', effect = effect)
            hateps <- resid(model.within, model = "pooling")
            qw <- crossprod(Within(hateps))
            qb <- crossprod(Between(hateps))

            # Between(hateps) identique a fixef(model.within, type = "dmean")[as.character(ids)]
            # qb est identique a sum(fixef(model.within, type = "dmean")[as.character(ids)]^2))
            # On purge de l'effet individuel l'influence des variables du modele
            fixef <- fixef(model.within, type = "dmean")[as.character(ids)]
            cstX <- attr(model.matrix(object, data, model = "within", rhs = 1), "constant")
            if (length(cstX) > 0){
                cstW <- attr(model.matrix(object, data, model = "within", rhs = 2), "constant")
                W <- model.matrix(object, data, model = "pooling", rhs = 2)[, ]
                X <- model.matrix(object, data, model = "pooling", rhs = 1)[, cstX]
                fixef2 <- twosls(fixef, X, W, TRUE)
                qb <- deviance(fixef2)
            }            
            df1 <- switch(as.character(dfcor[1]),
                          "0" = O,
                          "1" = O - N)
            df2 <- switch(as.character(dfcor[2]),
                         "0" = N,
                         "1" = N - K - 1)
            if (dfcor[1] == 2){
                M <- matrix(c(O - N - K,
                              N - 1,
                              0,
                              O - sum(T ^ 2) / O
                              ), 2)
            }
        }
        if (dfcor[1] < 2 & method != "nerlove"){
            s2idios <- as.numeric(qw) / df1
            s2iota <- as.numeric(qb) / df2
            s2id <- (s2iota - s2idios) / T[1]
        }
        if (dfcor[1] == 2 & method != "nerlove"){
            s2 <- solve(M, c(qw, qb))
            s2idios <- s2[1]
            s2id <- s2[2]
            if (balanced) s2iota <- s2id * T[1] + s2idios else s2iota <- s2id * T + s2idios
        }
        if (method == "nerlove"){
            if(! balanced) stop("nerlove variance decomposition not implemented for unbalanced panels")
            model.within <- plm.fit(object, data, model = 'within', effect = effect)
            df1 <- switch(as.character(dfcor[1]),
                          "0" = O,
                          "1" = O - N,
                          "2" = O - N - K)
            df2 <- switch(as.character(dfcor[2]),
                         "0" = N,
                         "1" = N - 1,
                         "2" = N - K - 1)
            s2idios <- deviance(model.within) / df1
            s2id <- sum(fixef(model.within, type = "dmean") ^ 2) / df2
            s2iota <- s2id * T[1] + s2idios
        }
        if (balanced) theta <- as.numeric(1 - sqrt(s2idios / s2iota))
        else theta <- (1 - sqrt(s2idios / (s2idios + T * s2id)))[as.character(ids)]
        sigma2 <- list(idios = s2idios, id = s2id, iota = s2iota)
        result <- list(sigma2 = sigma2, theta = theta)
    }
    else{
        if (!balanced) stop("twoways effect variance computation is not implemented for unbalanced data")
        if(method == "nerlove") stop("nerlove variance decomposition not implemented for twoways effects")
        N <- length(unique(index(data, "id")))
        T <- length(unique(index(data, "time")))
        O <- N * T
        if (method == 'walhus'){
            id <- index(data, "id")
            time <- index(data, "time")
            model.pooling <- plm.fit(object, data, model = "pooling", effect = effect)
            hateps <- resid(model.pooling, model = "pooling", effect = effect) -
                resid(model.pooling, model = "Between", effect = "individual") -
                    resid(model.pooling, model = "Between", effect = "time")
            lambda <- list(idios = sum(hateps ^ 2) / ( (N - 1) * (T - 1)),
                           id = sum(resid(model.pooling, model = "between", effect = "individual") ^ 2) * T / (N - 1),
                           time = sum(resid(model.pooling, model = "between", effect = "time") ^ 2) * N / (T - 1)
                         )
        }
        if (method == 'swar'){
            model.within <- plm.fit(object, data, model = "within", effect = effect)
            model.between.id <- plm.fit(object, data, model = "between", effect = "individual")
            model.between.time <- plm.fit(object, data, model = "between", effect = "time")
            theta <- list()
            lambda <- list(idios = deviance(model.within) / ((N - 1) * (T - 1) - length(coef(model.within))),
                           id = T * deviance(model.between.id) / df.residual(model.between.id),
                           time = N * deviance(model.between.time) / df.residual(model.between.time)
                         )
        }
        if (method == "amemiya"){
            model.within <- plm.fit(object, data, model = "within", effect = effect)
            hateps <- resid(model.within, model = "pooling") -
                resid(model.within, model = "Between", effect = "individual") -
                    resid(model.within, model = "Between", effect = "time")
            lambda <- list(idios = sum(hateps ^ 2) / ( (N - 1) * (T - 1) ),
                           id = sum(resid(model.within, model = "between", effect = "individual") ^ 2) * T / (N - 1),
                           time = sum(resid(model.within, model = "between", effect = "time") ^ 2) * N / (T - 1)
                           )
        }
        lambda$total <- lambda$id + lambda$time - lambda$idios
        sigma2 <- list(
            idios = lambda$idios,
            id = (lambda$id - lambda$idios) / T,
            time = (lambda$time - lambda$idios) / N
            )
        theta <- list(
            id = 1 - sqrt(lambda$idios / lambda$id),
            time = 1 - sqrt(lambda$idios / lambda$time),
            total = 1 - sqrt(lambda$idios / lambda$id) - sqrt(lambda$idios / lambda$time) + sqrt(lambda$idios / lambda$total)
            )
        if (sigma2$id < 0) warning("the estimated variance of the individual effect is negative")
        if (sigma2$time < 0) warning("the estimated variance of the time effect is negative")
        if (sigma2$time < 0) theta$time <- theta$total <- sigma2$time <- 0
        result <- list(sigma2 = sigma2, theta = theta)
    }
    structure(result, class = "ercomp", balanced = balanced, effect = effect)
}

print.ercomp <- function(x, digits= max(3, getOption("digits") - 3), ...){
    effect <- attr(x, "effect")
    balanced <- attr(x, "balanced")
    sigma2 <- x$sigma2
    theta <- x$theta
    
    if (effect=="twoways"){
        sigma2 <- unlist(sigma2)
        sigma2Table <- cbind(var=sigma2,std.dev=sqrt(sigma2),share=sigma2/sum(sigma2))
        rownames(sigma2Table) <- c("idiosyncratic","individual","time")
    }
    else{
        sigma2 <- unlist(sigma2[c("idios", "id")])
        sigma2Table <- cbind(var=sigma2,std.dev=sqrt(sigma2),share=sigma2/sum(sigma2))
        rownames(sigma2Table) <- c("idiosyncratic",effect)
    }
    printCoefmat(sigma2Table,digits)
    
    if (!is.null(x$theta)){
        if (effect!="twoways"){
            if (balanced){
                cat(paste("theta: ",signif(x$theta,digits)," \n"))
            }
            else{
                cat("theta  : \n")
                print(summary(x$theta))
            }
        }
        else{
            if(balanced){
                cat(paste("theta  : ", signif(x$theta$id,digits), " (id) ", signif(x$theta$time,digits), " (time) ",
                          signif(x$theta$total,digits), " (total)\n", sep = ""))
            }
        }
    }
}
