trace <- function(x) sum(diag(x))

is.constant <- function(x) (max(x) - min(x)) < sqrt(.Machine$double.eps)
        

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
                           effect = c("individual", "time", "twoways", "nested"),
                           method = NULL,
                           models = NULL,
                           dfcor = NULL,                           
                           index = NULL, ...){
    effect <- match.arg(effect)

    # if formula is not a pFormula object, coerce it
    if (!inherits(object, "pFormula")) object <- pFormula(object)

    # if the data argument is not a pdata.frame, create it using plm
    if (! inherits(data, "pdata.frame"))
        data <- plm(object, data, model = NA, index = index)
    if(is.null(attr(data, "terms"))) data <- model.frame(object, data)
    
    # check whether the panel is balanced
    balanced <- pdim(data)$balanced
    
    # method and models arguments can't be both set
    if (! is.null(method) & ! is.null(models))
        stop("you can't use both method and models arguments")

    # method and models arguments aren't set, use swar
    if (is.null(method) & is.null(models)) method <- "swar"
    
    # dfcor is set, coerce it to a length 2 vector if necessary
    if (! is.null(dfcor)){
        if (length(dfcor) > 2) stop("dfcor length should be at most 2")
        if (length(dfcor) == 1) dfcor <- rep(dfcor, 2)
        if (! balanced & any(dfcor != 3))
            stop("dfcor should equal 3 for unbalanced panels")
    }

    # we use later a general expression for the three kind of effects,
    # select the relevant lines

    therows <- switch(effect,
                      individual = 1:2,
                      time = c(1, 3),
                      twoways = 1:3)

    if (! is.null(method) && method == "nerlove"){
        if (! balanced) stop("Nerlove method only implemented for balanced models")
        est <- plm.fit(object, data, model = "within", effect = effect)
        N <- pdim(data)$nT$n
        TS <- pdim(data)$nT$T
        O <- pdim(data)$nT$N
        NTS <- N * (effect != "time") + TS * (effect != "individual") - 1 * (effect == "twoways")
        s2nu <- deviance(est) / (O - NTS)
        s2eta <- s2mu <- NULL
        if (effect != "time")
            s2eta <- sum(fixef(est, type = "dmean", effect = "individual") ^ 2) / (N - 1)
        if (effect != "individual")
            s2mu <- sum(fixef(est, type = "dmean", effect = "time") ^ 2) / (TS - 1)
        sigma2 <- c(idios = s2nu, id = s2eta, time = s2mu)
        theta <- list()
        if (effect != "time")       theta$id   <- (1 - (1 + TS * sigma2["id"]   / sigma2["idios"]) ^ (-0.5))
        if (effect != "individual") theta$time <- (1 - (1 + N  * sigma2["time"] / sigma2["idios"]) ^ (-0.5))
        if (effect == "twoways")
            theta$total <- theta$id + theta$time - 1 +
                (1 + N * sigma2["time"] / sigma2["idios"] +
                 TS * sigma2["id"] / sigma2["idios"]) ^ (-0.5)
        if (effect != "twoways") theta <- theta[[1]]
        result <- list(sigma2 = sigma2, theta = theta)
        result <- structure(result, class = "ercomp", balanced = balanced, effect = effect)
        return(result)
    }

    if (! is.null(method) && method == "ht"){
        N <- pdim(data)$nT$n
        TS <- pdim(data)$nT$T
        O <- pdim(data)$nT$N
        wm <- plm.fit(object, data, effect = "individual", model = "within")
        s2nu <- deviance(wm) / (O - N)
        s2eta <- sum(fixef(wm, type = "dmean") ^ 2) / N
        X <- model.matrix(object, data, rhs = 1)
        constants <- apply(X, 2, function(x) all(tapply(x, index(data)[[1]], is.constant)))
        if (length(object)[2] > 1){
            W1 <- model.matrix(object, data, rhs = 2)
            ra <- twosls(fixef(wm, type = "dmean")[as.character(index(data)[[1]])], X[, constants], W1)
        }
        else{
            FES <- fixef(wm, type = "dmean")[as.character(index(data)[[1]])]
            XCST <- X[, constants]
            ra <- lm(FES ~ XCST - 1)
        }
        s2nu <- deviance(wm) / (O - N)
        s21 <- sum(fixef(wm, type = "dmean") ^ 2) / N
        s21 <- deviance(ra) / N
        s2eta <- (s21 - s2nu) / TS
        sigma2 <- c(idios = s2nu, id = s2eta)
        theta <- (1 - (1 + TS * sigma2["id"] / sigma2["idios"]) ^ (-0.5))
        result <- list(sigma2 = sigma2, theta = theta)
        result <- structure(result, class = "ercomp", balanced = balanced, effect = effect)
        return(result)
    }

    
    # method argument is used, check its validity and set the relevant
    # models and dfcor
    if (! is.null(method)){
        if (! method %in% c("swar", "walhus", "amemiya"))
            stop(paste(method, "is not a relevant method"))
        if (method == "swar")    models <- c("within",  "Between")
        if (method == "walhus")  models <- c("pooling", "pooling")
        if (method == "amemiya") models <- c("within",  "within")
        if (is.null(dfcor)){
            if (balanced){
                dfcor <- switch(method,
                                'swar'    = c(2, 2),
                                'walhus'  = c(1, 1),
                                'amemiya' = c(1, 1)
                                )
            }
            else dfcor <- c(3, 3)
        }        
    }
    else{
        # the between estimator is only relevant for the second
        # quadratic form
        if (models[1] %in% c("Between", "between"))
            stop("the between estimator is only relevant for the between quadratic form")
        # if the argument is of length 2, duplicate the second value
        if (length(models) == 2) models <- c(models[1], rep(models[2], 2))
        # if the argument is of length 1, triple its value
        if (length(models) == 1) models <- c(rep(models, 3))
        # set one of the last two values to NA in the case of one way
        # model
        if (effect == "individual") models[3] <- NA
        if (effect == "time") models[2] <- NA
         # default value of dfcor 3,3
        if (is.null(dfcor)) dfcor <- c(3, 3)
    }

    # The nested error component model
    if (effect == "nested"){
        tss <- attr(data, "index")[[2]]
        ids <- attr(data, "index")[[1]]
        gps <- attr(data, "index")[[3]]
        G <- length(unique(gps))
        Z <- model.matrix(object, data, model = "pooling")
        y <- pmodel.response(object, data, model = "pooling")
        O <- nrow(Z)
        K <- ncol(Z) - 1                                                                                       # INTERCEPT
        N <- pdim(data)$nT$n
        TS <- pdim(data)$nT$T
        TG <- unique(data.frame(tss, gps))
        TG <- table(TG$gps)
        NG <- unique(data.frame(ids, gps))
        NG <- table(NG$gps)
        Tn <- pdim(data)$Tint$Ti
        Nt <- pdim(data)$Tint$nt
        quad <- vector(length = 3, mode = "numeric")
        
        M <- matrix(NA, nrow = 3, ncol = 3,
                    dimnames = list(c("w", "id", "gp"),
                        c("nu", "eta", "lambda")))
        if (method == "walhus"){
            estm <- plm.fit(object, data, model = "pooling", effect = "individual")
            hateps <- as.numeric(resid(estm, model = "pooling"))
            quad <- c(crossprod(resid(estm, model = "within", effect = "individual")),
                      crossprod(Between(hateps, ids) - Between(hateps, gps)),
                      crossprod(Between(hateps, gps)))
#                      crossprod(resid(estm, model = "Between", effect = "nested")))
            Z <- model.matrix(estm, model = "pooling")
            ZSeta <- model.matrix(estm, model = "Sum", effect = "individual")
            ZSlambda <- apply(Z, 2, tapply, gps, sum)[as.character(gps), , drop = FALSE]
            CPZM <- solve(crossprod(Z))
            CPZSeta <-    crossprod(ZSeta,    Z)
            CPZSlambda <- crossprod(ZSlambda, Z)
            CPZW <- crossprod(Z - Between(Z, ids))
            CPZBetaBlambda <-     crossprod(Between(Z, ids) - Between(Z, gps))
            CPZBetaBlambdaSeta <- crossprod(Between(Z, ids) - Between(Z, gps) , ZSeta)
            CPZBlambdaSeta <- crossprod(Between(Z, gps), ZSeta)
            CPZBlambda <- crossprod(Between(Z, gps))
            M["w", "nu"] <- O - N - trace(crossprod(CPZM, CPZW))
            M["w", "eta"] <-    trace( CPZM %*% CPZW %*% CPZM %*% CPZSeta)
            M["w", "lambda"] <- trace( CPZM %*% CPZW %*% CPZM %*% CPZSlambda)
            M["id", "nu"] <- N - G - trace(crossprod(CPZM, CPZBetaBlambda))
            M["id", "eta"] <- O - sum(TG) - 2 * trace(crossprod(CPZM, CPZBetaBlambdaSeta)) +
                trace( CPZM %*% CPZBetaBlambda %*% CPZM %*% CPZSeta)
            M["id", "lambda"] <- trace(crossprod(CPZM, CPZBetaBlambda) %*% crossprod( CPZM, CPZSlambda))
            
            M["gp", "nu"] <- G - trace(crossprod(CPZM, CPZBlambda))
            M["gp", "eta"] <- sum(TG) - 2 * trace(crossprod(CPZM, CPZBlambdaSeta)) +
                trace( CPZM %*% CPZBlambda %*% CPZM %*% CPZSeta)
            M["gp", "lambda"] <- O - 2 * trace(crossprod(CPZM, CPZSlambda)) +                         
                trace( CPZM %*% CPZBlambda %*% CPZM %*% CPZSlambda)
        }
        if (method == "amemiya"){
            estm <- plm.fit(object, data, effect = "individual", model = "within")
            hateps <- as.numeric(resid(estm, model = "pooling"))
            hatepsBeta <- Between(hateps, ids)
            hatepsBlambda <- Between(hateps, gps)
#            quad <- c(crossprod(resid(estm, model = "within", effect = "individual")), ## Problem, don't return the within residuals, why ?
            quad <- c(crossprod(resid(estm)), 
                      crossprod(Between(hateps, ids) - Between(hateps, gps)),
                      crossprod(Between(hateps, gps)))
            WX <- model.matrix(estm, model = "within", effect = "individual", null.rm = TRUE)
            X <- model.matrix(estm, model = "pooling")[, -1, drop = FALSE]
            XBetaBlambda <- Between(X, ids) - Between(X, gps)
            XBlambda <- Between(X, gps)
            XBlambda <- t(t(XBlambda) - apply(XBlambda, 2, mean))
            CPXBlambda <- crossprod(XBlambda)
            CPXM <- solve(crossprod(WX))
            CPXBetaBlambda <- crossprod(XBetaBlambda)
            K <- ncol(WX)
            MK <- length(attr(WX, "constant"))
            KW <- ncol(WX)

            M["w", "nu"] <- O - N - K + MK                                                       # INTERCEPT
            M["w", "eta"] <- 0
            M["w", "lambda"] <- 0

            M["id", "nu"] <- N - G + trace( crossprod(CPXM, CPXBetaBlambda))
            M["id", "eta"] <- O - sum(TG)
            M["id", "lambda"] <- 0
        
            M["gp", "nu"] <- G - 1 + trace( crossprod(CPXM, CPXBlambda ) )
            M["gp", "eta"] <- sum(TG) - sum(NG  * TG ^ 2) / O
            M["gp", "lambda"] <- O - sum(NG ^ 2 * TG ^ 2) / O
        }
        
        if (method == "swar"){
            X <- Z[, -1, drop = FALSE]

            yBetaBlambda <- Between(y, ids) - Between(y, gps)
            ZBetaBlambda <- Between(Z, ids) - Between(Z, gps)
            XBetaBlambda <- Between(X, ids) - Between(X, gps)
            ZBlambda <- Between(Z, gps)
            yBlambda <- Between(y, gps)
            ZSeta    <- apply(Z, 2, tapply, ids, sum)[as.character(ids), , drop = FALSE]
            ZSlambda <- apply(Z, 2, tapply, gps, sum)[as.character(gps), , drop = FALSE]
            XSeta    <- apply(X, 2, tapply, ids, sum)[as.character(ids), , drop = FALSE]
            
            estm1 <- plm.fit(object, data, effect = "individual", model = "within")
            estm2 <- lm.fit(ZBetaBlambda, yBetaBlambda)
#            estm3 <- plm.fit(object, data, effect = "group", model = "between")
            estm3 <- lm.fit(ZBlambda, yBlambda)
            quad <- c(crossprod(resid(estm1)),
                      crossprod(resid(estm2)),
                      crossprod(resid(estm3)))
        
            M["w", "nu"] <- O - N - K
            M["w", "eta"] <- 0
            M["w", "lambda"] <- 0
            
            M["id", "nu"] <- N - G - K
            M["id", "eta"] <- O - sum(TG) - trace(solve(crossprod(XBetaBlambda)) %*% crossprod(XSeta, XBetaBlambda))
            M["id", "lambda"] <- 0
            
            M["gp", "nu"] <- G - K - 1
            M["gp", "eta"] <- sum(TG) - trace( solve(crossprod(ZBlambda)) %*% crossprod(ZBlambda, ZSeta))
            M["gp", "lambda"] <- O - trace( solve(crossprod(ZBlambda)) %*% crossprod(ZSlambda, Z))
        }
        ids <- index(data)[[1]]
        tss <- index(data)[[2]]
        gps <- index(data)[[3]]
        Gs <- as.numeric(table(gps)[as.character(gps)])
        Tn <- as.numeric(table(ids)[as.character(ids)])
        sigma2 <- as.numeric(solve(M, quad))
#        print(M)
#        print(quad)
        names(sigma2) <- c("idios", "id", "gp")
        theta <- list(id = 1 - sqrt(sigma2["idios"] /  (Tn * sigma2["id"] + sigma2["idios"])),
                      gp = sqrt(sigma2["idios"] /  (Tn * sigma2["id"] + sigma2["idios"])) -
                          sqrt(sigma2["idios"] / (Gs * sigma2["gp"] + Tn * sigma2["id"] + sigma2["idios"]))
                      )
        result <- list(sigma2 = sigma2, theta = theta)
        return(structure(result, class = "ercomp", balanced = balanced, effect = effect))
    }

    # the "classic" error component model    

    Z <- model.matrix(object, data)
    O <- nrow(Z)
    K <- ncol(Z) - 1                                                                                       # INTERCEPT
    N <- pdim(data)$nT$n
    TS <- pdim(data)$nT$T
    NTS <- N * (effect != "time") + TS * (effect != "individual") - 1 * (effect == "twoways")
    Tn <- pdim(data)$Tint$Ti
    Nt <- pdim(data)$Tint$nt
    # Estimate the relevant models
    estm <- vector(length = 3, mode = "list")
    estm[[1]] <-  plm.fit(object, data, model = models[1], effect = effect)
    # Check what is the second model
    secmod <- na.omit(models[2:3])[1]
    if (secmod %in% c("within", "pooling")){
        amodel <- plm.fit(object, data, model = secmod, effect = effect)
        if (effect != "time") estm[[2]] <- amodel
        if (effect != "individual") estm[[3]] <- amodel
    }
    if (secmod %in% c("between", "Between")){
        if (effect != "time") estm[[2]] <- plm.fit(object, data, model = secmod, effect = "individual")
        if (effect != "individual") estm[[3]] <- plm.fit(object, data, model = secmod, effect = "time")
    }
    KS <- sapply(estm, function(x) length(coef(x))) - sapply(estm, function(x){ "(Intercept)" %in% names(coef(x))})
    quad <- vector(length = 3, mode = "numeric")
    # first quadratic form, within transformation
    hateps_w <- resid(estm[[1]], model = "pooling")

    if (effect != "twoways"){
        quad[1] <- crossprod(Within(hateps_w, effect = effect))
    }
    else{
        if (balanced){
            hateps_w <- hateps_w - Within(hateps_w, "individual") - Within(hateps_w, "time")
        }
        else{
            time <- index(data)[[2]]
            id <- index(data)[[1]]
            Dmu <- model.matrix(~ time - 1)
            W1 <- Within(hateps_w, "individual")
            WDmu <- Within(Dmu, id)
            W2 <- fitted(lm.fit(WDmu, hateps_w))
            hateps_w <- W1 - W2
        }
        quad[1] <- crossprod(hateps_w)
    }

    # second quadratic form, between transformation
    if (effect != "time"){
        hateps_id <- resid(estm[[2]], model = "pooling")
        quad[2] <- crossprod(Between(hateps_id, effect = "individual"))
        RESB <- Between(hateps_id, effect = "individual")
    }
    if (effect != "individual"){
        hateps_ts <- resid(estm[[3]], model = "pooling")
        quad[3] <- crossprod(Between(hateps_ts, effect = "time"))
    }
    M <- matrix(NA, nrow = 3, ncol = 3,
                dimnames = list(c("w", "id", "ts"),
                    c("nu", "eta", "mu")))

    # Compute the M matrix :
    ## (    q_w)    ( w_nu      w_eta     w_mu    )   ( s^2_nu )
    ## |       |  = |                             |   |        |
    ## (  q_bid)    ( bid_nu    bid_eta   bid_mu  )   ( s^2_eta)
    ## |       |  = |                             |   |        |
    ## (q_btime)    ( btime_nu  btime_eta btime_mu)   ( s^2_mu )

    
    # In case of balanced panels, simple denominators are
    # available if dfcor < 3
    # if (balanced & dfcor[1] != 3){
    if (dfcor[1] != 3){
        # The number of time series in the balanced panel is replaced
        # by the harmonic mean of the number of time series in case of
        # unbalanced panels
        barT <- ifelse(balanced, TS, length(Tn) / sum(Tn ^ (- 1)))
        M["w", "nu"] <- O
        if (dfcor[1] == 1) M["w", "nu"] <- M["w", "nu"] - NTS
        if (dfcor[1] == 2) M["w", "nu"] <- M["w", "nu"] - NTS - KS[1]
        if (effect != "time"){
            M["w", "eta"] <- 0
            M["id", "nu"] <- ifelse(dfcor[2] == 2, N - KS[2] - 1, N)
            M["id", "eta"] <- barT * M["id", "nu"]
        }
        if (effect != "individual"){
            M["w", "mu"] <- 0
            M["ts", "nu"] <- ifelse(dfcor[2] == 2, TS - KS[3] - 1, TS)
            M["ts", "mu"] <- N * M["ts", "nu"]
        }
        if (effect == "twoways")
            M["ts", "eta"] <- M["id", "mu"] <- 0
    }
    else{
        # General case, compute the unbiased version of the estimators
        if ("pooling" %in% models){
            mp <- match("pooling", models)
            Z <- model.matrix(estm[[mp]], model = "pooling")
            CPZM <- solve(crossprod(Z))
            if (effect != "time"){
                ZSeta <- model.matrix(estm[[mp]], model = "Sum", effect = "individual")
                CPZSeta <- crossprod(ZSeta, Z)
            }
            if (effect != "individual"){
                ZSmu <- model.matrix(estm[[mp]], model = "Sum", effect = "time")
                CPZSmu <- crossprod(ZSmu, Z)
            }
        }
        if (models[1] == "pooling"){
            ZW <- model.matrix(estm[[1]], model = "within", effect = effect)
            CPZW <- crossprod(cbind(0, ZW))                                                              # INTERCEPT
            M["w", "nu"] <- O - NTS - trace(crossprod(CPZM, CPZW))
            if (effect != "time"){
                M["w", "eta"] <- trace( CPZM %*% CPZW %*% CPZM %*% CPZSeta)
            }
            if (effect != "individual"){
                M["w", "mu"] <- trace( CPZM %*% CPZW %*% CPZM %*% CPZSmu)
            }
        }
        if (secmod == "pooling"){
            if (effect != "time"){
                ZBeta <- model.matrix(estm[[2]], model = "Between", effect = "individual")
                CPZBeta <- crossprod(ZBeta)
                M["id", "nu"] <- N - trace(crossprod(CPZM, CPZBeta))
                M["id", "eta"] <- O - 2 * trace(crossprod(CPZM, CPZSeta)) +
                    trace( CPZM %*% CPZBeta %*% CPZM %*% CPZSeta)
            }
            if (effect != "individual"){
                ZBmu <- model.matrix(estm[[3]], model = "Between", effect = "time")
                CPZBmu <- crossprod(ZBmu)
                M["ts", "nu"] <- TS - trace(crossprod(CPZM, CPZBmu))
                M["ts", "mu"] <- O - 2 * trace(crossprod(CPZM, CPZSmu)) +
                    trace( CPZM %*% CPZBmu %*% CPZM %*% CPZSmu)
            }
            if (effect == "twoways"){
                CPZBmuSeta <- crossprod(ZBmu, ZSeta)
                CPZBetaSmu <- crossprod(ZBeta, ZSmu)
                M["id", "mu"] <- N - 2 * trace(crossprod(CPZM, CPZBetaSmu)) +                         
                    trace( CPZM %*% CPZBeta %*% CPZM %*% CPZSmu)
                M["ts", "eta"] <- TS - 2 * trace(crossprod(CPZM, CPZBmuSeta)) +
                    trace( CPZM %*% CPZBmu %*% CPZM %*% CPZSeta)
            }
        }
        if ("within" %in% models){
            WX <- model.matrix(estm[[match("within", models)]], model = "within",
                               effect = effect, null.rm = TRUE)
            K <- ncol(WX)
            MK <- length(attr(WX, "constant"))
            KW <- ncol(WX)
            if (models[1] == "within"){
                M["w", "nu"] <- O - NTS - K + MK                                                       # INTERCEPT
                if (effect != "time") M["w", "eta"] <- 0
                if (effect != "individual") M["w", "mu"] <- 0
            }
            if (secmod == "within"){
                CPXM <- solve(crossprod(WX))
                if (effect != "time"){
                    XBeta <- model.matrix(estm[[2]], model = "Between",
                                          effect = "individual")[, - 1]                                # INTERCEPT
                    XBeta <- t(t(XBeta) - apply(XBeta, 2, mean))
                    CPXBeta <- crossprod(XBeta)
                    M["id", "nu"] <- N - 1 + trace( crossprod(CPXM, CPXBeta) )
                    M["id", "eta"] <-  O - sum(Tn ^ 2) / O
                }
                if (effect != "individual"){
                    XBmu <- model.matrix(estm[[3]], model = "Between",
                                         effect = "time")[, - 1]                                 # INTERCEPT
                    XBmu <- t(t(XBmu) - apply(XBmu, 2, mean))
                    CPXBmu <- crossprod(XBmu)
                    M["ts", "nu"] <- TS - 1 + trace( crossprod(CPXM, CPXBmu) )
                    M["ts", "mu"] <- O - sum(Nt ^ 2) / O
                }
                if (effect == "twoways"){
                    M["id", "mu"] <- N - sum(Nt ^ 2) / O
                    M["ts", "eta"] <- TS - sum(Tn ^ 2) / O
                }
            }
        }
        if (length(intersect(c("between", "Between"), models))){
            if (effect != "time"){
                Zeta <- model.matrix(estm[[2]], model = "pooling", effect = "individual")
                ZBeta <- model.matrix(estm[[2]], model = "Between", effect = "individual")
                ZSeta <- model.matrix(estm[[2]], model = "Sum", effect = "individual")
                CPZSeta <- crossprod(ZSeta, Z)
                CPZMeta <- solve(crossprod(ZBeta))
                M["id", "nu"] <- N - K - 1
                M["id", "eta"] <- O - trace( crossprod(CPZMeta, CPZSeta) )
            }
            if (effect != "individual"){
                Zmu <- model.matrix(estm[[3]], model = "pooling", effect = "time")
                ZBmu <- model.matrix(estm[[3]], model = "Between", effect = "time")
                ZSmu <- model.matrix(estm[[3]], model = "Sum", effect = "time")
                CPZSmu <- crossprod(ZSmu, Z)
                CPZMmu <- solve(crossprod(ZBmu))
                M["ts", "nu"] <- TS - K - 1
                M["ts", "mu"] <- O - trace( crossprod(CPZMmu, CPZSmu) )
            }
            if (effect == "twoways"){
                if (! balanced){
                    ZSmuBeta <- Sum(ZBeta, effect = index(data)[[2]])
                    ZBetaSmuBeta <- crossprod(ZBeta, ZSmuBeta)
                    ZSetaBmu <- Sum(ZBmu, effect = index(data)[[1]])
                    ZBmuSetaBmu <- crossprod(ZBmu, ZSetaBmu)
                    M["id", "mu"] <- N - trace(CPZMeta %*% ZBetaSmuBeta)
                    M["ts", "eta"] <- TS - trace(CPZMmu %*% ZBmuSetaBmu)
                }
                else M["id", "mu"] <- M["ts", "eta"] <- 0
            }
        }
    }
    sigma2 <- as.numeric(solve(M[therows, therows], quad[therows]))
    names(sigma2) <- c("idios", "id", "time")[therows]
    sigma2[sigma2 < 0] <- 0
    theta <- list()
    if (! balanced){
        ids <- index(data)[[1]]
        tss <- index(data)[[2]]
        Tns <- Tn[as.character(ids)]
        Nts <- Nt[as.character(tss)]
    }
    else{
        Tns <- TS
        Nts <- N
    }
    if (effect != "time") theta$id <- (1 - (1 + Tns * sigma2["id"] / sigma2["idios"]) ^ (-0.5))
    if (effect != "individual") theta$time <- (1 - (1 + Nts * sigma2["time"] / sigma2["idios"]) ^ (-0.5))
    if (effect == "twoways")
        theta$total <- theta$id + theta$time - 1 +
            (1 + Nts * sigma2["time"] / sigma2["idios"] +
                 Tns * sigma2["id"] / sigma2["idios"]) ^ (-0.5)
    if (effect != "twoways") theta <- theta[[1]]
    result <- list(sigma2 = sigma2, theta = theta)
    structure(result, class = "ercomp", balanced = balanced, effect = effect)
}

print.ercomp <- function(x, digits = max(3, getOption("digits") - 3), ...){
    effect <- attr(x, "effect")
    balanced <- attr(x, "balanced")
    sigma2 <- x$sigma2
    theta <- x$theta
    
    if (effect == "twoways"){
        sigma2 <- unlist(sigma2)
        sigma2Table <- cbind(var = sigma2, std.dev = sqrt(sigma2), share = sigma2 / sum(sigma2))
        rownames(sigma2Table) <- c("idiosyncratic", "individual", "time")
    }
    if (effect == "individual"){
        sigma2 <- unlist(sigma2[c("idios", "id")])
        sigma2Table <- cbind(var = sigma2, std.dev = sqrt(sigma2), share = sigma2 / sum(sigma2))
        rownames(sigma2Table) <- c("idiosyncratic", effect)
    }
    if (effect == "time"){
        sigma2 <- unlist(sigma2[c("idios", "time")])
        sigma2Table <- cbind(var = sigma2, std.dev = sqrt(sigma2), share = sigma2 / sum(sigma2))
        rownames(sigma2Table) <- c("idiosyncratic", effect)
    }
    if (effect == "nested"){
        sigma2 <- unlist(sigma2)
        sigma2Table <- cbind(var = sigma2, std.dev = sqrt(sigma2), share = sigma2 / sum(sigma2))
        rownames(sigma2Table) <- c("idiosyncratic", "individual", "group")
    }

    printCoefmat(sigma2Table, digits)
    
    if (! is.null(x$theta)){
        if (effect %in% c("individual", "time")){
            if (balanced){
                cat(paste("theta: ",signif(x$theta,digits),"\n", sep = ""))
            }
            else{
                cat("theta:\n")
                print(summary(x$theta))
            }
        }
        if (effect == "twoways"){
            if(balanced){
                cat(paste("theta: ", signif(x$theta$id,digits), " (id) ", signif(x$theta$time,digits), " (time) ",
                          signif(x$theta$total,digits), " (total)\n", sep = ""))
            } else {
              cat("theta:\n")
              print(rbind(id = summary(x$theta$id),
                          time = summary(x$theta$time),
                          total =  summary(x$theta$total)))
            }
        }
        if (effect == "nested"){
            cat("theta:\n")
            print(rbind(id = summary(x$theta$id),
                        group = summary(x$theta$gp)))
        }
    }
}
