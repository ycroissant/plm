trace <- function(x) sum(diag(x))


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
                           method = NULL,
                           models = NULL,
                           dfcor = NULL,                           
                           index = NULL, ...){
    effect <- match.arg(effect)

    # if the data argument is not a pdata.frame, create it using plm
    if (!inherits(data, "pdata.frame"))
        data <- plm(object, data, model = NA, index = index)

    # if formula is not a pFormula object, coerce it
    if (!inherits(object, "pFormula")) object <- pFormula(object)

    # check whether the panel is balanced
    balanced <- pdim(data)$balanced
    
    # method and models arguments can't be both set
    if (! is.null(method) & ! is.null(models))
        stop("you can't use both method and models arguments")

    # method and models arguments aren't set, use swar
    if (is.null(method) & is.null(models)) method <- "swar"

    # dfcor is set, coerce it to a length 2 vector if necessary
    if (! is.null(dfcor)){
        if (length(dfcor) > 2) stop("dfcor length should be at least 2")
        if (length(dfcor) == 1) dfcor <- rep(dfcor, 2)
        if (! balanced & any(dfcor != 3))
            stop("dfcor should equal 3 for unbalanced panels")
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
            stop("the between estimator is only relevant for the  between quadratic form")
        # if the argument is of length 2, duplicate the second valuer
        if (length(models) == 2) models <- c(models[1], rep(models[2], 2))
        # if the argument is of length 2, duplicate the second valuer
        if (length(models) == 1) models <- c(rep(models, 3))
        # set one of the last two values to NA in the case of one way
        # model
        if (effect == "individual") models[3] <- NA
        if (effect == "time") models[2] <- NA
         # default value of dfcor 3,3
        if (is.null(dfcor)) dfcor <- c(3, 3)
    }
    
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

    if (balanced & dfcor[1] != 3){
        M["w", "nu"] <- O
        if (dfcor[1] == 1) M["w", "nu"] <- M["w", "nu"] - NTS
        if (dfcor[1] == 2) M["w", "nu"] <- M["w", "nu"] - NTS - K
        if (effect != "time"){
            M["w", "eta"] <- 0
            M["id", "nu"] <-ifelse(dfcor[2] == 2, N - K - 1, N)
            M["id", "eta"] <- TS * M["id", "nu"]
        }
        if (effect != "individual"){
            M["w", "mu"] <- 0
            M["ts", "nu"] <-ifelse(dfcor[2] == 2, TS - K - 1, TS)
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
    therows <- switch(effect,
                      individual = 1:2,
                      time = c(1, 3),
                      twoways = 1:3)
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
