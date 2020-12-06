trace <- function(x) sum(diag(x))

is.constant <- function(x) (max(x) - min(x)) < sqrt(.Machine$double.eps)

#' Estimation of the error components
#' 
#' This function enables the estimation of the variance components of a panel
#' model.
#' 
#' 
#' @aliases ercomp
#' @param object a `formula` or a `plm` object,
#' @param data a `data.frame`,
#' @param effect the effects introduced in the model, see [plm()] for
#'     details,
#' @param method method of estimation for the variance components, see
#'     [plm()] for details,
#' @param models the models used to estimate the variance components
#'     (an alternative to the previous argument),
#' @param dfcor a numeric vector of length 2 indicating which degree
#'     of freedom should be used,
#' @param index the indexes,
#' @param x an `ercomp` object,
#' @param digits digits,
#' @param \dots further arguments.
#' @return An object of class `"ercomp"`: a list containing \itemize{
#'     \item `sigma2` a named numeric with estimates of the variance
#'     components, \item `theta` contains the parameter(s) used for
#'     the transformation of the variables: For a one-way model, a
#'     numeric corresponding to the selected effect (individual or
#'     time); for a two-ways model a list of length 3 with the
#'     parameters. In case of a balanced model, the numeric has length
#'     1 while for an unbalanced model, the numerics' length equal the
#'     number of observations. }
#' @export
#' @author Yves Croissant
#' @seealso [plm()] where the estimates of the variance components are
#'     used if a random effects model is estimated
#' @references
#'
#' \insertRef{AMEM:71}{plm}
#'
#' \insertRef{NERLO:71}{plm}
#'
#' \insertRef{SWAM:AROR:72}{plm}
#'
#' \insertRef{WALL:HUSS:69}{plm}
#' 
#' @keywords regression
#' @examples
#' 
#' data("Produc", package = "plm")
#' # an example of the formula method
#' ercomp(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc,
#'        method = "walhus", effect = "time")
#' # same with the plm method
#' z <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
#'          data = Produc, random.method = "walhus",
#'          effect = "time", model = "random")
#' ercomp(z)
#' # a two-ways model
#' ercomp(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc,
#'        method = "amemiya", effect = "twoways")
#' 
ercomp <- function(object, ...){
  UseMethod("ercomp")
}

#' @rdname ercomp
#' @export
ercomp.plm <- function(object, ...){
  model <- describe(object, "model")
  if (model != "random") stop("ercomp only relevant for random models")
  object$ercomp
}

#' @rdname ercomp
#' @export
ercomp.pdata.frame <- function(object, effect = c("individual", "time", "twoways", "nested"),
                               method = NULL,
                               models = NULL,
                               dfcor = NULL,
                               index = NULL, ...){
    data <- object
    object <- attr(data, "formula")
    ercomp(object, data, effect = effect, method = method, models = models, dfcor = dfcor, index = index, ...)
}

#' @rdname ercomp
#' @export
ercomp.formula <- function(object, data, 
                           effect = c("individual", "time", "twoways", "nested"),
                           method = NULL,
                           models = NULL,
                           dfcor = NULL,
                           index = NULL, ...){
    effect <- match.arg(effect)

    if (! inherits(object, "Formula")) object <- as.Formula(object)
    # if the data argument is not a pdata.frame, create it using plm
    if (! inherits(data, "pdata.frame"))
        data <- plm(object, data, model = NA, index = index)
    if(is.null(attr(data, "terms"))) data <- model.frame(data, object)
    # check whether the panel is balanced
    balanced <- is.pbalanced(data)
    
    # method and models arguments can't be both set
    if (! is.null(method) && ! is.null(models))
        stop("you can't use both, the 'method' and the 'models' arguments")

    # method and models arguments aren't set, use swar
    if (is.null(method) && is.null(models)) method <- "swar"
    
    # dfcor is set, coerce it to a length 2 vector if necessary
    if (! is.null(dfcor)){
        if (length(dfcor) > 2) stop("dfcor length should be at most 2")
        if (length(dfcor) == 1) dfcor <- rep(dfcor, 2L)
        if (! balanced && any(dfcor != 3))
            stop("dfcor should equal 3 for unbalanced panels")
    }

    # we use later a general expression for the three kinds of effects,
    # select the relevant lines

    therows <- switch(effect,
                      individual = 1:2,
                      time = c(1, 3),
                      twoways = 1:3)

    if (! is.null(method) && method == "nerlove"){
        if (! balanced) stop("Nerlove method only implemented for balanced models")
        est <- plm.fit(data, model = "within", effect = effect)
        pdim <- pdim(data)
        N <- pdim$nT$n
        TS <- pdim$nT$T
        O <- pdim$nT$N
        NTS <- N * (effect != "time") + TS * (effect != "individual") - 1 * (effect == "twoways")
        s2nu <- deviance(est) / O
        # NB: Nerlove takes within residual sums of squares divided by #obs without df correction (Baltagi (2013), p. 23/45)
        s2eta <- s2mu <- NULL
        if (effect != "time")
            s2eta <- sum(fixef(est, type = "dmean", effect = "individual") ^ 2) / (N - 1)
        if (effect != "individual")
            s2mu <- sum(fixef(est, type = "dmean", effect = "time") ^ 2) / (TS - 1)
        sigma2 <- c(idios = s2nu, id = s2eta, time = s2mu)
        theta <- list()
        if (effect != "time")       theta$id   <- (1 - (1 + TS * sigma2["id"]   / sigma2["idios"]) ^ (-0.5))
        if (effect != "individual") theta$time <- (1 - (1 + N  * sigma2["time"] / sigma2["idios"]) ^ (-0.5))
        if (effect == "twoways") {
            theta$total <- theta$id + theta$time - 1 +
                (1 + N * sigma2["time"] / sigma2["idios"] +
                    TS * sigma2["id"]   / sigma2["idios"]) ^ (-0.5)
               # tweak for numerical precision:
            	  # if either theta$id or theta$time is 0 => theta$total must be zero
            	  # but in calculation above some precision is lost
        		 if(  isTRUE(all.equal(sigma2[["time"]], 0, check.attributes = FALSE))
        		 	 || isTRUE(all.equal(sigma2[["id"]],   0, check.attributes = FALSE)))
        		 	     theta$total <- 0
        }
        if (effect != "twoways") theta <- theta[[1L]]
        result <- list(sigma2 = sigma2, theta = theta)
        result <- structure(result, class = "ercomp", balanced = balanced, effect = effect)
        return(result)
    }

    if (! is.null(method) && method == "ht"){
        pdim <- pdim(data)
        N <- pdim$nT$n
        TS <- pdim$nT$T
        O <- pdim$nT$N
        wm <- plm.fit(data, effect = "individual", model = "within")
        X <- model.matrix(data, rhs = 1)
        constants <- apply(X, 2, function(x) all(tapply(x, index(data)[[1L]], is.constant)))
        if (length(object)[2L] > 1){
            W1 <- model.matrix(data, rhs = 2)
            ra <- twosls(fixef(wm, type = "dmean")[as.character(index(data)[[1L]])], X[ , constants, drop = FALSE], W1)
        }
        else{
            FES <- fixef(wm, type = "dmean")[as.character(index(data)[[1L]])]
            XCST <- X[ , constants, drop = FALSE]
            ra <- lm(FES ~ XCST - 1)
        }
        s2nu <- deviance(wm) / (O - N)
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
                                "swar"    = c(2L, 2L),
                                "walhus"  = c(1L, 1L),
                                "amemiya" = c(1L, 1L))
            }
            else dfcor <- c(3L, 3L)
        }
    }
    else{
        # the between estimator is only relevant for the second
        # quadratic form
        if (models[1L] %in% c("Between", "between"))
            stop("the between estimator is only relevant for the between quadratic form")
        # if the argument is of length 2, duplicate the second value
        if (length(models) == 2) models <- c(models[1L], rep(models[2L], 2L))
        # if the argument is of length 1, triple its value
        if (length(models) == 1) models <- c(rep(models, 3L))
        # set one of the last two values to NA in the case of one way
        # model
        if (effect == "individual") models[3L] <- NA
        if (effect == "time")       models[2L] <- NA
         # default value of dfcor 3,3
        if (is.null(dfcor)) dfcor <- c(3L, 3L)
    }

    # The nested error component model
    if (effect == "nested"){
        ids <- attr(data, "index")[[1L]]
        tss <- attr(data, "index")[[2L]]
        gps <- attr(data, "index")[[3L]]
        G <- length(unique(gps))
        Z <- model.matrix(data, model = "pooling")
        X <- model.matrix(data, model = "pooling", cstcovar.rm = "intercept")
        y <- pmodel.response(data, model = "pooling", effect = "individual")
        O <- nrow(Z)
        K <- ncol(Z) - (ncol(Z) - ncol(X))
        pdim <- pdim(data)
        N <- pdim$nT$n
        TS <- pdim$nT$T
        TG <- unique(data.frame(tss, gps))
        TG <- table(TG$gps)
        NG <- unique(data.frame(ids, gps))
        NG <- table(NG$gps)
        Tn <- pdim$Tint$Ti
        Nt <- pdim$Tint$nt
        quad <- vector(length = 3L, mode = "numeric")
        
        M <- matrix(NA_real_, nrow = 3L, ncol = 3L,
                    dimnames = list(c("w", "id", "gp"),
                                    c("nu", "eta", "lambda")))
        
        if (method == "walhus"){
            estm <- plm.fit(data, model = "pooling", effect = "individual")
            hateps <- resid(estm, model = "pooling")
            Between.hateps.group <- Between(hateps, effect = "group")
            quad <- c(crossprod(Within(hateps, effect = "individual")),
                      crossprod(Between(hateps, effect = "individual") - Between.hateps.group),
                      crossprod(Between.hateps.group))
            ZSeta <- model.matrix(estm, model = "Sum", effect = "individual")
            ZSlambda <- Sum(Z, effect = "group")
            CPZM <- solve(crossprod(Z))
            CPZSeta    <- crossprod(ZSeta,    Z)
            CPZSlambda <- crossprod(ZSlambda, Z)
            Between.Z.ind   <- Between(Z, "individual")
            Between.Z.group <- Between(Z, "group")
            Between.Z.ind_minus_Between.Z.group <- Between.Z.ind - Between.Z.group
            CPZW <- crossprod(Z - Between.Z.ind)
            CPZBlambda <- crossprod(Between.Z.group)
            CPZM.CPZW       <- crossprod(CPZM, CPZW)
            CPZM.CPZBlamda  <- crossprod(CPZM, CPZBlambda)
            CPZM.CPZSeta    <- crossprod(CPZM, CPZSeta)
            CPZM.CPZSlambda <- crossprod(CPZM, CPZSlambda)
            CPZM.CPZW.CPZM.CPZSeta    <- crossprod(t(CPZM.CPZW), CPZM.CPZSeta)
            CPZM.CPZW.CPZM.CPZSlambda <- crossprod(t(CPZM.CPZW), CPZM.CPZSlambda)
            
            CPZBetaBlambda     <- crossprod(Between.Z.ind_minus_Between.Z.group)
            CPZBetaBlambdaSeta <- crossprod(Between.Z.ind_minus_Between.Z.group , ZSeta)
            CPZBlambdaSeta     <- crossprod(Between.Z.group, ZSeta)
            
            CPZM.CPZBetaBlambda <- crossprod(CPZM, CPZBetaBlambda)
            CPZM.CPZBlambda     <- crossprod(CPZM, CPZBlambda)
            CPZM.CPZSeta        <- crossprod(CPZM, CPZSeta)
            CPZM.CPZBlambda     <- crossprod(CPZM, CPZBlambda)
            
            M["w", "nu"]      <- O - N - trace(CPZM.CPZW)
            M["w", "eta"]     <- trace(CPZM.CPZW.CPZM.CPZSeta)
            M["w", "lambda"]  <- trace(CPZM.CPZW.CPZM.CPZSlambda)
            M["id", "nu"]     <- N - G - trace(CPZM.CPZBetaBlambda)
            M["id", "eta"]    <- O - sum(TG) - 2 * trace(crossprod(CPZM, CPZBetaBlambdaSeta)) +
                                                   trace(crossprod(t(CPZM.CPZBetaBlambda), CPZM.CPZSeta))
            M["id", "lambda"] <- trace(crossprod(t(CPZM.CPZBetaBlambda), CPZM.CPZSlambda))
            M["gp", "nu"]     <- G - trace(CPZM.CPZBlambda)
            M["gp", "eta"]    <- sum(TG) - 2 * trace(crossprod(CPZM, CPZBlambdaSeta)) +
                                               trace(crossprod(t(CPZM.CPZBlambda), CPZM.CPZSeta))
            M["gp", "lambda"] <- O - 2 * trace(CPZM.CPZSlambda) + 
                                         trace(crossprod(t(CPZM.CPZBlambda), CPZM.CPZSlambda))
        }
        
        if (method == "amemiya"){
            estm <- plm.fit(data, effect = "individual", model = "within")
            hateps <- resid(estm, model = "pooling")
            Betweeen.hateps.group <- Between(hateps, effect = "group")
            XBlambda <- Between(X, "group")
            quad <- c(crossprod(Within(hateps, effect = "individual")),
                      crossprod(Between(hateps, effect = "individual") - Betweeen.hateps.group),
                      crossprod(Betweeen.hateps.group))
            WX <- model.matrix(estm, model = "within", effect = "individual", cstcovar.rm = "all")
            XBetaBlambda <- Between(X, "individual") - XBlambda
            XBlambda <- t(t(XBlambda) - colMeans(XBlambda))
            CPXBlambda <- crossprod(XBlambda)
            CPXM <- solve(crossprod(WX))
            CPXBetaBlambda <- crossprod(XBetaBlambda)
            K <- ncol(WX)
            MK <- length(setdiff("(Intercept)", attr(WX, "constant"))) # Pas sur, a verifier
            KW <- ncol(WX)
            M["w", "nu"]      <- O - N - K + MK
            M["w", "eta"]     <- 0
            M["w", "lambda"]  <- 0
            M["id", "nu"]     <- N - G + trace(crossprod(CPXM, CPXBetaBlambda))
            M["id", "eta"]    <- O - sum(TG)
            M["id", "lambda"] <- 0
            M["gp", "nu"]     <- G - 1 + trace(crossprod(CPXM, CPXBlambda))
            M["gp", "eta"]    <- sum(TG) - sum(NG     * TG ^ 2) / O
            M["gp", "lambda"] <- O       - sum(NG ^ 2 * TG ^ 2) / O
        }
        
        if (method == "swar"){
            yBetaBlambda <- pmodel.response(data, model = "Between", effect = "individual") -
                pmodel.response(data, model = "Between", effect = "group")
            ZBlambda <- Between(Z, "group")
            CPZBlambda.solve <- solve(crossprod(ZBlambda))
            ZBetaBlambda <- Between(Z, "individual") - ZBlambda
            XBetaBlambda <- Between(X, "individual") - Between(X, "group")
            yBlambda <- pmodel.response(data, model = "Between", effect = "group")
            ZSeta <- Sum(Z, effect = "individual")
            ZSlambda <- Sum(Z, effect = "group")
            XSeta <- Sum(X, effect = "individual")
            estm1 <- plm.fit(data, effect = "individual", model = "within")
            estm2 <- lm.fit(ZBetaBlambda, yBetaBlambda)
            estm3 <- lm.fit(ZBlambda, yBlambda)
            quad <- c(crossprod(resid(estm1)),
                      crossprod(resid(estm2)),
                      crossprod(resid(estm3)))
            M["w", "nu"]      <- O - N - K
            M["w", "eta"]     <- 0
            M["w", "lambda"]  <- 0
            M["id", "nu"]     <- N - G - K
            M["id", "eta"]    <- O - sum(TG) - trace(crossprod(t(solve(crossprod(XBetaBlambda))), crossprod(XSeta, XBetaBlambda)))
            M["id", "lambda"] <- 0
            M["gp", "nu"]     <- G - K - 1
            M["gp", "eta"]    <- sum(TG) - trace(crossprod(t(CPZBlambda.solve), crossprod(ZBlambda, ZSeta)))
            M["gp", "lambda"] <- O       - trace(crossprod(t(CPZBlambda.solve), crossprod(ZSlambda, Z)))
        }
        Gs <- as.numeric(table(gps)[as.character(gps)])
        Tn <- as.numeric(table(ids)[as.character(ids)])
        sigma2 <- as.numeric(solve(M, quad))
        names(sigma2) <- c("idios", "id", "gp")
        theta <- list(id = 1 - sqrt(sigma2["idios"] / (Tn * sigma2["id"] + sigma2["idios"])),
                      gp = sqrt(sigma2["idios"] / (Tn * sigma2["id"] + sigma2["idios"])) -
                           sqrt(sigma2["idios"] / (Gs * sigma2["gp"] + Tn * sigma2["id"] + sigma2["idios"]))
                      )
        result <- list(sigma2 = sigma2, theta = theta)
        return(structure(result, class = "ercomp", balanced = balanced, effect = effect))
    } ### END nested models

    # the "classic" error component model
    Z <- model.matrix(data)
    O <- nrow(Z)
    K <- ncol(Z) - 1  # INTERCEPT
    pdim <- pdim(data)
    N <- pdim$nT$n
    TS <- pdim$nT$T
    NTS <- N * (effect != "time") + TS * (effect != "individual") - 1 * (effect == "twoways")
    Tn <- pdim$Tint$Ti
    Nt <- pdim$Tint$nt
    # Estimate the relevant models
    estm <- vector(length = 3L, mode = "list")
    estm[[1L]] <- plm.fit(data, model = models[1L], effect = effect)
    # Check what is the second model
    secmod <- na.omit(models[2:3])[1L]
    if (secmod %in% c("within", "pooling")){
        amodel <- plm.fit(data, model = secmod, effect = effect)
        if (effect != "time")       estm[[2L]] <- amodel
        if (effect != "individual") estm[[3L]] <- amodel
    }
    if (secmod %in% c("between", "Between")){
        if (effect != "time")       estm[[2L]] <- plm.fit(data, model = secmod, effect = "individual")
        if (effect != "individual") estm[[3L]] <- plm.fit(data, model = secmod, effect = "time")
        # check if Between model was estimated correctly
        swar_Between_check(estm[[2L]], method)
        swar_Between_check(estm[[3L]], method)
    }
    KS <- sapply(estm, function(x) length(coef(x))) - sapply(estm, function(x){ "(Intercept)" %in% names(coef(x))})
    quad <- vector(length = 3L, mode = "numeric")
    # first quadratic form, within transformation
    hateps_w <- resid(estm[[1L]], model = "pooling")
    quad[1L] <- crossprod(Within(hateps_w, effect = effect))
    # second quadratic form, between transformation
    if (effect != "time"){
        hateps_id <- resid(estm[[2L]], model = "pooling")
        quad[2L] <- as.numeric(crossprod(Between(hateps_id, effect = "individual")))
    }
    if (effect != "individual"){
        hateps_ts <- resid(estm[[3L]], model = "pooling")
        quad[3L] <- as.numeric(crossprod(Between(hateps_ts, effect = "time")))
    }
    M <- matrix(NA_real_, nrow = 3L, ncol = 3L,
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

    if (dfcor[1L] != 3L){
        # The number of time series in the balanced panel is replaced
        # by the harmonic mean of the number of time series in case of
        # unbalanced panels
        barT <- ifelse(balanced, TS, length(Tn) / sum(Tn ^ (- 1)))
        M["w", "nu"] <- O
        if (dfcor[1L] == 1L) M["w", "nu"] <- M["w", "nu"] - NTS
        if (dfcor[1L] == 2L) M["w", "nu"] <- M["w", "nu"] - NTS - KS[1L]
        if (effect != "time"){
            M["w", "eta"] <- 0
            M["id", "nu"] <- ifelse(dfcor[2L] == 2L, N - KS[2L] - 1, N)
            M["id", "eta"] <- barT * M["id", "nu"]
        }
        if (effect != "individual"){
            M["w", "mu"] <- 0
            M["ts", "nu"] <- ifelse(dfcor[2L] == 2L, TS - KS[3L] - 1, TS)
            M["ts", "mu"] <- N * M["ts", "nu"]
        }
        if (effect == "twoways") {
            M["ts", "eta"] <- M["id", "mu"] <- 0
        }
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
        if (models[1L] == "pooling"){
            ZW <- model.matrix(estm[[1L]], model = "within", effect = effect, cstcovar.rm = "none")
            CPZW <- crossprod(ZW)
            CPZM.CPZW    <- crossprod(CPZM, CPZW)
            M["w", "nu"] <- O - NTS - trace(CPZM.CPZW)
            if (effect != "time"){
                CPZM.CPZSeta <- crossprod(CPZM, CPZSeta)
                M["w", "eta"] <- trace(crossprod(t(CPZM.CPZW), CPZM.CPZSeta))
            }
            if (effect != "individual"){
                CPZM.CPZSmu  <- crossprod(CPZM, CPZSmu)
                M["w", "mu"] <- trace(crossprod(t(CPZM.CPZW), CPZM.CPZSmu))
            }
        }
        if (secmod == "pooling"){
            if (effect != "time"){
                ZBeta <- model.matrix(estm[[2L]], model = "Between", effect = "individual")
                CPZBeta <- crossprod(ZBeta)
                CPZM.CPZBeta <- crossprod(CPZM, CPZBeta)
                CPZM.CPZSeta <- crossprod(CPZM, CPZSeta)
                CPZM.CPZBeta.CPZM.CPZSeta <- crossprod(t(CPZM.CPZBeta), CPZM.CPZSeta) # == CPZM %*% CPZBeta %*% CPZM %*% CPZSeta
                M["id", "nu"]  <- N -     trace(CPZM.CPZBeta)
                M["id", "eta"] <- O - 2 * trace(CPZM.CPZSeta) + 
                                          trace(CPZM.CPZBeta.CPZM.CPZSeta)
            }
            if (effect != "individual"){
                ZBmu <- model.matrix(estm[[3L]], model = "Between", effect = "time")
                CPZBmu <- crossprod(ZBmu)
                CPZM.CPZBmu <- crossprod(CPZM, CPZBmu)
                CPZM.CPZSmu <- crossprod(CPZM, CPZSmu)
                CPZM.CPZBmu.CPZM.CPZSmu <- crossprod(t(CPZM.CPZBmu), CPZM.CPZSmu)
                M["ts", "nu"] <- TS -    trace(CPZM.CPZBmu)
                M["ts", "mu"] <- O - 2 * trace(CPZM.CPZSmu) +
                                         trace(CPZM.CPZBmu.CPZM.CPZSmu)
            }
            if (effect == "twoways"){
                CPZBmuSeta <- crossprod(ZBmu, ZSeta)
                CPZBetaSmu <- crossprod(ZBeta, ZSmu)
                CPZM.CPZBetaSmu <- crossprod(CPZM, CPZBetaSmu)
                CPZM.CPZBmuSeta <- crossprod(CPZM, CPZBmuSeta)
                	## These are already calc. by effect != "individual" / effect != "time"
                	# CPZM.CPZSmu <- crossprod(CPZM, CPZSmu) 
                	# CPZM.CPZBmu <- crossprod(CPZM, CPZBmu)
                	# CPZM.CPZBeta <- crossprod(CPZM, CPZBeta)
                	# CPZM.CPZSeta <- crossprod(CPZM, CPZSeta)
                CPZM.CPZBeta.CPZM.CPZSmu <- crossprod(t(CPZM.CPZBeta), CPZM.CPZSmu) # == CPZM %*% CPZBeta %*% CPZM %*% CPZSmu
                CPZM.CPZBmu.CPZM.CPZSeta <- crossprod(t(CPZM.CPZBmu), CPZM.CPZSeta) # == CPZM %*% CPZBmu %*% CPZM %*% CPZSeta
                M["id", "mu"]  <- N  - 2 * trace(CPZM.CPZBetaSmu) + 
                                           trace(CPZM.CPZBeta.CPZM.CPZSmu) 
                M["ts", "eta"] <- TS - 2 * trace(CPZM.CPZBmuSeta) +
                                           trace(CPZM.CPZBmu.CPZM.CPZSeta)
            }
        }
        if ("within" %in% models){
            WX <- model.matrix(estm[[match("within", models)]], model = "within",
                               effect = effect, cstcovar.rm = "all")
#            K <- ncol(WX)
#            MK <- length(attr(WX, "constant")) - 1
            KW <- ncol(WX)
            if (models[1L] == "within"){
                M["w", "nu"] <- O - NTS - KW # + MK                                        # INTERCEPT
                if (effect != "time")       M["w", "eta"] <- 0
                if (effect != "individual") M["w", "mu"]  <- 0
            }
            if (secmod == "within"){
                CPXM <- solve(crossprod(WX))
                if (effect != "time"){
                    XBeta <- model.matrix(estm[[2L]], model = "Between",
                                          effect = "individual")[ , -1L, drop = FALSE]    # INTERCEPT
                    XBeta <- t(t(XBeta) - colMeans(XBeta))
                    CPXBeta <- crossprod(XBeta)
                    amemiya_check(CPXM, CPXBeta, method) # catch non-estimable 'amemiya'
                    M["id", "nu"] <- N - 1 + trace( crossprod(CPXM, CPXBeta) )
                    M["id", "eta"] <- O - sum(Tn ^ 2) / O
                }
                if (effect != "individual"){
                    XBmu <- model.matrix(estm[[3L]], model = "Between",
                                         effect = "time")[ , -1L, drop = FALSE]           # INTERCEPT
                    XBmu <- t(t(XBmu) - colMeans(XBmu))
                    CPXBmu <- crossprod(XBmu)
                    amemiya_check(CPXM, CPXBmu, method) # catch non-estimable 'amemiya'
                    M["ts", "nu"] <- TS - 1 + trace( crossprod(CPXM, CPXBmu) )
                    M["ts", "mu"] <- O - sum(Nt ^ 2) / O
                }
                if (effect == "twoways"){
                    M["id", "mu"] <- N - sum(Nt ^ 2) / O
                    M["ts", "eta"] <- TS - sum(Tn ^ 2) / O
                }
            }
        } # END if ("within" %in% models)
        if (length(intersect(c("between", "Between"), models))){
            if (effect != "time"){
                Zeta  <- model.matrix(estm[[2L]], model = "pooling", effect = "individual")
                ZBeta <- model.matrix(estm[[2L]], model = "Between", effect = "individual")
                ZSeta <- model.matrix(estm[[2L]], model = "Sum", effect = "individual")
                CPZSeta <- crossprod(ZSeta, Z)
                CPZMeta <- solve(crossprod(ZBeta))
                M["id", "nu"]  <- N - K - 1
                M["id", "eta"] <- O - trace( crossprod(CPZMeta, CPZSeta) )
            }
            if (effect != "individual"){
                Zmu  <- model.matrix(estm[[3L]], model = "pooling", effect = "time")
                ZBmu <- model.matrix(estm[[3L]], model = "Between", effect = "time")
                ZSmu <- model.matrix(estm[[3L]], model = "Sum", effect = "time")
                CPZSmu <- crossprod(ZSmu, Z)
                CPZMmu <- solve(crossprod(ZBmu))
                M["ts", "nu"] <- TS - K - 1
                M["ts", "mu"] <- O - trace( crossprod(CPZMmu, CPZSmu) )
            }
            if (effect == "twoways"){
                if (! balanced){
                    ZSmuBeta <- Sum(ZBeta, effect = "time")
                    ZBetaSmuBeta <- crossprod(ZBeta, ZSmuBeta)
                    ZSetaBmu <- Sum(ZBmu, effect = "individual")
                    ZBmuSetaBmu <- crossprod(ZBmu, ZSetaBmu)
                    M["id", "mu"]  <- N  - trace(crossprod(CPZMeta, ZBetaSmuBeta))
                    M["ts", "eta"] <- TS - trace(crossprod(CPZMmu, ZBmuSetaBmu))
                }
                else M["id", "mu"] <- M["ts", "eta"] <- 0
            }
        }
    } ## END of General case, compute the unbiased version of the estimators
    sigma2 <- as.numeric(solve(M[therows, therows], quad[therows]))
    names(sigma2) <- c("idios", "id", "time")[therows]
    sigma2[sigma2 < 0] <- 0
    theta <- list()
    if (! balanced){
        ids <- index(data)[[1L]]
        tss <- index(data)[[2L]]
        Tns <- Tn[as.character(ids)]
        Nts <- Nt[as.character(tss)]
    }
    else{
        Tns <- TS
        Nts <- N
    }
    if (effect != "time")       theta$id   <- (1 - (1 + Tns * sigma2["id"]   / sigma2["idios"]) ^ (-0.5))
    if (effect != "individual") theta$time <- (1 - (1 + Nts * sigma2["time"] / sigma2["idios"]) ^ (-0.5))
    if (effect == "twoways") {
        theta$total <- theta$id + theta$time - 1 +
            (1 + Nts * sigma2["time"] / sigma2["idios"] +
                 Tns * sigma2["id"]   / sigma2["idios"]) ^ (-0.5)
        # tweak for numerical precision:
        # if either theta$id or theta$time is 0 => theta$total must be zero
        # but in calculation above some precision is lost
        if(     isTRUE(all.equal(sigma2[["time"]], 0, check.attributes = FALSE))
                 || isTRUE(all.equal(sigma2[["id"]],   0, check.attributes = FALSE)))
            theta$total <- 0
    }
    if (effect != "twoways") theta <- theta[[1L]]
    result <- list(sigma2 = sigma2, theta = theta)
    structure(result, class = "ercomp", balanced = balanced, effect = effect)
}

#' @rdname ercomp
#' @export
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
                cat(paste("theta: ", signif(x$theta,digits), "\n", sep = ""))
            }
            else{
                cat("theta:\n")
                print(summary(x$theta))
            }
        }
        if (effect == "twoways"){
            if(balanced){
                cat(paste("theta: ", signif(x$theta$id,digits), " (id) ",
                                     signif(x$theta$time,digits), " (time) ",
                                     signif(x$theta$total,digits), " (total)\n", sep = ""))
            } else {
              cat("theta:\n")
              print(rbind(id = summary(x$theta$id),
                          time = summary(x$theta$time),
                          total = summary(x$theta$total)))
            }
        }
        if (effect == "nested"){
            cat("theta:\n")
            print(rbind(id = summary(x$theta$id),
                        group = summary(x$theta$gp)))
        }
    }
}

amemiya_check <- function(matA, matB, method) {
  ## non-exported, used in ercomp()
  ## little helper function to check matrix multiplication compatibility
  ## in ercomp() for the amemiya estimator: if model contains variables without
  ## within variation (individual or time), the model is not estimable
  if (NROW(matA) < NCOL(matB) && method == "amemiya" ) {
    offending_vars <- setdiff(colnames(matB), rownames(matA))
    offending_vars <- if (length(offending_vars) > 3) {
      paste0(paste(offending_vars[1:3], collapse = ", "), ", ...") 
      } else { 
        paste(offending_vars, collapse = ", ")
      }
    stop(paste0("'amemiya' model not estimable due to variable(s) lacking within variation: ", offending_vars))
  } else NULL
}


swar_Between_check <- function(x, method) {
    ## non-exported, used in ercomp()
    ## little helper function to check feasibility of Between model in Swamy-Arora estimation
    ## in ercomp(): if model contains too few groups (individual, time) the Between
    ## model is not estimable (but does not error)
    if (describe(x, "model") %in% c("between", "Between")) {
    pdim <- pdim(x)
    grp <- switch(describe(x, "effect"),
                        "individual" = pdim$nT$n,
                        "time"       = pdim$nT$T)
    # cannot use df.residual(x) here because that gives the number for the "uncompressed" Between model
    if (length(x$aliased) >= grp) stop(paste0("model not estimable as there are ", length(x$aliased),
                                                          " coefficient(s) (incl. intercept) to be estimated for the between model but only ",
                                                          grp, " ", describe(x, "effect"), "(s)"))
    } else NULL
}
