pgmm <- function(formula, data, subset, na.action,
                 effect = c("twoways", "individual"),
                 model = c("onestep", "twosteps"),
                 collapse = FALSE,
                 lost.ts = NULL,
                 transformation = c("d", "ld"), fsm = NULL,
                 index = NULL, ...){

  # yX : response / covariates, W : gmm instruments, Z : normal
  # instruments, V : time dummies
  
  cl <- match.call(expand.dots = FALSE)
  cl <- match.call(expand.dots = TRUE)
  effect <- match.arg(effect)
  model <- match.arg(model)
  transformation <- match.arg(transformation)
  namesV <- NULL
  
  #################################################################
  ##### 1. Backward compatibility with the old formula / dynformula
  ##### interface
  #################################################################
  
  if (inherits(formula, "dynformula") || length(Formula(formula))[2] == 1){
    if (!inherits(formula, "dynformula")){
      formula <- match.call(expand.dots = TRUE)
      m <- match(c("formula", "lag.form", "diff.form", "log.form"),names(formula),0)
      formula <- formula[c(1, m)]
      formula[[1]] <- as.name("dynformula")
      formula <- cl$formula <- eval(formula, parent.frame())
    }
    response.name <- paste(deparse(formula[[2]]))
    main.lags <- attr(formula, "lag")
    if (length(main.lags[[1]]) == 1 && main.lags[[1]] > 1)
      main.lags[[1]] <- c(1, main.lags[[1]])
    main.lags[2:length(main.lags)] <- lapply(main.lags[2:length(main.lags)],
                        function(x){
                          if (length(x) == 1 && x != 0) x <- c(0, x)
                          x
                        })
    main.form <- dynterms2formula(main.lags, response.name)
    dots <- list(...)
    gmm.inst <- dots$gmm.inst
    lag.gmm <- dots$lag.gmm
    instruments <- dots$instruments
    gmm.form <- dynformula(gmm.inst, lag.form = lag.gmm)
    gmm.lags <- attr(gmm.form, "lag")
    gmm.lags <- lapply(gmm.lags, function(x) min(x):max(x))
    gmm.form <- dynterms2formula(gmm.lags)
    formula <- as.Formula(main.form, gmm.form)
  }

  #################################################################
  ##### 2. Extract the response/covariates, the gmm instruments and
  ##### the "normal" instruments, as a named list containing the lag
  ##### structure
  #################################################################
  
  x <- formula
  if (!inherits(x, "Formula")) x <- Formula(formula)
  # gmm instruments : named list with the lags, names being the variables
  gmm.form <- formula(x, rhs = 2, lhs = 0)
  gmm.lags <- dynterms(gmm.form)

  cardW <- length(gmm.lags)
  if (is.null(names(collapse))){
    if (length(collapse) == 1){
      collapse <- as.vector(rep(collapse, cardW), mode = "list")
    }
    else{
      if (length(collapse) != cardW) stop("the collapse vector has a wrong length")
    }
    names(collapse) <- names(gmm.lags)
  }
  else{
     if (any(! (names(collapse) %in% names(gmm.lags)))) stop("unknown names in the collapse vector")
     else{
       bcollapse <- as.vector(rep(FALSE, cardW), mode = "list")
       names(bcollapse) <- names(gmm.lags)
       bcollapse[names(collapse)] <- collapse
       collapse <- bcollapse
     }
  }
   
  # covariates : named list with the lags, names being the variables
  main.form <- formula(x, rhs = 1, lhs = 1)
  main.lags <- dynterms(main.form)

  # Three possibilities for 'normal' instruments :
  # 1. the third part of the formula describes them
  # 2. all variables not used as gmm are normal instruments
  # 3. all variables are gmm instruments and therefore, there are no
  # normal instruments except maybe time dummies
  
  # the third part of the formula (if any) deals with the 'normal' instruments
  if (length(x)[2] == 3){
    normal.instruments <- TRUE
    inst.form <- formula(x, rhs = 3, lhs = 0)
    # the . - x1 + x2 syntax is allowed, in this case update with the first part
    inst.form <- update(main.form, inst.form)
    inst.form <- formula(Formula(inst.form), lhs = 0)
    inst.lags <- dynterms(inst.form)
  }
  else{
    # the default 'normal' instruments is the subset of covariates
    # which are not used as gmm instruments
    iv <- names(main.lags)[! names(main.lags) %in% names(gmm.lags)]
    inst.lags <- main.lags[iv]
    # generate the formula for 'normal' instruments
    if (length(inst.lags) > 0){
      normal.instruments <- TRUE
      inst.form <- dynterms2formula(inst.lags)
    }
    else{
      # the case where there are no normal instruments : put inst.form
      # and inst.lags to NULL
      normal.instruments <- FALSE
      inst.form <- NULL
      inst.lags <- NULL
    }
  }
  
  #################################################################
  ##### 3. How many time series are lost
  #################################################################

  if (!is.null(lost.ts)){
    if (!is.numeric(lost.ts)) stop("lost.ts should be numeric")
    lost.ts <- as.numeric(lost.ts)
    if (!(length(lost.ts) %in% c(1, 2))) stop("lost.ts should be of length 1 or 2")
    TL1 <- lost.ts[1]
    TL2 <- ifelse(length(lost.ts == 1), TL1 - 1, lost.ts[2])
  }
  else{
    # How many time series are lost ? May be the maximum number of lags
    # of any covariates + 1 because of first - differencing or the
    # largest minimum lag for any gmm or normal instruments
    gmm.minlag <- max(sapply(gmm.lags, min))
    # min or max to select the number of lost time series ?
    gmm.minlag <- min(sapply(gmm.lags, min))
    if (!is.null(inst.lags)) inst.maxlag <- max(sapply(inst.lags, max))
    else inst.maxlag <- 0
    main.maxlag <- max(sapply(main.lags, max))
    TL1 <- max(main.maxlag + 1, inst.maxlag + 1, gmm.minlag)
    TL2 <- max(main.maxlag, inst.maxlag, gmm.minlag - 1)
    # if TL2 = 0 (no lags), one observation is lost anyway because of
    # the differentiation of the lag instruments
    TL1 <- max(main.maxlag + 1, gmm.minlag)
    TL2 <- max(main.maxlag, gmm.minlag - 1)
  }

  #################################################################
  ##### 4. Compute the model frame which contains the
  ##### response/covariates, the gmm instruments and the 'normal'
  ##### instruments without the lags
  #################################################################
  
  gmm.form <- as.formula(paste("~", paste(names(gmm.lags), collapse = "+")))
  if (!is.null(inst.form))  Form <- as.Formula(main.form, gmm.form, inst.form)
  else Form <- as.Formula(main.form, gmm.form)
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "na.action", "index"), names(mf), 0)
  mf <- mf[c(1, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1]] <- as.name("plm")
  mf$model <- NA
  mf$formula <- Form
  mf$na.action <- "na.pass"
  mf$subset <- NULL
  data <- eval(mf, parent.frame())
  index <- index(data)
  N <- length(levels(index[[1]]))
  T <- length(levels(index[[2]]))
  pdim <- pdim(data)
  balanced <- pdim$balanced

  # if the data is unbalanced, "balance" the data
  if (!balanced){
    un.id <- sort(unique(index(data, "id")))
    un.time <- sort(unique(index(data, "time")))
    rownames(data) <- paste(index(data, "id"), index(data, "time"), sep = ".")
    allRows <- as.character(t(outer(un.id, un.time, paste, sep = ".")))
    data <- data[allRows, ]
    rownames(data) <- allRows
    index <- data.frame(id = rep(un.id, each = length(un.time)),
                        time = rep(un.time, length(un.id)),
                        row.names = rownames(data))
    class(index) <- c("pindex", "data.frame")
    attr(data, "index") <- index
  }
  
  #################################################################
  ##### 5. Get the response/covariates matrix yX, the gmm instruments
  ##### matrix W and the normal instruments matrix inst, splited by
  ##### individuals
  #################################################################

  attr(data, "formula") <- formula(main.form)
  yX <- extract.data(data)
  names.coef <- colnames(yX[[1]])[-1]
  if (normal.instruments){
    attr(data, "formula") <- inst.form
    Z <- extract.data(data)
  }
  else Z <- NULL
  attr(data, "formula") <- gmm.form
  W <- extract.data(data, as.matrix = FALSE)
  
  #################################################################
  ##### 6. Create the matrix of response/covariates, gmm instruments
  ##### and normal instruments for the diff model
  #################################################################
  # create the matrix of gmm instruments for every individual
  W1 <- lapply(W,
               function(x){
                 u <- mapply(makegmm, x, gmm.lags, TL1, collapse, SIMPLIFY = FALSE)
                 u <- matrix(unlist(u), nrow = nrow(u[[1]]))
                 u
               }
               )

  # differenciate the matrix of response/covariates (and of normal
  # instruments if any) and remove T1 - 1 time series (xd is already
  # differenced)
  yX1 <- lapply(yX,
                function(x){
                  xd <- diff(x)
                  xd <- xd[- c(1:(TL1 - 1)), , drop = FALSE]
                  xd
                }
                )
  if (normal.instruments){
    Z1 <- lapply(Z,
                 function(x){
                   xd <- diff(x)
                   xd <- xd[- c(1:(TL1 - 1)), , drop = FALSE]
                   xd
                 }
                 )
  }
  
  #################################################################
  ##### 7. In case of system gmm, create the matrix of
  ##### response/covariates, gmm instruments and normal instruments
  ##### for the level model and merge with the diff model
  #################################################################

  if (transformation == "ld"){
    W2 <- lapply(W,
                 function(x){
                   u <- mapply(makeW2,x, collapse, SIMPLIFY = FALSE)
                   # the matrix of instruments in difference has T - 2
                   # rows if one time series is lost (there are no gmm
                   # instruments for t = 2 but there is a moment
                   # condition with the intercept. In this case, a row
                   # of 0 should be added. Otherwise, the number of
                   # rows is just T - TL2
                   nrow.ud <- ifelse(TL2 == 1, T - 2, T - TL2)
                   u <- matrix(unlist(u), nrow = nrow.ud)
                   if (TL2 == 1) u <- rbind(0, u)
                   u
                 }
                 )
    # remove the relevant number of time series for data in level
    yX2 <- lapply(yX,
                  function(x){
                    x <- x[- c(0:TL2), , drop = FALSE]
                    x
                  }
                  )
    if (normal.instruments){
      Z2 <- lapply(Z, function(x){x <- x[- c(0:TL2), , drop = FALSE]; x})
    }
  }

  #################################################################
  ##### 8. Add time dummies if effect = twoways
  #################################################################

  if (effect == "twoways"){
    namesV <- levels(index(data, which = "time"))
    if (transformation == "d"){
      V1 <-td.model.diff <- diff(diag(1, T - TL1 + 1))[, -1]
      namesV <- namesV[- c(0:(TL1))]
    }
    else{
      td <- cbind(1, rbind(0, diag(1, T - 1)))
      # remove as many columns and row as there are lost time series
      # in level (the difference of position between rows and columns
      # is due to the fact that the first column of td is the
      # intercept and should be kept anyway
      V2 <- td[- c(1:TL2), - c(2:(2 + TL2 - 1))]
      V1 <- diff(V2)
      namesV <- c("(intercept)", namesV[- c(0:TL2 + 1)])
    }
    for (i in 1:N){
      yX1[[i]] <- cbind(yX1[[i]], V1)
      if (transformation == "d"){
        W1[[i]] <- cbind(W1[[i]], V1)
      }
      else{
        W2[[i]] <- cbind(W2[[i]], V2)
        yX2[[i]] <- cbind(yX2[[i]], V2)
      }
    }
  }
  # A QAD fix for the bug in mtest for ld model without time.dummies
  if (effect == "individual" && transformation == "ld"){
    namesV <- levels(index(data, which = "time"))
    namesV <- c("(intercept)", namesV[-c(0:TL2 + 1)])
  }
  
  #################################################################
  ##### 9. In case of unbalanced data, replace NA's by 0 and overwrite
  ##### rows for missing time series with 0
  #################################################################

  for (i in 1:N){
    narows <- apply(yX1[[i]], 1, function(z) any(is.na(z)))
    yX1[[i]][narows, ] <- 0
    W1[[i]][is.na(W1[[i]])] <- 0
    W1[[i]][narows, ] <- 0
    if (normal.instruments){
      Z1[[i]][is.na(Z1[[i]])] <- 0
      Z1[[i]][narows, ] <- 0
    }
    if (transformation == "ld"){
      narows <- apply(yX2[[i]], 1, function(z) any(is.na(z)))
      yX2[[i]][narows, ] <- 0
      W2[[i]][is.na(W2[[i]])] <- 0
      W2[[i]][narows, ] <- 0
      if (normal.instruments){
        Z2[[i]][is.na(Z2[[i]])] <- 0
        Z2[[i]][narows, ] <- 0
      }
    }
  }

  #################################################################
  ##### 10. In case of sys gmm, bdiag or rbind the diff and level
  ##### matrices
  #################################################################
  
  if (transformation == "ld"){
    for (i in 1:N){
      W1[[i]] <- bdiag(W1[[i]], W2[[i]])
      yX1[[i]] <- rbind(yX1[[i]], yX2[[i]])
      if (normal.instruments) Z1[[i]] <- bdiag(Z1[[i]], Z2[[i]])
    }
  }
  if (normal.instruments){
    for (i in 1:N) W1[[i]] <- cbind(W1[[i]], Z1[[i]])
  }

  
  #################################################################
  ##### 11. Compute the estimator
  #################################################################

  W <- W1 ; yX <- yX1
  
  # Compute the first step matrices
  if (transformation == "d") A1 <- tcrossprod(diff(diag(1, T - TL1 + 1)))
  if (transformation == "ld") A1 <- FSM(T - TL2, "full")

  # compute the estimator
  
  ## WX <- mapply(function(x, y) crossprod(x, y), W, yX, SIMPLIFY = FALSE)
  ## WX <- Reduce("+", WX)
  ## zerolines <- which(apply(WX, 1, function(z) sum(abs(z))) == 0)
  ## for (i in 1:N) W[[i]] <- W[[i]][, - zerolines]

  WX <- mapply(function(x, y) crossprod(x, y), W, yX, SIMPLIFY = FALSE)
  Wy <- lapply(WX, function(x) x[, 1])
  WX <- lapply(WX, function(x) x[, -1])
  A1 <- lapply(W, function(x) crossprod(t(crossprod(x, A1)), x))
  A1 <- Reduce("+", A1)
  minevA1 <- min(eigen(A1)$values)
  eps <- 1E-9
  if (minevA1 < eps){
    A1 <- ginv(A1) * length(W)
    warning("the first-step matrix is singular, a general inverse is used")
  }
  else A1 <- solve(A1) * length(W)
  WX <- Reduce("+", WX)
  Wy <- Reduce("+", Wy)
  B1 <- solve(crossprod(WX, t(crossprod(WX, A1))))
  Y1 <- crossprod(t(crossprod(WX, A1)), Wy)
  coefficients <- as.numeric(crossprod(B1, Y1))
  if (effect == "twoways") names.coef <- c(names.coef, namesV)
  names(coefficients) <- names.coef
  residuals <- lapply(yX,
                      function(x)
                      as.vector(x[,1] -  crossprod(t(x[,-1, drop=FALSE]), coefficients)))
  outresid <- lapply(residuals,function(x) outer(x,x))
  A2 <- mapply(function(x, y) crossprod(t(crossprod(x, y)), x), W, outresid, SIMPLIFY = FALSE)
  A2 <- Reduce("+", A2)
  minevA2 <- min(eigen(A2)$values)
  eps <- 1E-9
  if (minevA2 < eps){
    A2 <- ginv(A2)
    warning("the second-step matrix is singular, a general inverse is used")
  }
  else A2 <- solve(A2)

  B2 <- solve(crossprod(WX, t(crossprod(WX, A2))))

  if (model == "twosteps"){
    coef1s <- coefficients
    Y2 <- crossprod(t(crossprod(WX, A2)), Wy)
    coefficients <- as.numeric(crossprod(B2, Y2))
    names(coefficients) <- c(names.coef)
    vcov <- B2
  }
  else vcov <- B1
  rownames(vcov) <- colnames(vcov) <- c(names.coef)
  residuals <- lapply(yX,
                      function(x){
                        nz <- rownames(x)
                        z <- as.vector(x[, 1] - crossprod(t(x[, -1, drop=FALSE]), coefficients))
                        names(z) <- nz
                        z
                      }
                      )
  fitted.values <- mapply(function(x,y) x[, 1] - y, yX, residuals)
  if (model == "twosteps") coefficients <- list(coef1s, coefficients)
  args <- list(model = model, effect = effect,
               transformation = transformation, namest = namesV)
  result <- list(coefficients = coefficients, residuals = residuals, vcov = vcov,
                 fitted.values = fitted.values,
                 df.residual = df.residual, 
                 model = yX, W = W, A1 = A1, A2 = A2,
                 call = cl, args = args)
  result <- structure(result, class = c("pgmm", "panelmodel"),
                      pdim = pdim)
  result
}

dynterms <- function(x){
  trms.lab <- attr(terms(x), "term.labels")
  result <- getvar(trms.lab)
  nv <- names(result)
  dn <- names(table(nv))[table(nv) > 1]
  un <- names(table(nv))[table(nv) == 1]
  resu <- result[un]
  for (i in dn){
    v <- sort(unique(unlist(result[nv == i])))
    names(v) <- NULL
    resu[[i]] <- v
  }
  resu
}

getvar <- function(x){
  x <- as.list(x)
  result <- lapply(x, function(y){
    deb <- as.numeric(gregexpr("lag\\(", y)[[1]])
    if (deb == -1){
      lags <- 0
      avar <- y
    }
    else{
#      inspar <- substr(y, deb + 2, nchar(y) - 1)
      inspar <- substr(y, deb + 4, nchar(y) - 1)
      coma <- as.numeric(gregexpr(",", inspar)[[1]][1])
      if (coma == -1){
        endvar <- nchar(inspar)
        lags <- 1
      }
      else{
        endvar <- coma - 1
        lags <- substr(inspar, coma + 1, nchar(inspar))
        lags <- eval(parse(text = lags))
      }
      avar <- substr(inspar, 1, endvar)
    }
    list(avar, lags)
  }
                   )
  nres <- sapply(result, function(x) x[[1]])
  result <- lapply(result, function(x) x[[2]])
  names(result) <- nres
  result
  
}

dynterms2formula <- function(x, response.name = NULL){
  result <- character(0)
  for (i in 1:length(x)){
    theinst <- x[[i]]
    # if the first element is zero, write the variable without lag and
    # drop the 0 from the vector
    if (theinst[1] == 0){
      at <- names(x)[i]
      theinst <- theinst[-1]
    }
    else{
      at <- character(0)
    }
    # if there are still some lags, write them
    if (length(theinst) > 0){
      if (length(theinst) > 1){
        at <- c(at, paste("lag(",names(x)[i],",c(",
                          paste(theinst, collapse = ","), "))", sep =""))
      }
      else{
        at <- c(at, paste("lag(",names(x)[i], ",", theinst, ")", sep =""))
      }
    }
    result <- c(result, at)
  }
  if (is.null(response.name)) as.formula(paste("~", paste(result, collapse = "+")))
  else as.formula(paste(response.name, "~", paste(result, collapse = "+")))
}

extract.data <- function(data, as.matrix = TRUE){
  # the previous version is *very* slow because :
  # 1. split works wrong on pdata.frame
  # 2. model.matrix is lapplied !
  form <- attr(data, "formula")
  trms <- terms(form)
  has.response <- attr(trms, 'response') == 1
  has.intercept <- attr(trms, 'intercept') == 1
  if (has.intercept == 1){
    # Formula is unable to update formulas with no lhs
    form <- Formula(update(formula(form), ~. -1))
#    form <- update(form, ~. -1)
  }
  index <- attr(data, "index")
  
  X <- model.matrix(form, data)
  if (has.response){
    X <- cbind(data[[1]], X)
    colnames(X)[1] <- deparse(trms[[2]])
  }
  data <- split(as.data.frame(X), index[[1]])
  time <- split(index[[2]], index[[1]])
  data <- mapply(
                 function(x, y){
                   rownames(x) <- y
                   if (as.matrix) x <- as.matrix(x)
                   x
                 }
                 , data, time, SIMPLIFY = FALSE)
  data
}

G <- function(t){
  G <- matrix(0,t,t)
  for (i in 1:(t-1)){
    G[i,i] <- 2
    G[i,i+1] <- -1
    G[i+1,i] <- -1
  }
  G[t,t] <- 2
  G
}

FD <- function(t){
  FD <- Id(t)[-1,]
  for (i in 1:(t-1)){
    FD[i,i] <- -1
  }
  FD
}

Id <- function(t){
  diag(rep(1,t))
}

FSM <- function(t,fsm){
  switch(fsm,
         "I"=Id(t),
         "G"=G(t),
         "GI"=bdiag(G(t-1),diag(1,t)),
         "full"=rbind(cbind(G(t-1),FD(t)),cbind(t(FD(t)),Id(t)))
         )
}

makegmm <- function(x, g, TL1, collapse = FALSE){
  T <- length(x)
  rg <- range(g)
  z <- as.list((TL1 + 1):T)
  x <- lapply(z, function(y) x[max(1, y - rg[2]):(y - rg[1])])
  if (collapse) {      
    x<-lapply(x, rev)
    m <- matrix(0, T - TL1, min(T - rg[1], rg[2]+1-rg[1]))
    for(y in 1:length(x)){ m[y,1:length(x[[y]])]<-x[[y]]}
    result<-m
   }
   else {
     lx <- sapply(x, length)
     n <- length(x)
     lxc <- cumsum(lx)
     before <- c(0, lxc[-n])
     after <- lxc[n] - sapply(x, length) - before
     result <- t(mapply(function(x, y, z) 
                        c(rep(0, y), x, rep(0, z)), 
                        x, before, after, SIMPLIFY = TRUE))
    }
    result
}


makeW2<-function (x, collapse = FALSE){
  if (collapse) {
    u<-diff(x[-c(length(x))])
   }
   else {
     u<-diag(diff(x[-c(length(x))]))
   }
   u
}

coef.pgmm <- function(object,...){
  model <- describe(object, "model")
  if(model == "onestep") coefficients <- object$coefficients
  else coefficients <- object$coefficients[[2]]
  coefficients
}

summary.pgmm <- function(object, robust = TRUE, time.dummies = FALSE, ...){
  model <- describe(object, "model")
  effect <- describe(object, "effect")
  transformation <- describe(object, "transformation")
  if (robust){
    vv <- vcovHC(object)
    A <- object$A2
  }
  else{
    vv <- vcov(object)
    A <- object$A1
  }
  if (model == "onestep")   K <- length(object$coefficients)
  else  K <- length(object$coefficients[[2]])
  object$sargan <- sargan(object, "twosteps")
  object$m1 <- mtest(object, 1, vv)
  object$m2 <- mtest(object, 2, vv)
  object$wald.coef <- wald(object, "coef", vv)
  if (describe(object, "effect") == "twoways") object$wald.td <- wald(object,"time",vv)
  Kt <- length(object$args$namest)
  if (! time.dummies && effect == "twoways") rowsel <- -c((K - Kt + 1):K)
  else rowsel <- 1:K
  std.err <- sqrt(diag(vv))
  b <- coef(object)
  z <- b / std.err
  p <- 2 * pnorm(abs(z), lower.tail = FALSE)
  coefficients <- cbind(b, std.err, z, p)
  colnames(coefficients) <- c("Estimate", "Std. Error", "z-value", "Pr(>|z|)")
  object$coefficients <- coefficients[rowsel, , drop = FALSE]
  class(object) <- "summary.pgmm"
  object
}

mtest <- function(object, order = 1, vcov = NULL){
  myvcov <- vcov
  if (is.null(vcov)) vv <- vcov(object)
  else if (is.function(vcov)) vv <- myvcov(object)
  else vv <- myvcov
  model <- describe(object, "model")
  transformation <- describe(object, "transformation")
  Kt <- length(object$args$namest)
  if (transformation == "d"){
    resid <- object$residuals
    residl <- lapply(resid,
                     function(x) c(rep(0,order), x[1:(length(x)-order)])
                     )
  }
  else{
    resid <- lapply(object$residuals,
                    function(x) c(x[-c(Kt:(2*Kt + 1))], rep(0, Kt)))
    residl <- lapply(object$residuals,
                     function(x) c(rep(0, order), x[1:(Kt-order-1)], rep(0, Kt)))
  }
  X <- lapply(object$model, function(x) x[,-1, drop=FALSE])
  W <- object$W
  if (model == "onestep") A <- object$A1
  else  A <- object$A2
  EVE <- Reduce("+",
                mapply(function(x, y) t(y) %*% x %*% t(x) %*%y, resid, residl, SIMPLIFY = FALSE))
  EX <- Reduce("+", mapply(crossprod, residl, X, SIMPLIFY = FALSE))
  XZ <- Reduce("+", mapply(crossprod, W, X, SIMPLIFY = FALSE))
  ZVE <- Reduce("+",
                mapply(function(x,y,z) t(x)%*%y%*%t(y)%*%z, W, resid, residl, SIMPLIFY = FALSE))

  denom <- EVE - 2 * EX %*% vcov(object) %*% t(XZ) %*% A %*% ZVE + EX %*% vv %*% t(EX)
  num <- Reduce("+", mapply(crossprod, resid, residl, SIMPLIFY = FALSE))
  stat <- num / sqrt(denom)
  names(stat) <- "normal"
  pval <- pnorm(abs(stat), lower.tail = FALSE)*2
  mtest <- list(statistic = stat,
                p.value = pval,
                method = paste("Autocorrelation test of degree", order))
  class(mtest) <- "htest"
  mtest
}

wald <- function(object, param = c("coef", "time", "all"), vcov = NULL){
  param <- match.arg(param)
  myvcov <- vcov
  if (is.null(vcov)) vv <- vcov(object)
  else if (is.function(vcov)) vv <- myvcov(object)
  else vv <- myvcov
  model <- describe(object, "model")
  effect <- describe(object, "effect")
  if (param == "time" && effect == "individual") stop("no time-dummies in this model")
  transformation <- describe(object, "transformation")
  if (model == "onestep") coefficients <- object$coefficients
  else coefficients <- object$coefficients[[2]]
  Ktot <- length(coefficients)
  Kt <- length(object$args$namest)
  if (param == "time"){
    start <- Ktot - Kt + ifelse(transformation == "ld", 2, 1)
    end <- Ktot
  }
  if (param == "coef"){
    start <- 1
    if (effect == "twoways") end <- Ktot-Kt else end <- Ktot
  }
  if (param == "all"){
    start <- 1
    end <- Ktot
  }
  coef <- coefficients[start:end]
  vv <- vv[start:end, start:end]
  stat <- t(coef) %*% solve(vv) %*% coef
  names(stat) <- "chisq"
  parameter <- length(coef)
  pval <- pchisq(stat, df = parameter, lower.tail = FALSE)
  wald <- list(statistic = stat,
               p.value = pval,
               parameter = parameter,
               method = "Wald test")
  class(wald) <- "htest"
  wald
}

print.summary.pgmm <- function(x, digits = max(3, getOption("digits") - 2),
                               width = getOption("width"),
                               ...){
  model <- describe(x, "model")
  transformation <- describe(x, "transformation")
  effect <- describe(x, "effect")

  pdim <- attr(x,"pdim")
  formula <- x$call$formula

  cat(paste(effect.pgmm.list[effect]," ",sep=""))
  cat(paste(model.pgmm.list[model],"\n",sep=""))
  cat("\nCall:\n")
  print(x$call)
  cat("\n")
  print(pdim)
  ntot <- sum(unlist(x$residuals) != 0)
  cat("\nNumber of Observations Used: ",ntot,"\n")
  
  cat("\nResiduals\n")
  print(summary(unlist(residuals(x))))
  cat("\nCoefficients\n")
  printCoefmat(x$coefficients,digits=digits)

  cat("\nSargan Test: ",names(x$sargan$statistic),
      "(",x$sargan$parameter,") = ",x$sargan$statistic,
      " (p.value=",format.pval(x$sargan$p.value,digits=digits),")\n",sep="")

  cat("Autocorrelation test (1): ",names(x$m1$statistic),
      " = ",x$m1$statistic,
      " (p.value=",format.pval(x$m1$p.value,digits=digits),")\n",sep="")
  
  cat("Autocorrelation test (2): ",names(x$m2$statistic),
      " = ",x$m2$statistic,
      " (p.value=",format.pval(x$m2$p.value,digits=digits),")\n",sep="")
  cat("Wald test for coefficients: ",names(x$wald.coef$statistic),
      "(",x$wald.coef$parameter,") = ",x$wald.coef$statistic,
      " (p.value=",format.pval(x$wald.coef$p.value,digits=digits),")\n",sep="")
  
  
  if (describe(x, "effect") == "twoways"){
    cat("Wald test for time dummies: ",names(x$wald.td$statistic),
        "(",x$wald.td$parameter,") = ",x$wald.td$statistic,
        " (p.value=",format.pval(x$wald.td$p.value,digits=digits),")\n",sep="")
  }
  invisible(x)
}

sargan <- function(object, weights = c("twosteps", "onestep")){
  weights <- match.arg(weights)
  model <- describe(object, "model")
  transformation <- describe(object, "transformation")
  if (model == "onestep") Ktot <- length(object$coefficient)
  else Ktot <- length(object$coefficient[[2]])
  N <- length(residuals(object))
  z <- as.numeric(Reduce("+",
                         lapply(seq_len(N),
                                function(i) crossprod(object$W[[i]], residuals(object)[[i]]))))
  p <- ncol(object$W[[1]])
  if (weights == "onestep") A <- object$A1 else A <- object$A2
  stat <- as.numeric(crossprod(z, t(crossprod(z, A))))
  parameter <- p - Ktot
  names(parameter) <- "df"
  names(stat) <- "chisq"
  method <- "Sargan test"
  pval <- pchisq(stat, df = parameter, lower.tail = FALSE)
  sargan <- list(statistic = stat,
                 p.value = pval,
                 parameter = parameter,
                 method = "Sargan Test")
  class(sargan) <- "htest"
  sargan
}
