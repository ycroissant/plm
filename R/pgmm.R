pgmm <- function(formula, data, subset, na.action,
                 effect = c("twoways", "individual"),
                 model = c("onestep", "twosteps"),
                 transformation = c("d", "ld"), fsm = NULL,
                 index = NULL, ...){
  cl <- match.call(expand.dots = FALSE)
  effect <- match.arg(effect)
  model <- match.arg(model)
  transformation <- match.arg(transformation)
  
  # the following section ensures backward compatibility with the old
  # formula and dynformula interfaces
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
  
  x <- formula
  if (!inherits(x, "Formula")) x <- Formula(formula)
  # gmm instruments : named list with the lags, names being the variables
  gmm.form <- formula(x, rhs = 2, lhs = 0)
  gmm.lags <- dynterms(gmm.form)
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
    if (length(inst.lags) > 0) inst.form <- dynterms2formula(inst.lags)
    else{
      # the case where there are no normal instruments : put inst.form
      # and inst.lags to NULL
      inst.form <- NULL
      inst.lags <- NULL
    }
  }
  
  # How many time series are lost ? May be the maximum number of lags
  # of any covariates + 1 because of first - differencing or the
  # largest minimum lag for any gmm or normal instruments
  gmm.minlag <- max(sapply(gmm.lags, min))
  # min or max to select the number of lost time series ?
  gmm.minlag <- min(sapply(gmm.lags, min))
  if (!is.null(inst.lags)) inst.maxlag <- max(sapply(inst.lags, max))
  else inst.maxlag <- 0
  main.maxlag <- max(sapply(main.lags, max))
  time.lost <- max(main.maxlag + 1, inst.maxlag + 1, gmm.minlag)
  time.lost.level <- max(main.maxlag, inst.maxlag)
  # if time.lost.level = 0 (no lags), one observation is lost anyway
  # because of the differentiation of the lag instruments
  time.lost.level <- max(1, time.lost.level)
  # Compute the model frame using the covariates, the 'normal'
  # instruments if any and the gmm instruments without the lags
  gmm.form <- as.formula(paste("~", paste(names(gmm.lags), collapse = "+")))
  if (!is.null(inst.form))  F <- as.Formula(main.form, gmm.form, inst.form)
  else F <- as.Formula(main.form, gmm.form)
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "na.action", "index"),names(mf),0)
  mf <- mf[c(1,m)]
  mf$drop.unused.levels <- TRUE
  mf[[1]] <- as.name("plm")
  mf$model <- NA
  mf$formula <- F
  mf$na.action <- "na.pass"
  data <- eval(mf, parent.frame())
  index <- attr(data, "index")
  T <- length(levels(index[[2]]))
  pdim <- pdim(data)

  # Compute the first step matrices
  if (transformation == "d") A1 <- tcrossprod(diff(diag(1, T - time.lost + 1)))
  if (transformation == "ld") A1 <- FSM(T - time.lost.level, "full")
  
  # Get the covariates matrix, split it by individual 
  attr(data, "formula") <- formula(main.form)
  yX <- extract.data(data)
  names.coef <- colnames(yX[[1]])[-1]
  # Get a list of missing time series for each individual : nats is a
  # list of two dimentional vectors containing the number of time
  # series lost at the begining and at the end of the series
  rn <- lapply(yX, rownames)
  allrn <- levels(attr(data, "index")[[2]])
  nats <- lapply(rn,
               function(x){
                 z <- allrn %in% x
                 z <- which(z)
                 c(z[1] - 1, T - z[length(z)])
               }
               )
  # Get the instruments and GMM matrix, split them by individual
  if (!is.null(inst.form)){
    attr(data, "formula") <- inst.form
    inst <- extract.data(data)
  }
  else inst <- NULL

  attr(data, "formula") <- gmm.form
  W <- extract.data(data, as.matrix = FALSE)
  # Add an nats attribute to the three lists of matrices
  W <- mapply(function(x, y){ attr(x, "nats") <- y;x}, W, nats, SIMPLIFY=FALSE)
  if (!is.null(inst))
    inst <- mapply(function(x, y){ attr(x, "nats") <- y;x}, inst, nats, SIMPLIFY=FALSE)
  yX <- mapply(function(x, y){ attr(x, "nats") <- y;x}, yX, nats, SIMPLIFY=FALSE)
  # Create the matrix of time dummies.
  namest <- levels(attr(data, "index")[,2])
  if (effect == "twoways"){
    if (transformation == "ld"){
      td <- cbind(1, rbind(0, diag(1, T - 1)))
      # remove as many columns and row as there are lost time series
      # in level (the difference of position between rows and columns
      # is due to the fact that the first column of td is the
      # intercept and should be kept anyway
      td.gmm.level <- td.model.level <-
        td[-c(1:time.lost.level), - c(2:(2+time.lost.level-1))]
      td.model.diff <- diff(td.model.level)
      namest <- c("(intercept)", namest[-c(0:time.lost.level+1)])
    }
    if (transformation == "d"){
      td.gmm.diff <- td.model.diff <- diff(diag(1, T - time.lost + 1))[, -1]
      namest <- namest[-c(0:(time.lost))]
    }
  }
  # A QAD fix for the bug in mtest for ld model without time.dummies
  if (effect == "individual" && transformation == "ld")
    namest <- c("(intercept)", namest[-c(0:time.lost.level+1)])

  # create the matrix of gmm instruments for every individual
  W <- lapply(W,
              # loop on all the individuals
              function(x){
                nats <- attr(x, "nats")
                # loop on every gmm instrument
                u <- mapply(
                            function(x, g){
                              rg <- range(g)
                              # Create the relevant matrix for one gmm instrument
                              x <- c(rep(0, nats[1]), x, rep(0, nats[2]))
                              z <- as.list((time.lost+1):T)
                              x <- lapply(z, function(y) x[max(1, y - rg[2]):(y - rg[1])])
                              lx <- sapply(x, length)
                              n <- length(x)
                              lxc <- cumsum(lx)
                              before <- c(0, lxc[-n])
                              after <- lxc[n] - sapply(x, length) - before
                              result <- mapply(function(x, y, z)
                                               c(rep(0, y), x, rep(0, z)),
                                               x, before, after)
                              matrix(unlist(result), nrow = length(x), byrow = TRUE)
                            },
                            x, gmm.lags,
                            SIMPLIFY = FALSE
                            )
                # cbind all the matrices and owerwrite rows with 0 if
                # there are some missing time series
                nr <- nrow(u[[1]])
                u <- unlist(u)
                u <- matrix(u, nrow = nr)
                # add the matrix of time dummies if required and if
                # diff gmm is computed
                if (effect == "twoways"){
                  if (transformation == "d") u <- cbind(u, td.gmm.diff)
                }
                if (nats[1]) u[1:nats[1], ] <- 0
                if (nats[2]) u[(T - time.lost - nats[2] + 1):(T - time.lost), ] <- 0
                # in case of 'sys' gmm, add for each time series the
                # difference, laged 1 of every gmm instrument
                if (transformation == "ld"){
                  ud <- lapply(x,
                               function(z)
                               diag(c(rep(0, nats[1]), diff(z[-c(length(z))]), rep(0, nats[2])))
                               )
                  # the matrix of instruments in difference has T - 2
                  # rows if one time series is lost (there are no gmm
                  # instruments for t = 2 but there is a moment
                  # condition with the intercept. In this case, a row
                  # of 0 should be added. Otherwise, the number of
                  # rows is just T - time.lost.level
                  nrow.ud <- ifelse(time.lost.level == 1, T - 2, T - time.lost.level)
                  ud <- matrix(unlist(ud), nrow = nrow.ud)
                  if (time.lost.level == 1) ud <- rbind(0, ud)
                  if (effect == "twoways") ud <- cbind(ud, td.gmm.level)
                  # owerwrite rows of  missing time series with 0
                  if (nats[1]) ud[c(0:nats[1]), ] <- 0
                  if (nats[2]) ud[(T-time.lost.level- nats[2]+1):(T-time.lost.level), ] <- 0
                  u <- bdiag(u, ud)
                }
                u
              }
              )

  yX <- lapply(yX,
               function(x){
                 nats <- attr(x, 'nats')
                 nc <- ncol(x)
                 # add as many rows of NAs than there are missing
                 # time series, at the begining and/or at the end
                 before <- matrix(NA, nats[1] ,nc)
                 after <- matrix(NA, nats[2], nc)
                 x <- rbind(before, x, after)
                 xd <- diff(x)
                 # remove time.lost - 1 time series (xd is already
                 # differenced
                 xd <- xd[-c(1:(time.lost-1)), , drop = FALSE]
                 # add time dummies if required
                 if (effect == "twoways") xd <- cbind(xd, td.model.diff)
                 # put 0 lignes for missing time series 
                 if (nats[1]) xd[0:nats[1], ] <- 0
                 if (nats[2]) xd[(T-time.lost- nats[2]+1):(T-time.lost), ] <- 0
                 if (transformation == "ld"){
                   # remove the relevent number of time series for
                   # data in level
                   x <- x[- c(0:time.lost.level), , drop=FALSE]
                   # add the matrix of time dummies if required
                   if (effect == "twoways"){
                     x <- cbind(x, td.model.level)
                   }
                   # owerwrite rows of  missing time series with 0
                   if (nats[1]) x[c(0:nats[1]), ] <- 0
                   if (nats[2]) x[(T-time.lost.level- nats[2]+1):(T-time.lost.level), ] <- 0
                   xd <- rbind(xd, x)
                 }
                 xd
               }
               )

  if (!is.null(inst)){
    inst <- lapply(inst,
                   function(x){
                     nats <- attr(x, 'nats')
                     nc <- ncol(x)
                     # add as many rows of NAs than there are missing
                     # time series, at the begining and/or at the end
                     before <- matrix(NA, nats[1] ,nc)
                     after <- matrix(NA, nats[2], nc)
                     x <- rbind(before, x, after)
                     xd <- diff(x)
                     # remove time.lost - 1 time series (xd is already
                     # differenced
                     xd <- xd[-c(1:(time.lost-1)), , drop = FALSE]
                     # put 0 lignes for missing time series 
                     if (nats[1]) xd[1:nats[1], ] <- 0
                     if (nats[2]) xd[(T-time.lost- nats[2]+1):(T-time.lost), ] <- 0
                     if (transformation == "ld"){
                       # remove the relevent number of time series for
                       # data in level
                       x <- x[- c(0:time.lost.level), , drop=FALSE]
                       # put 0 lignes for missing time series 
                       if (nats[1]) x[c(0:nats[1]), ] <- 0
                       if (nats[2]) x[(T-time.lost.level- nats[2]+1):(T-time.lost.level), ] <- 0
                       xd <- bdiag(xd, x)
                     }
                     xd
                   }
                   )
    # cbind the matrices of gmm and normal instruments
    W <- mapply(cbind, W, inst, SIMPLIFY = FALSE)
  }
  # compute the estimator
  WX <- mapply(function(x, y) crossprod(x, y), W, yX, SIMPLIFY = FALSE)
  Wy <- lapply(WX, function(x) x[, 1])
  WX <- lapply(WX, function(x) x[, -1])
  A1 <- lapply(W, function(x) crossprod(t(crossprod(x, A1)), x))
  A1 <- solve(Reduce("+", A1))*length(W)
  WX <- Reduce("+", WX)
  Wy <- Reduce("+", Wy)
  B1 <- solve(crossprod(WX, t(crossprod(WX, A1))))
  Y1 <- crossprod(t(crossprod(WX, A1)), Wy)
  coefficients <- as.numeric(crossprod(B1, Y1))
  if (effect == "twoways") names.coef <- c(names.coef, namest)
  names(coefficients) <- names.coef
  residuals <- lapply(yX,
                      function(x)
                      as.vector(x[,1] -  crossprod(t(x[,-1]), coefficients)))
  outresid <- lapply(residuals,function(x) outer(x,x))
  A2 <- mapply(function(x, y) crossprod(t(crossprod(x, y)), x), W, outresid, SIMPLIFY = FALSE)
  A2 <- solve(Reduce("+", A2))
#  A2 <- pseudoinverse(Reduce("+", A2))
#  A2 <- ginv(Reduce("+", A2))
  B2 <- solve(crossprod(WX, t(crossprod(WX, A2))))

  if (model=="twosteps"){
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
                        z <- as.vector(x[, 1] - crossprod(t(x[, -1]), coefficients))
                        names(z) <- nz
                        z
                      }
                      )
  fitted.values <- mapply(function(x,y) x[, 1] - y, yX, residuals)
  if (model == "twosteps") coefficients <- list(coef1s, coefficients)
  args <- list(model = model, effect = effect,
               transformation = transformation, namest = namest)
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

