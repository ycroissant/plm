## this version 5: add small-number-of-clusters correction à la Stata
## as 'type="sss" ' meaning "Stata small sample"
## added to reproduce Stata's small-sample correction: see Cameron, Gelbach
## and Miller 2006 (2008 version, p.10)


## needed for standalone operation
#describe <- plm:::describe
#model.matrix.plm<-plm:::model.matrix.plm
#pmodel.response<-plm:::pmodel.response.plm


pvcovHC <- function(x, ...){
  UseMethod("vcovHC")
}

vcovSCC <- function(x, ...){
  UseMethod("vcovSCC")
}

vcovNW <- function(x, ...){
  UseMethod("vcovNW")
}

vcovDC <- function(x, ...){
  UseMethod("vcovDC")
}

vcovG <- function(x, ...) {
    UseMethod("vcovG")
}


vcovG.plm <-function(x,type=c("HC0", "sss", "HC1", "HC2", "HC3", "HC4"),
                      cluster=c("group","time"),
                     l=0,
                     inner=c("cluster","white"),
                     ...) {

  ## general building block for vcov
  ## for panel models (pooling, random, within or fd type plm obj.)
  ##
  ## This version: as July 28th 2011, + 'sss' December 18th, 2013 (!)

    type <- match.arg(type)
    model <- describe(x, "model")
    if (!model %in% c("random", "within", "pooling", "fd")) {
        stop("Model has to be either random, within, pooling or fd model")
    }

  ## extract demeaned data

    demX <- model.matrix(x, model = model)
    demy <- pmodel.response(x, model = model)
    dimnames(demX)[[2]][1] <- attr(vcov(x), "dimnames")[[1]][1]

    pdim <- pdim(x)
    nT <- pdim$nT$N
    Ti <- pdim$Tint$Ti
    k <- dim(demX)[[2]]

    n0 <- pdim$nT$n
    t0 <- pdim$nT$T

  ## extract residuals
    uhat <- x$residuals

  ## define residuals weighting function omega(res)
  ## (code taken from meatHC and modified)
  ##
  ## (see the theoretical comments in pvcovHC)

    ## diaghat function for matrices
    dhat <- function(x) {tx<-t(x)
                         diag(crossprod(tx,solve(crossprod(x),tx)))}

    ## this is computationally heavy, do only if needed
    switch(match.arg(type), HC0 = {diaghat<-NULL},
                            sss = {diaghat<-NULL},
                            HC1 = {diaghat<-NULL},
                            HC2 = {diaghat<-try(dhat(demX), silent = TRUE)},
                            HC3 = {diaghat<-try(dhat(demX), silent = TRUE)},
                            HC4 = {diaghat<-try(dhat(demX), silent = TRUE)})
    df <- nT - k
    switch(match.arg(type), HC0 = {
            omega <- function(residuals, diaghat, df) residuals
        }, sss = {
            omega <- function(residuals, diaghat, df) residuals *
                sqrt(n/(n-1)*((nT-1)/(nT-k)))
        }, HC1 = {
            omega <- function(residuals, diaghat, df) residuals *
                sqrt(length(residuals)/df)
        }, HC2 = {
            omega <- function(residuals, diaghat, df) residuals /
                sqrt(1 - diaghat)
        }, HC3 = {
            omega <- function(residuals, diaghat, df) residuals /
                (1 - diaghat)
        }, HC4 = {
            omega <- function(residuals, diaghat, df) {
                residuals/sqrt(1 - diaghat)^
                 pmin(4, length(residuals) *
                      diaghat/as.integer(round(sum(diaghat),
                digits = 0)))
            }
        })

  ## transform residuals by weights
  uhat<-omega(uhat,diaghat,df)

  ## CODE TAKEN FROM pvcovHC() UNTIL HERE except for ind/time labeling ##

  ## Definition module for E(u,v)
    if(is.function(inner)) {
        E=inner
    } else {
      ## outer for clustering/arellano, diag(diag(inner)) for white
      switch(match.arg(inner), cluster={
          E=function(u,v) outer(u,v)
      }, white={
          E=function(u,v) diag(diag(outer(u,v)))
      })
  }

    ## try passing: function (a or b) or matrix (unconditional) to vcovG

  ## robustifying against either serial or xs intragroup dependence:
  ## if 'group' then keep current indexing, if 'time' then swap i<->t
  ## so that residuals get 'clustered' by time period instead of by
  ## group (i.e. the vcov estimator is robust vs. xsectional dependence)

  ## extract indices
    groupind<-as.numeric(attr(x$model, "index")[,1])
    timeind<-as.numeric(attr(x$model, "index")[,2])

  ## fix for 'fd' model (losing first time period)
     if(model == "fd") {
       groupi <- as.numeric(groupind)
       ## make vector =1 on first obs in each group, 0 elsewhere
       selector <-groupi-c(0,groupi[-length(groupi)])
       selector[1] <- 1 # the first must always be 1
       ## eliminate first obs in time for each group
       groupind <- groupind[!selector]
       timeind <- timeind[!selector]
       nT <- nT-n0
       Ti <- Ti-1
       t0 <- t0-1
     }

  ## set grouping indexes
    switch(match.arg(cluster), group = {
           n <- n0
           t <- t0
           relevant.ind <- groupind
           lab <- timeind
         }, time = {
           n <- t0
           t <- n0
           relevant.ind <- timeind
           lab <- groupind
         })
    tind <- vector("list", n)
    tlab <- vector("list", n)
    for (i in 1:length(unique(relevant.ind))) {
        tind[[i]] <- which(relevant.ind==i)
        tlab[[i]] <- lab[which(relevant.ind==i)]
    }

  ## now lab is the 'labels' (a numeric, actually) for the relevant index

  ## (code partly taken from pggls)

    ## compute basic block: X'_t u_t u'_(t-l) X_(t-l) foreach t

    ## here the benchmark case is time-clustering, but beware
    ## that group-clustering is the default

    ## "pre-allocate" arrays for X_t, X_(t-l), u_t, u_(t-l)
    ## where third dimension is (t-l)
    ## NB indices swapped here! X_i is txk, u_i is tx1

    ## (l=0 gives the special contemporaneous case where Xi=Xil, ui=uil
    ## for computing W, CX, CT)

    X <- array(dim=c(t,k,n-l))
    Xl <- array(dim=c(t,k,n-l))

    u <- array(dim=c(t,1,n-l)) # was: tres
    ul <- array(dim=c(t,1,n-l))

    ## array of X_i, u_i subsets
    ## for each group 1..n
    ## (use subscripting from condition 'label in labels' set',
    ## the rest stays NA if any so mean(na.rm=TRUE will work)
    for(i in (1+l):n) {
        ## check 'occupied' positions (for both i and i-l)
        tpos<-(1:t)[unique(lab) %in% tlab[[i]]]
        tposl<-(1:t)[unique(lab) %in% tlab[[i-l]]]

        X[tpos, , i-l] <- demX[tind[[i]], ]
        Xl[tposl, , i-l] <- demX[tind[[i-l]],]

        u[tpos, 1 , i-l] <- uhat[tind[[i]]]
        ul[tposl, 1, i-l] <- uhat[tind[[(i-l)]]]
      }

    ## preallocate k x k x (T-l) array for 'pile' of kxk matrices
    ## holding the X' E(u,ul) X elements
    Sl <- array(dim=c(k, k, n-l))

    ## calculate V_yy:

    ## populate Sl
    for(i in 1:(n-l)) {
        Eu <- E(u[,,i], ul[,,i])
        Sl[,,i] <- crossprod(X[,,i], Eu) %*% Xl[,,i]
    }

    ## in order to sum on available observations two things can be done:
    ## a) apply sum(..., na.rm=TRUE) over the third dim
    ## b) apply mean(..., na.rm=TRUE) idem and multiply by n-l
    ## In case a) averaging is then done dividing each covariance point
    ## by (n-l), regardless of whether there are NAs in the "vertical"
    ## vector Sl[p,q, ]
    ## In case b) each mean is calculated correctly on the right number
    ## of observations, excluding missing data. 'salame' has to be
    ## multiplied by (n-l)

    ## meat
    salame <- apply(Sl, 1:2, mean, na.rm=TRUE) * (n-l)

    ## bread by mean(X'X) method
    #X0 <- array(dim=c(k,k,n))
    #Xi <- array(dim=c(t,k,n))
    #for(i in 1:n) {
    #    ## check 'occupied' positions (for both i and i-l)
    #    tpos<-(1:t)[unique(lab) %in% tlab[[i]]]
    #    Xi[tpos, , i] <- demX[tind[[i]], ]
    #    X0[,,i] <- crossprod(Xi[,,i])
    #  }

    #pane<-solve(apply(X0, 1:2, mean, na.rm=TRUE))

    ## bread by standard method
    pane <- solve(crossprod(demX))

    ## sandwich
    mycov <- pane %*% salame %*% pane  #*1/n instead of n if mean(X'X)

    return(mycov)
}

#################################################################

## user-level wrappers:

vcovHC.plm <- function(x, method=c("arellano","white1","white2"),
                       type=c("HC0", "sss", "HC1", "HC2", "HC3", "HC4"),
                       cluster=c("group","time"), ...) {
    ## user-level wrapper for White-Arellano covariances

    ## translate arguments
    inner <- switch(match.arg(method),
                    arellano = "cluster",
                    white1 = "white",
                    white2 = "white")  # fix this: add white2 to vcovG

    return(vcovG(x, type=type, cluster=cluster,
                        l=0, inner=inner, ...))
}

vcovNW.plm <- function(x, type=c("HC0", "sss", "HC1", "HC2", "HC3", "HC4"),
                       maxlag=NULL,
                       wj=function(j, maxlag) 1-j/(maxlag+1),
                       ...) {
    ## user-level wrapper for panel Newey-West estimator

    ## set default lag order
    if(is.null(maxlag)) maxlag <- floor((max(pdim(x)$Tint$Ti))^(1/4))

    return(vcovSCC(x, type=type, maxlag=maxlag, inner="white", wj=wj, ...))
}

vcovDC.plm <- function(x, type=c("HC0", "sss", "HC1", "HC2", "HC3", "HC4"),
                       ...) {
    ## user-level wrapper for double-clustering (no persistence)

    Vcx <- vcovG(x, type=type, cluster="group",
                        l=0, inner="cluster", ...)
    Vct <- vcovG(x, type=type, cluster="time",
                        l=0, inner="cluster", ...)
    Vw <- vcovG(x, type=type, l=0, inner="white", ...)

    return(Vcx + Vct -Vw)
}

vcovSCC.plm <- function(x,type=c("HC0", "sss", "HC1", "HC2", "HC3", "HC4"),
                        cluster="time",
                        maxlag=NULL,
                        inner=c("cluster","white"),
                        wj=function(j, maxlag) 1-j/(maxlag+1),
                        ...) {

    ## replicates vcovSCC

    ## set default lag order
    if(is.null(maxlag)) maxlag <- floor((max(pdim(x)$Tint$Ti))^(1/4))

    ## def. Bartlett kernel
    ## wj<-function(j, maxlag) 1-j/(maxlag+1)
    ## has been passed as argument

    S0 <- vcovG(x, type=type, cluster=cluster, l=0, inner=inner)

    if(maxlag>0) {

        for(i in 1:maxlag) {

            Vctl <- vcovG(x, type=type, cluster=cluster,
                             l=i, inner=inner)
            S0 <- S0 + wj(i, maxlag) * (Vctl+t(Vctl))
        }
    }

    return(S0)
}


vcovHC.pgmm <- function(x,...){
  model <- describe(x, "model")
  transformation <- describe(x, "transformation")
  A1 <- x$A1
  A2 <- x$A2

  if (transformation=="ld"){
##     yX <- lapply(x$model,function(x) rbind(diff(x),x))
##     residuals <-lapply(x$residuals,function(x) c(diff(x),x))
    yX <- x$model
    residuals <- x$residuals
  }
  else{
    yX <- x$model
    residuals <- x$residuals
  }    
  minevA2 <- min(abs(Re(eigen(A2)$values)))
  eps <- 1E-9
  if (minevA2 < eps){
    SA2 <- ginv(A2)
    warning("a general inverse is used")
  }
  else SA2 <- solve(A2)
  
  if (model=="twosteps"){
    coef1s <- x$coefficients[[1]]
    res1s <- lapply(yX, function(x) x[,1] - crossprod(t(x[, -1, drop=FALSE]), coef1s))
    K <- ncol(yX[[1]])
    D <- c()
    WX <- Reduce("+",
                 mapply(function(x, y) crossprod(x, y[,-1, drop=FALSE]), x$W, yX, SIMPLIFY = FALSE))
    We <- Reduce("+", mapply(function(x, y) crossprod(x, y), x$W, residuals, SIMPLIFY = FALSE))
    B1 <- solve(t(WX) %*% A1 %*% WX)
    B2 <- vcov(x)

    vcov1s <- B1 %*% (t(WX) %*% A1 %*% SA2 %*% A1 %*% WX) %*% B1
    for (k in 2:K){
      exk <- mapply(
                    function(x,y){
                      z <- crossprod(t(x[,k, drop=FALSE]),t(y))
                      - z - t(z)
                    },
                    yX, res1s, SIMPLIFY = FALSE)
      wexkw <- Reduce("+",
                      mapply(
                             function(x, y)
                             crossprod(x, crossprod(y, x)),
                             x$W, exk, SIMPLIFY = FALSE))
      Dk <- -B2 %*% t(WX) %*% A2 %*% wexkw %*% A2 %*% We
      D <- cbind(D,Dk)
    }
    vcovr <- B2 + crossprod(t(D), B2) + t(crossprod(t(D), B2)) + D %*% vcov1s %*% t(D)
  }
  else{
    res1s <- lapply(yX, function(z) z[,1] - crossprod(t(z[, -1, drop=FALSE]), x$coefficients))
    K <- ncol(yX[[1]])
    WX <- Reduce("+", mapply(function(z, y) crossprod(z[,-1, drop=FALSE], y), yX, x$W, SIMPLIFY = FALSE))
    B1 <- vcov(x)
    vcovr <- B1 %*% (WX %*% A1 %*% SA2 %*% A1 %*% t(WX)) %*% B1
  }
  vcovr
}
