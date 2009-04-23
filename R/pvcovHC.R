pvcovHC <- function(x, ...){
  UseMethod("vcovHC")
}

vcovHC.panelmodel <-function(x,
                              method = c("arellano", "white1", "white2"),
                              type = c("HC0", "HC1", "HC2", "HC3", "HC4"),
                              ...) {
  ## Robust vcov for panel models (random or within type plm obj.)
  ##
  ## This function takes the demeaned data from the
  ## plm object, then vcovHC as in Greene, Ec. An. (2003), pag. 315
  ## (Greene mentions the RE case on page 316, next par.)
  ## The Arellano estimator is referenced in Wooldridge as well:
  ## Wooldridge 2002, Econometrics of cross-section and
  ## panel data:
  ## 10.5.4 (and formula (10.59)) for FE/within,
  ## 10.4.2 (referring to formula 7.49) for RE.
  ##
  ## This version 1: compliant with plm.0.1-3; lmtest.
  ## 
  ## Usage: 
  ## myplm <- plm(<model>,<data>,type=<one of "within","random">)
  ## # default (White 1):
  ## coeftest(myplm, vcov=vcovHC)
  ## # Arellano (1987):
  ## coeftest(myplm, vcov=function(x) vcovHC(x,type="arellano"))
  ## # idem, HC3 weighting:
  ## coeftest(myplm, vcov=function(x) vcovHC(x,type="arellano",weights="HC3"))
  ## waldtest(myplm,update(myplm,<new formula>),vcov=vcovHC)
  ##
  ## This weighted version implements a system of weights as 
  ## in vcovHC/meatHC. Sure this makes sense for white1, but it
  ## is open to question for white2 and arellano. We'll see.

  method <- match.arg(method)
  type <- match.arg(type)

  model <- describe(x, "model")
  if(!model %in% c("random","within","pooling","fd")) {
    stop("Model has to be either random, within or pooling model")
  }

  ## extract demeaned data from the plm
  theta <- x$errcomp$theta
  demX <- model.matrix(x, model = model)
  demy <- pmodel.response(x, model = model)
  ## name intercept: a fix for the "" name of demX_1 ##
  dimnames(demX)[[2]][1]<-attr(vcov(x),"dimnames")[[1]][1]

  pdim <- pdim(x)
  n <- pdim$nT$n
  t <- pdim$nT$T
  nT <- pdim$nT$N

  ## (re)create groupwise index
  ## sure this can be done better! ##
  Ti <- pdim$Tint$Ti
  tind <- vector("list",n)
  tfirst <- 0
  for(i in 1:n) {
    tind[[i]] <- (tfirst+1):(tfirst+Ti[i])
    tfirst <- max(tind[[i]])
    }

  k <- dim(demX)[[2]]

  ## extract residuals
  uhat <- x$residuals
 
  ## define residuals weighting function omega(res)
  ## (code taken from meatHC and modified)
  ## 
  ## theor. comment:
  ## here we are decomposing the corrected (uhat_i)^2 in White and
  ## MacKinnon in uhat_i*uhat_i, in order to construct the submatrix
  ## Omega_i as:
  ## white1: crossprod(uhat_i)
  ## white2: crossprod(rep(mean(uhat_i),length(uhat_i))
  ## arellano: outer(uhat_i)
  ##
  ## Let cf be the correction factor (HC0: 1, HC1:(n/n-k), HC2: 1/(1-h_ii),
  ## HC3: 1/(1-h_ii)^2 etc.); it is easily seen that cf>0, cf->1 as nT->Inf.
  ## Looks like the diagonal values of the
  ## hat matrix X(X'X)^(-1)X' are always <1, thus (1-h_ii) in HC2 is
  ## always positive as well. The same for HC3 where it is squared.
  ## Note that for HC>2, cf=cf(i).
  ## We transform the residuals by rcf=sqrt(cf), so that the diag elements
  ## of Omegai for the White est.s are:
  ## [Omegai]_ii = (rcf_i*uhat_i)^2 = cf_i * uhat_i^2 (White 1)
  ## [Omegai]_ii = 1/n*sum((rcf_i*uhat_i)^2) = 1/n*sum(rcf_i * uhat_i^2) (White 2)
  ## while the generic element of Omegai for Arellano is:
  ## [Omegai]_ij = rcf_i*uhat_i * rcf_j*uhat_j
  ## and the diagonal is as in White 1
  ## [Omegai]_ii = cf_i * uhat_i^2
  ##
  ## in HC1-2 we are correcting by non-negative quantities and taking 
  ## only squares of errors. 

    ## diaghat function for matrices
    dhat <- function(x) {tx<-t(x)
                         diag(crossprod(tx,solve(crossprod(x),tx)))}

    ## this is computationally heavy, do only if needed
    switch(type,
           HC0 = {diaghat<-NULL},
           HC1 = {diaghat<-NULL},
           HC2 = {diaghat<-try(dhat(demX), silent = TRUE)}, 
           HC3 = {diaghat<-try(dhat(demX), silent = TRUE)},
           HC4 = {diaghat<-try(dhat(demX), silent = TRUE)})
  df <- nT - k
  switch(type, HC0 = {
    omega <- function(residuals, diaghat, df) residuals
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
    omega <- function(residuals, diaghat, df) residuals/
      sqrt(1 - diaghat)^pmin(4, length(residuals) * diaghat/as.integer(round(sum(diaghat), 
                                                                             digits = 0)))
        })

  ## transform residuals by weights
  uhat<-omega(uhat,diaghat,df)
  ## define Omegai(e_i) function for Omega_i diag. blocks in E^2
  ## in Greene's formula (top of page 315)
  switch(method,
               white1 = {Omegai<-function(x) diag(x^2)},
               white2 = {Omegai<-function(x) {
                         n<-length(x)
                         diag(sum(x^2)/n,n)
                         }},
               arellano = {Omegai<-function(x) outer(x,x)})

  salame<-array(dim=c(k,k,n))
  for(i in 1:n) {
      groupinds<-tind[[i]]
      xi<-demX[groupinds, , drop = FALSE]
      ui<-uhat[groupinds]
      salame[,,i]<-crossprod(xi,Omegai(ui))%*%xi
      }

  ## meat
  salame<-apply(salame,1:2,sum)
  
  ## bread
  pane<-solve(crossprod(demX))
  
  ## sandwich
  mycov <- pane %*% salame %*% pane
  return(mycov)
}

vcovHC.pgmm <- function(x,...){
  model <- ifelse(is.null(x$call$model), "onestep", x$call$model)
  transformation <- ifelse(is.null(x$call$transformation), "d", x$call$transformation)
  A1 <- x$A1
  A2 <- x$A2

  if (transformation=="ld"){
    yX <- lapply(x$model,function(x) rbind(diff(x),x))
    residuals <-lapply(x$residuals,function(x) c(diff(x),x))
  }
  else{
    yX <- x$model
    residuals <- x$residuals
  }    
  
  if (model=="twosteps"){
    coef1s <- x$coefficients[[1]]
    res1s <- lapply(yX,function(x) x[,1]-crossprod(t(x[,-1]),coef1s))
    K <- ncol(yX[[1]])
    D <- c()
    WX <- suml(mapply(function(x,y) crossprod(x,y[,-1]),x$W,yX,SIMPLIFY=FALSE))
    We <- suml(mapply(function(x,y) crossprod(x,y),x$W,residuals,SIMPLIFY=FALSE))
    B1 <- solve(t(WX)%*%A1%*%WX)
    B2 <- vcov(x)
    vcov1s <- B1%*%(t(WX)%*%A1%*%solve(A2)%*%A1%*%WX)%*%B1
    for (k in 2:K){
      exk <- mapply(function(x,y){ z <- crossprod(t(x[,k]),t(y));-z-t(z)},yX,res1s)
      wexkw <- suml(mapply(function(x,y) crossprod(x,crossprod(y,x)), x$W,exk,SIMPLIFY=FALSE))
      Dk <- -B2%*%t(WX)%*%A2%*%wexkw%*%A2%*%We
      D <- cbind(D,Dk)
    }
    vcovr <- B2+crossprod(t(D),B2)+t(crossprod(t(D),B2))+D%*%vcov1s%*%t(D)

  }
  else{
    res1s <- lapply(yX,function(z) z[,1]-crossprod(t(z[,-1]),x$coefficients))
    K <- ncol(yX[[1]])
    WX <- suml(mapply(function(z,y) crossprod(z[,-1],y),yX,x$W,SIMPLIFY=FALSE))
    B1 <- vcov(x)
    vcovr <- B1%*%(WX%*%A1%*%solve(A2)%*%A1%*%t(WX))%*%B1
  }
  vcovr
}

