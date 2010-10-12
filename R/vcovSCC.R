vcovSCC <- function(x, ...) {
    UseMethod("vcovSCC")
}


vcovSCC.plm<-function(x, type=c("HC0", "HC1", "HC2", "HC3", "HC4"), maxlag=NULL, ...) {
  ## Sectional-correlation-robust vcov for panel models (random or within type plm obj.)
  ##
  ## Ref.: Driscoll and Kraay, RES 1998
  ##
  ## Usage:
  ## myplm <- plm(<model>,<data>,type=<one of "pooling","within","random">)
  ## # default:
  ## coeftest(myplm, vcov=pvcovSCC)
  ## # White and MacKinnon (1985):
  ## coeftest(myplm, vcov=function(x) pvcovSCC(x,w="HC3"))
  ## waldtest(myplm,update(myplm,<new formula>),vcov=pvcovHC)
  ##
  ## This weighted version implements a system of weights as
  ## in vcovHC/meatHC. Undocumented.
  ##
  ## This version 6: interface as in vcovHC.panelmodel
  ## ('type' instead of 'weights', default to "HC0")

  model <- describe(x, "model")

  ## this has to be uncommented in packaged version
  #if(!(attr(x,"pmodel")$model %in% c("pooling","random","within","fd"))) {
  #  stop("Model has to be of type pooling, random, within or fd")
  #  }

  ## extract demeaned data from the plm
    demX <- model.matrix(x, model = model)
    demy <- pmodel.response(x, model = model)
    dimnames(demX)[[2]][1] <- attr(vcov(x), "dimnames")[[1]][1]
    pdim <- pdim(x)
    n <- pdim$nT$n
    T. <- pdim$nT$T
    nT <- pdim$nT$N
    time.index <- as.numeric(attr(x$model, "index")[,2])


  ## Achim's fix for 'fd' model (losing first time period)
     if(model == "fd") {
       nT <- nT-n
       T. <- T.-1
       time.index <- time.index[which(time.index!=time.index[1])]
     }

  ## set maxlag
  if(is.null(maxlag)) maxlag<-floor(T.^(1/4))

  ## make positions vector for records in each time period
  allpos <- 1:nT
  gind<-vector("list",T.)
  timevals <- unique(time.index)
  for(i in 1:T.) {
    gind[[i]] <- allpos[time.index==timevals[i]]
    }

  k<-dim(demX)[[2]]

  ## extract residuals
  uhat<-x$residuals

  ## define residuals weighting function omega(res)
  ## (code taken from meatHC and modified)
  ##

    ## diaghat function for matrices
    dhat <- function(x) {tx<-t(x)
                         diag(crossprod(tx,solve(crossprod(x),tx)))}

    weights <- match.arg(type)
    ## this is computationally heavy, do only if needed
    switch(weights, HC0 = {diaghat<-NULL},
                    HC1 = {diaghat<-NULL},
                    HC2 = {diaghat<-try(dhat(demX), silent = TRUE)},
                    HC3 = {diaghat<-try(dhat(demX), silent = TRUE)},
                    HC4 = {diaghat<-try(dhat(demX), silent = TRUE)})
    df <- nT - k
    switch(weights, HC0 = {
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
            omega <- function(residuals, diaghat, df) residuals/sqrt(1 -
                diaghat)^pmin(4, length(residuals) * diaghat/as.integer(round(sum(diaghat),
                digits = 0)))
        })

  ## transform residuals by weights
  uhat<-omega(uhat,diaghat,df)

  ## until here, more or less as pvcovHC() ##

  ## make hT matrix (ht kx1 vector of moment conditions for each t)
  hT<-matrix(nrow=T.,ncol=k)

  for(t in 1:T.) {
    ## extract X.tbar amd u.tbar

    ## this already works with unbalanced panels

    timeinds<-gind[[t]]
    Xt<-demX[timeinds,]
    ut<-uhat[timeinds]

    hT[t,]<-crossprod(Xt,ut)
    }

  ## calc. Shat (the meat)
  ## nb this port of Hoechle's Stata code gives the very same result as Version <3

  Shat <- crossprod(hT)  #/(nT^2/T.)

  if(maxlag>0) {
    ## def. Bartlett kernel
    wj<-function(j, maxlag) 1-j/(maxlag+1)

## use kweights() here for flexible kernel weighting

    ## from 1 to maxlag, sum outer(hT(t),hT(t-j)) with its transpose,
    ## which is equal to outer(hT(t-j),hT(t)), both weighted at wj(j)
    ## (lag-fwd plus lag-bwd, see Theorem in Driscoll and Kraay)
    for(j in 1:maxlag) {
      omegaj <- crossprod(hT[(j+1):T., ], hT[1:(T.-j), ])  #/(nT^2/T.)
      Shat <- Shat + wj(j, maxlag)*(omegaj+t(omegaj))
      }
    }

  ## calc Dhat (Dhat^(-1) is the bread)
  Dhat<-crossprod(demX)

  ## sandwich
  mycov <- solve(Dhat) %*% Shat %*% solve(Dhat)   #* (nT^2/T.)
  return(mycov)
  }
