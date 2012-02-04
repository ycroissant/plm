vcovBK <- function(x, ...) {
    UseMethod("vcovBK")
}

vcovBK.plm <-function(x,type=c("HC0", "HC1", "HC2", "HC3", "HC4"),
                      cluster=c("group","time"),
                      diagonal=FALSE, ...) {

  ## Robust vcov à la Beck and Katz (1995; AKA 'pcse')
  ## for panel models (pooling, random, within or fd type plm obj.)
  ##
  ## This version: October 20th, 2009; allows choosing the clustering dimension
  ## so as to have serial- or x-sectional-correlation robustness;
  ##
  ## This function takes the demeaned data from the
  ## plm object, then estimates an *unconditional* error covariance by
  ## averaging the empirical covariance blocks by group (time period);
  ## this average block (say, OmegaM in Eviews notation) is then put into
  ## White's formula instead of each Omega_i.
  ##
  ## The clustering defaults to "group" for consistency with pvcovHC;
  ## nevertheless the most likely usage is cluster="time" for robustness vs.
  ## cross-sectional dependence, as in the original Beck and Katz paper (where
  ## it is applied to "pooling" models).
  ##
  ## This version: compliant with plm.1.2-0; lmtest.
  ## Code is identical to pvcovHC until mark.
  ##
  ## Usage:
  ## myplm <- plm(<model>,<data>, ...)
  ## # default (cluster by group = robust vs. serial correlation):
  ## coeftest(myplm, vcov=vcovBK)
  ## # cluster by time period (robust vs. XS correlation):
  ## coeftest(myplm, vcov=function(x) vcovBK(x, cluster="time"))
  ## # idem, HC3 weighting:
  ## coeftest(myplm, vcov=function(x) vcovBK(x,cluster="time",type="HC3"))
  ## waldtest(myplm,update(myplm,<new formula>),vcov=vcovBK)
  ##
  ## This weighted version implements a system of weights as
  ## in vcovHC/meatHC. Sure this makes sense for white1, but it
  ## is open to question for white2 and arellano. We'll see.
  ##
  ## Results OK vs. Eviews, vcov=PCSE. Unbal. case not exactly the
  ## same (but then, who knows what Eviews does!)

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

  ## robustifying against either serial or xs intragroup dependence:
  ## if 'group' then keep current indexing, if 'time' then swap i<->t
  ## so that residuals get 'clustered' by time period instead of by
  ## group (i.e. the vcov estimator is robust vs. xsectional dependence)

  ## extract indices
    groupind<-as.numeric(attr(x$model, "index")[,1])
    timeind<-as.numeric(attr(x$model, "index")[,2])

  ## Achim's fix for 'fd' model (losing first time period)
     if(model == "fd") {
       groupind <- groupind[timeind > 1]
       timeind <- timeind[timeind > 1]
       nT <- nT-n0
       Ti <- Ti-1
       t0 <- t0-1
     }

  ## set grouping indexes
    switch(match.arg(cluster), group = {
           n <- n0 # this is needed only for 'pcse'
           t <- t0 # this is needed only for 'pcse'
           relevant.ind <- groupind
           lab <- timeind
         }, time = {
           n <- t0 # this is needed only for 'pcse'
           t <- n0 # this is needed only for 'pcse'
           relevant.ind <- timeind
           lab <- groupind
         })
    tind <- vector("list", n)
    tlab <- vector("list", n)
    for (i in 1:length(unique(relevant.ind))) {
        tind[[i]] <- which(relevant.ind==i)
        tlab[[i]] <- lab[which(relevant.ind==i)]
    }

  ## define residuals weighting function omega(res)
  ## (code taken from meatHC and modified)
  ##
  ## (see the theoretical comments in pvcovHC)

    ## diaghat function for matrices
    dhat <- function(x) {tx<-t(x)
                         diag(crossprod(tx,solve(crossprod(x),tx)))}

    ## this is computationally heavy, do only if needed
    switch(match.arg(type), HC0 = {diaghat<-NULL},
                            HC1 = {diaghat<-NULL},
                            HC2 = {diaghat<-try(dhat(demX), silent = TRUE)},
                            HC3 = {diaghat<-try(dhat(demX), silent = TRUE)},
                            HC4 = {diaghat<-try(dhat(demX), silent = TRUE)})
    df <- nT - k
    switch(match.arg(type), HC0 = {
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

  ## CODE TAKEN FROM pvcovHC() UNTIL HERE except for ind/time labeling ##

  ## the PCSE covariance estimator is based on the unconditional estimate
  ## of the intragroup (intraperiod) covariance of errors, OmegaT or OmegaM
  ## in the Eviews help.
  ## we calculate this based on code from pggls().
  ## the Omegai function is then:
  ## - constant if the panel is balanced
  ## - depending only on the intragroup (intraperiod) position index
  ##   if the panel is unbalanced.

  ## (code for estimating OmegaM/OmegaT partly taken from pggls)

    ## est. omega submatrix
    ## "pre-allocate" an empty array
    tres<-array(dim=c(t,t,n))

    ## array of n "empirical omega-blocks"
    ## with outer product of t(i) residuals
    ## for each group 1..n
    ## (use subscripting from condition 'label in labels' set', the rest stays NA if any)
    for(i in 1:n) {
      ut<-uhat[tind[[i]]]
      tpos<-(1:t)[unique(lab) %in% tlab[[i]]]
      ## put nondiag elements to 0 if diag=TRUE
      if(diagonal) {
          tres[tpos,tpos,i]<-diag(diag(ut%o%ut))
        } else {
          tres[tpos,tpos,i]<-ut%o%ut
        }
      }

    ## average over all omega blocks, removing NAs (apply preserving
    ## *two* dimensions, i.e. over the third) to get the unconditional
    ## covariance matrix of errors for a group (viz. time period):
    OmegaT<-apply(tres,1:2,mean,na.rm=TRUE)

  ## end of PCSE covariance calculation. Now

  ## fetch (all, unique) values of the relevant labels
  unlabs <- unique(lab)

  salame<-array(dim=c(k,k,n))
  for(i in 1:n) {
      groupinds<-tind[[i]]
      grouplabs<-tlab[[i]]
      xi<-demX[groupinds, , drop=FALSE]
      ## for every group, take relevant positions
      tpos <- unlabs %in% grouplabs
      OmegaTi <- OmegaT[tpos, tpos, drop=FALSE]
      salame[,,i]<-crossprod(xi,OmegaTi)%*%xi
      }

  ## meat
  salame<-apply(salame,1:2,sum)

  ## bread
  pane<-solve(crossprod(demX))

  ## sandwich
  mycov <- pane %*% salame %*% pane
  return(mycov)
}
