
############## Pesaran's CDtest ###############

  ## Pesaran's CD test for cross-sectional dependence in panel data models
  ## (and Breusch and Pagan's LM and scaled LM)
  ## ref. Pesaran, General diagnostic tests..., CesIfo WP 1229, 2004

  ## In case K+1>T the group-specific model is not estimable;
  ## as in Greene 11.7.2, formula (11.23) we use the group-specific residuals
  ## of a consistent estimator. This may be pooled OLS, RE, FE. Here the 
  ## default is set to FE.

  ## Note that the test can be performed on the results of plm's with
  ## any kind of effects: having "time" effects means checking for 
  ## xs-dependence *after* introducing time dummies.

  ## In principle, the test can be performed on the results of *any* 
  ## panelmodel object. Some issues remain regarding standardization of
  ## model output: some missing pieces are e.g. the 'model$indexes'
  ## in ggls. ''fd'' models are also not compatible because of indexes
  ## keeping the original timespan, while data lose the first period.

## production version, generic and based on plm

## this version 8: 
## - modular formula and panelmodel methods
## - modifications to tests to be made only on pcdres(),
##   the rest is interface :^)
## - allows local test by w[nxn] matrix; if w=NULL,
##   w is set to ones[nxn] and you get the global test

pcdtest<-function (x, ...) 
 {
    UseMethod("pcdtest")
 }

pcdtest.formula <- function(x, data, index = NULL,
                            model = NULL, test = c("cd","sclm","lm"),
                            w = NULL, ...) {

  data <- plm.data(data, index = index)
  
  ## (need a pooling model anyway, to get min(T) and k:
  ## this is inefficient but still rather cheap)          
  mymod <- plm(x, data, model = "pooling")



  ## check feasibility of separate regressions
  if(is.null(model) & min(pdim(mymod)$Tint$Ti)<length(mymod$coefficients)+1) {
    warning("Insufficient number of observations in time to estimate heterogeneous model: using within residuals",
            call.=FALSE)
            model<-"within"
  }

  ## get indices
  index <- attr(model.frame(mymod), "index")
  tind <- as.numeric(index[[2]])
  ind <- as.numeric(index[[1]])

  if(is.null(model)) {
    ## estimate the separate regressions
    X <- model.matrix(mymod)
    y <- model.response(model.frame(mymod))
    ## det. number of groups and df
    unind <- unique(ind)
    n <- length(unind)

    ## one regression for each group i in 1..n
    ## and retrieve residuals
    ## putting them into a list (might be unbalanced => t1!=t2)

    ## "pre-allocate" an empty list of length n
    tres <- vector("list", n)
    ## list of n:
    ## t_i (time series-) residuals
    ## for each x-sect. 1..n
    for(i in 1:n) {
      tX <- X[ind==unind[i],]
      ty <- y[ind==unind[i]]
      tres[[i]] <- lm.fit(tX,ty)$resid
      ## name resids after the time index
      names(tres[[i]]) <- tind[ind==unind[i]]
    }
    
  }
  else{
    ## estimate whatever model and fetch residuals
    mymod <-  plm(x, data, model = model, ...)
    myres <- mymod$residuals

    ## det. number of groups and df
    unind <- unique(ind)
    n <- length(unind)
    ## det. minimum group numerosity
    t <- min(pdim(mymod)$Tint$Ti)
    ## det. total number of obs. (robust vs. unbalanced panels)
    nT <- length(ind)
    k <- length(mymod$coefficients)

    ## "pre-allocate" an empty list of length n
    tres <- vector("list", n)


    ## use model residuals, group by group
    ## list of n:
    ## t_i residuals for each x-sect. 1..n
    for(i in 1:n) {
      tres[[i]] <- myres[ind==unind[i]]
      ## name resids after the time index
      names(tres[[i]]) <- tind[ind==unind[i]]
    }
  }
  return(pcdres(tres=tres, n=n, w=w,
                form=paste(deparse(substitute(x))),
                test=match.arg(test)))
}

pcdtest.panelmodel <- function(x,test=c("cd","sclm","lm"),
                             w=NULL,...) {

  ## this is taken from the last piece of pcdtest.formula, 
  ## after estimating relevant model

  ## fetch residuals
  myres <- resid(x)

  ## get indices
  index <- attr(model.frame(x), "index")
  tind <- as.numeric(index[[2]])
  ind <- as.numeric(index[[1]])

  ## det. number of groups and df
  unind <- unique(ind)
  n <- length(unind)
  ## det. minimum group numerosity
  t <-  pdim(x)$Tint$Ti
  ## det. total number of obs. (robust vs. unbalanced panels)
  nT <- length(ind)
  k <- length(x$coefficients)

  ## "pre-allocate" an empty list of length n
  tres <- vector("list", n)


  ## use model residuals, group by group
  ## list of n:
  ## t_i residuals for each x-sect. 1..n
  for(i in 1:n) {
              tres[[i]] <- myres[ind==unind[i]]
              ## name resids after the time index
              names(tres[[i]]) <- tind[ind==unind[i]]
              }

  return(pcdres(tres=tres, n=n, w=w,
                form=paste(deparse(substitute(formula))),
                test=match.arg(test)))
}


pcdres <- function(tres, n, w, form, test) {

  ## Take list of model residuals, group by group, and calc. test
  ## (from here on, what's needed for rho_ij is ok)
  ## this function is the modulus calculating the test, 
  ## to be called from either pcdtest.formula or 
  ## pcdtest.panelmodel

  ## rho_ij matrix
  rho <- matrix(NA,ncol=n,nrow=n)
  ## T_ij matrix
  t.ij <- matrix(NA,ncol=n,nrow=n)

  for(i in 1:n) {
    for(j in 1:n) {

      ## determination of joint range m_i | m_j 
      ## m_ij=m_i|m_j, working on names of the residuals' vectors
      m.ij <- intersect( names(tres[[i]]), names(tres[[j]]) )

      ## for this ij do me_i=mean_t(e_it[m_ij]), idem j
      ## and rho and T_ij as in Pesaran, page 18
      ## (as mean(ei)=0 doesn't necessarily hold any more)  

      ei <- tres[[i]][m.ij]
      ej <- tres[[j]][m.ij]
      dei <- ei-mean(ei)
      dej <- ej-mean(ej)
      rho[i,j] <- ( dei%*%dej )/( sqrt(dei%*%dei) * sqrt(dej%*%dej) )

      ## put this here inside summations, as for unbalanced panels
      ## "common obs. numerosity" T_ij may vary on i,j
      t.ij[i,j] <- length(m.ij)
      
      }
    }

  ## begin features for local test ####################
  ## higher orders omitted for now, use wlag() explicitly

  ## if global test, set all elements in w to 1
  if(is.null(w)) {w <- matrix(1,ncol=n,nrow=n)
                  dep <- ""} else { dep <- "local" }

  ## make (binary) selector matrix based on the contiguity matrix w
  ## and extracting elements corresponding to ones in the lower triangle
  ## excluding the diagonal
  
  ## transform in logicals (0=FALSE, else=TRUE: no need to worry
  ## about row-std. matrices)
  selector.mat <- matrix(as.logical(w),ncol=n)
  ## set upper tri and diagonal to false
  selector.mat[upper.tri(selector.mat,diag=TRUE)] <- FALSE

  ## number of elements in selector.mat
  elem.num <- sum(selector.mat)  

  ## end features for local test ######################

  ## Breusch-Pagan or Pesaran statistic for cross-sectional dependence,
  ## robust vs. unbalanced panels:

  switch(test, 
   lm = {
    CDstat <- sum((t.ij*rho^2)[selector.mat])
    pCD <- pchisq(CDstat,df=elem.num,lower.tail=F)
    names(CDstat) <- "chisq"
    parm <- elem.num
    names(parm) <- "df"
    testname <- "Breusch-Pagan LM test"
   },
   sclm = {
    CDstat <- sqrt(1/(2*elem.num))*sum((t.ij*rho^2-1)[selector.mat])
    pCD <- pnorm(CDstat,lower.tail=F)
    names(CDstat) <- "z"
    parm <- NULL
    testname <- "Scaled LM test"
   },
   cd = {
    CDstat <- sqrt(1/elem.num)*sum((sqrt(t.ij)*rho)[selector.mat])
    pCD <- 2*pnorm(abs(CDstat),lower.tail=F)
    names(CDstat) <- "z"
    parm <- NULL
    testname <- "Pesaran CD test"
   })

  ##(insert usual htest features)
  dname <-  paste(deparse(substitute(formula)))
  RVAL <-  list(statistic = CDstat, parameter = parm,
               method = paste(testname, "for", dep, "cross-sectional dependence in panels"),
               alternative = "cross-sectional dependence",
               p.value = pCD,
               data.name =   dname)
  class(RVAL) <- "htest"
  return(RVAL)
  }

