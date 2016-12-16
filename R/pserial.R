#### pbgtest

pbgtest <- function (x, ...) {
    UseMethod("pbgtest")
}

pbgtest.formula <- function(x, order = NULL, type = c("Chisq", "F"), data, model=c("pooling", "random", "within"), ...) {
  ## formula method for pbgtest;
  ## defaults to a pooling model
  cl <- match.call(expand.dots = TRUE)
  if (names(cl)[3] == "") names(cl)[3] <- "data"
  if (is.null(cl$model)) cl$model <- "pooling"
  names(cl)[2] <- "formula"
  m <- match(plm.arg, names(cl), 0)
  cl <- cl[c(1L,m)]
  cl[[1L]] <- quote(plm)
  plm.model <- eval(cl,parent.frame())
  pbgtest(plm.model, order = order, type = type, data = data, ...)
}

pbgtest.panelmodel <- function(x, order = NULL, type = c("Chisq", "F"), ...) {
  ## residual serial correlation test based on the residuals of the demeaned
  ## model (see Wooldridge (2002), p. 288) and the regular bgtest() in {lmtest}

  ## structure:
  ## 1: take demeaned data from 'plm' object
  ## 2: est. auxiliary model by OLS on demeaned data
  ## 3: apply lmtest::bgtest() to auxiliary model and return the result

  model <- describe(x, "model")
  effect <- describe(x, "effect")
  theta <- x$ercomp$theta

  ## retrieve demeaned data
  demX <- model.matrix(x, model = model, effect = effect, theta = theta)
  demy <- pmodel.response(model.frame(x), model = model, effect = effect, theta = theta)
  
  ## ...and group numerosities
  Ti <- pdim(x)$Tint$Ti
  ## set lag order to minimum group numerosity if not specified by user
  ## (check whether this is sensible)
  
  if(is.null(order)) order <- min(Ti)
  ## bg test on the demeaned model:
  
  ## check package availability and load if necessary
  #lm.ok <- require("lmtest")
  #if(!lm.ok) stop("package lmtest is needed but not available")
  
  ## bgtest is the bgtest, exception made for the method attribute
  auxformula <- demy~demX-1 #if(model == "within") demy~demX-1 else demy~demX
  lm.mod <- lm(auxformula)
  bgtest <- bgtest(lm.mod, order = order, type = type, ...)
  bgtest$method <- "Breusch-Godfrey/Wooldridge test for serial correlation in panel models"
  bgtest$alternative <- "serial correlation in idiosyncratic errors"
  bgtest$data.name <- paste(deparse(x$call$formula))
  names(bgtest$statistic) <- if(length(bgtest$parameter)==1) "chisq" else "F"
  return(bgtest)
}

### pwtest

pwtest <- function(x, ...){
  UseMethod("pwtest")
}

pwtest.formula <- function(x, data, effect = c("individual", "time"), ...) {
  
  effect <- match.arg(effect, choices = c("individual", "time")) # match effect to pass it on to pwtest.panelmodel

  cl <- match.call(expand.dots = TRUE)
  if (names(cl)[3] == "") names(cl)[3] <- "data"
  if (is.null(cl$model)) cl$model <- "pooling"
  if (cl$model != "pooling") stop("pwtest only relevant for pooling models")
  names(cl)[2] <- "formula"
  m <- match(plm.arg, names(cl), 0)
  cl <- cl[c(1L,m)]
  cl[[1L]] <- quote(plm)
  plm.model <- eval(cl,parent.frame())
  # pwtest(plm.model)
  pwtest.panelmodel(plm.model, effect = effect) # pass on desired 'effect' argument to pwtest.panelmodel
  
  ## "RE" test a la Wooldridge (2002/2010), see 10.4.4
  ## (basically the scaled and standardized estimator for sigma from REmod)
  ## does not rely on normality or homoskedasticity; 
  ## H0: composite errors uncorrelated

  ## ref. Wooldridge (2002), pp. 264-265; Wooldridge (2010), pp. 299-300

  ######### from here generic testing interface from
  ######### plm to my code
}

pwtest.panelmodel <- function(x, effect = c("individual", "time"), ...) {
  ## tind is actually not needed here
  if (describe(x, "model") != "pooling") stop("pwtest only relevant for pooling models")
  effect <- match.arg(effect, choices = c("individual", "time")) # was: effect <- describe(x, "effect")
                                                                 # here we want the effect as in the call of pwtest(),
                                                                 # not of the already estimated model, because that is
                                                                 # always a pooling model
  data <- model.frame(x)
  ## extract indices

  ## if effect="individual" std., else swap
  index <- attr(data, "index")
  if (effect == "individual"){
    index <- index[[1]]
    tindex <- index[[2]]
  }
  else{
    index <- index[[2]]
    tindex <- index[[1]]
  }
  ## det. number of groups and df
  n <- length(unique(index))
  X <- model.matrix(x)

  k <- ncol(X)
  ## det. total number of obs. (robust vs. unbalanced panels)
  nT <- nrow(X)
  ## det. max. group numerosity
  t <- max(tapply(X[,1],index,length))

  ## ref. Wooldridge (2002), p.264 / Wooldridge (2010), p.299
    
  ## extract resids
  u <- resid(x)

  ## est. random effect variance
  ## "pre-allocate" an empty list of length n
  tres <- vector("list", n)

  ## list of n "empirical omega-blocks"
  ## with averages of xproducts of t(i) residuals
  ## for each group 1..n 
  ## (possibly different sizes if unbal., thus a list
  ## and thus, unlike Wooldridge (eq.10.37), we divide 
  ## every block by *his* t(t-1)/2)
#  unind <- unique(ind)
  unind <- unique(index) # ????
 
  for(i in 1:n) {
    ut <- u[index == unind[i]]
    tres[[i]] <- ut%o%ut
  }

  ## sum over all upper triangles of emp. omega blocks:
  ## define aux. function
  uptrisum <- function(x) {
    uts <- sum(x[upper.tri(x,diag=FALSE)])
    return(uts)}
  
  ## det. # of upper triangle members (n*t(t-1)/2 if balanced)
  ti <- sapply(tres, function(x) dim(x)[[1]])
  uptrinum <- sum(ti*(ti-1)/2)  # don't need this!!

  ## ...apply to list and sum over resulting vector (df corrected)
  W <- sum(sapply(tres,uptrisum)) # /sqrt(n) simplifies out
  
  ## calculate se(Wstat) as in 10.40
  seW <- sqrt( sum( sapply(tres,uptrisum)^2 ) )
  
  ## NB should we apply a df correction here, maybe that of the standard
  ## RE estimator? (see page 261) 

  Wstat <- W/seW
  names(Wstat) <- "z"
  pW <- 2*pnorm(abs(Wstat),lower.tail=FALSE) # unlike LM, test is two-tailed!
  
  ##(insert usual htest features)
  dname <- paste(deparse(substitute(formula)))
  RVAL <- list(statistic = Wstat, parameter = NULL,
               method = paste("Wooldridge's test for unobserved",
                              effect, "effects"),
               alternative = "unobserved effect",
               p.value = pW,
               data.name =   dname)
  class(RVAL) <- "htest"
  return(RVAL)
}


### pwartest

pwartest <- function(x, ...){
  UseMethod("pwartest")
}

pwartest.formula <- function(x,  data, ...) {
  ## small-sample serial correlation test for FE models
  ## ref.: Wooldridge (2002/2010) 10.5.4 
  ##if(!require(car)) stop("Library 'car' is needed")

  cl <- match.call(expand.dots = TRUE)
  if (is.null(cl$model)) cl$model <- "within"
  if (cl$model != "within") stop("pwartest only relevant for within models")
  if (names(cl)[3] == "") names(cl)[3] <- "data"
  names(cl)[2] <- "formula"
  m <- match(plm.arg, names(cl), 0)
  cl <- cl[c(1L,m)]
  cl[[1L]] <- quote(plm)
  plm.model <- eval(cl, parent.frame())
  pwartest(plm.model)
}

pwartest.panelmodel <- function(x, ...){
  FEres <- resid(x)
  data <- model.frame(x)
  if (describe(x, "model") != "within") stop("pwartest only relevant for within models")
  
  ## this is a bug fix for incorrect naming of the "data" attr.
  ## for the pseries in pdata.frame()
  
  attr(FEres,"data") <- NULL
  N <- length(FEres)
  FEres.1 <- c(NA,FEres[1:(N-1)])
  index <- attr(data, "index")
  id <- index[[1]]
  time <- index[[2]]
  lagid <- as.numeric(id)-c(NA,as.numeric(id)[1:(N-1)])
  FEres.1[lagid!=0] <- NA
  data <- data.frame(id, time, FEres = unclass(FEres), FEres.1 = unclass(FEres.1))
  names(data)[c(1,2)] <- c("id","time")
  data <- na.omit(data)
  auxmod <- plm(FEres~FEres.1, data = data, model = "pooling", index = c("id", "time"))

  ## calc. theoretical rho under H0: no serial corr. in errors
  t. <- pdim(x)$nT$T
  rho.H0 <- -1/(t.-1)
  ## test H0: rho=rho.H0 with HAC t-test (HC0-3 parm may be passed)
  myvcov <- function(x) vcovHC(x, method="arellano", ...)

  myH0 <- paste("FEres.1 = ", as.character(rho.H0), sep="")
  # args(linearHypothesis) = test = c("F","Chisq") white.adjust = FALSE
  lhtest <- linearHypothesis(model=auxmod, myH0, vcov.=myvcov, ...)
#  lhtest <- linearHypothesis(model=auxmod, myH0, vcov.=myvcov(x), ...)

  ##(insert usual htest features)  
  FEARstat <- lhtest[2,3]
  names(FEARstat) <- dimnames(lhtest)[[2]][3]
  if (names(FEARstat)=="Chisq") names(FEARstat) <- "chisq"
  ## this is either 'F' or 'Chisq' and is the name of 3rd
  ## column because we are supplying a vcov matrix
  pFEAR <- lhtest[2,4]

  dname <- paste(deparse(substitute(x)))
  RVAL <- list(statistic = FEARstat,
               parameter = NULL,
               method = "Wooldridge's test for serial correlation in FE panels",
               alternative = "serial correlation",
               p.value = pFEAR,
               data.name =   dname)
  class(RVAL) <- "htest"
  return(RVAL)

}

### pbsytest

# NB: There is also a version of pbsytest which supports unbalanced panels.
#     It resides in SVN on r-forge: branches/kt_unbalanced/pbsytest/
#     and awaits approval

pbsytest <- function (x, ...){
  UseMethod("pbsytest")
}

pbsytest.formula <- function(x, data, ..., test=c("ar","re","j")) {

  ## Bera., Sosa-Escudero and Yoon type LM test for random effects
  ## under serial correlation (H0: no random effects) or the inverse;
  ## ref. Baltagi 2005, pages 96-97;
  ## original ref. Bera, Sosa-Escudero and Yoon, JE 101 (2001)
  ## test="AR" you get the serial corr. test robust vs. RE
  ## test="RE" you get the RE test robust vs. serial corr.

  ######### from here generic testing interface from
  ######### plm to my code
  if (length(test) == 1) test <- tolower(test) # for backward compatibility: allow upper case
  test <- match.arg(test)
  
  cl <- match.call(expand.dots = TRUE)
  if (is.null(cl$model)) cl$model <- "pooling"
  if (cl$model != "pooling") stop("pbsytest only relevant for pooling models")
  names(cl)[2] <- "formula"
  if (names(cl)[3] == "") names(cl)[3] <- "data"
  m <- match(plm.arg, names(cl), 0)
  cl <- cl[c(1L,m)]
  cl[[1L]] <- quote(plm)
  plm.model <- eval(cl, parent.frame())
  pbsytest(plm.model, test = test)
}

pbsytest.panelmodel <- function(x, test=c("ar","re","j"), ...){
  ### as this is the version without support for unbalanced data, issue a warning
  ##  unbalanced capable version resides in SVN on r-forge: branches/kt_unbalanced/pbsytest/
  ##  and awaits approval
  if (!is.pbalanced(x)) warning("unbalanced tests not yet implemented for pbsytest(), applying balanced tests to unbalanced model...")
  
  poolres <- resid(x)
  data <- model.frame(x)
  ## extract indices
  index <- attr(data, "index")
  tindex <- index[[2]]
  index <- index[[1]]
  
  ## till here. 
  ## ordering here if needed.
  
  ## this needs ordering of obs. on time, regardless 
  ## whether before that on groups or after
  
  ## and numerosity check
  
  ## order by group, then time
  oo <- order(index,tindex)
  ind <- index[oo]
  tind <- tindex[oo]
  poolres <- poolres[oo]
  ## det. number of groups and df
  n <- length(unique(index))
  k <- ncol(model.matrix(x))
  ## det. max. group numerosity
  t <- max(pdim(x)$Tint$Ti)
  ## det. total number of obs. (robust vs. unbalanced panels)
  nT <- length(ind)

  ## calc. A and B:
  S1 <- sum( tapply(poolres,ind,sum)^2 )
  S2 <- sum( poolres^2 )
            
  A <- S1/S2-1

  unind <- unique(ind)
  uu <- rep(NA,length(unind))
  uu1 <- rep(NA,length(unind))
  for(i in 1:length(unind)) {
    u.t <- poolres[ind==unind[i]]
    u.t.1 <- u.t[-length(u.t)]
    u.t <- u.t[-1]
    uu[i] <- crossprod(u.t)
    uu1[i] <- crossprod(u.t,u.t.1)
  }
  
  B <- sum(uu1)/sum(uu)
  
  switch(match.arg(test),
         ar ={LM <- (n * t^2 * (B - (A/t))^2) / ((t-1)*(1-(2/t)))
             df <- c(df=1)
             names(LM) <- "chisq"
             pLM <- pchisq(LM,df=1,lower.tail=FALSE)
             tname <- "Bera, Sosa-Escudero and Yoon locally robust test"
             myH0 <- "AR(1) errors sub random effects"
           },
         re={LM <- (A - 2*B) * sqrt( (n * t) / (2*(t-1)*(1-(2/t))) )
             names(LM) <- "z"
             df <- NULL
             pLM <- pnorm(LM,lower.tail=FALSE)
             tname <- "Bera, Sosa-Escudero and Yoon locally robust test"
             myH0 <- "random effects sub AR(1) errors"
           },              
         j={LM <- (n * t^2) / (2*(t-1)*(t-2)) * (A^2 - 4*A*B + 2*t*B^2) 
            df <- c(df=2)
            names(LM) <- "chisq"
            pLM <- pchisq(LM,df=1,lower.tail=FALSE)
            tname <- "Baltagi and Li AR-RE joint test"
            myH0 <- "AR(1) errors or random effects"
          }
         )

  dname <- paste(deparse(substitute(formula)))
  RVAL <- list(statistic = LM,
               parameter = df,
               method = tname,
               alternative = myH0,
               p.value = pLM,
               data.name = dname)
  class(RVAL) <- "htest"
  return(RVAL)

  }

### pdwtest

pdwtest <- function (x, ...) {
    UseMethod("pdwtest")
}

pdwtest.formula <- function(x, data, ...) {
  ## formula method for pdwtest;
  ## defaults to a RE model

  cl <- match.call(expand.dots = TRUE)
  if (is.null(cl$model)) cl$model <- "pooling"
  names(cl)[2] <- "formula"
  if (names(cl)[3] == "") names(cl)[3] <- "data"
  m <- match(plm.arg, names(cl), 0)
  cl <- cl[c(1L,m)]
  cl[[1L]] <- quote(plm)
  plm.model <- eval(cl, parent.frame())
  pdwtest(plm.model, ...)
}

pdwtest.panelmodel <- function(x,...) {
  ## residual serial correlation test based on the residuals of the demeaned
  ## model and the regular dwtest() in {lmtest}
  ## reference Baltagi (page 98) for FE application, Wooldridge page 288 for
  ## the general idea.


  ## structure:
  ## 1: take demeaned data from 'plm' object
  ## 2: est. auxiliary model by OLS on demeaned data
  ## 3: apply bgtest() to auxiliary model and return the result

  model <- describe(x, "model")
  effect <- describe(x, "effect")
  theta <- x$ercomp$theta
                                                    
  ## retrieve demeaned data
  demX <- model.matrix(x, model = model, effect = effect, theta=theta)
  demy <- pmodel.response(model.frame(x), model = model, effect = effect, theta=theta)
 

  ## dw test on the demeaned model:
  
  ## check package availability and load if necessary
  ##lm.ok <- require("lmtest")
  ##if(!lm.ok) stop("package lmtest is needed but not available")
  ## ARtest is the bgtest, exception made for the method attribute
  dots <- match.call(expand.dots=FALSE)[["..."]]
  if (is.null(dots$order.by)) order.by <- NULL else order.by <- dots$order.by
  if (is.null(dots$alternative)) alternative <- "greater" else alternative <- dots$alternative
  if (is.null(dots$iterations)) iterations <- 15 else iterations <- dots$iterations
  if (is.null(dots$exact)) exact <- NULL else exact <- dots$exact
  if (is.null(dots$tol)) tol <- 1e-10 else tol <- dots$tol


  auxformula <- demy~demX-1 #if(model == "within") demy~demX-1 else demy~demX
  lm.mod <- lm(auxformula)
  
  ARtest <- dwtest(lm.mod, order.by = order.by,
                   alternative = alternative,
                   iterations = iterations, exact = exact, tol = tol)
#  ARtest <- dwtest(lm(demy~demX-1))

  ARtest$method <- "Durbin-Watson test for serial correlation in panel models"
  ARtest$alternative <- "serial correlation in idiosyncratic errors"
  ARtest$data.name <- paste(deparse(x$call$formula))
  return(ARtest)
}


######### Baltagi and Li's LM_rho|mu ########
## ex Baltagi and Li (1995) Testing AR(1) against MA(1)...,
## JE 68, 133-151, test statistic (one-sided) is LM_4;
## see also idem (1997), Monte carlo results...,
## Annales d'Econometrie et Statistique 48, formula (8)

## from version 2: disposes of Kronecker products,
## thus much faster and feasible on large NT (original
## is already infeasible for NT>3000, this takes 10''
## on N=3000, T=10 and even 20000x10 (55'') is no problem;
## lme() hits the memory limit at ca. 20000x20)

pbltest.formula <- function(x, data, alternative = c("twosided", "onesided"), index=NULL, ...) {
 ## this version (pbltest0) based on a "formula, pdataframe" interface


  ## reduce X to model matrix value (no NAs)
  X<-model.matrix(x,data=data)
  ## reduce data accordingly
  data <- data[which(row.names(data)%in%row.names(X)),]

  data <- pdata.frame(data,index=index)

  ## need name of individual index
  gindex <- dimnames(attr(data, "index"))[[2]][1]

 ## make random effects formula
  rformula <- NULL
  eval(parse(text=paste("rformula <- ~1|",gindex,sep="")))

  ## est. MLE model
  mymod <- lme(x,data=data,random=rformula,method="ML")

  nt. <- mymod$dims$N
  n. <- as.numeric(mymod$dims$ngrps[1])
  t. <- nt./n.
  Jt <- matrix(1,ncol=t.,nrow=t.)/t.
  Et <- diag(1,t.)-Jt
  ## make 'bidiagonal' matrix (see BL, p.136)
  G <- matrix(0,ncol=t.,nrow=t.)
  for(i in 2:t.) {
    G[i-1,i] <- 1
    G[i,i-1] <- 1
    }

  ## retrieve composite (=lowest level) residuals
  uhat <- residuals(mymod,level=0)

  ## sigma2.e and sigma2.1 as in BL
  ## break up residuals by group to get rid of Kronecker prod.
  ## data have to be balanced and sorted by group/time, so this works
  uhat.i <- vector("list",n.)
  for(i in 1:n.) {
    uhat.i[[i]] <- uhat[t.*(i-1)+1:t.]
    }
  s2e <- rep(NA,n.)
  s21 <- rep(NA,n.)
  for(i in 1:n.) {
    u.i <- uhat.i[[i]]
    s2e[i] <- as.numeric(crossprod(u.i,Et) %*% u.i)
    s21[i] <- as.numeric(crossprod(u.i,Jt) %*% u.i)
    }
  sigma2.e <- sum(s2e) / (n.*(t.-1))
  sigma2.1 <- sum(s21) / n.

  ## calc. score under the null:
  star1 <- (Jt/sigma2.1 + Et/sigma2.e) %*% G %*% (Jt/sigma2.1 + Et/sigma2.e)
  star2 <- rep(NA,n.)
  ## again, do this group by group to avoid Kronecker prod.
  for(i in 1:n.) {
    star2[i] <- as.numeric(crossprod(uhat.i[[i]],star1) %*% uhat.i[[i]])
    }
  star2 <- sum(star2)
  Drho <- (n.*(t.-1)/t.) * (sigma2.1-sigma2.e)/sigma2.1 + sigma2.e/2 * star2
  ## star2 is (crossprod(uhat, kronecker(In, star1)) %*% uhat)

  ## components for the information matrix
  a <- (sigma2.e-sigma2.1)/(t.*sigma2.1)
  j.rr <- n. * (2 * a^2 * (t.-1)^2 + 2*a*(2*t.-3) + (t.-1))
  j.12 <- n.*(t.-1)*sigma2.e / sigma2.1^2
  j.13 <- n.*(t.-1)/t. * sigma2.e * (1/sigma2.1^2 - 1/sigma2.e^2)
  j.22 <- (n. * t.^2) / (2 * sigma2.1^2)
  j.23 <- (n. * t.) / (2 * sigma2.1^2)
  j.33 <- (n./2) * (1/sigma2.1^2 + (t.-1)/sigma2.e^2)

  ## build up information matrix
  Jmat <- matrix(nrow=3,ncol=3)
  Jmat[1,] <- c(j.rr,j.12,j.13)
  Jmat[2,] <- c(j.12,j.22,j.23)
  Jmat[3,] <- c(j.13,j.23,j.33)

  J11 <- n.^2 * t.^2 * (t.-1) / (det(Jmat) * 4*sigma2.1^2 * sigma2.e^2)
  ## this is the same as J11 <- solve(Jmat)[1,1], see BL page 73

  switch(match.arg(alternative),
         onesided = {
           LMr.m <- Drho * sqrt(J11)
           pval <- pnorm(LMr.m,lower.tail=FALSE)
                                        #    names(LMr.m) <- "Z"
           names(LMr.m) <- "z"
           method1 <- "one-sided"
           method2 <- "H0: rho = 0, HA: rho > 0"
           parameter <- NULL
         },
         twosided = {
           LMr.m <- Drho^2 * J11
           pval <- pchisq(LMr.m,1,lower.tail=FALSE)
           names(LMr.m) <- "chisq"
           parameter <- c(df=1)
           method1 <- "two-sided"
           method2 <- "H0: rho = 0, HA: rho != 0"
         }
         )
  dname <- paste(deparse(substitute(x)))
  method <- paste("Baltagi and Li", method1,"LM test")
  alternative <- "AR(1)/MA(1) errors in RE panel model"

  res <- list(statistic = LMr.m,
              p.value = pval,
              method = method,
              alternative = alternative,
              parameter = parameter,
              data.name = dname)

  class(res) <- "htest"
  res
}

pbltest.plm <- function(x, alternative = c("twosided", "onesided"), ...) {
  # only continue if random effects model
  if (describe(x, "model") != "random") stop("Test is only for random effects models.")
  
  # call pbltest.formula in the right way
  pbltest.formula(formula(x$formula), data=cbind(index(x), x$model), index=names(index(x)), alternative = alternative, ...)
}

pbltest <- function (x, ...) 
{
  UseMethod("pbltest")
}


pwfdtest <- function(x, ...){
  UseMethod("pwfdtest")
}

pwfdtest.formula <- function(x, data, ..., h0 = c("fd", "fe")){
  cl <- match.call(expand.dots = TRUE)
  if (is.null(cl$model)) cl$model <- "fd"
  names(cl)[2] <- "formula"
  if (names(cl)[3] == "") names(cl)[3] <- "data"
  m <- match(plm.arg, names(cl), 0)
  cl <- cl[c(1L,m)]
  cl[[1L]] <- quote(plm)
  plm.model <- eval(cl, parent.frame())
  pwfdtest(plm.model, ..., h0 = h0)
}

pwfdtest.panelmodel <- function(x, ..., h0 = c("fd", "fe")) {
  ## first-difference-based serial correlation test for panel models
  ## ref.: Wooldridge (2002/2010), par. 10.6.3 
  
  # interface check
  model <- describe(x, "model")
  if (model != "fd") stop(paste0("input 'x' needs to be a \"fd\" model (first-differenced model), but is \"", model, "\""))

  ##if(!require(car)) stop("Library 'car' is needed")

  ## fetch fd residuals
  FDres <- resid(x)
  ## indices (full length! must reduce by 1st time period)
   ## this is an ad-hoc solution for the fact that the 'fd' model
   ## carries on the full indices while losing the first time period
  index <- attr(model.frame(x), "index")
  time <- as.numeric(index[[2]])
  id <- as.numeric(index[[1]])

  ## fetch dimensions and adapt to those of indices
  pdim <- pdim(x)
  n <- pdim$nT$n

 
  ## (re)create groupwise-separated index from 1 to nT 
  ## - dropping first time period
  ## - correcting Ti=Ti+1
  Ti <- pdim$Tint$Ti-1
  
  redind <- vector("list",n)
  tfirst <- 0
  for(i in 1:n) {
    redind[[i]] <- (tfirst+2):(tfirst+Ti[i]+1)
    tfirst <- max(redind[[i]])
  }
   ## reduce indices by 1st time period
  redind <- unlist(redind)
  time <- time[redind]
  id <- id[redind]
    
  N <- length(FDres)

  FDres.1 <- c(NA,FDres[1:(N-1)])

  lagid <- id - c(NA,id[1:(N-1)])

  FDres.1[lagid!=0] <- NA


  ## make (panel) dataframe for auxiliary regression
  auxdata <- as.data.frame(cbind(id,time))
  auxdata$FDres <- FDres
  auxdata$FDres.1 <- FDres.1
  ## pooling model FDres vs. lag(FDres),
  ## with intercept (might as well do it w.o.)
  auxmod <- plm(FDres ~ FDres.1, na.omit(auxdata), model = "pooling")

  switch(match.arg(h0), 
             fd = {h0des <- "differenced"
                   ## theoretical rho under H0: no serial 
                   ## corr. in differenced errors is 0
                   rho.H0 <- 0},
             fe = {h0des <- "original"
                   ## theoretical rho under H0: no serial 
                   ## corr. in original errors is -0.5
                   rho.H0 <- -0.5})

  ## test H0: rho=rho.H0 with HAC t-test (HC0-3 parm may be passed)
  myvcov <- function(x) vcovHC(x, method="arellano", ...)

  myH0 <- paste("FDres.1 = ", as.character(rho.H0), sep="")
  lhtest <- linearHypothesis(model=auxmod, myH0, vcov.=myvcov, ...)
  
  ## (insert usual htest features)  
  FDARstat <- lhtest[2,3]
  names(FDARstat) <- dimnames(lhtest)[[2]][3] 
  if (names(FDARstat)=="Chisq") names(FDARstat) <- "chisq"
  ## this is either 'F' or 'Chisq' and is the name of 3rd
  ## column because we are supplying a vcov matrix
  pFDAR <- lhtest[2,4]

  dname <- paste(deparse(substitute(x)))
  RVAL <- list(statistic   = FDARstat, 
               parameter   = NULL,
               method      = "Wooldridge's first-difference test for serial correlation in panels",
               alternative = paste("serial correlation in", h0des, "errors"),
               p.value     = pFDAR,
               data.name   = dname)
  class(RVAL) <- "htest"
  return(RVAL)

}
