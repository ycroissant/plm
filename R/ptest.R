data.name <- function(x){
  data.name <- paste(deparse(x$call$formula))
  if (length(data.name) > 1) paste(data.name[1], "...")
  else data.name
}

############## phtest() ############################################
phtest <- function(x,...){
  UseMethod("phtest")
}

phtest.formula <- function(x, data, model = c("within", "random"),
                            method = c("chisq", "aux"),
                            index=NULL, vcov=NULL, ...){
    if(length(model)!=2) stop("two models should be indicated")
    for (i in 1:2){
        model.name <- model[i]
        if(!(model.name %in% names(model.plm.list))){
            stop("model must be one of ", oneof(model.plm.list))
        }
    }
    switch(match.arg(method),
           chisq={
               cl <- match.call(expand.dots = TRUE)
               cl$model <- model[1]
               names(cl)[2] <- "formula"
               m <- match(plm.arg,names(cl),0)
               cl <- cl[c(1,m)]
               cl[[1]] <- as.name("plm")
               plm.model.1 <- eval(cl,parent.frame())
               plm.model.2 <- update(plm.model.1, model = model[2])
               return(phtest(plm.model.1, plm.model.2))
           },
           aux={
               ## some interface checks here
               if(model[1]!="within") {
                   stop("Please supply 'within' as first model type")
               }
             
               ## set pdata
               if (!inherits(data, "pdata.frame")) data <- plm.data(data, indexes=index) #, ...)
               
               row.names(data) <- NULL # reset rownames of original data set (number rownames in clean sequence) to make rownames
                                       # comparable for later comparision to obs used in estimation of models (get rid of NA values)
                                       # [needed becausepmodel.response() and model.matrix() do not retain fancy rownames, but rownames]
               
               # calculatate FE and RE model
               fe_mod <- plm(formula=x, data=data, model=model[1])
               re_mod <- plm(formula=x, data=data, model=model[2])
               
               reY <- pmodel.response(re_mod)
               reX <- model.matrix(re_mod)[ , -1] # intercept not needed
               feX <- model.matrix(fe_mod)
               dimnames(feX)[[2]] <- paste(dimnames(feX)[[2]],
                                           "tilde", sep=".")
               
               ## estimated models could have fewer obs (due droping of NAs) compared to the original data
               ## => match original data and observations used in estimated models
               ## routine adapted from lmtest::bptest
               commonrownames <- intersect(intersect(intersect(row.names(data), names(reY)), row.names(reX)), row.names(feX))
               if (!(all(c(row.names(data) %in% commonrownames, commonrownames %in% row.names(data))))) {
                 data <- data[commonrownames, ]
                 reY  <- reY[commonrownames]
                 reX  <- reX[commonrownames, ]
                 feX  <- feX[commonrownames, ]
               }
               
               # Tests of correct matching of obs (just for safety ...)
                if (!all.equal(length(reY), nrow(data), nrow(reX), nrow(feX)))
                  stop("number of cases/observations do not match, most likely due to NAs in \"data\"")
                if (any(c(is.na(names(reY)), is.na(row.names(data)), is.na(row.names(reX)), is.na(row.names(feX)))))
                    stop("one (or more) rowname(s) is (are) NA")
                if (!all.equal(names(reY), row.names(data), row.names(reX), row.names(feX)))
                  stop("row.names of cases/observations do not match, most likely due to NAs in \"data\"")

               ## fetch indices here, check pdata
               ## construct data set and formula for auxiliary regression
               data <- data.frame(cbind(data[, 1:2], reY, reX, feX))
               auxfm <- as.formula(paste("reY~",
                                         paste(dimnames(reX)[[2]],
                                               collapse="+"), "+",
                                         paste(dimnames(feX)[[2]],
                                               collapse="+"), sep=""))
               auxmod <- plm(formula=auxfm, data=data, model="pooling")
               nvars <- dim(feX)[[2]]
               R <- diag(1, nvars)
               r <- rep(0, nvars) # here just for clarity of illustration
               omega0 <- vcov(auxmod)[(nvars+2):(nvars*2+1),
                                      (nvars+2):(nvars*2+1)]
               Rbr <- R %*% coef(auxmod)[(nvars+2):(nvars*2+1)] - r

               h2t <- crossprod(Rbr, solve(omega0, Rbr))
               ph2t <- pchisq(h2t, df=nvars, lower.tail=FALSE)

               df <- nvars
               names(df) <- "df"
               names(h2t) <- "chisq"

               if(!is.null(vcov)) {
                   vcov <- paste(", vcov: ",
                                  paste(deparse(substitute(vcov))),
                                  sep="")
               }

               haus2 <- list(statistic   = h2t,
                             p.value     = ph2t,
                             parameter   = df,
                             method      = paste("Regression-based Hausman test",
                                              vcov, sep=""),
                             alternative = "one model is inconsistent",
                             data.name   = paste(deparse(substitute(x))))
               class(haus2) <- "htest"
               return(haus2)
           })
}

phtest.panelmodel <- function(x, x2, ...){
  coef.wi <- coef(x)
  coef.re <- coef(x2)
  vcov.wi <- vcov(x)
  vcov.re <- vcov(x2)
  names.wi <- names(coef.wi)
  names.re <- names(coef.re)
  common_coef_names <- names.re[names.re%in%names.wi]
  common_coef_names <- common_coef_names[!(common_coef_names %in% "(Intercept)")] # drop intercept if included (when between model inputted)
  coef.h <- common_coef_names
  dbeta <- coef.wi[coef.h]-coef.re[coef.h]
  df <- length(dbeta)
  dvcov <- vcov.re[coef.h,coef.h]-vcov.wi[coef.h,coef.h]
  stat <- abs(t(dbeta)%*%solve(dvcov)%*%dbeta)
#  pval <- (1-pchisq(stat,df=df))
  pval <- pchisq(stat,df=df,lower.tail=FALSE)
  names(stat) <- "chisq"
  parameter <- df
  names(parameter) <- "df"
  alternative <- "one model is inconsistent"
#  null.value <- "both models are consistent"
  res <- list(statistic = stat,
              p.value = pval,
              parameter = parameter,
              method = "Hausman Test",
              data.name = data.name(x),
 #             null.value=null.value,
              alternative=alternative)
  class(res) <- "htest"
  return(res)
}

############## plmtest() ############################################
# For a concise overview with original references see
# Baltagi (2013), Econometric Analysis of Panel Data, 5th edition, pp. 68-76 (balanced), pp. 200-203 (unbalanced).
#
# balanced (original) version of Breusch-Pagan test:
#     T.S. Breusch & A.R. Pagan (1979),
#       A Simple Test for Heteroscedasticity and Random Coefficient Variation,
#       Econometrica 47, pp. 1287-1294
#
# unbalanced version:
#     Baltagi/Li (1990),
#       A lagrange multiplier test for the error components model with incomplete panels,
#       Econometric Reviews, 9, pp. 103-107,

plmtest <- function(x,...){
  UseMethod("plmtest")
}

plmtest.plm <- function(x,
                        effect = c("individual", "time", "twoways"),
                        type = c("honda", "bp", "ghm", "kw"),
                        ...) {
  
  effect <- match.arg(effect)
  type <- match.arg(type)
  if (describe(x, "model") != "pooling") x <- update(x, model = "pooling")
  pdim <- pdim(x)
  n <- pdim$nT$n
  T <- pdim$nT$T
  N_obs <- pdim$nT$N
  balanced <- pdim$balanced
  index <- attr(model.frame(x), "index")
  id <- index[[1]]
  time <- index[[2]]
  T_i <- pdim$Tint$Ti
  N_t <- pdim$Tint$nt
  res <- resid(x)
  
  ### calc of parts of test statistic ##
  # calc. is done w/o using matrix calculation, see e.g. Baltagi/Li (1990), p. 106
  A1 <- as.numeric(crossprod(tapply(res,id,sum))/sum(res^2) - 1)   # == A1 <- sum(tapply(res,id,sum)^2)/sum(res^2) - 1
  A2 <- as.numeric(crossprod(tapply(res,time,sum))/sum(res^2) - 1) # == A2 <- sum(tapply(res,time,sum)^2)/sum(res^2) - 1
  
  M11 <- sum(T_i^2)
  M22 <- sum(N_t^2)
  
  LM1 <- N_obs * (1/sqrt(2*(M11 - N_obs))) * A1 # == sqrt( (((N_obs)^2) / 2) * ( A1^2 / (M11 - N_obs)) ) [except sign due to positive sqrt]
  LM2 <- N_obs * (1/sqrt(2*(M22 - N_obs))) * A2 # == sqrt( (((N_obs)^2) / 2) * ( A2^2 / (M22 - N_obs)) ) [except sign due to positive sqrt]
  ### END calc of parts of test statistic ##
  
  
  if (effect != "twoways"){
    # oneway
    if (!type %in% c("honda", "bp", "kw"))
      stop("type must be one of \"honda\", \"bp\" or \"kw\" for a one way model") # kw oneway coincides with honda
    
    ifelse(effect == "individual", stat <- LM1, stat <- LM2)
    stat <- switch(type,
                   honda = c(normal = stat),
                   bp    = c(chisq  = stat^2),
                   kw    = c(normal = stat))
    
    parameter <- switch(type,
                          honda = NULL,
                          bp = c(df = 1), # df = 1 in the oneway case (Baltagi (2013), p. 70)
                          kw = NULL)
    
    pval <- switch(type,
                     honda = pnorm(stat, lower.tail = FALSE), # honda oneway ~ N(0,1), alternative is one-sided (Baltagi (2013), p. 71/202)
                     bp    = pchisq(stat, df = parameter, lower.tail = FALSE), # is df=1 in the one-way case, alternative is two-sided (Baltagi (2013), p. 70/201)
                     kw    = pnorm(stat, lower.tail = FALSE)) # kw oneway ~ N(0,1), alternative is one-sided (Baltagi (2013), p. 71/202)
    # END oneway
  }
  else { # twoways
    stat <- switch(type,
                    honda = c(normal = (LM1+LM2)/sqrt(2)),
                    bp    = c(chisq = LM1^2+LM2^2),
                    kw    = c(normal = (sqrt(M11-N_obs)/sqrt(M11+M22-2*N_obs))*LM1+(sqrt(M22-N_obs)/sqrt(M11+M22-2*N_obs))*LM2),
                    ghm   = c(chibarsq = max(0,LM1)^2+max(0,LM2)^2))
    
    parameter <- switch(type,
                          honda = NULL,
                          bp    = c(df = 2), # df = 2 in the twoway case (Baltagi (2013), p. 70/201)
                          kw    = NULL,
                          ghm   = c(df0 = 0L, df1=1L, df2=2L, w0=1/4, w1=1/2, w2=1/4)) # chibarsquared (mixed chisq) has several dfs and weights (Baltagi (2013), p. 72/202)
    
    pval <- switch(type,
                     honda = pnorm(stat, lower.tail = FALSE), # honda two-ways ~ N(0,1), alternative is one-sided (Baltagi (2013), p. 71/202)
                     bp    = pchisq(stat, df = parameter, lower.tail = FALSE),  # is df = 2 in the twoway case, alternative is two-sided (Baltagi (2013), p. 70/201)
                     kw    = pnorm(stat, lower.tail = FALSE), # kw twoways ~ N(0,1), alternative is one-sided (Baltagi (2013), p. 71/202)
                     ghm   = (1/4)*pchisq(stat, df=0, lower.tail = F) + (1/2) * pchisq(stat, df=1, lower.tail = F) + (1/4) * pchisq(stat, df=2, lower.tail = FALSE)) # mixed chisq (also called chi-bar-square), see Baltagi (2013), pp. 71-72, 74, 88, 202-203, 209
  } # END twoways
  
  method.type <- switch(type,
                          honda  = "Honda",
                          bp     = "Breusch-Pagan",
                          ghm    = "Gourieroux, Holly and Monfort",
                          kw     = "King and Wu")
  
  method.effect <- switch(effect,
                            id      = "individual effects",
                            time    = "time effects",
                            twoways = "two-ways effects")
  
  balanced.type <- ifelse(balanced, "balanced", "unbalanced")
  
  method <- paste("Lagrange Multiplier Test - ", method.effect,
                  " (", method.type, ") for ", balanced.type, " panels", sep="")
  
  if (type %in% c("honda", "kw")) {
    RVAL <- list(statistic = stat,
                 p.value   = pval,
                 method    = method,
                 data.name = data.name(x))
  }
  else { # bp, ghm
    RVAL <- list(statistic = stat,
                 p.value   = pval,
                 method    = method,
                 parameter = parameter,
                 data.name = data.name(x))
  }
  
  
  RVAL$alternative <- "significant effects" # TODO: maybe distinguish be b/w one-sided and two-sided alternatives?
                                            #       (bp: two-sided alt.; all others: one-sided alt.?)
  
  class(RVAL) <- "htest"
  return(RVAL)
}


plmtest.formula <- function(x, data, ...,
                            effect = c("individual", "time", "twoways"),
                            type = c("honda", "bp", "ghm", "kw")) {
  
  cl <- match.call(expand.dots = TRUE)
  cl$model <- "pooling" # plmtest is performed on the pooling model...
  cl$effect <- NULL     # ... and pooling model has no argument effect...
  cl$type <- NULL       # ... and no argument type => see below: pass on args effect and type to plmtest.plm()
  names(cl)[2] <- "formula"
  m <- match(plm.arg, names(cl), 0)
  cl <- cl[c(1,m)]
  cl[[1]] <- as.name("plm")
  plm.model <- eval(cl, parent.frame())
  plmtest(plm.model, effect = effect, type = type) # pass on args effect and type to plmtest.plm()
}


############## pFtest() ############################################
pFtest <- function(x,...){
  UseMethod("pFtest")
}

pFtest.formula <- function(x, data, ...){
  cl <- match.call(expand.dots = TRUE)
  cl$model <- "within"
  names(cl)[2] <- "formula"
  m <- match(plm.arg,names(cl),0)
  cl <- cl[c(1,m)]
  cl[[1]] <- as.name("plm")
  plm.within <- eval(cl,parent.frame())
  plm.pooling <- update(plm.within, model = "pooling")
  pFtest(plm.within, plm.pooling, ...)
}
  
pFtest.plm <- function(x, z, ...){
  within <- x
  pooling <- z
  if (! (describe(x, "model") == "within" && describe(z, "model") == "pooling"))
   stop("the two arguments should be a 'within' and a 'pooling' model (in this order")
  
  effect <- describe(x, "effect")
  df1 <- df.residual(pooling)-df.residual(within)
  df2 <- df.residual(within)
  ssrp <- sum(residuals(pooling)^2)
  ssrw <- sum(residuals(within)^2)
  stat <- (ssrp-ssrw)/ssrw/df1*df2
  names(stat) <- "F"
  parameter <- c(df1,df2)
  names(parameter) <- c("df1","df2")
  pval <- pf(stat,df1,df2,lower.tail=FALSE)
  alternative <- "significant effects"
  res <- list(statistic = stat,
              p.value = pval,
              method = paste("F test for ",effect," effects",sep=""),
              parameter=parameter,
              data.name=data.name(x),
              alternative=alternative)
  class(res) <- "htest"
  res
}

############## Ftest() ############################################
Ftest <- function(x, test = c("Chisq", "F"), ...){
  model <- describe(x, "model")
  test <- match.arg(test)
  df1 <- ifelse(model == "within",
                length(coef(x)),
                length(coef(x)) - has.intercept(x))
  df2 <- df.residual(x)
  tss <- tss(x)
  ssr <- deviance(x)
  if (test == "Chisq"){
    stat <- (tss-ssr)/(ssr/df2)
    names(stat) <- "Chisq"
    pval <- pchisq(stat,df1,lower.tail=FALSE)
    parameter <- c(df = df1)
    method = "Wald test"
  }
  else{
    stat <- (tss-ssr)/ssr*df2/df1
    names(stat) <- "F"
    pval <- pf(stat,df1,df2,lower.tail=FALSE)
    parameter <- c(df1 = df1, df2 = df2)
    method = "F test"
  }
  res <- list(data.name = data.name(x),
              statistic = stat,
              parameter = parameter,
              p.value = pval,
              method = method
              )
  class(res) <- "htest"
  res
}

############## pooltest() ############################################

pooltest <- function(x,...){
  UseMethod("pooltest")
}

pooltest.formula <- function(x, data, ...){
  cl <- match.call(expand.dots = TRUE)
  cl[[1]] <- as.name("plm")
  names(cl)[[2]] <- "formula"
  if (is.null(cl$effect)) cl$effect <- "individual"
  plm.model <- eval(cl,parent.frame())

  cl[[1]] <- as.name("pvcm")
  names(cl)[[2]] <- "formula"
  if (is.null(cl$effect)) cl$effect <- "individual"
  cl$model <- "within"
  pvcm.model <- eval(cl,parent.frame())
  
  pooltest(plm.model,pvcm.model)
}

pooltest.plm <- function(x, z, ...){
  rss <- deviance(x)
  uss <- sum(unlist(residuals(z))^2)
  dlr <- df.residual(x)
  dlu <- df.residual(z)
  df1 <- dlr-dlu
  df2 <- dlu
  stat <- (rss-uss)/uss*df2/df1
  pval <- pf(stat,df1,df2,lower.tail=FALSE)
  parameter <- c(df1 = df1, df2 = df2)
  names(stat) <- "F"
  res <- list(statistic   = stat,
              parameter   = parameter,
              p.value     = pval,
              data.name   = data.name(x),
              alternative = "unstability",
              method      = "F statistic")
  class(res) <- "htest"
  res
}

############## pwaldtest() ############################################

pwaldtest <- function(x, ...){
  pdim <- attr(x,"pdim")
  df <- switch(x$model.name,
                "within"=pdim$Kw,
                "between"=pdim$Kb,
                "pooling"=pdim$K,
                "random"=pdim$K,
                "ht"=pdim$K
                )
  if (names(coefficients(x))[1]=="(intercept)"){
    coef <- coefficients(x)[-1]
    vcv <- vcov(x)[-1,-1]
  }
  else{
    coef <- coefficients(x)
    vcv <- vcov(x)
  }
  parameter <- length(coef)
  stat <- coef%*%solve(vcv)%*%coef
  names(stat) <- "chisq"
  names(parameter) <- "df"
  pval <- pchisq(stat,df,lower.tail=FALSE)
  res <- list(statistic = stat,
              p.value = pval,
              parameter = parameter,
              parameter.name = "df",
              method = "Wald Test",
              data.name = "data.name")
  class(res) <- "htest"
  res
}

pwaldtest <- function(x, ...){
  UseMethod("pwaldtest")
}

pwaldtest.formula <- function(x, ...){
  cl <- match.call(expand.dots = TRUE)
  if (names(cl)[3] == "") names(cl)[3] <- "data"
  if (is.null(cl$model)) cl$model <- "within"
  names(cl)[2] <- "formula"
  m <- match(plm.arg,names(cl),0)
  cl <- cl[c(1,m)]
  cl[[1]] <- as.name("plm")
  plm.model <- eval(cl,parent.frame())
  pwaldtest(plm.model)
}


pwaldtest.panelmodel <- function(x, ...){
  if (has.intercept(x)){
    mycoef <- coef(x)[-1]
    myvcov <- vcov(x)[-1,-1]
  }
  else{
    mycoef <- coef(x)
    myvcov <- vcov(x)
  }
  df <- c(df = length(mycoef))
  stat <- crossprod(solve(myvcov,mycoef),mycoef)
  pval <- pchisq(stat,df,lower.tail = FALSE)
  names(stat) <- "Chisq"
  result <- list(statistic = stat,
                 parameter = df,
                 p.value   = pval,
                 data.name = data.name(x))
  class(result) <- "htest"
  result
                 
}


  
