data.name <- function(x){
  data.name <- paste(deparse(x$call$formula))
  if (length(data.name) > 1) paste(data.name[1], "...")
  else data.name
}
  
phtest <- function(x,...){
  UseMethod("phtest")
}

phtest.formula <- function(x, data, model = c("within","random"),
                            method = c("chisq", "aux"),
                            index=NULL, vcov=NULL, ...){
    if(length(model)!=2) stop("two models should be indicated")
    for (i in 1:2){
        model.name <- model[i]
        if(!(model.name %in% names(model.plm.list))){
            stop("model must be one of ",oneof(model.plm.list))
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
               data <- plm.data(data, indexes=index) #, ...)
               rey <- pmodel.response(plm(formula=x, data=data,
                                          model=model[2]))
               reX <- model.matrix(plm(formula=x, data=data,
                                       model=model[2]))
               feX <- model.matrix(plm(formula=x, data=data,
                                       model=model[1]))
               dimnames(feX)[[2]] <- paste(dimnames(feX)[[2]],
                                           "tilde", sep=".")
               ## fetch indices here, check pdata
               data <- data.frame(cbind(data[, 1:2], rey, reX, feX))[,-4]
               auxfm <- as.formula(paste("rey~",
                                         paste(dimnames(reX)[[2]][-1],
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
               names(h2t) <- "Chisq"

               if(!is.null(vcov)) {
                   vcov=paste(", covariance: ",
                       paste(deparse(substitute(vcov))),
                       sep="")
               }

               haus2 <- list(statistic=h2t,
                             p.value=ph2t,
                             parameter=nvars,
                             method=paste("Regression-based Hausman test",
                                 vcov, sep=""),
                             alternative="one model is inconsistent",
                             data.name=paste(deparse(substitute(fm))))
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
  coef.h <- names.re[names.re%in%names.wi]
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


plmtest <- function(x,...){
  UseMethod("plmtest")
}

plmtest.plm <- function(x,
                        effect = c("individual", "time", "twoways"),
                        type = c("honda", "bp", "ghm","kw"),
                        ...){

  effect <- match.arg(effect)
  type <- match.arg(type)
  if (describe(x, "model") != "pooling") x <- update(x, model = "pooling")
  pdim <- pdim(x)
  n <- pdim$nT$n
  T <- pdim$nT$T
  balanced <- pdim$balanced
  index <- attr(model.frame(x), "index")
  id <- index[[1]]
  time <- index[[2]]
  res <- resid(x)
  
  if (effect != "twoways"){
    if (!type %in% c("honda", "bp"))
      stop("type must be one of honda or bp for a one way model")
    if(effect == "individual"){ condvar <- id ; card.cond <- n ; card.other <- T}
    else{condvar <- time ; card.cond <- T ; card.other <- n}
    stat <-  sqrt(card.other*card.cond/(2*(card.other-1)))*
      (crossprod(tapply(res,condvar,mean))*card.other^2/sum(res^2)-1)
    stat <- switch(type,
                   honda = c(normal = stat),
                   bp    = c(chisq  = stat^2))
    parameter <- switch(type,
                        honda = NULL,
                        bp = 1)
    pval <- switch(type,
                   honda = pnorm(abs(stat), lower.tail = FALSE)*2,
                   bp    = pchisq(stat, df = 1, lower.tail = FALSE))
  }
  else{
    stat1 <-  sqrt(n*T/(2*(T-1)))*(crossprod(tapply(res,id,mean))*T^2/sum(res^2)-1)
    stat2 <-  sqrt(n*T/(2*(n-1)))*(crossprod(tapply(res,time,mean))*n^2/sum(res^2)-1)
    stat <- switch(type,
                   ghm   = c(chisq = max(0,stat1)^2+max(0,stat2)^2),
                   bp    = c(chisq = stat1^2+stat2^2),
                   honda = c(normal = (stat1+stat2)/sqrt(2)),
                   kw    = c(normal = sqrt((T-1)/(n+T-2))*stat1+sqrt((n-1)/(n+T-2))*stat2))
    parameter <- 2
    pval <- switch(type,
                   ghm   = pchisq(stat,df=2,lower.tail=FALSE),
                   honda = pnorm(abs(stat),lower.tail=FALSE)*2,
                   bp    = pchisq(stat,df=2,lower.tail=FALSE),
                   kw    = pnorm(abs(stat),lower.tail=FALSE)*2)
  }
  
  method.type <- switch(type,
                        honda  = "Honda",
                        bp     = "Breusch-Pagan",
                        ghm    = "Gourieroux, Holly and Monfort",
                        kw     = "King and Wu")
  method.effect <- switch(effect,
                          id      = "individual effects",
                          time    = "time effects",
                          twoways = "two-ways effects")
  method <- paste("Lagrange Multiplier Test - ",method.effect,
                  " (",method.type,")\n",sep="")

  if(type == "honda"){
    res <- list(statistic = stat,
                p.value   = pval,
                method    = method,
                data.name = data.name(x))
  }
  else{
    names(parameter) <- "df"
    res <- list(statistic = stat,
                p.value   = pval,
                method    = method,
                parameter = parameter,
                data.name = data.name(x))
  }
  res$alternative <- "significant effects"
  class(res) <- "htest"
  res
}


plmtest.formula <- function(x, data, ...,
                            effect = c("individual", "time", "twoways"),
                            type = c("honda", "bp", "ghm", "kw")){
  
  cl <- match.call(expand.dots = TRUE)
  cl$model <- "pooling"
  names(cl)[2] <- "formula"
  m <- match(plm.arg,names(cl),0)
  cl <- cl[c(1,m)]
  cl[[1]] <- as.name("plm")
  plm.model <- eval(cl,parent.frame())
  plmtest(plm.model, effect = effect, type = type)
}

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
#  if (! (describe(x, "model") == "within" && describe(z, "model") == "pooling"))
#    stop("the two arguments should be a within and a pooling model")
  
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

has.intercept.plm <- function(object, part = "first", ...){
  has.intercept(formula(object), part = part)
}
  
