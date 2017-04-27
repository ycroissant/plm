

x1 <- c(
-1.504,-1.514,-1.522,-1.520,-1.526,-1.523,-1.527,-1.519,-1.524,-1.532,
-1.488,-1.503,-1.516,-1.514,-1.519,-1.520,-1.524,-1.519,-1.522,-1.530,
-1.319,-1.387,-1.428,-1.443,-1.460,-1.476,-1.493,-1.490,-1.498,-1.514,
-1.306,-1.366,-1.413,-1.433,-1.453,-1.471,-1.489,-1.486,-1.495,-1.512,
-1.171,-1.260,-1.329,-1.363,-1.394,-1.428,-1.454,-1.458,-1.470,-1.495,
    NA,    NA,-1.313,-1.351,-1.384,-1.421,-1.451,-1.454,-1.467,-1.494,
    NA,    NA,    NA,-1.289,-1.331,-1.380,-1.418,-1.427,-1.444,-1.476,
    NA,    NA,    NA,-1.273,-1.319,-1.371,-1.411,-1.423,-1.441,-1.474,
    NA,    NA,    NA,-1.212,-1.266,-1.329,-1.377,-1.393,-1.415,-1.456
)

x2 <- c(
1.069,0.923,0.851,0.809,0.789,0.770,0.760,0.749,0.736,0.735,
1.255,1.011,0.915,0.861,0.831,0.803,0.781,0.770,0.753,0.745,
1.421,1.078,0.969,0.905,0.865,0.830,0.798,0.789,0.766,0.754,
1.759,1.181,1.037,0.952,0.907,0.858,0.819,0.802,0.782,0.761,
2.080,1.279,1.097,1.005,0.946,0.886,0.842,0.819,0.801,0.771,
   NA,   NA,1.171,1.055,0.980,0.912,0.863,0.839,0.814,0.781,
   NA,   NA,   NA,1.114,1.023,0.942,0.886,0.858,0.834,0.795,
   NA,   NA,   NA,1.164,1.062,0.968,0.910,0.875,0.851,0.806,
   NA,   NA,   NA,1.217,1.105,0.996,0.929,0.896,0.871,0.818
)

x3 <- c(
-2.166,-2.167,-2.168,-2.167,-2.172,-2.173,-2.176,-2.174,-2.174,-2.177,
-2.173,-2.169,-2.172,-2.172,-2.173,-2.177,-2.180,-2.178,-2.176,-2.179,
-1.914,-1.999,-2.047,-2.074,-2.095,-2.120,-2.137,-2.143,-2.146,-2.158,
-1.922,-1.977,-2.032,-2.065,-2.091,-2.117,-2.137,-2.142,-2.146,-2.158,
-1.750,-1.823,-1.911,-1.968,-2.009,-2.057,-2.091,-2.103,-2.114,-2.135,
    NA,    NA,-1.888,-1.955,-1.998,-2.051,-2.087,-2.101,-2.111,-2.135,
    NA,    NA,    NA,-1.868,-1.923,-1.995,-2.042,-2.065,-2.081,-2.113,
    NA,    NA,    NA,-1.851,-1.912,-1.986,-2.036,-2.063,-2.079,-2.112,
    NA,    NA,    NA,-1.761,-1.835,-1.925,-1.987,-2.024,-2.046,-2.088
)

x4 <- c(
1.132,0.869,0.763,0.713,0.690,0.655,0.633,0.621,0.610,0.597,
1.453,0.975,0.845,0.769,0.734,0.687,0.654,0.641,0.627,0.605,
1.627,1.036,0.882,0.796,0.756,0.702,0.661,0.653,0.634,0.613,
2.482,1.214,0.983,0.861,0.808,0.735,0.688,0.674,0.650,0.625,
3.947,1.332,1.052,0.913,0.845,0.759,0.705,0.685,0.662,0.629,
   NA,   NA,1.165,0.991,0.899,0.792,0.730,0.705,0.673,0.638,
   NA,   NA,   NA,1.055,0.945,0.828,0.753,0.725,0.689,0.650,
   NA,   NA,   NA,1.145,1.009,0.872,0.786,0.747,0.713,0.661,
   NA,   NA,   NA,1.208,1.063,0.902,0.808,0.766,0.728,0.670
)


adj.ips <- c(x1, x2, x3, x4)

adj.ips <- array(adj.ips, dim=c(10,9,2,2),
           dimnames = list(
             c(10,15,20,25,30,40,50,60,70,100),
             0:8,
             c('mean', 'var'),
             c('intercept', 'trend'))
           )
                  
adj.ips <- aperm(adj.ips, c(2,1,3,4))

################

Tn <- c(  25,  30,  35,  40,  45,  50,  60,   70,   80,   90,  100,  250,   500)

v <- c(c( .004, .003, .002, .002, .001, .001, .001,0.000,0.000,0.000,0.000,0.000,0.000),
       c(1.049,1.035,1.027,1.021,1.017,1.014,1.011,1.008,1.007,1.006,1.005,1.001,1.000),
       c(-.554,-.546,-.541,-.537,-.533,-.531,-.527,-.524,-.521,-.520,-.518,-.509,-.500),
       c(0.919,0.889,0.867,0.850,0.837,0.826,0.810,0.798,0.789,0.782,0.776,0.742,0.707),
       c(-.703,-.674,-.653,-.637,-.624,-.614,-.598,-.587,-.578,-.571,-.566,-.533,-.500),
       c(1.003,0.949,0.906,0.871,0.842,0.818,0.780,0.751,0.728,0.710,0.695,0.603,0.500)
       )

adj.levinlin <- array(v, dim=c(13,2,3),
                      dimnames = list(Tn, c("mu","sigma"),
                        c("none", "intercept", "trend")))

names.exo <- c(none = 'None',
               intercept = 'Individual Intercepts',
               trend = 'Individual Intercepts and Trend')

names.test <- c(levinlin = 'Levin-Lin-Chu Unit-Root Test',
                ips = 'Im-Pesaran-Shin Unit-Root Test',
                madwu = 'Maddala-Wu Unit-Root Test',
                hadri = 'Hadri Test')

my.lm.fit <- function(X, y, dfcor = TRUE, ...){
  object <- lm.fit(X, y)
  ## 'as' summary method for lm.fit
  p <- object$rank ; Qr <- object$qr ; n <- NROW(Qr$qr)
  rdf <- n - p ; p1 <- 1L:p ; r <- object$residuals
  rss <- sum(r^2) ;
  resvar <- ifelse(dfcor, rss/rdf, rss/n)
  sigma <- sqrt(resvar)
  R <- chol2inv(Qr$qr[p1, p1, drop = FALSE])
  thecoef <- object$coefficients[Qr$pivot[p1]] #[lags+1]
  these <- sigma*sqrt(diag(R)) #[lags+1])
  list(coef = thecoef, se = these, sigma = sigma,
       rss = rss, n = n, K = p, rdf = rdf)
}


## General functions to transform series :

YClags <- function(object,  k = 3){
  if (k > 0)
    sapply(1:k, function(x) c(rep(NA,x),object[1:(length(object)-x)]))
  else
    NULL
}

YCtrend <- function(object) 1:length(object)

YCdiff <- function(object){
  c(NA, object[2:length(object)] - object[1:(length(object)-1)])
}

selectT <- function(x, Ts){
  ## This function selects the length of the series as it is tabulated
  if (x %in% Ts) return(x)
  if (x < Ts[1]){
    warning("the time series is short")
    return(Ts[1])
  }
  if (x > Ts[length(Ts)]){
    warning("the time series is long")
    return(Ts[length(Ts)])
  }
  pos <- which((Ts - x)>0)[1]
  return(Ts[c(pos-1,pos)])
}
    
lagsel <- function(object, exo = c("intercept", "none", "trend"),
                   method = c("Hall", "AIC", "SIC"), pmax = 10, dfcor=
                   FALSE, fixedT = TRUE, ...){
  # select the optimal number of lags using Hall method or AIC or SIC
  # method
  method <- match.arg(method)
  y <- object
  Dy <- YCdiff(object)
  Ly <- c(NA, object[1:(length(object)-1)])
  if (exo == "none") m <- NULL
  if (exo == "intercept") m <- rep(1, length(object))
  if (exo == "trend") m <- cbind(1, YCtrend(object))
  LDy <- YClags(Dy, pmax)
  decreasei <- TRUE
  i <- 0
  narow <- 1:(pmax+1)
  if (method == "Hall"){
    while(decreasei){
      lags <- pmax - i
      if (!fixedT) narow <- 1:(lags+1)
      X <- cbind(Ly, LDy[ , 0:lags], m)[-narow,,drop=FALSE]
      y <- Dy[-narow]
      sres <- my.lm.fit(X, y, dfcor = dfcor)
      tml <- sres$coef[lags+1]/sres$se[lags+1]
      if (abs(tml) < 1.96 && lags > 0)
        i <- i + 1
      else
        decreasei <- FALSE
    }
  }
  else{
    l <- c()
    while(i <= pmax){
      lags <- pmax - i
      if (!fixedT) narow <- 1:(lags+1)
      X <- cbind(Ly, LDy[ , 0:lags], m)[-narow,,drop=FALSE]
      y <- Dy[-narow]
      sres <- my.lm.fit(X, y, dfcor = dfcor)
      AIC <- ifelse(method == "AIC",
                    log(sres$rss/sres$n)+2*sres$K/sres$n,
                    log(sres$rss/sres$n)+sres$K*log(sres$n)/sres$n)
      l <- c(l, AIC)
      i <- i + 1
    }
    lags <- pmax + 1 - which.min(l)
  }
  lags
}        


adj.levinlin.value <- function(l, exo = c("intercept", "none", "trend")){
  ## extract the adjustment values for Levin-Lin-Chu test
  theTs <- as.numeric(dimnames(adj.levinlin)[[1]])
  Ts <- selectT(l, theTs)
  if (length(Ts) == 1){
    return(adj.levinlin[as.character(Ts), , exo])
  }
  else{
    low <- adj.levinlin[as.character(Ts[1]), , exo]
    high <- adj.levinlin[as.character(Ts[2]), , exo]
    return(low + (l - Ts[1])/(Ts[2] - Ts[1])*(high - low))
  }
}

adj.ips.value <- function(l = 30, lags = 2,
                          exo = c("intercept", "trend")){
  ## extract the adjustment values for Im-Pesaran-Shin test
  if (!lags %in% 0:8) warning("lags should be an integer between 0 and 8")
  lags <- min(lags, 8)
  theTs <- as.numeric(dimnames(adj.ips)[[2]])
  Ts <- selectT(l, theTs)
  if (length(Ts) == 1){
    return(adj.ips[as.character(lags),as.character(Ts),,exo])
  }
  else{
    low <- adj.ips[as.character(lags),as.character(Ts[1]),,exo]
    high <- adj.ips[as.character(lags),as.character(Ts[2]),,exo]
    return(low + (l - Ts[1])/(Ts[2] - Ts[1])*(high - low))
  }
}


tsadf <- function(object, exo = c("intercept", "none", "trend"),
                  lags = NULL, dfcor = FALSE, comp.aux.reg = FALSE, ...){
  # compute some adf regression for each time serie
  y <- object
  L <- length(y)
  Dy <- YCdiff(object)
  Ly <- c(NA, object[1:(length(object)-1)])
  if (exo == "none") m <- NULL
  if (exo == "intercept") m <- rep(1, length(object))
  if (exo == "trend") m <- cbind(1, YCtrend(object))
  narow <- 1:(lags+1)
  LDy <- YClags(Dy, lags)
  X <- cbind(Ly, LDy, m)[-narow, , drop = FALSE]
  y <- Dy[- narow]
  result <- my.lm.fit(X, y, dfcor = dfcor)
  sigma <- result$sigma
  rho <- result$coef[1]
  sdrho <- result$se[1]
  trho <- rho/sdrho
  result <- list(rho = rho,
                 sdrho = sdrho,
                 trho = trho,
                 sigma = sigma,
                 T = L,
                 lags = lags)
  if (comp.aux.reg){
    # for Levin-Lin-Chu only, computes the residuals of the auxiliary
    # regressions
    X <- cbind(LDy[ , 0:lags], m)[-narow, , drop = FALSE]
    if (lags == 0 && exo == "none"){
      resid.diff <- Dy[-narow]/sigma
      resid.level <- Ly[-narow]/sigma
    }
    else{
      y <- Dy[-narow]
      resid.diff <- lm.fit(X, y)$residuals/sigma
      y <- Ly[-narow]
      resid.level <- lm.fit(X, y)$residuals/sigma
    }
    result$resid <- data.frame(resid.diff = resid.diff, resid.level = resid.level)
  }
  result
}        
  

longrunvar <- function(x, exo = c("intercept", "none", "trend"), q = NULL){
  # compute the long run variance of the dependent variable
  T <- length(x)
  if (is.null(q)) q <- round(3.21*T^(1/3))
  dx <- x[2:T]-x[1:(T-1)]
  if (exo == "intercept") dx <- dx - mean(dx)
  if (exo == "trend") dx <- lm.fit(cbind(1,1:length(dx)), dx)$residuals
  dx <- c(NA, dx)
  1/(T-1)*sum(dx[-1]^2)+
    2*sum(
          sapply(1:q,
                 function(L){
                   sum(dx[2:(T-L)]*dx[(L+2):T]) / (T-1) *
                     (1 - L / (q+1))
                 }
                 )
          )
}

purtest <- function(object, data = NULL, index = NULL,
                    test = c("levinlin", "ips", "madwu", "hadri"),
                    exo = c("none", "intercept", "trend"),
                    lags = c("SIC", "AIC", "Hall"), pmax = 10,
                    Hcons = TRUE,
                    q = NULL, dfcor = FALSE, fixedT = TRUE, ...){

  data.name <- paste(deparse(substitute(object)))

  id <- NULL
  if (inherits(object, 'formula')){
    terms <- terms(object)
    lab <- labels(terms)
    if (length(lab) == 0){
      if (attr(terms, "intercept")) exo <- "intercept"
      else exo <- "none"
    }
    else{
      if (length(lab) > 1 || lab != "trend") stop("incorrect formula")
      exo <- "trend"
    }
    object <- paste(deparse(object[[2]]))
    if (exists(object) && is.vector(get(object))){
      # is.vector because, eg, inv exists as a function
      object <- get(object)
    }
    else{
      if (is.null(data)) stop("unknown response")
      else{
        if (object %in% names(data)){
          object <- data[[object]]
          if (!inherits(data, "pdata.frame")){
            if (is.null(index)) stop("the index attribute is required")
            else data <- pdata.frame(data, index)
          }
          id <- attr(data, "index")[[1]]
        }
        else{
          stop("unknown response")
        }
      }
    }
  }
  else{
    exo <- match.arg(exo)
    if (is.null(dim(object))){
      if (inherits(object, 'pseries')){
        id <- attr(object, "index")[[1]]
      }
      else stop("the individual dimension is undefined")
    }
    if (is.matrix(object)) object <- as.data.frame(object)
  }
  if (!inherits(object, "data.frame")){
    if (is.null(id)) stop("the individual dimension is undefined")
    object <- as.data.frame(split(object, id))
  }

  cl <- match.call()
  test <- match.arg(test)
  if (is.character(lags)) lags <- match.arg(lags) # if character, select one possible value
  args <- list(test = test, exo = exo, pmax = pmax, lags = lags,
               dfcor = FALSE, fixedT = fixedT)
  L <- nrow(object)
  n <- ncol(object)
  parameter <- NULL
  alternative <- 'stationarity'
  method <- paste0(names.test[test], " (ex. var.: ",
                    names.exo[exo],")")

  if (test == "hadri"){
    if (exo == "intercept"){
      resid <- lapply(object, function(x) lm(x~1)$residuals)
      adj <- c(1/6, 1/45)
    }
    if (exo == "trend"){
      resid <- lapply(object,
                      function(x){
                        trend <- 1:length(x)
                        lm(x~trend)$residuals
                      }
                      )
      adj <- c(1/15, 11/6300)
    }      
    sigma2 <- mean(unlist(resid)^2)
    cumres2 <- lapply(resid, function(x) cumsum(x)^2)
    if (!Hcons){
      S <- sum(unlist(cumres2))/(L^2)
      LM <- S / sigma2
    }
    else{
      sigma2i <- lapply(resid, function(x) mean(x^2))
      Sit2 <- mapply("/", cumres2, sigma2i)
      LM <- sum(unlist(Sit2))/ (L^2 * n)
    }
    stat <- c(z = sqrt(n) * (LM - adj[1])  / sqrt(adj[2]))
    pvalue <- 2 * (pnorm(abs(stat), lower.tail = FALSE))
    htest <- structure(list(statistic = stat,
                            parameter = NULL,
                            alternative = "at least one series has a unit root",
                            data.name = data.name,
                            method = method,
                            p.value = pvalue),
                       class = "htest")
  
    result <- list(statistic = htest,
                   call = cl,
                   args = args)

    class(result) <- "purtest"
    return(result)
  }  
  
  # compute the lags for each time series if necessary
  if (is.numeric(lags)){
    if (length(lags) == 1) lags <- rep(lags, n)
    else{
      if (length(lags) != n) stop("lags should be of length 1 or n")
      else lags <- as.list(lags)
    }
  }
  else{
    lags <- match.arg(lags)
    lags <- sapply(object, function(x)
                   lagsel(x, exo = exo, method = lags,
                          pmax = pmax, dfcor = dfcor, fixedT = fixedT))
  }
  
  # compute the augmented Dickey-Fuller regressions for each time
  # series
  comp.aux.reg <- (test == "levinlin")
  idres <- mapply(function(x, y)
                  tsadf(x, exo = exo, lags = y, dfcor = dfcor,
                        comp.aux.reg = comp.aux.reg),
                  object, as.list(lags), SIMPLIFY = FALSE)
  
  if (test == "levinlin"){
    # get the adjustment parameters for the mean and the variance
    adjval <- adj.levinlin.value(L, exo = exo)
    mymu <- adjval[1]
    mysig <- adjval[2]
    # calculate the ration of LT/ST variance
    sigmaST <- sapply(idres, function(x) x[["sigma"]])
    sigmaLT <- sqrt(sapply(object, longrunvar, exo = exo, q = q))
    si <- sigmaLT/sigmaST
    sbar <- mean(si)
    # stack the residuals of each time series and perform the pooled
    # regression
    res.level <- unlist(lapply(idres, function(x) x$resid[["resid.level"]]))
    res.diff <- unlist(lapply(idres, function(x) x$resid[["resid.diff"]]))
    z <- my.lm.fit(as.matrix(res.level), res.diff, dfcor = dfcor)
    # compute the levin-lin-chu statistic
    tildeT <- L-mean(lags)-1
    sigmaeps2 <- z$rss/(n*tildeT)
    rho <- z$coef
    sdrho <- z$se
    trho <- rho/sdrho
    stat <- c(z = (trho - n * tildeT * sbar / sigmaeps2 * sdrho * mymu)/mysig)
    pvalue <- 2*pnorm(abs(stat), lower.tail = FALSE)
  }

  if (test == "ips"){
    if (exo == "none") stop("ips test is not implemented for exo = \"none\"")
    lags <- sapply(idres, function(x) x[["lags"]])
    L <- sapply(idres, function(x) x[["T"]]) - lags - 1
    adjval <- mapply(function(x, y) adj.ips.value(x, y, exo = exo),
                     as.list(L), as.list(lags))
    # get the adjustment parameters for the mean and the variance
    trho <- sapply(idres, function(x) x[["trho"]])
    tbar <- mean(trho)
    Etbar <- mean(adjval[1,])
    Vtbar <- mean(adjval[2,])
    stat <- c(z = sqrt(n)*(tbar-Etbar)/sqrt(Vtbar))
    pvalue <- 2*pnorm(abs(stat), lower.tail = FALSE)
  }

  if (test == "madwu"){
    trho <- sapply(idres, function(x) x[["trho"]])
    pvalue <- 2*pnorm(abs(trho), lower.tail = FALSE)
    stat <- c(chisq = - 2*sum(log(pvalue)))
    n <- length(trho)
    pvalue <- pchisq(stat, df = 2*n, lower.tail = FALSE)
    adjval <- NULL
    parameter <- c(df = 2 * n)
  }
  
  htest <- structure(list(statistic = stat,
                          parameter = parameter,
                          alternative = alternative,
                          data.name = data.name,
                          method = method,
                          p.value = pvalue),
                     class = "htest")
  
  result <- list(statistic = htest,
                 call = cl,
                 args = args,
                 idres = idres,
                 adjval = adjval)
  class(result) <- "purtest"
  result
}


print.purtest <- function(x, ...){
  print(x$statistic, ...)
}

summary.purtest <- function(object, ...){
  lags <- sapply(object$idres, function(x) x[["lags"]])
  L <- sapply(object$idres, function(x) x[["T"]])
  nam <- names(object$idres)
  rho <- sapply(object$idres, function(x) x[["rho"]])
  sdrho <- sapply(object$idres, function(x) x[["sdrho"]])
  trho <- rho / sdrho
  sumidres <- cbind(lags = lags, obs = L - lags - 1, rho = rho, trho = trho)
  if (object$args$test == "ips"){
    sumidres <- cbind(sumidres, t(object$adjval))
  }
  rownames(sumidres) <- nam
  object$sumidres <- sumidres
  class(object) <- c("summary.purtest", "purtest")
  object
}

print.summary.purtest <- function(x, ...){
  cat(paste(names.test[x$args$test], "\n"))
  cat(paste('Exogenous variables:', names.exo[x$args$exo], '\n'))
  thelags <- sapply(x$idres, function(x) x[["lags"]])
  if (is.character(x$args$lags)){
    cat(paste0('Automatic selection of lags using ', x$args$lags, ': ',
              min(thelags), ' - ', max(thelags), ' lags (max: ', x$args$pmax, ')\n'))
  }
  else{
    cat('User-provided lags\n')
  }
  cat(paste('statistic:', round(x$statistic$statistic, 3), '\n'))
  cat(paste('p-value:', round(x$statistic$p.value, 3), '\n'))
  print(x$sumidres, ...)
}

