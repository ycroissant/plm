padf <- function(x, exo = c("none", "intercept", "trend")){
 # p-value approx. by MacKinnon (1994), used in some panel unit root tests
  
  # values from MacKinnon (1994), table 3, 4
      small <- matrix(c(0.6344, 1.2378, 3.2496,
                        2.1659, 1.4412, 3.8269,
                        3.2512, 1.6047, 4.9588),
                      nrow = 3, byrow = TRUE)
      small <- t(t(small) / c(1, 1, 100))
      large <- matrix(c(0.4797, 9.3557, -0.6999,  3.3066,
                        1.7339, 9.3202, -1.2745, -1.0368,
                        2.5261, 6.1654, -3.7956, -6.0285),
                      nrow = 3, byrow = TRUE)
      large <- t(t(large) / c(1, 10, 10, 100))
      limit <- c(-1.04, -1.61, -2.89)
      rownames(small) <- rownames(large) <- names(limit) <- c("none", "intercept", "trend")
  
  exo <- match.arg(exo)
  psmall <- apply(small[exo, ] * rbind(1, x, x ^ 2), 2, sum)
  plarge <- apply(large[exo, ] * rbind(1, x, x ^ 2, x ^ 3), 2, sum)
  as.numeric(pnorm(psmall * (x <= limit[exo]) + plarge * (x > limit[exo])))
}


## IPS (2003), table 3 for Wtbar statistic
# x1: means without time trend from table 3 in IPS (2003)
adj.ips.wtbar.x1 <- c(
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
# x2: variances without time trend from table 3 in IPS (2003)
adj.ips.wtbar.x2 <- c(
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

# x3: means with time trend from table 3 in IPS (2003)
adj.ips.wtbar.x3 <- c(
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

# x4: variances with time trend from table 3 in IPS (2003)
adj.ips.wtbar.x4 <- c(
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

adj.ips.wtbar <- c(adj.ips.wtbar.x1, adj.ips.wtbar.x2,
                   adj.ips.wtbar.x3, adj.ips.wtbar.x4)

adj.ips.wtbar <- array(adj.ips.wtbar, dim = c(10, 9, 2, 2),
                 dimnames = list(
                   c(10, 15, 20, 25, 30, 40, 50, 60, 70, 100),
                   0:8,
                   c("mean", "var"),
                   c("intercept", "trend"))
)

adj.ips.wtbar <- aperm(adj.ips.wtbar, c(2, 1, 3, 4))



###############
## IPS (2003), table 2 (obvious typos (missing minus signs corrected))

# intercept 1% critical values
critval.ips.tbar.int1 <- c(
-3.79, -2.66, -2.54, -2.50, -2.46, -2.44, -2.43, -2.42, -2.42, -2.40, -2.40,
-3.45, -2.47, -2.38, -2.33, -2.32, -2.31, -2.29, -2.28, -2.28, -2.28, -2.27,
-3.06, -2.32, -2.24, -2.21, -2.19, -2.18, -2.16, -2.16, -2.16, -2.16, -2.15,
-2.79, -2.14, -2.10, -2.08, -2.07, -2.05, -2.04, -2.05, -2.04, -2.04, -2.04,
-2.61, -2.06, -2.02, -2.00, -1.99, -1.99, -1.98, -1.98, -1.98, -1.97, -1.97,
-2.51, -2.01, -1.97, -1.95, -1.94, -1.94, -1.93, -1.93, -1.93, -1.93, -1.92,
-2.20, -1.85, -1.83, -1.82, -1.82, -1.82, -1.81, -1.81, -1.81, -1.81, -1.81,
-2.00, -1.75, -1.74, -1.73, -1.73, -1.73, -1.73, -1.73, -1.73, -1.73, -1.73)
# intercept 5% critical values
critval.ips.tbar.int5 <- c(
-2.76, -2.28, -2.21, -2.19, -2.18, -2.16, -2.16, -2.15, -2.16, -2.15,-2.15,
-2.57, -2.17, -2.11, -2.09, -2.08, -2.07, -2.07, -2.06, -2.06, -2.06,-2.05,
-2.42, -2.06, -2.02, -1.99, -1.99, -1.99, -1.98, -1.98, -1.97, -1.98,-1.97,
-2.28, -1.95, -1.92, -1.91, -1.90, -1.90, -1.90, -1.89, -1.89, -1.89,-1.89,
-2.18, -1.89, -1.87, -1.86, -1.85, -1.85, -1.85, -1.85, -1.84, -1.84,-1.84,
-2.11, -1.85, -1.83, -1.82, -1.82, -1.82, -1.81, -1.81, -1.81, -1.81,-1.81,
-1.95, -1.75, -1.74, -1.73, -1.73, -1.73, -1.73, -1.73, -1.73, -1.73,-1.73,
-1.84, -1.68, -1.67, -1.67, -1.67, -1.67, -1.67, -1.67, -1.67, -1.67,-1.67)
# intercept 10% critical values	
critval.ips.tbar.int10 <- c(
-2.38, -2.10, -2.06, -2.04, -2.04, -2.02, -2.02, -2.02, -2.02, -2.02, -2.01,
-2.27, -2.01, -1.98, -1.96, -1.95, -1.95, -1.95, -1.95, -1.94, -1.95, -1.94,
-2.17, -1.93, -1.90, -1.89, -1.88, -1.88, -1.88, -1.88, -1.88, -1.88, -1.88,
-2.06, -1.85, -1.83, -1.82, -1.82, -1.82, -1.81, -1.81, -1.81, -1.81, -1.81,
-2.00, -1.80, -1.79, -1.78, -1.78, -1.78, -1.78, -1.78, -1.78, -1.77, -1.77,
-1.96, -1.77, -1.76, -1.75, -1.75, -1.75, -1.75, -1.75, -1.75, -1.75, -1.75,
-1.85, -1.70, -1.69, -1.69, -1.69, -1.69, -1.68, -1.68, -1.68, -1.68, -1.69,
-1.77, -1.64, -1.64, -1.64, -1.64, -1.64, -1.64, -1.64, -1.64, -1.64, -1.64)
# trend 1% critical values
critval.ips.tbar.trend1 <- c(
-8.12, -3.42, -3.21, -3.13, -3.09, -3.05, -3.03, -3.02, -3.00, -3.00, -2.99,
-7.36, -3.20, -3.03, -2.97, -2.94, -2.93, -2.90, -2.88, -2.88, -2.87, -2.86,
-6.44, -3.03, -2.88, -2.84, -2.82, -2.79, -2.78, -2.77, -2.76, -2.75, -2.75,
-5.72, -2.86, -2.74, -2.71, -2.69, -2.68, -2.67, -2.65, -2.66, -2.65, -2.64,
-5.54, -2.75, -2.67, -2.63, -2.62, -2.61, -2.59, -2.60, -2.59, -2.58, -2.58,
-5.16, -2.69, -2.61, -2.58, -2.58, -2.56, -2.55, -2.55, -2.55, -2.54, -2.54,
-4.50, -2.53, -2.48, -2.46, -2.45, -2.45, -2.44, -2.44, -2.44, -2.44, -2.43,
-4.00, -2.42, -2.39, -2.38, -2.37, -2.37, -2.36, -2.36, -2.36, -2.36, -2.36)
# trend 5% critical values
critval.ips.tbar.trend5 <- c(
-4.66, -2.98, -2.87, -2.82, -2.80, -2.79, -2.77, -2.76, -2.75, -2.75, -2.75,
-4.38, -2.85, -2.76, -2.72, -2.70, -2.69, -2.68, -2.67, -2.67, -2.66, -2.66,
-4.11, -2.74, -2.66, -2.63, -2.62, -2.60, -2.60, -2.59, -2.59, -2.58, -2.58,
-3.88, -2.63, -2.57, -2.55, -2.53, -2.53, -2.52, -2.52, -2.52, -2.51, -2.51,
-3.73, -2.56, -2.52, -2.49, -2.48, -2.48, -2.48, -2.47, -2.47, -2.46, -2.46,
-3.62, -2.52, -2.48, -2.46, -2.45, -2.45, -2.44, -2.44, -2.44, -2.44, -2.43,
-3.35, -2.42, -2.38, -2.38, -2.37, -2.37, -2.36, -2.36, -2.36, -2.36, -2.36,
-3.13, -2.34, -2.32, -2.32, -2.31, -2.31, -2.31, -2.31, -2.31, -2.31, -2.31)
# trend 10% critical values
critval.ips.tbar.trend10 <- c(
-3.73, -2.77, -2.70, -2.67, -2.65, -2.64, -2.63, -2.62, -2.63, -2.62, -2.62,
-3.60, -2.68, -2.62, -2.59, -2.58, -2.57, -2.57, -2.56, -2.56, -2.55, -2.55,
-3.45, -2.59, -2.54, -2.52, -2.51, -2.51, -2.50, -2.50, -2.50, -2.49, -2.49,
-3.33, -2.52, -2.47, -2.46, -2.45, -2.45, -2.44, -2.44, -2.44, -2.44, -2.44,
-3.26, -2.47, -2.44, -2.42, -2.41, -2.41, -2.41, -2.40, -2.40, -2.40, -2.40,
-3.18, -2.44, -2.40, -2.39, -2.39, -2.38, -2.38, -2.38, -2.38, -2.38, -2.38,
-3.02, -2.36, -2.33, -2.33, -2.33, -2.32, -2.32, -2.32, -2.32, -2.32, -2.32,
-2.90, -2.30, -2.29, -2.28, -2.28, -2.28, -2.28, -2.28, -2.28, -2.28, -2.28)

critval.ips.tbar <- c(critval.ips.tbar.int1,
                      critval.ips.tbar.int5,
                      critval.ips.tbar.int10,
                      critval.ips.tbar.trend1,
                      critval.ips.tbar.trend5,
                      critval.ips.tbar.trend10)

critval.ips.tbar <- array(critval.ips.tbar, dim = c(11, 8, 3, 2),
                 dimnames = list(
                   c(5, 10, 15, 20, 25, 30, 40, 50, 60, 70, 100),
                   c(5, 7, 10, 15, 20, 25, 50, 100),
                   c("1%", "5%", "10%"),
                   c("intercept", "trend"))
)

critval.ips.tbar <- aperm(critval.ips.tbar, c(2, 1, 3, 4))


###############

## IPS (2003), table 1
# right hand pane of table 1 for Ztbar statistic
adj.ips.zbar.time  <- c(6, 7, 8, 9, 10, 15, 20, 25, 30, 40, 50, 100, 500, 1000, 2000)
adj.ips.zbar.means <- c(-1.520, -1.514, -1.501, -1.501, -1.504, -1.514, -1.522, -1.520, -1.526, -1.523, -1.527, -1.532, -1.531, -1.529, -1.533)
adj.ips.zbar.vars  <- c(1.745, 1.414, 1.228, 1.132, 1.069, 0.923, 0.851, 0.809, 0.789, 0.770, 0.760, 0.735, 0.715, 0.707, 0.706) 
names(adj.ips.zbar.time) <- names(adj.ips.zbar.means) <- names(adj.ips.zbar.vars) <- adj.ips.zbar.time

# left pane of table 1 [not used]
adj.ips.zbarL.means <- c(-1.125, -1.178, -1.214, -1.244, -1.274, -1.349, -1.395, -1.423, -1.439, -1.463, -1.477, -1.504, -1.526, -1.526, -1.533)
adj.ips.zbarL.vars  <- c(0.497, 0.506, 0.506, 0.527, 0.521, 0.565, 0.592, 0.609, 0.623, 0.639, 0.656, 0.683, 0.704, 0.702, 0.706)

################

# table 2 in LLC (2002): mean and standard deviation adjustments
Tn <- c(  25,  30,  35,  40,  45,  50,  60,   70,   80,   90,  100,  250,   500)

v <- c(c( 0.004,  0.003,  0.002,  0.002,  0.001,  0.001,  0.001,  0.000,  0.000,  0.000,  0.000,  0.000,  0.000),
       c( 1.049,  1.035,  1.027,  1.021,  1.017,  1.014,  1.011,  1.008,  1.007,  1.006,  1.005,  1.001,  1.000),
       c(-0.554, -0.546, -0.541, -0.537, -0.533, -0.531, -0.527, -0.524, -0.521, -0.520, -0.518, -0.509, -0.500),
       c( 0.919,  0.889,  0.867,  0.850,  0.837,  0.826,  0.810,  0.798,  0.789,  0.782,  0.776,  0.742,  0.707),
       c(-0.703, -0.674, -0.653, -0.637, -0.624, -0.614, -0.598, -0.587, -0.578, -0.571, -0.566, -0.533, -0.500),
       c( 1.003,  0.949,  0.906,  0.871,  0.842,  0.818,  0.780,  0.751,  0.728,  0.710,  0.695,  0.603,  0.500)
       )

adj.levinlin <- array(v, dim = c(13, 2, 3),
                      dimnames = list(Tn,
                                      c("mu", "sigma"),
                                      c("none", "intercept", "trend")))

purtest.names.exo <- c(none      = "None",
                       intercept = "Individual Intercepts",
                       trend     = "Individual Intercepts and Trend")

purtest.names.test <- c(levinlin  = "Levin-Lin-Chu Unit-Root Test",
                        ips       = "Im-Pesaran-Shin Unit-Root Test",
                        madwu     = "Maddala-Wu Unit-Root Test",
                        Pm        = "Choi's modified P Unit-Root Test",
                        invnormal = "Choi's Inverse Normal Unit-Root Test",
                        logit     = "Choi's Logit Unit-Root Test",
                        hadri     = "Hadri Test")

my.lm.fit <- function(X, y, dfcor = TRUE, ...){
  reg <- lm.fit(X, y)
  ## 'as' summary method for lm.fit
  p <- reg$rank
  Qr <- reg$qr
  n <- NROW(Qr$qr)
  rdf <- n - p
  p1 <- 1L:p
  r <- reg$residuals
  rss <- as.numeric(crossprod(r))
  resvar <- if (dfcor) rss/rdf else rss/n
  sigma <- sqrt(resvar)
  R <- chol2inv(Qr$qr[p1, p1, drop = FALSE])
  thecoef <- reg$coefficients[Qr$pivot[p1]] #[lags+1]
  these <- sigma * sqrt(diag(R)) #[lags+1])
  list(coef = thecoef, se = these, sigma = sigma,
       rss = rss, n = n, K = p, rdf = rdf)
}


## General functions to transform series:

YClags <- function(object,  k = 3){
  if (k > 0)
    sapply(1:k, function(x) c(rep(NA, x), object[1:(length(object)-x)]))
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
  pos <- which((Ts - x) > 0)[1]
  return(Ts[c(pos - 1, pos)])
}

lagsel <- function(object, exo = c("intercept", "none", "trend"),
                   method = c("Hall", "AIC", "SIC"), pmax = 10, 
                   dfcor = FALSE, fixedT = TRUE, ...){
  # select the optimal number of lags using Hall method, AIC, or SIC
  method <- match.arg(method)
  y <- object
  Dy <- YCdiff(object)
  Ly <- c(NA, object[1:(length(object)-1)])
  if (exo == "none")      m <- NULL
  if (exo == "intercept") m <- rep(1, length(object))
  if (exo == "trend")     m <- cbind(1, YCtrend(object))
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
      AIC <- if (method == "AIC") {
        log(sres$rss / sres$n) + 2 * sres$K / sres$n
      } else {
        log(sres$rss / sres$n) + sres$K * log(sres$n) / sres$n
      }
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
    return(low + (l - Ts[1])/(Ts[2] - Ts[1]) * (high - low))
  }
}

adj.ips.wtbar.value <- function(l = 30, lags = 2, exo = c("intercept", "trend")){
  ## extract the adjustment values for Im-Pesaran-Shin test for Wtbar statistic (table 3 in IPS (2003))
  if (!lags %in% 0:8) warning("lags should be an integer between 0 and 8")
  lags <- min(lags, 8)
  theTs <- as.numeric(dimnames(adj.ips.wtbar)[[2L]])
  Ts <- selectT(l, theTs)
  if (length(Ts) == 1){
    # take value as in table
    return(adj.ips.wtbar[as.character(lags), as.character(Ts), , exo])
  }
  else{
    # interpolate value from table
    low  <- adj.ips.wtbar[as.character(lags), as.character(Ts[1L]), , exo]
    high <- adj.ips.wtbar[as.character(lags), as.character(Ts[2L]), , exo]
    return(low + (l - Ts[1])/(Ts[2] - Ts[1]) * (high - low))
  }
}

adj.ips.ztbar.value <- function(l = 30L, time, means, vars){
  ## extract the adjustment values for Im-Pesaran-Shin test's Ztbar statistic
  ## from table 1, right hand pane in IPS (2003) fed by arguments means and vars
  Ts <- selectT(l, time)
  if (length(Ts) == 1L){
    # take value as in table
    return(c("mean" = means[as.character(Ts)], "var" = vars[as.character(Ts)]))
  }
  else{
    # interpolate value from table
    low  <- c("mean" = means[as.character(Ts[1L])], "var" = vars[as.character(Ts[1L])])
    high <- c("mean" = means[as.character(Ts[2L])], "var" = vars[as.character(Ts[2L])])
    return(low + (l - Ts[1L])/(Ts[2L] - Ts[1L]) * (high - low))
  }
}

critval.ips.tbar.value <- function(ind = 10L, time = 19L, critvals, exo = c("intercept", "trend")){
  ## extract the critical values for Im-Pesaran-Shin test for tbar statistic (table 2 in IPS (2003))
  theInds <-  as.numeric(dimnames(critvals)[[1L]])
  theTs <-  as.numeric(dimnames(critvals)[[2L]])
  Inds <- selectT(ind, theInds)
  Ts <- selectT(time, theTs)
  
  # DEBUG / TODO:
  experimental_critval.ips.tbar.value <- TRUE  ## switch for experimental implementation
  
  if (length(Inds) == 1L && length(Ts) == 1L) {
    # exact hit for individual AND time: take value as in table
    return(critvals[as.character(Inds), as.character(Ts), , exo])
  }
  else{
    if (experimental_critval.ips.tbar.value) return(NA) # TODO
    
    if (length(Inds) == 1L || length(Ts) == 1L) {
      # exact hit for individual (X)OR time: interpolate other dimension
      if (length(Inds) == 1L) {
        low  <- critvals[as.character(Inds), as.character(Ts[1L]), , exo]
        high <- critvals[as.character(Inds), as.character(Ts[2L]), , exo]
        return(low + (Inds - Ts[1L])/(Ts[2L] - Ts[1L]) * (high - low))
      }
      if (length(Ts) == 1L) {
        # TODO: cannot be correct as, e.g. ind = 6, time = 10 yield same value as for ind = 5, time = 10
        low  <- critvals[as.character(Inds[1L]), as.character(Ts), , exo]
        high <- critvals[as.character(Inds[2L]), as.character(Ts), , exo]
       return(low + (Ts - Inds[1L])/(Inds[2L] - Inds[1L]) * (high - low))
        
        # TODO: can take this(?):
        # w1 <- 1 / abs(ind - Inds[1L])
        # w2 <- 1 / abs(ind - Inds[2L])
        # return ( (w1* low + w2*high )/ (w1 + w2))
      }
    } else {
      # only get to this part when both dimensions are not an exact hit:
      # interpolate from both dimensions via billinear interpolation 
      # by solving a system of linear equations
      
      # DEBUG:
      print("interpolate both dimensions")
      
      # extract the 4 critical values as basis of interpolation ("corners of box")
      crit4   <- critvals[as.character(Inds), as.character(Ts), , exo]
      dot <- c(ind, time) # point of interest
      p <- c("1%", "5%", "10%")
      funcval <- sapply(p, function(p) as.numeric(crit4[ , , p]))
      m <- as.matrix(expand.grid(Inds, Ts))
      mat <- cbind(1, m, m[ , 1] * m[ , 2])
      s <- solve(mat, funcval)
      vec <- c(1, dot[1], dot[2], dot[1]*dot[2])
      res <- as.numeric(crossprod(s, vec))
      names(res) <- p
      return(res)
      
      ## gretl: seems to apply inverse distance weighting (IDS) via euclidean distance - why? calculation below
      ##   https://stats.stackexchange.com/questions/64538/how-do-i-find-values-not-given-in-interpolate-in-statistical-tables
      ##    -> mentions the inverse linear approx often gives a better approximation as as t, chi^2, and F distribution as
      ##       values are linear in the reciprocal.

      ## Stata: derived from Stata's documentation per example, Stata seems to just take the "upper right" value, i.e. Inds[2], Ts[2]
      # extract the 4 critical values as basis of interpolation interpolate ("corners of box")
      #
      # crit4 <- critvals[as.character(Inds), as.character(Ts), , exo]
      # dot <- c(ind, time) # point of interest
      # m <- as.matrix(expand.grid(Inds, Ts))
      # colnames(m) <- c("ind", "time")
      # distance <- lapply(1:4, function(x) m[x, ] - dot)
      # weight <- vapply(distance, function(x) 1/sqrt(as.numeric(crossprod(x))), 0.0)
      # 
      #  res <- (
      #     crit4[as.character(Inds[1L]), as.character(Ts[1L]), ] * weight[1] +
      #     crit4[as.character(Inds[2L]), as.character(Ts[1L]), ] * weight[2] +
      #     crit4[as.character(Inds[1L]), as.character(Ts[2L]), ] * weight[3] +
      #     crit4[as.character(Inds[2L]), as.character(Ts[2L]), ] * weight[4]) / sum(weight)
      # return(res)
      
       
       ## example from Wikipedia: https://en.wikipedia.org/wiki/Bilinear_interpolation
       # funcval2 <- c(91,162,210,95)
       # dot2 <- c(20.2, 14.5)
       # m2 <- matrix(c(20,14,21,14,20,15,21,15), ncol = 2, byrow = TRUE)
       # 
       # mat2 <- cbind(1, m2, m2[ , 1] * m2[ , 2])
       # s2 <- solve(mat2, funcval2)
       # vec2 <- c(1, dot2[1], dot2[2], dot2[1]*dot2[2])
       # 
       # print(as.numeric(crossprod(s2, vec2))) # == s[1] + s[2] * dot2[1] + s[3] * dot2[2] + s[4] * dot2[1]*dot2[2]
       # 
       #   # gretl:
       #   funcval2mat <- matrix(funcval2, ncol = 2, byrow = F)
       #   distance2 <- lapply(1:4, function(x) m2[x, ] - dot2)
       #   weight2 <- vapply(distance2, function(x) 1/sqrt(as.numeric(crossprod(x))), 0.0)
       #   
       #   res <- 
       #     (funcval2[1] * weight2[1] +
       #     funcval2[2] * weight2[2] +
       #     funcval2[3] * weight2[3] + 
       #     funcval2[4] * weight2[4] ) / sum(weight2)
    }
  }
}


tsadf <- function(object, exo = c("intercept", "none", "trend"),
                  lags = NULL, dfcor = FALSE, comp.aux.reg = FALSE, ...){
  # compute some ADF regressions for each time series
  y <- object
  L <- length(y)
  Dy <- YCdiff(object)
  Ly <- c(NA, object[1:(length(object) - 1)])
  if (exo == "none")      m <- NULL
  if (exo == "intercept") m <- rep(1, length(object))
  if (exo == "trend")     m <- cbind(1, YCtrend(object))
  narow <- 1:(lags+1)
  LDy <- YClags(Dy, lags)
  X <- cbind(Ly, LDy, m)[-narow, , drop = FALSE]
  y <- Dy[- narow]
  result <- my.lm.fit(X, y, dfcor = dfcor)
  sigma <- result$sigma
  rho <- result$coef[1]
  sdrho <- result$se[1]
  trho <- rho/sdrho
  p.trho <- padf(trho, exo = exo)
  result <- list(rho    = rho,
                 sdrho  = sdrho,
                 trho   = trho,
                 sigma  = sigma,
                 T      = L,
                 lags   = lags,
                 p.trho = p.trho)
  
  if (comp.aux.reg){
    # for Levin-Lin-Chu test only, compute the residuals of the auxiliary
    # regressions
    X <- cbind(LDy[ , 0:lags], m)[-narow, , drop = FALSE]
    if (lags == 0 && exo == "none"){
      resid.diff  <- Dy[-narow]/sigma
      resid.level <- Ly[-narow]/sigma
    }
    else{
      y <- Dy[-narow]
      resid.diff <- lm.fit(X, y)$residuals/sigma
      y <- Ly[-narow]
      resid.level <- lm.fit(X, y)$residuals/sigma
    }
    result$resid <- data.frame(resid.diff  = resid.diff,
                               resid.level = resid.level)
  }
  result
}


longrunvar <- function(x, exo = c("intercept", "none", "trend"), q = NULL){
  # compute the long run variance of the dependent variable

  # q: lag truncation parameter: default (q == NULL) as in LLC, p. 14
  # it can be seen from LLC, table 2, that round() was used to get an
  # integer from that formula (not, e.g. trunc)
  T <- length(x)
  if (is.null(q)) q <- round(3.21 * T^(1/3))
  dx <- x[2:T] - x[1:(T-1)]
  if (exo == "intercept") dx <- dx - mean(dx)
  if (exo == "trend") dx <- lm.fit(cbind(1, 1:length(dx)), dx)$residuals
  dx <- c(NA, dx)
  res <- 1/(T-1)*sum(dx[-1]^2)+
    2*sum(
          sapply(1:q,
                 function(L){
                   sum(dx[2:(T-L)] * dx[(L+2):T]) / (T-1) *
                     (1 - L / (q+1))
                 }
                 )
          )
  return(res)
}


hadritest <- function(object, exo, Hcons, dfcor, method,
                      cl, args, data.name, ...) {
  ## used by purtest(<.>, test = "hadri"); non-exported function
  ## Hadri's test is applicable to balanced data only
  ## object is a list with observations per individual
  if (!is.list(object)) stop("argument 'object' in hadritest is supposed to be a list")
  if (exo == "none") stop("exo = \"none\" is not a valid option for Hadri's test")
  if (length(unique(sapply(object, length))) > 1L) stop("Hadri test is not applicable to unbalanced panels")
  # determine L (= time periods), unique for balanced panel and number of individuals (n)
  L <- unique(sapply(object, length))
  n <- length(object)
  
  if (exo == "intercept"){
    resid <- lapply(object, function(x) lm(x ~ 1)$residuals)
    adj <- c(1/6, 1/45) # xi, zeta^2 in eq. (17) in Hadri (2000)
  }
  
  if (exo == "trend"){
    resid <- lapply(object,
                    function(x){
                      trend <- 1:length(x)
                      lm(x ~ trend)$residuals
                    })
    adj <- c(1/15, 11/6300) # xi, zeta^2 in eq. (25) in Hadri (2000)
  }
  
  cumres2 <- lapply(resid, function(x) cumsum(x)^2)
  if (!Hcons){
    sigma2 <- if (!dfcor) { 
      mean(unlist(resid)^2)
    } else {
      # df correction as suggested in Hadri (2000), p. 157
      # -> apply to full length residuals over all individuals -> n*(L-1) or n*(L-2)
      dfcorval <- switch(exo, "intercept" = n * (L-1), "trend" = n * (L-2))
      sum(unlist(resid)^2) / dfcorval
    }
    S <- sum(unlist(cumres2))/(L^2 * n)
    LM <- S / sigma2
    
    ## This would give the individual LM statistics for this case:
    ## TODO: generalise for all cases and plug in in returned object,
    ## so summary() on a Hadri returns these
    # indsigma2 <- unlist(lapply(resid, function(x) mean(x^2)))
    # indS <- unlist(lapply(cumres2, function(x) sum(x)))
    # indLM <- indS / (L^2 * n) / indsigma2
  }
  else{
    sigma2i <- if (!dfcor) {
      lapply(resid, function(x) mean(x^2))
    } else {
      # df correction as suggested in Hadri (2000), p. 157
      # -> apply to individual residuals' length, so just L -> L-1 or L-2
      dfcorval <- switch(exo, "intercept" = (L-1), "trend" = (L-2))
      lapply(resid, function(x) sum(x^2)/dfcorval)
    }
    Sit2 <- mapply("/", cumres2, sigma2i)
    LM <- sum(unlist(Sit2))/ (L^2 * n)
    method <- paste0(method, " (Heterosked. Consistent)")
  }
  stat <- c(z = sqrt(n) * (LM - adj[1])  / sqrt(adj[2])) # eq. (14), (22) in Hadri (2000)
  pvalue <- pnorm(stat, lower.tail = FALSE) # is one-sided! was until rev. 572: 2*(pnorm(abs(stat), lower.tail = FALSE))
  
  htest <- structure(list(statistic   = stat,
                          parameter   = NULL,
                          alternative = "at least one series has a unit root", # correct alternative (at least one unit root)
                          data.name   = data.name,
                          method      = method,
                          p.value     = pvalue),
                     class = "htest")
  
  result <- list(statistic = htest,
                 call = cl,
                 args = args)
  
  class(result) <- "purtest"
  return(result)
} # END hadritest


#' Unit root tests for panel data
#' 
#' `purtest` implements several testing procedures that have been proposed
#' to test unit root hypotheses with panel data.
#' 
#' 
#' All these tests except `"hadri"` are based on the estimation of
#' augmented Dickey-Fuller (ADF) regressions for each time series. A
#' statistic is then computed using the t-statistics associated with
#' the lagged variable. The Hadri residual-based LM statistic is the
#' cross-sectional average of the individual KPSS statistics
#' \insertCite{KWIA:PHIL:SCHM:SHIN:92}{plm}, standardized by their
#' asymptotic mean and standard deviation.
#' 
#' Several Fisher-type tests that combine p-values from tests based on
#' ADF regressions per individual are available:
#'
#' - `"madwu"` is the inverse chi-squared test
#' \insertCite{MADDA:WU:99}{plm}, also called P test by
#' \insertCite{CHOI:01;textual}{plm}.
#'
#' - `"Pm"` is the modified P test proposed by
#' \insertCite{CHOI:01;textual}{plm} for large N,
#'
#' - `"invnormal"` is the inverse normal test by \insertCite{CHOI:01}{plm}, and
#' 
#' - `"logit"` is the logit test by \insertCite{CHOI:01}{plm}.
#'
#' The individual p-values for the Fisher-type tests are approximated
#' as described in \insertCite{MACK:94;textual}{plm}.
#' 
#' Hadri's test and the test of Levin/Lin/Chu are not applicable to unbalanced
#' panels.
#' 
# TODO: once code changed, change comment here
#' For the test statistic tbar of the test of Im/Peseran/Shin (2003)
#' (`ips.stat = "tbar`), no p-value is computed.
#' 
#' The exogeneous instruments of the tests (where applicable) can be specified
#' in several ways, depending on how the data is handed over to the function:
#' 
#' - For the `formula`/`data` interface (if `data` is a `data.frame`,
#' an additional `index` argument should be specified); the formula
#' should be of the form: `y ~ 0`, `y ~ 1`, or `y ~ trend` for a test
#' with no exogenous variables, with an intercept, or with individual
#' intercepts and time trend, respectively. The `exo` argument is
#' ignored in this case.
#' 
#' - For the `data.frame`, `matrix`, and `pseries` interfaces: in
#' these cases, the exogenous variables are specified using the `exo`
#' argument.
#' 
#' With the associated `summary` and `print` methods, additional
#' information can be extracted/displayed (see also Value).
#' 
#' @aliases purtest
#' @param object,x Either a `"data.frame"` or a matrix containing the
#'     time series (individuals as columns), a `"pseries"` object, a formula;
#'     a `"purtest"` object for the print and summary methods,
#' @param data a `"data.frame"` or a `"pdata.frame"` object (required for
#'     formula interface, see Details and Examples),
#' @param index the indexes,
#' @param test the test to be computed: one of `"levinlin"` for
#'     \insertCite{LEVIN:LIN:CHU:02;textual}{plm}, `"ips"` for
#'     \insertCite{IM:PESAR:SHIN:03;textual}{plm}, `"madwu"` for
#'     \insertCite{MADDA:WU:99;textual}{plm}, `"Pm"` , `"invnormal"`,
#'     or `"logit"` for various tests as in
#'     \insertCite{CHOI:01;textual}{plm}, or `"hadri"` for
#'     \insertCite{HADR:00;textual}{plm}, see Details,
#' @param exo the exogenous variables to introduce in the augmented
#'     Dickey--Fuller (ADF) regressions, one of: no exogenous
#'     variables (`"none"`), individual intercepts (`"intercept"`), or
#'     individual intercepts and trends (`"trend"`), but see Details,
#' @param lags the number of lags to be used for the augmented
#'     Dickey-Fuller regressions: either an integer (the number of
#'     lags for all time series), a vector of integers (one for each
#'     time series), or a character string for an automatic
#'     computation of the number of lags, based on the AIC
#'     (`"AIC"`), the SIC (`"SIC"`), or on the method by
#'     \insertCite{HALL:94;textual}{plm} (`"Hall"`),
#' @param pmax maximum number of lags,
#' @param Hcons logical, only relevant for `test = "hadri"`,
#'     indicating whether the heteroskedasticity-consistent test of
#'     \insertCite{HADR:00;textual}{plm} should be computed,
#' @param q the bandwidth for the estimation of the long-run variance 
#'     (only relevant for `test = "levinlin"`, the default (`q = NULL``)
#'     gives the value as suggested by the authors as round(3.21 * T^(1/3))),
#' @param dfcor logical, indicating whether the standard deviation of
#'     the regressions is to be computed using a degrees-of-freedom
#'     correction,
#' @param fixedT logical, indicating whether the different ADF
#'     regressions are to be computed using the same number of
#'     observations,
#' @param ips.stat `NULL` or character of length 1 to request a specific
#'     IPS statistic, one of `"Wtbar"` (also default if `ips.stat = NULL`),
#'     `"Ztbar"`, `"tbar"`,
#' @param \dots further arguments.
#' @return An object of class `"purtest"`: a list with the elements
#'     `"statistic"` (a `"htest"` object), `"call"`, `"args"`,
#'     `"idres"` (containing results from the individual regressions),
#'     and `"adjval"` (containing the simulated means and variances
#'     needed to compute the statistic).
#' @export
#' @author Yves Croissant and for "Pm", "invnormal", and "logit" Kevin
#'     Tappe
#' @seealso [cipstest()]
#' @references
#'
#' \insertAllCited{}
#'  
#' @keywords htest
#' @examples
#' 
#' data("Grunfeld", package = "plm")
#' y <- data.frame(split(Grunfeld$inv, Grunfeld$firm)) # individuals in columns
#' 
#' purtest(y, pmax = 4, exo = "intercept", test = "madwu")
#' 
#' ## same via formula interface
#' purtest(inv ~ 1, data = Grunfeld, index = c("firm", "year"), pmax = 4, test = "madwu")
#' 
purtest <- function(object, data = NULL, index = NULL,
                    test = c("levinlin", "ips", "madwu", "Pm" , "invnormal", "logit", "hadri"),
                    exo = c("none", "intercept", "trend"),
                    lags = c("SIC", "AIC", "Hall"),
                    pmax = 10, Hcons = TRUE, q = NULL, dfcor = FALSE, 
                    fixedT = TRUE, ips.stat = NULL, ...) {
  
  data.name <- paste(deparse(substitute(object)))

  id <- NULL
  if (inherits(object, "formula")){
    # exo is derived from specified formula:
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
        if (!inherits(data, "data.frame")) stop("'data' does not specify a data.frame/pdata.frame")
        if (object %in% names(data)){
          object <- data[[object]]
          if (!inherits(data, "pdata.frame")){
            if (is.null(index)) stop("the index attribute is required")
            else data <- pdata.frame(data, index)
          }
          id <- attr(data, "index")[[1]]
        }
        else{
          stop(paste0("unknown response (\"", object, "\" not in data)"))
        }
      }
    }
  } # END object is a formula
  else{
    exo <- match.arg(exo)
    if (is.null(dim(object))){
      if (inherits(object, "pseries")){
        id <- attr(object, "index")[[1]]
      }
      else stop("the individual dimension is undefined") # cannot derive individual dimension from a vector if not pseries
    }
    if (is.matrix(object) || is.data.frame(object)) {
      if (!is.null(data)) stop("object is data.frame or matrix but data is not NULL")
      if (is.matrix(object)) object <- as.data.frame(object)
    }
  }
  
  # by now, object is either a pseries to be split or a data.frame 
  
  if (!inherits(object, "data.frame")){
    if (is.null(id)) stop("the individual dimension is undefined")
    object <- split(object, id)
  } else {
    if (!ncol(object) > 1) warning("data.frame or matrix specified in argument object does not contain more than one individual (individuals are supposed to be in columns)")
    object <- as.list(object)
  }
  
  cl <- match.call()
  test <- match.arg(test)
  ips.stat <- if (is.null(ips.stat)) "Wtbar" else ips.stat # set default for IPS test
  if (is.character(lags)) lags <- match.arg(lags) # if character, select one possible value
  args <- list(test = test, exo = exo, pmax = pmax, lags = lags,
               dfcor = dfcor, fixedT = fixedT, ips.stat = ips.stat)
  n <- length(object) # number of individuals, assumes object is a list
  sigma2 <- NULL
  pvalues.trho <- NULL
  alternative <- "stationarity"
  method <- paste0(purtest.names.test[test], " (ex. var.: ",
                   purtest.names.exo[exo],")")
  
  # If Hadri test, call function and exit early
  if (test == "hadri") return(hadritest(object, exo, Hcons, dfcor,
                                        method, cl, args, data.name, ...)) 
  
  # compute the lags for each time series if necessary
  if (is.numeric(lags)){
    if (length(lags) == 1L) lags <- rep(lags, n)
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
  
  # compute the augmented Dickey-Fuller regressions for each time series
  comp.aux.reg <- (test == "levinlin")
  idres <- mapply(function(x, y)
                  tsadf(x, exo = exo, lags = y, dfcor = dfcor, comp.aux.reg = comp.aux.reg),
                  object, as.list(lags), SIMPLIFY = FALSE)
  # data requirement per individual to be able to derive all estimates in ADF regression:
  #    obs - lags - 1 >= lags + x + 1 + 1, where x = 1 ("none"), 2 ("intercept"), 3 ("trend")
  # => obs >= 2 * lags + 1|2|3 + 1 + 1
  
  
  if (test == "levinlin"){
    if (length(unique(sapply(object, length))) > 1L) stop("test = \"levinlin\" is not applicable to unbalanced panels")
    
    L.levinlin <- unique(sapply(object, length)) # time periods
    
    # get the adjustment parameters for the mean and the variance
    adjval <- adj.levinlin.value(L.levinlin, exo = exo)
    mymu  <- adjval[1]
    mysig <- adjval[2]
    # calculate the ratio of LT/ST variance
    sigmaST <- sapply(idres, function(x) x[["sigma"]])
    sigmaLT <- sqrt(sapply(object, longrunvar, exo = exo, q = q))
    si <- sigmaLT/sigmaST # LLC (2002), formula 6
    sbar <- mean(si) 

    # stack the residuals of each time series and perform the pooled
    # regression
    res.level <- unlist(lapply(idres, function(x) x$resid[["resid.level"]]))
    res.diff  <- unlist(lapply(idres, function(x) x$resid[["resid.diff"]]))
    z <- my.lm.fit(as.matrix(res.level), res.diff, dfcor = dfcor)
    # compute the Levin-Lin-Chu statistic
    tildeT <- L.levinlin - mean(lags) - 1
    sigmaeps2 <- z$rss / (n * tildeT)
    rho   <- z$coef
    sdrho <- z$se
    trho  <- rho/sdrho
    stat <- (trho - n * tildeT * sbar / sigmaeps2 * sdrho * mymu)/mysig # LLC (2002), formula 12
    names(stat) <- "z" # avoids a concatenated name like z.x1
    pvalue <- pnorm(stat, lower.tail = TRUE) # need lower.tail = TRUE (like ADF one-sided to the left)
    parameter <- NULL
    sigma2 <- cbind(sigmaST^2, sigmaLT^2)
    colnames(sigma2) <- c("sigma2ST", "sigma2LT")
    pvalues.trho <- sapply(idres, function(x) x[["p.trho"]])
  }
  
  if (test == "ips"){
    if (exo == "none") stop("exo = \"none\" is not a valid option for the Im-Pesaran-Shin test")
    if (!is.null(ips.stat) && !any(ips.stat %in% c("Wtbar", "Ztbar", "tbar"))) stop("argument 'ips.stat' must be one of \"Wtbar\", \"Ztbar\", \"tbar\"")
    
    lags  <- sapply(idres, function(x) x[["lags"]])
    L.ips <- sapply(idres, function(x) x[["T"]]) - lags - 1
    trho  <- sapply(idres, function(x) x[["trho"]])
    tbar <- mean(trho)
    parameter <- NULL
    adjval <- NULL
    pvalues.trho <- padf(trho, exo = exo)
    
    if (is.null(ips.stat) || ips.stat == "Wtbar") {
      # calc Wtbar - default
      adjval <- mapply(function(x, y) adj.ips.wtbar.value(x, y, exo = exo),
                       as.list(L.ips), as.list(lags))
      Etbar <- mean(adjval[1, ])
      Vtbar <- mean(adjval[2, ])
      stat <- c("Wtbar" = sqrt(n) * (tbar - Etbar) / sqrt(Vtbar)) # (3.13) = (4.10) in IPS (2003) [same generic formula for Ztbar and Wtbar]
      pvalue <- pnorm(stat, lower.tail = TRUE) # need lower.tail = TRUE (like ADF one-sided to the left), was until rev. 577: 2*pnorm(abs(stat), lower.tail = FALSE)
    }
    
    if (!is.null(ips.stat) && ips.stat == "Ztbar") {
      # calc Ztbar
      adjval <- adjval.ztbar <- sapply(L.ips, adj.ips.ztbar.value, 
                                       adj.ips.zbar.time, adj.ips.zbar.means, adj.ips.zbar.vars)
      rownames(adjval) <- rownames(adjval.ztbar) <- c("mean", "var")
      Etbar.ztbar <- mean(adjval.ztbar[1, ])
      Vtbar.ztbar <- mean(adjval.ztbar[2, ])
      stat <- stat.ztbar <- c("Ztbar" = sqrt(n) * (tbar - Etbar.ztbar) / sqrt(Vtbar.ztbar)) # (3.13) = (4.10) in IPS (2003) [same generic formula for Ztbar and Wtbar]
      pvalue <- pvalue.ztbar <- pnorm(stat.ztbar, lower.tail = TRUE)
    }
    
    if (!is.null(ips.stat) && ips.stat == "tbar") {
      # give tbar
      stat <- tbar
      names(stat) <- "tbar"
      pvalue <- NA # TODO: interpolate / connect to function critval.ips.tbar.value
      adjval <- NULL
    }
  }
  
  if (test == "madwu"){
    # Maddala/Wu (1999), pp. 636-637; Choi (2001), p. 253; Baltagi (2013), pp. 283-285
    ## does not require a balanced panel
    
    trho <- sapply(idres, function(x) x[["trho"]])
    pvalues.trho <- sapply(idres, function(x) x[["p.trho"]])
    stat <- c(chisq = - 2 * sum(log(pvalues.trho)))
    n.madwu <- length(trho)
    parameter <- c(df = 2 * n.madwu)
    pvalue <- pchisq(stat, df = parameter, lower.tail = FALSE)
    adjval <- NULL
  }
  
  if (test == "Pm"){
    ## Choi Pm (modified P) [proposed for large N]
    trho <- sapply(idres, function(x) x[["trho"]])
    pvalues.trho <- sapply(idres, function(x) x[["p.trho"]])
    n.Pm <- length(trho)
    # formula (18) in Choi (2001), p. 255:
    stat <- c( "Pm" = 1/(2 * sqrt(n.Pm)) * sum(-2 * log(pvalues.trho) - 2) ) # == 1/sqrt(n.Pm) * sum(log(pvalues.trho) +1)
    pvalue <- pnorm(stat, lower.tail = FALSE) # one-sided
    parameter <- NULL
    adjval <- NULL
  }
  
  if (test == "invnormal"){
    # inverse normal test as in Choi (2001)
    trho <- sapply(idres, function(x) x[["trho"]])
    pvalues.trho <- sapply(idres, function(x) x[["p.trho"]])
    n.invnormal <- length(trho)
    stat <- c("z" = sum(qnorm(pvalues.trho)) / sqrt(n.invnormal)) # formula (9), Choi (2001), p. 253
    pvalue <- pnorm(stat, lower.tail = TRUE) # formula (12), Choi, p. 254
    parameter <- NULL
    adjval <- NULL
  }
  
  if (test == "logit"){
    # logit test as in Choi (2001)
    trho <- sapply(idres, function(x) x[["trho"]])
    pvalues.trho <- sapply(idres, function(x) x[["p.trho"]])
    n.logit <- length(trho)
    l_stat <-  c("L*" = sum(log(pvalues.trho / (1 - pvalues.trho)))) # formula (10), Choi (2001), p. 253
    k <- (3 * (5 * n.logit + 4)) / (pi^2 * n.logit * (5 * n.logit + 2))
    stat <- sqrt(k) * l_stat  # formula (13), Choi (2001), p. 254
    parameter <- c("df" = 5 * n.logit + 4)
    pvalue <- pt(stat, df = parameter, lower.tail = TRUE)
    adjval <- NULL
  }
  
  htest <- structure(list(statistic   = stat,
                          parameter   = parameter,
                          alternative = alternative,
                          data.name   = data.name,
                          method      = method,
                          p.value     = pvalue),
                     class = "htest")
  
  result <- list(statistic = htest,
                 call      = cl,
                 args      = args,
                 idres     = idres,
                 adjval    = adjval,
                 p.trho    = pvalues.trho,
                 sigma2    = sigma2)
  class(result) <- "purtest"
  result
}


#' @rdname purtest
#' @export
print.purtest <- function(x, ...){
  print(x$statistic, ...)
}

#' @rdname purtest
#' @export
summary.purtest <- function(object, ...){
  if (object$args$test == "hadri"){
    stop("summary() not applicable for Hadri's test, i.e. for the result of purtest(<.>, test = \"hadri\")")
  }
  
  lags   <- sapply(object$idres, function(x) x[["lags"]])
  L      <- sapply(object$idres, function(x) x[["T"]])
  rho    <- sapply(object$idres, function(x) x[["rho"]])
  trho   <- sapply(object$idres, function(x) x[["trho"]])
  p.trho <- sapply(object$idres, function(x) x[["p.trho"]])
  nam <- names(object$idres)
  sumidres <- cbind(
      "lags"   = lags,
      "obs"    = L - lags - 1,
      "rho"    = rho,
      "trho"   = trho,
      "p.trho" = p.trho)
  if (object$args$test == "ips" && !object$args$ips.stat == "tbar") {
    sumidres <- cbind(sumidres, t(object$adjval))
  }
  if (object$args$test == "levinlin") {
    sumidres <- cbind(sumidres, object$sigma2)
  }
  
  rownames(sumidres) <- nam
  object$sumidres <- sumidres
  class(object) <- c("summary.purtest", "purtest")
  object
}

#' @rdname purtest
#' @export
print.summary.purtest <- function(x, ...){
  cat(paste(purtest.names.test[x$args$test], "\n"))
  cat(paste("Exogenous variables:", purtest.names.exo[x$args$exo], "\n"))
  thelags <- sapply(x$idres, function(x) x[["lags"]])
  if (is.character(x$args$lags)){
    lagselectionmethod <- if (x$args$lags == "Hall") "Hall's method" else x$args$lags
    cat(paste0("Automatic selection of lags using ", lagselectionmethod, ": ",
              min(thelags), " - ", max(thelags), " lags (max: ", x$args$pmax, ")\n"))
  }
  else{
    cat("User-provided lags\n")
  }
  if (x$args$test == "ips") {
    cat(paste(paste0("statistic (", x$args$ips.stat,"):"), round(x$statistic$statistic, 3), "\n"))
  } else {
    cat(paste("statistic:", round(x$statistic$statistic, 3), "\n"))
  }
  cat(paste("p-value:", round(x$statistic$p.value, 3),   "\n"))
  print(x$sumidres, ...)
}

