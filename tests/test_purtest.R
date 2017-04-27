# Various run tests for purtest()

data("Grunfeld", package = "plm")
pG <- pdata.frame(Grunfeld)

y <- data.frame(split(Grunfeld$inv, Grunfeld$firm))

purtest(pG$inv, pmax = 4, exo = "intercept", test = "ips")
purtest(inv ~ 1, data = Grunfeld, index = "firm", pmax = 4, test = "madwu")

summary(a1 <- purtest(pG$inv, lags = "SIC", exo = "intercept", test = "ips", pmax = 8))
print(a1$args$lags)

summary(a2 <- purtest(pG$inv, lags = 2, exo = "intercept", test = "ips"))
print(a2$args$lags)

summary(a3 <- purtest(pG$inv, lags = c(2,3,1,5,8,1,4,6,7,1), exo = "intercept", test = "ips"))
length(a3$args$lags)
print(a3$args$lags)

### pseries
purtest(pdata.frame(Grunfeld)[ , "inv"],  pmax = 4, test = "ips", exo = "intercept") # works
purtest(pdata.frame(Grunfeld)[ , "inv"],  pmax = 4, test = "ips", exo = "trend")     # works
# purtest(pdata.frame(Grunfeld)[ , "inv"],  pmax = 4, test = "ips", exo = "none")      # works as intended: gives informative error msg

### pdata.frame
# purtest(pdata.frame(Grunfeld)[ , "inv", drop = F],  pmax = 4, test = "ips", exo = "intercept") # runs but but gives different results! and a warning!
# purtest(pdata.frame(Grunfeld)[ , "inv", drop = F],  pmax = 4, test = "ips", exo = "trend")     # runs but but gives different results! and a warning!
# purtest(pdata.frame(Grunfeld)[ , "inv", drop = F],  pmax = 4, test = "ips", exo = "none")     # works as intended: gives informative error msg


