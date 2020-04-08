# Various run tests for purtest()

# NB: p-values can differ slightly relative to .Rout.save file due to availabilty of package 'urca'
#     for p-value approximation in individiual (A)DF-regressions.

library(plm)
data("Grunfeld", package = "plm")
pG <- pdata.frame(Grunfeld)
y <- data.frame(split(Grunfeld$inv, Grunfeld$firm))

# some general run tests

purtest(pG$inv, pmax = 4, exo = "intercept", test = "ips")
purtest(inv ~ 1, data = Grunfeld, index = "firm", pmax = 4, test = "madwu")

summary(a1 <- purtest(pG$inv, lags = "SIC", exo = "intercept", test = "ips", pmax = 8)) # TODO: why is the data requirement check not triggered
print(a1$args$lags)
if (length(a1$args$lags) != 1) stop("length(return_value$args$lags must be 1")
if (a1$args$lags != "SIC") stop("length(return_value$args$lags must be \"SIC\"")

summary(a2 <- purtest(pG$inv, lags = 2, exo = "intercept", test = "ips"))
print(a2$args$lags)
if (length(a2$args$lags) != 1) stop("length(return_value$args$lags must be 1")

summary(a3 <- purtest(pG$inv, lags = c(2,3,1,5,8,1,4,6,7,1), exo = "intercept", test = "ips")) # TODO: why is the data requirement check not triggered
summary(a3_ok <- purtest(pG$inv, lags = c(2,3,1,4,4,1,4,4,4,1), exo = "intercept", test = "ips")) # TODO: ... and this works
length(a3$args$lags)
print(a3$args$lags)
if (length(a3$args$lags) != 10) stop("length(return_value$args$lags must be 10")

### pseries
purtest(pdata.frame(Grunfeld)[ , "inv"],  pmax = 4, test = "ips", exo = "intercept") # works
purtest(pdata.frame(Grunfeld)[ , "inv"],  pmax = 4, test = "ips", exo = "trend")     # works
# purtest(pdata.frame(Grunfeld)[ , "inv"],  pmax = 4, test = "ips", exo = "none")    # works as intended: gives informative error msg

### pdata.frame - individuals must be in columns!
df_inv <- data.frame(split(Grunfeld$inv, Grunfeld$firm)) 
purtest(df_inv, pmax = 4, test = "ips", exo = "intercept")
### matrix
purtest(as.matrix(df_inv), pmax = 4, test = "ips", exo = "intercept")


#### Hadri (2000) test
## matches results vom EViews 9.5 (if dfcor = FALSE):
## z stat     =  4.18428, p = 0.0000 (intercept)
## z stat het = 10.1553,  p = 0.0000 (intercept)
## z stat     =  4.53395, p = 0.0000 (trend)
## z stat het =  9.57816, p = 0.0000 (trend)
h_1.1 <- purtest(pG$value, exo = "intercept", test = "hadri", Hcons = FALSE)
h_1.2 <- purtest(pG$value, exo = "intercept", test = "hadri", Hcons = FALSE, dfcor = TRUE)
h_2.1 <- purtest(pG$value, exo = "intercept", test = "hadri")
h_2.2 <- purtest(pG$value, exo = "intercept", test = "hadri", dfcor = TRUE)
h_3.1 <- purtest(pG$value, exo = "trend", test = "hadri", Hcons = FALSE)
h_3.2 <- purtest(pG$value, exo = "trend", test = "hadri", Hcons = FALSE, dfcor = TRUE)
h_4.1 <- purtest(pG$value, exo = "trend", test = "hadri")
h_4.2 <- purtest(pG$value, exo = "trend", test = "hadri", dfcor = TRUE)

## IGNORE_RDIFF_BEGIN
summary(h_1.1)
summary(h_1.2)
summary(h_2.1)
summary(h_2.2)
summary(h_3.1)
summary(h_3.2)
summary(h_4.1)
summary(h_4.2)
## IGNORE_RDIFF_END

### IPS (2003) test
## use dfcor = TRUE to match gretl 2017c and EViews 9.5 exactly
b <- purtest(pG$value, test = "ips", exo = "intercept", lags = 0, dfcor = TRUE)
summary(b)

# NB: In case of lags = 0 (DF-regression), gretl 2019d takes the finite sample p-values
#    (finite sample p-values are not applicable for augmented DF-regressions)

# For reference/reproducability purposes, use MacKinnon (1994) and MacKinnon (1996) each once:
summary(purtest(pG$value, test = "ips", exo = "intercept", lags = 2, dfcor = TRUE, p.approx = "MacKinnon1994"))
summary(purtest(pG$value, test = "ips", exo = "intercept", lags = 2, dfcor = TRUE, p.approx = "MacKinnon1996"))

## lags = 2 (lags > 0 gives the Wtbar stat in gretl and EViews)
b_lag2 <- purtest(pG$value, test = "ips", exo = "intercept", lags = 2, dfcor = TRUE)
summary(b_lag2)

# unbalanced IPS
pG_unbal2 <- pG[1:190, ]
b_unbal <- purtest(pG_unbal2$value, test = "ips", exo = "intercept", lags = 0, dfcor = TRUE)
summary(b_unbal)

# IPS - Ztbar
# matches gretl exactly # Z_tbar = -1.12782 [0.1297]; difference to EViews: Adjustment parameters (Etbar, Vtbar):
summary(b_unbal3 <- purtest(pG_unbal2$value, test = "ips", exo = "intercept", lags = 0, dfcor = TRUE, ips.stat = "Ztbar"))

summary(b_ztbar        <- purtest(pG$value,        test = "ips", exo = "intercept", lags = 0, dfcor = TRUE, ips.stat = "Ztbar"))
summary(b_ztbar_unbal  <- purtest(pG_unbal2$value, test = "ips", exo = "intercept", lags = 0, dfcor = TRUE, ips.stat = "Ztbar"))
summary(b_ztbar_unbal2 <- purtest(pG_unbal2$value, test = "ips", exo = "intercept", lags = 2, dfcor = TRUE, ips.stat = "Ztbar"))
summary(b_lag2_ztbar   <- purtest(pG$value,        test = "ips", exo = "intercept", lags = 2, dfcor = TRUE, ips.stat = "Ztbar"))


## these two correctly errors with an informative message:
# summary(b_lag2_tbar <- purtest(pG$value, test = "ips", exo = "intercept", lags = 2, dfcor = TRUE, ips.stat = "tbar"))

# TODO: in case of automatic lag selection, the selected lags need to be outputted
# purtest(pG$value, test = "ips", dfcor = TRUE, ips.stat = "wtbar", exo = "intercept") # due to automatic lag selection (yields 10)


value4 <- pG[pG$year %in% as.character(1935:1938), ]$value
purtest(value4, test = "ips", dfcor = TRUE, ips.stat = "tbar", exo = "intercept", lags = 0)



purtest(pG$value, test = "ips", exo = "intercept", lags = 5, dfcor = TRUE, ips.stat = "Wtbar") # TODO: how to detect this not detected?
summary(purtest(pG$value, test = "ips", exo = "intercept", lags = 4, dfcor = TRUE, ips.stat = "Wtbar"))

#### various tests from Choi (2001) [besides test = "madwu"]
purtest(pG$value, test = "Pm",        exo = "intercept", lags = 2, dfcor = TRUE)
purtest(pG$value, test = "invnormal", exo = "intercept", lags = 2, dfcor = TRUE)
purtest(pG$value, test = "logit",     exo = "intercept", lags = 2, dfcor = TRUE)



#### Levin-Lin-Chu test
# matches gretl (almost) exactly: match gretl, set dfcor = FALSE
# NB: one remaining (asymptotically irrelevant) difference
# between gretl and purtest for LLC. Bandwidth calc for Bartlett kernel (in longrunvar),
# 3.21 * T^(1/3) purtest rounds, gretl truncates (no clear answer to this, LLC
# use rounding as becomes clear from their table 2 as they apply rounding for their
# "quick-and-dirty" values for bandwidth cutoff).
llc <- purtest(pG$value, test = "levinlin", exo = "none", lags = 0, dfcor = FALSE)
summary(llc)

llc_int <- purtest(pG$value, test = "levinlin", exo = "intercept", lags = 0, dfcor = FALSE)
summary(llc_int)

llc_trend <- purtest(pG$value, test = "levinlin", exo = "trend", lags = 0, dfcor = FALSE)
summary(llc_trend)
