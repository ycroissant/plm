# tests for is.pconsecutive

############## test with consecutive and non-consecutive time periods ####

library(plm)
data("Grunfeld", package = "plm")
Grunfeld_missing_period <- Grunfeld[-2, ]

pGrunfeld <- pdata.frame(Grunfeld)
pGrunfeld_missing_period <- pdata.frame(Grunfeld_missing_period) # delete one time period of first individual (1-1936 is missing)

# Expected results:
#   Grunfeld: rep(TRUE, 10)
#   Grunfeld_missing_period: c(FALSE, rep(TRUE, 9))

# test on data.frame
is.pconsecutive(Grunfeld)
is.pconsecutive(Grunfeld_missing_period)

is.pconsecutive(Grunfeld, index=c("firm", "year"))
is.pconsecutive(Grunfeld_missing_period, index=c("firm", "year"))

# should result in informative error message: is.pconsecutive(Grunfeld, index=c("firm")
# because we need both dimensions when index != NULL
  ttC <- tryCatch(is.pconsecutive(Grunfeld, index=c("firm")), error=function(e) e, warning=function(w) w)
  if(!is(ttC,"error")) stop("error for non supplied time dimension in index not working")
  # print(ttC$message)


# test on pdata.frame
if(!all(is.pconsecutive(pGrunfeld))) stop("is.pconsecutive on pdata.frame: wrong result")
if(!all.equal(is.pconsecutive(pGrunfeld_missing_period), c(FALSE, rep(TRUE, 9)),  check.names = FALSE)) stop("is.pconsecutive on pdata.frame: wrong result")


# test on panelmodel object
estimation_pGrunfeld <- plm(inv ~ value + capital, data = pGrunfeld)
estimation_pGrunfeld_missing_period <- plm(inv ~ value + capital, data = pGrunfeld_missing_period)
nobs(estimation_pGrunfeld) # 200
nobs(estimation_pGrunfeld_missing_period) # 199

is.pconsecutive(estimation_pGrunfeld)
is.pconsecutive(estimation_pGrunfeld_missing_period)


# test on pseries
pinv <- pGrunfeld$inv
pinv_missing_period <- pGrunfeld_missing_period$inv

is.pconsecutive(pinv)
is.pconsecutive(pinv_missing_period)



######## with different data set "Hedonic"
data("Hedonic", package = "plm")
Hed <- Hedonic
pHed <- pdata.frame(Hedonic, index = "townid")
Hed_missing_period <- Hedonic[-c(5,11), ] # delete 3-2 and 4-5

pHed_missing_period <- pdata.frame(Hedonic, index = "townid") # make pdata.frame first to produce a time index
pHed_missing_period <- as.data.frame(pHed_missing_period)
pHed_missing_period <- pHed_missing_period[-c(5,11), ]  # delete 3-2 and 4-5
pHed_missing_period <- pdata.frame(pHed_missing_period, index = c("townid", "time"))


# Expected results
#  Hed: all TRUE (rep(TRUE, 92))
#  Hed_missing_period: 3rd and 4th individual FALSE, rest TRUE
expected_Hed <- rep(TRUE, 92)
expected_Hed_missing_period <- expected_Hed
expected_Hed_missing_period[c(3,4)] <- FALSE

# test on data.frame
Hed_df <- as.data.frame(pHed)
Hed_df_missing_period <- as.data.frame(pHed_missing_period)
is.pconsecutive(Hed_df, index = c("townid", "time"))
is.pconsecutive(Hed_df_missing_period, index = c("townid", "time"))

# test on pdata.frame
if(!all.equal(is.pconsecutive(pHed), expected_Hed, check.names = FALSE)) stop("is.pconsecutive on pdata.frame: wrong result")
if(!all.equal(is.pconsecutive(pHed_missing_period), expected_Hed_missing_period, check.names = FALSE)) stop("is.pconsecutive on pdata.frame: wrong result")

# test on panelmodel object
estimation_pHed <- plm(mv ~ crim + indus, data = pHed)
estimation_pHed_missing_period <- plm(mv ~ crim + indus, data = pHed_missing_period)

is.pconsecutive(estimation_pHed)
is.pconsecutive(estimation_pHed_missing_period)

# test on pseries
pmv <- pHed$mv
pmv_missing_period <- pHed_missing_period$mv

if(!all.equal(is.pconsecutive(pmv), expected_Hed, check.names = FALSE)) stop("is.pconsecutive on pseries: wrong result")
if(!all.equal(is.pconsecutive(pmv_missing_period), expected_Hed_missing_period, check.names = FALSE)) stop("is.pconsecutive on pseries: wrong result")

######## with different data set "Gasoline" (has "named" individuals, not just numbers)
data("Gasoline", package = "plm")
pGasoline <- pdata.frame(Gasoline)

# test on data.frame
is.pconsecutive(Gasoline, index = c("country", "year"))

# test on pdata.frame
is.pconsecutive(pGasoline)


