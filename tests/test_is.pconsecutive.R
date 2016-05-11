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
if(!isTRUE(all.equal(is.pconsecutive(pGrunfeld_missing_period), c(FALSE, rep(TRUE, 9)),  check.names = FALSE))) stop("is.pconsecutive on pdata.frame: wrong result")


# test on panelmodel object
estimation_pGrunfeld <- plm(inv ~ value + capital, data = pGrunfeld)
estimation_pGrunfeld_missing_period <- plm(inv ~ value + capital, data = pGrunfeld_missing_period)
nobs(estimation_pGrunfeld) # 200
nobs(estimation_pGrunfeld_missing_period) # 199

is.pconsecutive(estimation_pGrunfeld)
is.pconsecutive(estimation_pGrunfeld_missing_period)


# default method (by dispatching)
# test on "numeric" and "NULL" -> should execute is.pconsecutive.default
is.pconsecutive(Grunfeld$inv,      id = Grunfeld$firm, time = Grunfeld$year)
is.pconsecutive(Grunfeld[["inv"]], id = Grunfeld$firm, time = Grunfeld$year)
is.pconsecutive(NULL, id = Grunfeld$firm, time = Grunfeld$year)


# test on pseries
pinv <- pGrunfeld$inv
pinv_missing_period <- pGrunfeld_missing_period$inv

is.pconsecutive(pinv)
is.pconsecutive(pinv_missing_period)


## more missing periods
Grunfeld_missing_periods <- Grunfeld[-c(2,6,7), ]
pGrunfeld_missing_periods <- pdata.frame(Grunfeld_missing_periods)
pinv_missing_periods <- pGrunfeld_missing_periods$inv



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
if(!isTRUE(all.equal(is.pconsecutive(pHed), expected_Hed, check.names = FALSE))) stop("is.pconsecutive on pdata.frame: wrong result")
if(!isTRUE(all.equal(is.pconsecutive(pHed_missing_period), expected_Hed_missing_period, check.names = FALSE))) stop("is.pconsecutive on pdata.frame: wrong result")

# test on panelmodel object
estimation_pHed <- plm(mv ~ crim + indus, data = pHed)
estimation_pHed_missing_period <- plm(mv ~ crim + indus, data = pHed_missing_period)

is.pconsecutive(estimation_pHed)
is.pconsecutive(estimation_pHed_missing_period)

# test on pseries
pmv <- pHed$mv
pmv_missing_period <- pHed_missing_period$mv

if(!isTRUE(all.equal(is.pconsecutive(pmv), expected_Hed, check.names = FALSE))) stop("is.pconsecutive on pseries: wrong result")
if(!isTRUE(all.equal(is.pconsecutive(pmv_missing_period), expected_Hed_missing_period, check.names = FALSE))) stop("is.pconsecutive on pseries: wrong result")

######## with different data set "Gasoline" (has "named" individuals, not just numbers)
data("Gasoline", package = "plm")
pGasoline <- pdata.frame(Gasoline)

# test on data.frame
is.pconsecutive(Gasoline, index = c("country", "year"))

# test on pdata.frame
is.pconsecutive(pGasoline)



######### test for case with a time period missing from whole data set
data("Grunfeld", package = "plm")
obs_3rd <- 3 + 20*c(0:9)
Grunfeld_wo_1937 <- pdata.frame(Grunfeld[-obs_3rd, ])

expected_Grunfeld_wo_1937 <- rep(FALSE, 10)

if(!isTRUE(all.equal(is.pconsecutive(Grunfeld_wo_1937), expected_Grunfeld_wo_1937, check.names = FALSE)))
  stop("is.pconsecutive on pdata.frame: wrong result for a missing time period in whole data set")

if(!isTRUE(all.equal(is.pconsecutive(Grunfeld_wo_1937$inv), expected_Grunfeld_wo_1937, check.names = FALSE)))
  stop("is.pconsecutive on pdata.frame: wrong result for a missing time period in whole data set")


########## Tests with NA in individual and time index ###########


### test with NA in time index ###
data("Grunfeld", package = "plm") # get fresh Grunfeld (no NAs)
Grunfeld_NA_time <- Grunfeld
Grunfeld_NA_time[2, "year"] <- NA # firm 1, year 1936: year set to NA

pGrunfeld_NA_time <- pdata.frame(Grunfeld_NA_time)
# time index with NA is in pdata.frame
# it gets sorted to end of firm 1
head(pGrunfeld_NA_time, 21)

expected_NA_time <- c(NA, rep(TRUE, 9))
expected_NA_time_na.rm.tindex <- c(FALSE, rep(TRUE, 9))

is.pconsecutive(Grunfeld_NA_time)
is.pconsecutive(Grunfeld_NA_time, na.rm.tindex = FALSE)
is.pconsecutive(Grunfeld_NA_time, na.rm.tindex = TRUE)

if(!isTRUE(all.equal(is.pconsecutive(Grunfeld_NA_time), is.pconsecutive(pGrunfeld_NA_time))))
  stop("is.pconsecutive not equal for data.frame and pdata.frame with 'NA' in time index")
if(!isTRUE(all.equal(is.pconsecutive(pGrunfeld_NA_time), expected_NA_time, check.names=FALSE)))
  stop("is.pconsecutive: not expected result with 'NA' in time index")
if(!isTRUE(all.equal(is.pconsecutive(pGrunfeld_NA_time, na.rm.tindex = TRUE), expected_NA_time_na.rm.tindex, check.names=FALSE)))
  stop("is.pconsecutive(, na.rm.tindex = TRUE: not expected result with 'NA' in time index - there should be no NA values left")

### test with NA in individual index ###
# get fresh Grunfeld (no NAs)
Grunfeld_NA_ind <- Grunfeld
Grunfeld_NA_ind[3, "firm"] <- NA # firm 1, year 1937: firm set to NA
pGrunfeld_NA_ind <- pdata.frame(Grunfeld_NA_ind)

# individual index with NA is in pdata.frame
# it gets sorted to end of individuals
tail(pGrunfeld_NA_ind, 21)

expected_NA_ind <- c(FALSE, rep(TRUE, 9))

if(!isTRUE(all.equal(is.pconsecutive(Grunfeld_NA_ind), is.pconsecutive(pGrunfeld_NA_ind))))
  stop("is.pconsecutive not equal for data.frame and pdata.frame with 'NA' in individual index")
if(!isTRUE(all.equal(is.pconsecutive(pGrunfeld_NA_ind), expected_NA_ind, check.names=FALSE)))
  stop("is.pconsecutive: not expected result with 'NA' in individual index")



### test with NA in individual AND time index ###
# get fresh Grunfeld (no NAs)
Grunfeld_NA_id_time <- Grunfeld
Grunfeld_NA_id_time[4, c("firm", "year")] <- NA # firm 1, year 1938: firm and year set to NA
pGrunfeld_NA_id_time <- pdata.frame(Grunfeld_NA_id_time)

# individual and time index with NA is in pdata.frame
# it gets sorted to end of individuals
tail(pGrunfeld_NA_id_time, 21)

expected_NA_ind_time <- c(FALSE, rep(TRUE, 9))

if(!isTRUE(all.equal(is.pconsecutive(Grunfeld_NA_id_time), is.pconsecutive(pGrunfeld_NA_id_time))))
  stop("is.pconsecutive not equal for data.frame and pdata.frame with 'NA' in individual AND time index")
if(!isTRUE(all.equal(is.pconsecutive(pGrunfeld_NA_id_time), expected_NA_ind_time, check.names=FALSE)))
  stop("is.pconsecutive: not expected result with 'NA' in individual AND time index")



