# tests of lagt and lead fort (note the "t") respecting time periods (not just shifting of rows)
# -> there is also a test file (test_lag_lead_factor_levels.R) for plm::lag which does not
#    treat the time variable as a numeric value (merely shifts rows)
#
#  (1) test of lagging of index variable
#  (2) some dropped factor levels / whole period missing
#  (3) general tests
#  (4) tests with non-consecutive time periods
#  (5) lagt and lag should yield same results on data with consecutive time periods 

library(plm)

#### functions lagt and leadt not exported yet
#### source or assign in NAMESPACE the new lagging function "lagt" and overwrite original function names to demonstrate testfile
#
# comment this block to run the old lag and lead function

### by putting lagt/leadt in global environment:

lag          <- plm:::lagt.pseries
# lag.pseries  <- lagt.pseries
lead         <- plm:::leadt.pseries
# lead.pseries <- leadt.pseries

### by assigning in NAMESPACE:
#library(plm)
#assignInNamespace("lag.pseries",  plm:::lagt.pseries,  envir = as.environment("package:plm"))
#assignInNamespace("lead.pseries", plm:::leadt.pseries, envir = as.environment("package:plm"))
######################



library(plm)
data("Grunfeld", package = "plm")

Grunfeld$fac <- factor(c(200:2, 1))
Grunfeld <- pdata.frame(Grunfeld)




############## (1) test of lagging of index variable ##########
## test of lagging of index variable
lag(Grunfeld$firm)

# variable identical to an index "on character level"
Grunfeld$firm2 <- Grunfeld$firm
lag(Grunfeld$firm2)



############## (2.1) tests with eliminated factor levels ##########

# lag by 1 eliminates some factor levels (e.g. "1" in the last observations)
# from the sample's unique factor levels, but it should stay in the levels
lag(Grunfeld$fac)
if (!(length(unique(Grunfeld$fac)) == 200)) stop("wrong factor levels") # 200
if (!(length(unique(lag(Grunfeld$fac))) == 191)) stop("wrong actually uniquely occuring factor levels")  # 191
if (!(length(levels(lag(Grunfeld$fac))) == 200)) stop("wrong factor levels")  # 200

# lead eliminates e.g. level "200"
lead(Grunfeld$fac)
if (!(length(unique(Grunfeld$fac)) == 200)) stop("wrong factor levels") # 200
if (!(length(unique(lead(Grunfeld$fac))) == 191)) stop("wrong factor levels") # 191
if (!(length(levels(lead(Grunfeld$fac))) == 200)) stop("wrong factor levels")  # 200



############### (2.2) test for case with a time period missing from whole data set
data("Grunfeld", package = "plm")
obs_3rd <- 3 + 20*c(0:9)
Grunfeld_wo_1937 <- pdata.frame(Grunfeld[-obs_3rd, ])

# illustration:
levels(Grunfeld_wo_1937$year) # no year 1937 anymore and no level for 1937 anymore
as.numeric(Grunfeld_wo_1937$year)                # as.numeric produces a consecutive series!
any(diff(as.numeric(Grunfeld_wo_1937$year)) > 1) # -> no gap detected

as.numeric(as.character(Grunfeld_wo_1937$year)) # use as.character before as.numeric!
any(diff(as.numeric(as.character(Grunfeld_wo_1937$year))) > 1) # -> gap now detected

# formal test:
if (!is.na(lag( Grunfeld_wo_1937$inv)["1-1938"])) stop("missing time period not detected (year 1937 is missing from whole data set)")
if (!is.na(lead(Grunfeld_wo_1937$inv)["1-1936"])) stop("missing time period not detected (year 1937 is missing from whole data set)")



############## (3) some general tests ##########
data("Grunfeld", package = "plm")

Grunfeld$fac <- factor(c(200:2, 1))
Grunfeld <- pdata.frame(Grunfeld)
## some more general testing of lag and lead
# do nothing
if (!isTRUE(all.equal(lag(Grunfeld$fac, 0), Grunfeld$fac))) stop("'lag( , 0)' not equal to 'do nothing'")
if (!isTRUE(all.equal(lead(Grunfeld$fac, 0), Grunfeld$fac))) stop("'lead( , 0)' not equal to 'do nothing'")
  # identical is even stricter than all.equal
  if (!identical(lag(Grunfeld$fac, 0), Grunfeld$fac)) stop("'lag( , 0)' not identical to 'do nothing'")
  if (!identical(lead(Grunfeld$fac, 0), Grunfeld$fac)) stop("'lead( , 0)' not identical to 'do nothing'")


# lag( , -k) == lead( , k)
if (!isTRUE(all.equal(lag(Grunfeld$fac, -1), lead(Grunfeld$fac, 1)))) stop("'lag( , -1)' not equal to 'lead( , 1)'")
if (!isTRUE(all.equal(lag(Grunfeld$fac, 1), lead(Grunfeld$fac, -1)))) stop("'lag( , 1)' not equal to 'lead( , -1)'")
  # identical is even stricter than all.equal
  if (!identical(lag(Grunfeld$fac, -1), lead(Grunfeld$fac, 1))) stop("'lag( , -1)' not identical to 'lead( , 1)'")
  if (!identical(lag(Grunfeld$fac, 1), lead(Grunfeld$fac, -1))) stop("'lag( , 1)' not identical to 'lead( , -1)'")

# with numeric
if (!isTRUE(all.equal(lag(Grunfeld$inv, -1), lead(Grunfeld$inv, 1)))) stop("'lag( , -1)' not equal to 'lead( , 1)'")
if (!isTRUE(all.equal(lag(Grunfeld$inv, 1), lead(Grunfeld$inv, -1)))) stop("'lag( , 1)' not equal to 'lead( , -1)'")

  if (!identical(lag(Grunfeld$inv, -1), lead(Grunfeld$inv, 1))) stop("'lag( , -1)' not identical to 'lead( , 1)'")
  if (!identical(lag(Grunfeld$inv, 1), lead(Grunfeld$inv, -1))) stop("'lag( , 1)' not identical to 'lead( , -1)'")



# with logical
Grunfeld$log <- rep(c(T, T, F, T), 50)
if (!isTRUE(all.equal(lag(Grunfeld$log, -1), lead(Grunfeld$log, 1)))) stop("'lag( , -1)' not equal to 'lead( , 1)'")
if (!isTRUE(all.equal(lag(Grunfeld$log, 1), lead(Grunfeld$log, -1)))) stop("'lag( , 1)' not equal to 'lead( , -1)'")

  if (!identical(lag(Grunfeld$log, -1), lead(Grunfeld$log, 1))) stop("'lag( , -1)' not identical to 'lead( , 1)'")
  if (!identical(lag(Grunfeld$log, 1), lead(Grunfeld$log, -1))) stop("'lag( , 1)' not identical to 'lead( , -1)'")


## other k's
if (!isTRUE(all.equal(lag(Grunfeld$inv, -5), lead(Grunfeld$inv, 5)))) stop("'lag( , -1)' not equal to 'lead( , 1)'")
if (!isTRUE(all.equal(lag(Grunfeld$inv, 5), lead(Grunfeld$inv, -5)))) stop("'lag( , 1)' not equal to 'lead( , -1)'")

if (!isTRUE(all.equal(lag(Grunfeld$inv, -3), lead(Grunfeld$inv, 3)))) stop("'lag( , -1)' not equal to 'lead( , 1)'")
if (!isTRUE(all.equal(lag(Grunfeld$inv, 3), lead(Grunfeld$inv, -3)))) stop("'lag( , 1)' not equal to 'lead( , -1)'")

  if (!identical(lag(Grunfeld$inv, -3), lead(Grunfeld$inv, 3))) stop("'lag( , -1)' not identical to 'lead( , 1)'")
  if (!identical(lag(Grunfeld$inv, 3), lead(Grunfeld$inv, -3))) stop("'lag( , 1)' not identical to 'lead( , -1)'")



# should be all NA
if(!isTRUE(all(is.na(lag(Grunfeld$inv, 20))))) stop("all-NA case not correct") # 20 is no of obs per id
if(!isTRUE(all(is.na(lag(Grunfeld$inv, 21))))) stop("all-NA case not correct") # 21 is more than obs per id available
if(!isTRUE(all(is.na(lead(Grunfeld$inv, 20))))) stop("all-NA case not correct") # 20 is no of obs per id
if(!isTRUE(all(is.na(lead(Grunfeld$inv, 21))))) stop("all-NA case not correct") # 21 is more than obs per id available

## length(k) > 1
lag(Grunfeld$inv, c(-2, -1, 0, 1, 2))
lead(Grunfeld$inv, c(-2, -1, 0, 1, 2))
if(!isTRUE(all.equal(lag(Grunfeld$inv, c(-2, -1, 0, 1, 2)),
                     lead(Grunfeld$inv, -1*c(-2, -1, 0, 1, 2)), check.attributes = FALSE))) stop("'lag( , c())' not equal to 'lead( , -1*c())'")

# other data set (different time periods)
# Hedonic is an unbalanced panel, townid is the individual index
data("Hedonic", package = "plm")
Hed <- pdata.frame(Hedonic, index = "townid")
head(Hed$age, 20)
head(lag(Hed$age), 20)
head(lag(Hed$age, c(0,1,2)), 20)
if (!isTRUE(all.equal(lag(Hed$age, c(0,1,2,3,4,5)), lead(Hed$age, -1*c(0,1,2,3,4,5)), check.attributes = FALSE))) stop("'lag( , 1)' not equal to 'lead( , -1)'")



# diff
if (!isTRUE(all.equal(diff(Grunfeld$inv), Grunfeld$inv - lag(Grunfeld$inv)))) stop("'diff()' not corresponding to differences with 'lag()'")
if (!isTRUE(all.equal(diff(Grunfeld$inv, 2), Grunfeld$inv - lag(Grunfeld$inv, 2)))) stop("'diff( , 2)' not corresponding to differences with 'lag( , 2)'")




############## (4) test with non-consecutive time periods ####
data("Grunfeld", package = "plm")

pGrunfeld_missing_period <- pdata.frame(Grunfeld[-2, ]) # delete one time period of first individual (1-1936 is missing (not NA))

is.pconsecutive(pGrunfeld_missing_period)

head(pGrunfeld_missing_period$inv, 25)
head(test_Grun_miss_p_lag1 <- lag(pGrunfeld_missing_period$inv), 25) # correct: additional NA for the missing time period is introduced at 1-1937
head(lag(pGrunfeld_missing_period$inv, 2), 25)
head(test_Grun_miss_p_lag3 <- lag(pGrunfeld_missing_period$inv, 3), 25) # correct 1-1938 is NA but should be non-NA (former 1-1935: 317.6)

### formal test for correct value
if(!is.na(test_Grun_miss_p_lag1["1-1937"])) stop("lag(pGrunfeld_missing_period$inv, 1)' for '1-1937' contains a value but should be 'NA'")


if (!is.na(test_Grun_miss_p_lag3["1-1938"])) {
  if(!isTRUE(all.equal(test_Grun_miss_p_lag3["1-1938"], pGrunfeld_missing_period$inv["1-1935"], check.names = FALSE)))
    stop("'lag(pGrunfeld_missing_period$inv, 3)' for '1-1938' is not the expected value of '1-1935' of original data 'pGrunfeld_missing_period$inv'")
  } else stop("'lag(pGrunfeld_missing_period$inv, 3)' is NA for '1-1938' but should be the value of '1-1935' from original data 'pGrunfeld_missing_period$inv'")


length(pGrunfeld_missing_period$inv) == length(lag(pGrunfeld_missing_period$inv))

# with different data set
data("Hedonic", package = "plm")
Hed_missing_period <- pdata.frame(Hedonic, index = "townid")
Hed_missing_period <- as.data.frame(Hed_missing_period)
Hed_missing_period <- Hed_missing_period[-c(5,11), ]  # delete 3-2 and 4-5
Hed_missing_period <- pdata.frame(Hed_missing_period, index = c("townid", "time"))

is.pconsecutive(Hed_missing_period)

head(Hed_missing_period$age, 20)
head(test_Hed_miss_p_lag1 <- lag(Hed_missing_period$age), 20)    # correct: lag(, 1): additional NAs introduced at (among others) 3-3 and 4-6
head(test_Hed_miss_p_lag2 <- lag(Hed_missing_period$age, 2), 20) # correct: lag(, 2): 4-6 is NA but should be non-NA (should be former 4-4: 85.89996)
                                                                 #                    3-3 is NA but should be non-NA (should be former 3-1: 45.79999)

head(lag(Hed_missing_period$age, c(0,1,2)), 20) # view all at once

### formal tests for correct values
# lag(, 1)
if(!is.na(test_Hed_miss_p_lag1["3-3"])) stop("lag(Hed_missing_period$age, 1)' for '3-3' contains a value but should be 'NA'")
if(!is.na(test_Hed_miss_p_lag1["4-6"])) stop("lag(Hed_missing_period$age, 1)' for '4-6' contains a value but should be 'NA'")

# lag(, 2)
if (!is.na(test_Hed_miss_p_lag2["3-3"])) {
  if(!isTRUE(all.equal(test_Hed_miss_p_lag2["3-3"], Hed_missing_period$age["3-1"], check.names = FALSE)))
    stop("'lag(Hed_missing_period$age, 2)' for '3-3' is not the expected value of '3-1' of original data 'Hed_missing_period$age'")
  } else stop("'lag(Hed_missing_period$age, 2)' is NA for '3-3' but should be the value of '3-1' from original data 'Hed_missing_period$age'")

if (!is.na(test_Hed_miss_p_lag2["4-6"])) {
  if(!isTRUE(all.equal(test_Hed_miss_p_lag2["4-6"], Hed_missing_period$age["4-4"], check.names = FALSE)))
    stop("'lag(Hed_missing_period$age, 2)' for '4-6' is not the expected value of '4-4' of original data 'Hed_missing_period$age'")
  } else stop("'lag(Hed_missing_period$age, 2)' is NA for '4-6' but should be the value of '4-4' from original data 'Hed_missing_period$age'")

##### delete two consecutive time periods
data("Hedonic", package = "plm")
Hed_missing_period2 <- pdata.frame(Hedonic, index = "townid")
Hed_missing_period2 <- as.data.frame(Hed_missing_period2)
Hed_missing_period2 <- Hed_missing_period2[-c(5,11,12), ]  # delete 3-2, 4-5, 4-6
Hed_missing_period2 <- pdata.frame(Hed_missing_period2, index = c("townid", "time"))

is.pconsecutive(Hed_missing_period2)

head(Hed_missing_period2$age, 20)
head(test_Hed_miss2_p_lag1 <- lag(Hed_missing_period2$age), 20)    # correct: lag(, 1): additional NAs introduced at 3-3 and 4-6
head(test_Hed_miss2_p_lag2 <- lag(Hed_missing_period2$age, 2), 20) # correct: 3-3 is NA but should be former 3-1 (45.79999)
head(test_Hed_miss2_p_lag3 <- lag(Hed_missing_period2$age, 3), 20) # correct: 4-7 is NA but should be former 4-4 (85.89996)
head(lag(Hed_missing_period2$age, c(0,1,2,3)), 20) # view all at once

### formal tests for correct values

## lag(, 2)
if (!is.na(test_Hed_miss2_p_lag2["3-3"])) {
 if(!isTRUE(all.equal(test_Hed_miss2_p_lag2["3-3"], Hed_missing_period2$age["3-1"], check.names = FALSE)))
   stop("'lag(Hed_missing_period2$age, 2)' for '3-3' is not the expected value of '3-1' of original data 'Hed_missing_period2$age'")
 } else stop("'lag(Hed_missing_period2$age, 2)' is NA for '3-3' but should be the value of '3-1' from original data 'Hed_missing_period2$age'")

# lag(, 3)
if (!is.na(test_Hed_miss2_p_lag3["4-7"])) {
 if(!isTRUE(all.equal(test_Hed_miss2_p_lag3["4-7"], Hed_missing_period2$age["4-4"], check.names = FALSE)))
   stop("'lag(Hed_missing_period2$age, 3)' for '4-7' is not the expected value of '4-4' of original data 'Hed_missing_period2$age'")
 } else stop("'lag(Hed_missing_period2$age, 3)' is NA for '4-7' but should be the value of '4-4' from original data 'Hed_missing_period2$age'")



############ (5) lagt and lag should yield same results on data with consecutive time periods ####################
data("Grunfeld", package = "plm")
Grunfeld <- pdata.frame(Grunfeld)

if (!isTRUE(identical(plm:::lag.pseries(Grunfeld$inv, k = c(-3,-2,-1,0,1,2,3)), plm:::lagt.pseries(Grunfeld$inv, k = c(-3,-2,-1,0,1,2,3)))))
  stop("lag and lagt not same on consecutive data.frame (but must be!)")





############## messy data set with lots of NAs ############
#### commented because it needs several extra package and loads data from the internet
## library(haven)
##
# nlswork_r8 <- haven::read_dta("http://www.stata-press.com/data/r8/nlswork.dta")
# nlswork_r8 <- as.data.frame(lapply(nlswork_r8, function(x) {attr(x, "label") <- NULL; x}))
# pnlswork_r8 <- pdata.frame(nlswork_r8, index=c("idcode", "year"), drop.index=F)
# 
# 
# ### on a consecutive pdata.frame, plm:::lag and plm:::lagt should yield same results (if no NA in id or time)
# pnlswork_r8_consec <- make.pconsecutive(pnlswork_r8)
# pnlswork_r8_consec_bal <- make.pconsecutive(pnlswork_r8, balanced = TRUE)
# pnlswork_r8_bal <- make.pbalanced(pnlswork_r8, balanced = TRUE)
# 
# if (!all.equal(plm:::lag.pseries(pnlswork_r8_consec$age), plm:::lagt.pseries(pnlswork_r8_consec$age)))
#   stop("lag and lagt not same on consecutive data.frame (but must be!)")
# 
# if (!all.equal(plm:::lag.pseries(pnlswork_r8_consec_bal$age), plm:::lagt.pseries(pnlswork_r8_consec_bal$age)))
#   stop("lag and lagt not same on consecutive data.frame (but must be!)")
# 
# ########### compare results to statar::tlag ########################
# #### commented because it needs several extra packages
# ## statar::tlag (and tlead) also works on the numbers of the time variable
# ##
# ### devtools::install_github("matthieugomez/statar")
# ## library(dplyr)
# ##
# # ## lag 1
# nlswork_r8_statar <- dplyr::mutate(dplyr::group_by(nlswork_r8, idcode), agel = statar::tlag(age, n = 1, time = year))
# if (!isTRUE(all.equal(nlswork_r8_statar$agel, as.numeric(plm:::lagt.pseries(pnlswork_r8$age))))) stop("not same")
# ## lag 2
# nlswork_r8_statar <- dplyr::mutate(dplyr::group_by(nlswork_r8, idcode), agel2 = statar::tlag(age, n = 2, time = year))
# if (!isTRUE(all.equal(nlswork_r8_statar$agel2, as.numeric(plm:::lagt.pseries(pnlswork_r8$age, 2))))) stop("not same")
# ## lag 3
# nlswork_r8_statar <- dplyr::mutate(dplyr::group_by(nlswork_r8, idcode), agel2 = statar::tlag(age, n = 3, time = year))
# if (!isTRUE(all.equal(nlswork_r8_statar$agel2, as.numeric(plm:::lagt.pseries(pnlswork_r8$age, 3))))) stop("not same")
# 
# ## lead 1
# nlswork_r8_statar <- dplyr::mutate(dplyr::group_by(nlswork_r8, idcode), agelead = statar::tlead(age, n = 1, time = year))
# if (!isTRUE(all.equal(nlswork_r8_statar$agelead, as.numeric(plm:::leadt.pseries(pnlswork_r8$age))))) stop("not same")
# ## lead 2
# nlswork_r8_statar <- dplyr::mutate(dplyr::group_by(nlswork_r8, idcode), agelead2 = statar::tlead(age, n = 2, time = year))
# if (!isTRUE(all.equal(nlswork_r8_statar$agelead2, as.numeric(plm:::leadt.pseries(pnlswork_r8$age, 2))))) stop("not same")
# ## lead 3
# nlswork_r8_statar <- dplyr::mutate(dplyr::group_by(nlswork_r8, idcode), agelead3 = statar::tlead(age, n = 3, time = year))
# if (!isTRUE(all.equal(nlswork_r8_statar$agelead3, as.numeric(plm:::leadt.pseries(pnlswork_r8$age, 3))))) stop("not same")


