# tests of lag and lead
#  (1) test of lagging of index variable
#  (2) some dropped factor levels / whole period missing
#  (3) general tests
#  (4) tests with non-consecutive time periods
# 
# ad (2) error prior to rev. 207:
# the lagging resulted in an error with factors in some cases,
# because some factor levels can get lost due to the lagging
# and the old code was not capable to manage this
#
# fixed in rev. 207 with better handling of factor levels and simpler code

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
length(unique(Grunfeld$fac)) # 200
length(unique(lag(Grunfeld$fac))) # 191

# lead eliminates e.g. level "200"
lead(Grunfeld$fac)
length(unique(lead(Grunfeld$fac))) # 191



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
if(!isTRUE(all.equal(diff(Grunfeld$inv), Grunfeld$inv - lag(Grunfeld$inv)))) stop("'diff()' not corresponding to differences with 'lag()'")
if(!isTRUE(all.equal(diff(Grunfeld$inv, 2), Grunfeld$inv - lag(Grunfeld$inv, 2)))) stop("'diff( , 2)' not corresponding to differences with 'lag( , 2)'")




############## (4) test with non-consecutive time periods ####
# rev. 209 (and former revisions): lag introduces to many NAs for lag(, k) with k>1 and non-consecutive time periods
# -> see tests below (active formal tests once fixed)
data("Grunfeld", package = "plm")

pGrunfeld_missing_period <- pdata.frame(Grunfeld[-2, ]) # delete one time period of first individual (1-1936 is missing (not NA))

is.pconsecutive(pGrunfeld_missing_period)

head(pGrunfeld_missing_period$inv, 25)
head(test_Grun_miss_p_lag1 <- lag(pGrunfeld_missing_period$inv), 25) # correct: additional NA for the missing time period is introduced at 1-1937
head(lag(pGrunfeld_missing_period$inv, 2), 25)
head(test_Grun_miss_p_lag3 <- lag(pGrunfeld_missing_period$inv, 3), 25) # not correct 1-1938 is NA but should be non-NA (former 1-1935: 317.6)

### formal test for correct value
if(!is.na(test_Grun_miss_p_lag1["1-1937"])) stop("lag(pGrunfeld_missing_period$inv, 1)' for '1-1937' contains a value but should be 'NA'")


# if (!is.na(test_Grun_miss_p_lag3["1-1938"])) {
#   if(!isTRUE(all.equal(test_Grun_miss_p_lag3["1-1938"], pGrunfeld_missing_period$inv["1-1935"], check.names = FALSE)))
#     stop("'lag(pGrunfeld_missing_period$inv, 3)' for '1-1938' is not the expected value of '1-1935' of original data 'pGrunfeld_missing_period$inv'")
#   } else stop("'lag(pGrunfeld_missing_period$inv, 3)' is NA for '1-1938' but should be the value of '1-1935' from original data 'pGrunfeld_missing_period$inv'")


length(pGrunfeld_missing_period$inv) == length(lag(pGrunfeld_missing_period$inv))

# with different data set
data("Hedonic", package = "plm")
Hed_missing_period <- pdata.frame(Hedonic, index = "townid")
Hed_missing_period <- as.data.frame(Hed_missing_period)
Hed_missing_period <- Hed_missing_period[-c(5,11), ]  # delete 3-2 and 4-5
Hed_missing_period <- pdata.frame(Hed_missing_period, index = c("townid", "time"))

is.pconsecutive(Hed_missing_period)

head(Hed_missing_period$age, 20)
head(test_Hed_miss_p_lag1 <- lag(Hed_missing_period$age), 20) # correct: lag(, 1): additional NAs introduced at 3-3 and 4-6
head(test_Hed_miss_p_lag2 <- lag(Hed_missing_period$age, 2), 20) # not correct: lag(, 2): 4-6 is NA but should be non-NA (should be former 4-4: 85.89996)
                                                                 #                        3-3 is NA but should be non-NA (should be former 3-1: 45.79999)

head(lag(Hed_missing_period$age, c(0,1,2)), 20) # view all at once

### formal tests for correct values
# lag(, 1)
if(!is.na(test_Hed_miss_p_lag1["3-3"])) stop("lag(Hed_missing_period$age, 1)' for '3-3' contains a value but should be 'NA'")
if(!is.na(test_Hed_miss_p_lag1["4-6"])) stop("lag(Hed_missing_period$age, 1)' for '4-6' contains a value but should be 'NA'")

# lag(, 2)
# if (!is.na(test_Hed_miss_p_lag2["3-3"])) {
#   if(!isTRUE(all.equal(test_Hed_miss_p_lag2["3-3"], Hed_missing_period$age["3-1"], check.names = FALSE)))
#     stop("'lag(Hed_missing_period$age, 2)' for '3-3' is not the expected value of '3-1' of original data 'Hed_missing_period$age'")
#   } else stop("'lag(Hed_missing_period$age, 2)' is NA for '3-3' but should be the value of '3-1' from original data 'Hed_missing_period$age'")

# if (!is.na(test_Hed_miss_p_lag2["4-6"])) {
#   if(!isTRUE(all.equal(test_Hed_miss_p_lag2["4-6"], Hed_missing_period$age["4-4"], check.names = FALSE)))
#     stop("'lag(Hed_missing_period$age, 2)' for '4-6' is not the expected value of '4-4' of original data 'Hed_missing_period$age'")
#   } else stop("'lag(Hed_missing_period$age, 2)' is NA for '4-6' but should be the value of '4-4' from original data 'Hed_missing_period$age'")

##### delete two consecutive time periods
data("Hedonic", package = "plm")
Hed_missing_period2 <- pdata.frame(Hedonic, index = "townid")
Hed_missing_period2 <- as.data.frame(Hed_missing_period2)
Hed_missing_period2 <- Hed_missing_period2[-c(5,11,12), ]  # delete 3-2, 4-5, 4-6
Hed_missing_period2 <- pdata.frame(Hed_missing_period2, index = c("townid", "time"))

is.pconsecutive(Hed_missing_period2)

head(Hed_missing_period2$age, 20)
head(test_Hed_miss2_p_lag1 <- lag(Hed_missing_period2$age), 20) # correct: lag(, 1): additional NAs introduced at 3-3 and 4-6
head(test_Hed_miss2_p_lag2 <- lag(Hed_missing_period2$age, 2), 20) # not correct: 3-3 is NA but should be former 3-1 (45.79999)
head(test_Hed_miss2_p_lag3 <- lag(Hed_missing_period2$age, 3), 20) # not correct: 4-7 is NA but should be former 4-4 (85.89996)
head(lag(Hed_missing_period2$age, c(0,1,2,3)), 20) # view all at once

### formal tests for correct values

## lag(, 2)
#if (!is.na(test_Hed_miss2_p_lag2["3-3"])) {
#  if(!isTRUE(all.equal(test_Hed_miss2_p_lag2["3-3"], Hed_missing_period2$age["3-1"], check.names = FALSE)))
#    stop("'lag(Hed_missing_period2$age, 2)' for '3-3' is not the expected value of '3-1' of original data 'Hed_missing_period2$age'")
#  } else stop("'lag(Hed_missing_period2$age, 2)' is NA for '3-3' but should be the value of '3-1' from original data 'Hed_missing_period2$age'")

## lag(, 3)
#if (!is.na(test_Hed_miss2_p_lag3["4-7"])) {
#  if(!isTRUE(all.equal(test_Hed_miss2_p_lag3["4-7"], Hed_missing_period2$age["4-4"], check.names = FALSE)))
#    stop("'lag(Hed_missing_period2$age, 3)' for '4-7' is not the expected value of '4-4' of original data 'Hed_missing_period2$age'")
#  } else stop("'lag(Hed_missing_period2$age, 3)' is NA for '4-7' but should be the value of '4-4' from original data 'Hed_missing_period2$age'")
