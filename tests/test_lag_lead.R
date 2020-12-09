# tests of lagr and leadr (shifting of rows), i.e., lag(..., shift = "row")
#
#  (1) test of lagging of index variable
#  (2) some dropped factor levels / whole period missing
#  (3) general tests
#  (4) test with non-consecutive time periods
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
plm:::lagr.pseries(Grunfeld$firm)

# variable identical to an index "on character level"
Grunfeld$firm2 <- Grunfeld$firm
plm:::lagr.pseries(Grunfeld$firm2)


############## (2.1) tests with eliminated factor levels ##########

# lag by 1 eliminates some factor levels (e.g., "1" in the last observations)
# from the sample's unique factor levels, but it should stay in the levels
plm:::lagr.pseries(Grunfeld$fac)
if (!(length(unique(Grunfeld$fac)) == 200)) stop("wrong factor values") # 200
if (!(length(unique(plm:::lagr.pseries(Grunfeld$fac))) == 191)) stop("plm:::lagr.pseries: wrong actually uniquely occuring factor values")  # 191
if (!(length(levels(plm:::lagr.pseries(Grunfeld$fac))) == 200)) stop("wrong factor levels")  # 200

# plm::lead eliminates e.g., level "200"
plm:::leadr.pseries(Grunfeld$fac)
if (!(length(unique(Grunfeld$fac)) == 200)) stop("wrong factor levels") # 200
if (!(length(unique(plm:::leadr.pseries(Grunfeld$fac))) == 191)) stop("plm:::leadr.pseries: wrong actually uniquely occuring factor values") # 191
if (!(length(levels(plm:::leadr.pseries(Grunfeld$fac))) == 200)) stop("plm:::leadr.pseries: wrong factor levels")  # 200


############### (2.2) test for case with a time period missing from whole data set
data("Grunfeld", package = "plm")
obs_3rd <- 3 + 20*c(0:9)
Grunfeld_wo_1937 <- pdata.frame(Grunfeld[-obs_3rd, ])

# illustration:
levels(Grunfeld_wo_1937$year) # no year 1937 anymore and no level for 1937 anymore (a year in between, i.e., not consecutive series anymore)
as.numeric(Grunfeld_wo_1937$year)                # as.numeric produces a consecutive series!
any(diff(as.numeric(Grunfeld_wo_1937$year)) > 1) # FALSE -> no gap detected

as.numeric(as.character(Grunfeld_wo_1937$year)) # use as.character before as.numeric!
any(diff(as.numeric(as.character(Grunfeld_wo_1937$year))) > 1) # TRUE -> gap now detected



############## (3) some general tests ##########
data("Grunfeld", package = "plm")

Grunfeld$fac <- factor(c(200:2, 1))
Grunfeld <- pdata.frame(Grunfeld)
## some more general testing of plm::lagr and plm:::leadr
# do nothing
if (!isTRUE(all.equal(plm:::lagr.pseries(Grunfeld$fac, 0), Grunfeld$fac)))  stop("'plm:::lagr.pseries( , 0)' not equal to 'do nothing'")
if (!isTRUE(all.equal(plm:::leadr.pseries(Grunfeld$fac, 0), Grunfeld$fac))) stop("'plm:::leadr.pseries( , 0)' not equal to 'do nothing'")
  # identical is even stricter than all.equal
  if (!identical(plm:::lagr.pseries(Grunfeld$fac, 0), Grunfeld$fac))  stop("'plm:::lagr.pseries( , 0)' not identical to 'do nothing'")
  if (!identical(plm:::leadr.pseries(Grunfeld$fac, 0), Grunfeld$fac)) stop("'plm:::leadr.pseries( , 0)' not identical to 'do nothing'")


# plm:::lagr.pseries( , -k) == plm:::leadr.pseries( , k)
if (!isTRUE(all.equal(plm:::lagr.pseries(Grunfeld$fac, -1), plm:::leadr.pseries(Grunfeld$fac, 1))))  stop("'plm:::lagr.pseries( , -1)' not equal to 'plm:::leadr.pseries( , 1)'")
if (!isTRUE(all.equal(plm:::lagr.pseries(Grunfeld$fac, 1),  plm:::leadr.pseries(Grunfeld$fac, -1)))) stop("'plm:::lagr.pseries( , 1)' not equal to 'plm:::leadr.pseries( , -1)'")
  # identical is even stricter than all.equal
  if (!identical(plm:::lagr.pseries(Grunfeld$fac, -1), plm:::leadr.pseries(Grunfeld$fac, 1))) stop("'plm:::lagr.pseries( , -1)' not identical to 'plm:::leadr.pseries( , 1)'")
  if (!identical(plm:::lagr.pseries(Grunfeld$fac, 1), plm:::leadr.pseries(Grunfeld$fac, -1))) stop("'plm:::lagr.pseries( , 1)' not identical to 'plm:::leadr.pseries( , -1)'")

# with numeric
if (!isTRUE(all.equal(plm:::lagr.pseries(Grunfeld$inv, -1), plm:::leadr.pseries(Grunfeld$inv, 1)))) stop("'plm:::lagr.pseries( , -1)' not equal to 'plm:::leadr.pseries( , 1)'")
if (!isTRUE(all.equal(plm:::lagr.pseries(Grunfeld$inv, 1), plm:::leadr.pseries(Grunfeld$inv, -1)))) stop("'plm:::lagr.pseries( , 1)' not equal to 'plm:::leadr.pseries( , -1)'")
  # identical is even stricter than all.equal
  if (!identical(plm:::lagr.pseries(Grunfeld$inv, -1), plm:::leadr.pseries(Grunfeld$inv, 1))) stop("'plm:::lagr.pseries( , -1)' not identical to 'plm:::leadr.pseries( , 1)'")
  if (!identical(plm:::lagr.pseries(Grunfeld$inv, 1), plm:::leadr.pseries(Grunfeld$inv, -1))) stop("'plm:::lagr.pseries( , 1)' not identical to 'plm:::leadr.pseries( , -1)'")



# with logical
Grunfeld$log <- rep(c(T, T, F, T), 50)
if (!isTRUE(all.equal(plm:::lagr.pseries(Grunfeld$log, -1), plm:::leadr.pseries(Grunfeld$log, 1)))) stop("'plm:::lagr.pseries( , -1)' not equal to 'plm:::leadr.pseries( , 1)'")
if (!isTRUE(all.equal(plm:::lagr.pseries(Grunfeld$log, 1), plm:::leadr.pseries(Grunfeld$log, -1)))) stop("'plm:::lagr.pseries( , 1)' not equal to 'plm:::leadr.pseries( , -1)'")
  # identical is even stricter than all.equal
  if (!identical(plm:::lagr.pseries(Grunfeld$log, -1), plm:::leadr.pseries(Grunfeld$log, 1))) stop("'plm:::lagr.pseries( , -1)' not identical to 'plm:::leadr.pseries( , 1)'")
  if (!identical(plm:::lagr.pseries(Grunfeld$log, 1), plm:::leadr.pseries(Grunfeld$log, -1))) stop("'plm:::lagr.pseries( , 1)' not identical to 'plm:::leadr.pseries( , -1)'")


## other k's
if (!isTRUE(all.equal(plm:::lagr.pseries(Grunfeld$inv, -5), plm:::leadr.pseries(Grunfeld$inv, 5)))) stop("'plm:::lagr.pseries( , -1)' not equal to 'plm:::leadr.pseries( , 1)'")
if (!isTRUE(all.equal(plm:::lagr.pseries(Grunfeld$inv, 5), plm:::leadr.pseries(Grunfeld$inv, -5)))) stop("'plm:::lagr.pseries( , 1)' not equal to 'plm:::leadr.pseries( , -1)'")

if (!isTRUE(all.equal(plm:::lagr.pseries(Grunfeld$inv, -3), plm:::leadr.pseries(Grunfeld$inv, 3)))) stop("'plm:::lagr.pseries( , -1)' not equal to 'plm:::leadr.pseries( , 1)'")
if (!isTRUE(all.equal(plm:::lagr.pseries(Grunfeld$inv, 3), plm:::leadr.pseries(Grunfeld$inv, -3)))) stop("'plm:::lagr.pseries( , 1)' not equal to 'plm:::leadr.pseries( , -1)'")

  if (!identical(plm:::lagr.pseries(Grunfeld$inv, -3), plm:::leadr.pseries(Grunfeld$inv, 3))) stop("'plm:::lagr.pseries( , -1)' not identical to 'plm:::leadr.pseries( , 1)'")
  if (!identical(plm:::lagr.pseries(Grunfeld$inv, 3), plm:::leadr.pseries(Grunfeld$inv, -3))) stop("'plm:::lagr.pseries( , 1)' not identical to 'plm:::leadr.pseries( , -1)'")



# should be all NA
if(!isTRUE(all(is.na(plm:::lagr.pseries(Grunfeld$inv, 20))))) stop("all-NA case not correct") # 20 is no of obs per id
if(!isTRUE(all(is.na(plm:::lagr.pseries(Grunfeld$inv, 21))))) stop("all-NA case not correct") # 21 is more than obs per id available
if(!isTRUE(all(is.na(plm:::leadr.pseries(Grunfeld$inv, 20))))) stop("all-NA case not correct") # 20 is no of obs per id
if(!isTRUE(all(is.na(plm:::leadr.pseries(Grunfeld$inv, 21))))) stop("all-NA case not correct") # 21 is more than obs per id available

## length(k) > 1
plm:::lagr.pseries(Grunfeld$inv, c(-2, -1, 0, 1, 2))
plm:::leadr.pseries(Grunfeld$inv, c(-2, -1, 0, 1, 2))
if(!isTRUE(all.equal(plm:::lagr.pseries(Grunfeld$inv, c(-2, -1, 0, 1, 2)),
                     plm:::leadr.pseries(Grunfeld$inv, -1*c(-2, -1, 0, 1, 2)), check.attributes = FALSE))) stop("'plm:::lagr.pseries( , c())' not equal to 'plm:::leadr.pseries( , -1*c())'")

# other data set (different time periods)
# Hedonic is an unbalanced panel, townid is the individual index
data("Hedonic", package = "plm")
Hed <- pdata.frame(Hedonic, index = "townid")
head(Hed$age, 20)
head(plm:::lagr.pseries(Hed$age), 20)
head(plm:::lagr.pseries(Hed$age, c(0,1,2)), 20)
if (!isTRUE(all.equal(plm:::lagr.pseries(Hed$age, c(0,1,2,3,4,5)), plm:::leadr.pseries(Hed$age, -1*c(0,1,2,3,4,5)), check.attributes = FALSE))) stop("'plm:::lagr.pseries( , 1)' not equal to 'plm:::leadr.pseries , -1)'")



# diff
if (!isTRUE(all.equal(diff(Grunfeld$inv), Grunfeld$inv - plm:::lagr.pseries(Grunfeld$inv)))) stop("'diff()' not corresponding to differences with 'plm:::lagr.pseries()'")
if (!isTRUE(all.equal(diff(Grunfeld$inv, 2), Grunfeld$inv - plm:::lagr.pseries(Grunfeld$inv, 2)))) stop("'diff( , 2)' not corresponding to differences with 'plm:::lagr.pseries( , 2)'")



############## (4) test with non-consecutive time periods ####
# this is to demonstrate the behaviour of lagr for non-consecutive data
# 
data("Grunfeld", package = "plm")

pGrunfeld_missing_period <- pdata.frame(Grunfeld[-2, ]) # delete one time period of first individual (1-1936 is missing (not NA))

is.pconsecutive(pGrunfeld_missing_period)

head(pGrunfeld_missing_period$inv, 25)
head(test_Grun_miss_p_lag1 <- plm:::lagr.pseries(pGrunfeld_missing_period$inv), 25) # correct: additional NA for the missing time period is introduced at 1-1937
head(plm:::lagr.pseries(pGrunfeld_missing_period$inv, 2), 25)
head(test_Grun_miss_p_lag3 <- plm:::lagr.pseries(pGrunfeld_missing_period$inv, 3), 25) # 1-1938 is NA for lagr (for lagt non-NA (former 1-1935: 317.6)


# with different data set
data("Hedonic", package = "plm")
Hed_missing_period <- pdata.frame(Hedonic, index = "townid")
Hed_missing_period <- as.data.frame(Hed_missing_period)
Hed_missing_period <- Hed_missing_period[-c(5,11), ]  # delete 3-2 and 4-5
Hed_missing_period <- pdata.frame(Hed_missing_period, index = c("townid", "time"))

is.pconsecutive(Hed_missing_period)

head(Hed_missing_period$age, 20)
head(test_Hed_miss_p_lag1 <- plm:::lagr.pseries(Hed_missing_period$age), 20)    # correct: plm:::lagr.pseries(, 1): additional NAs introduced at (among others) 3-3 and 4-6
head(test_Hed_miss_p_lag2 <- plm:::lagr.pseries(Hed_missing_period$age, 2), 20) # plm:::lagr.pseries(, 2): 4-6 is NA (for lagt non-NA (former 4-4: 85.89996)
                                                                                #                          3-3 is NA (for lagt non-NA (former 3-1: 45.79999))

head(plm:::lagr.pseries(Hed_missing_period$age, c(0,1,2)), 20) # view all at once


##### delete two consecutive time periods
data("Hedonic", package = "plm")
Hed_missing_period2 <- pdata.frame(Hedonic, index = "townid")
Hed_missing_period2 <- as.data.frame(Hed_missing_period2)
Hed_missing_period2 <- Hed_missing_period2[-c(5,11,12), ]  # delete 3-2, 4-5, 4-6
Hed_missing_period2 <- pdata.frame(Hed_missing_period2, index = c("townid", "time"))

is.pconsecutive(Hed_missing_period2)

head(Hed_missing_period2$age, 20)
head(test_Hed_miss2_p_lag1 <- plm:::lagr.pseries(Hed_missing_period2$age), 20)    # correct: plm:::lagr.pseries(, 1): additional NAs introduced at 3-3 and 4-6
head(test_Hed_miss2_p_lag2 <- plm:::lagr.pseries(Hed_missing_period2$age, 2), 20) # 3-3 is NA (for lagt former 3-1 (45.79999))
head(test_Hed_miss2_p_lag3 <- plm:::lagr.pseries(Hed_missing_period2$age, 3), 20) # 4-7 is NA (for lagt former 4-4 (85.89996))
head(plm:::lagr.pseries(Hed_missing_period2$age, c(0,1,2,3)), 20) # view all at once


############## messy data set with lots of NAs ############
#### commented because it needs several extra packages and loads data from the internet
# library(haven)
#
# nlswork_r8 <- haven::read_dta("http://www.stata-press.com/data/r8/nlswork.dta")
# nlswork_r8 <- as.data.frame(lapply(nlswork_r8, function(x) {attr(x, "label") <- NULL; x}))
# pnlswork_r8 <- pdata.frame(nlswork_r8, index=c("idcode", "year"), drop.index=F)
# 
# 
# ### on a consecutive pdata.frame, plm:::lagr and plm:::lagt should yield same results (if no NA in id or time)
# pnlswork_r8_consec <- make.pconsecutive(pnlswork_r8)
# pnlswork_r8_consec_bal <- make.pconsecutive(pnlswork_r8, balanced = TRUE)
# pnlswork_r8_bal <- make.pbalanced(pnlswork_r8, balanced = TRUE)
# 
# if (!all.equal(plm::lagr.pseries(pnlswork_r8_consec$age), plm:::lagt.pseries(pnlswork_r8_consec$age)))
#  stop("lagr and lagt not same on consecutive data.frame (but must be!)")
# 
# if (!all.equal(plm:::lagr.pseries(pnlswork_r8_consec_bal$age), plm:::lagt.pseries(pnlswork_r8_consec_bal$age)))
#  stop("lagr and lagt not same on consecutive data.frame (but must be!)")


