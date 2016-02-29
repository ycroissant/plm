# tests of lag and lead
#  (1)  some dropped factor levels
#  (2) general tests
# 
# error prior to rev. 207:
# the lagging resulted in an error,
# because some factor levels can get lost due to the lagging
# and the old code was not capable to manage this
#
# fixed in rev. 207 with better handling of factor levels and simpler code

library(plm)
data("Grunfeld", package = "plm")

Grunfeld$fac <- factor(c(200:2, 1))
Grunfeld <- pdata.frame(Grunfeld)

############## (1) tests with eliminated factor levels ##########

# lag by 1 eliminates some factor levels (e.g. "1" in the last observations)
# from the sample's unique factor levels, but it should stay in the levels
plm::lag(Grunfeld$fac)
length(unique(Grunfeld$fac)) # 200
length(unique(plm::lag(Grunfeld$fac))) # 191

# lead eliminates e.g. level "200"
plm::lead(Grunfeld$fac)
length(unique(plm::lead(Grunfeld$fac))) # 191


############## (2) some general tests ##########
## some more general testing of lag and lead
# do nothing
if (!all.equal(plm::lag(Grunfeld$fac, 0), Grunfeld$fac)) stop("'lag( , 0)' not equal to 'do nothing'")
if (!all.equal(plm::lead(Grunfeld$fac, 0), Grunfeld$fac)) stop("'lead( , 0)' not equal to 'do nothing'")

# lag( , -k) == lead( , k)
if (!all.equal(plm::lag(Grunfeld$fac, -1), plm::lead(Grunfeld$fac, 1))) stop("'lag( , -1)' not equal to 'lead( , 1)'")
if (!all.equal(plm::lag(Grunfeld$fac, 1), plm::lead(Grunfeld$fac, -1))) stop("'lag( , 1)' not equal to 'lead( , -1)'")

# with numeric
if (!all.equal(plm::lag(Grunfeld$inv, -1), plm::lead(Grunfeld$inv, 1))) stop("'lag( , -1)' not equal to 'lead( , 1)'")
if (!all.equal(plm::lag(Grunfeld$inv, 1), plm::lead(Grunfeld$inv, -1))) stop("'lag( , 1)' not equal to 'lead( , -1)'")

# with logical
Grunfeld$log <- rep(c(T, T, F, T), 50)
if (!all.equal(plm::lag(Grunfeld$log, -1), plm::lead(Grunfeld$log, 1))) stop("'lag( , -1)' not equal to 'lead( , 1)'")
if (!all.equal(plm::lag(Grunfeld$log, 1), plm::lead(Grunfeld$log, -1))) stop("'lag( , 1)' not equal to 'lead( , -1)'")

## other k's
if (!all.equal(plm::lag(Grunfeld$inv, -5), plm::lead(Grunfeld$inv, 5))) stop("'lag( , -1)' not equal to 'lead( , 1)'")
if (!all.equal(plm::lag(Grunfeld$inv, 5), plm::lead(Grunfeld$inv, -5))) stop("'lag( , 1)' not equal to 'lead( , -1)'")

if (!all.equal(plm::lag(Grunfeld$inv, -3), plm::lead(Grunfeld$inv, 3))) stop("'lag( , -1)' not equal to 'lead( , 1)'")
if (!all.equal(plm::lag(Grunfeld$inv, 3), plm::lead(Grunfeld$inv, -3))) stop("'lag( , 1)' not equal to 'lead( , -1)'")

# should be all NA
if(!all(is.na(plm::lag(Grunfeld$inv, 20)))) stop("all-NA case not correct") # 20 is no of obs per id
if(!all(is.na(plm::lag(Grunfeld$inv, 21)))) stop("all-NA case not correct") # 21 is more than data available
if(!all(is.na(plm::lead(Grunfeld$inv, 20)))) stop("all-NA case not correct") # 20 is no of obs per id
if(!all(is.na(plm::lead(Grunfeld$inv, 21)))) stop("all-NA case not correct") # 21 is more than data available

## length(k) > 1
plm::lag(Grunfeld$inv, c(-2, -1, 0, 1, 2))
plm::lead(Grunfeld$inv, c(-2, -1, 0, 1, 2))
if(!all.equal(plm::lag(Grunfeld$inv, c(-2, -1, 0, 1, 2)),
              plm::lead(Grunfeld$inv, -1*c(-2, -1, 0, 1, 2)), check.attributes = FALSE)) stop("'lag( , c())' not equal to 'lead( , -1*c())'")

# diff
if(!all.equal(plm:::diff.pseries(Grunfeld$inv), Grunfeld$inv - lag(Grunfeld$inv))) stop("'diff()' not corresponding to differences with 'lag()'")
if(!all.equal(plm:::diff.pseries(Grunfeld$inv, 2), Grunfeld$inv - lag(Grunfeld$inv, 2))) stop("'diff( , 2)' not corresponding to differences with 'lag( , 2)'")


