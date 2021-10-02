## tests for pcdtest

## test pcdtest for NaN value in result
##  * due to non-intersecting pairs, fixed in rev. 339
##  * due to only ony period in intersection, fixed in rev. 345

library(plm)
data("Grunfeld", package = "plm")

## just a run test without obstacles
mod_pool <- plm(inv ~ value + capital, data = Grunfeld, model = "pooling")
testres1 <- pcdtest(mod_pool, test = "cd")
if (is.nan(testres1$statistic)) stop("statistic is NaN")
if (is.na(testres1$statistic)) stop("statistic is NA")
if (is.na(testres1$p.value)) stop("p-value is NA")

## no intersection for firm 1 and 2:
# firm 1 years: 1935 to 1944
# firm 2 years: 1945 to 1954
Grunfeld_no_intersect <- Grunfeld[-c(11:20, 21:30), ]
mod_pool_no_intersect <- plm(inv ~ value + capital, data = Grunfeld_no_intersect, model = "pooling")

testres2 <- pcdtest(mod_pool_no_intersect, test = "cd")
if (is.nan(testres2$statistic)) stop("statistic is NaN")
if (is.na(testres2$statistic)) stop("statistic is NA")
if (is.na(testres2$p.value)) stop("p-value is NA")


## fixed in rev. 345
## only 1 intersection for firm 1 and 2:
# firm 1 years: 1935 to 1945
# firm 2 years: 1945 to 1954
Grunfeld_one_intersect <- Grunfeld[-c(12:20, 20:30), ]
mod_pool_one_intersect <- plm(inv ~ value + capital, data = Grunfeld_one_intersect, model = "pooling")
testres3 <- pcdtest(mod_pool_one_intersect, test = "cd")

if (is.nan(testres3$statistic)) stop("statistic is NaN")
if (is.na(testres3$statistic)) stop("statistic is NA")
if (is.na(testres3$p.value)) stop("p-value is NA")


## make it also unbalanced for other individuals
Grunfeld_no_intersect_unbal <- Grunfeld_no_intersect[-c(65:66, 71, 103:110), ]
mod_pool_no_intersect_unbal <- plm(inv ~ value + capital, data = Grunfeld_no_intersect_unbal, model = "pooling")
testres4 <- pcdtest(mod_pool_no_intersect_unbal, test = "cd")
if (is.nan(testres4$statistic)) stop("statistic is NaN")
if (is.na(testres4$statistic)) stop("statistic is NA")
if (is.na(testres4$p.value)) stop("p-value is NA")


## test case for regression of variable on constant
## resulted in error pre rev. 342:
## "Error in lm.fit(tX, ty) : 'x' must be a matrix"
pcdtest(value ~ 1, data = Grunfeld)

## tests of local test (with arg w)
w <- diag(1, nrow = 10)
w[2,1] <- 1
testres5 <- pcdtest(mod_pool, test = "cd", w = w)
if (is.nan(testres5$statistic)) stop("statistic is NaN")
if (is.na(testres5$statistic)) stop("statistic is NA")
if (is.na(testres5$p.value)) stop("p-value is NA")

### should result in meaningful errors
## upper and lower triangular part define different neighbours
# w1 <- diag(1, nrow = 10)
# w1[1,3] <- 1
# w1[2,1] <- 1
# pcdtest(mod_pool, test = "cd", w = w1)
## wrong dimension
# w2 <- diag(1, nrow = 10, ncol = 11)
# pcdtest(mod_pool, test = "cd", w = w2)
