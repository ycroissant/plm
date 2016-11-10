## test pcdtest for NaN value in result (due to non-intersecting pairs)
## fixed in rev. 339

library(plm)
data("Grunfeld", package = "plm")

## just a run test without obstacles
mod_pool <- plm(inv ~ value + capital, data = Grunfeld, model = "pooling")
testres1 <- pcdtest(mod_pool, test = "cd")
if (is.nan(testres1$statistic)) stop("statistic is NaN")

## no intersection for firm 1 and 2:
# firm 1 years: 1935 to 1944
# firm 2 years: 1945 to 1954
Grunfeld_no_intersect <- Grunfeld[-c(11:20, 21:30), ]
mod_pool_no_intersect <- plm(inv ~ value + capital, data = Grunfeld_no_intersect, model = "pooling")

testres2 <- pcdtest(mod_pool_no_intersect, test = "cd")
if (is.nan(testres2$statistic)) stop("statistic is NaN")

## make it also unbalanced for other individuals
Grunfeld_no_intersect_unbal <- Grunfeld_no_intersect[-c(65:66, 71, 103:110), ]
mod_pool_no_intersect_unbal <- plm(inv ~ value + capital, data = Grunfeld_no_intersect_unbal, model = "pooling")
testres3 <- pcdtest(mod_pool_no_intersect_unbal, test = "cd")
if (is.nan(testres3$statistic)) stop("statistic is NaN")
