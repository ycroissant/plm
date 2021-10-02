## Test if pdwtest gives the same values for statistic and p-value for pooling and FE model
## as lmtest::dwtest
##
## bug fixed in rev. 127 / 2015-08-14

library(plm)
library(lmtest)
data("Grunfeld", package = "plm")

# Use lm() for pooled OLS and fixed effects
lm_pool <- lm(inv ~ value + capital, data = Grunfeld)
lm_fe   <- lm(inv ~ value + capital + factor(firm), data = Grunfeld)

# Use plm() for pooled OLS and fixed effects
plm_pool <- plm(inv ~ value + capital, data=Grunfeld, model = "pooling")
plm_fe   <- plm(inv ~ value + capital, data=Grunfeld, model = "within")

# pre-check: Are the residuals for the pooled OLS and fixed effects model by plm() and lm() the same?
if (!isTRUE(all.equal(as.numeric(residuals(plm_pool)), residuals(lm_pool), check.attributes = FALSE))) stop("pooling residuals not equal")
if (!isTRUE(all.equal(as.numeric(residuals(plm_fe)),   residuals(lm_fe),   check.attributes = FALSE))) stop("FE residuals not equal")

# check if statistics and p-values match
res_dwtest_pool  <- lmtest::dwtest(lm_pool)
res_dwtest_fe    <- lmtest::dwtest(lm_fe)
res_pdwtest_pool <- pdwtest(plm_pool)
res_pdwtest_fe   <- pdwtest(plm_fe)

if (!isTRUE(all.equal(res_dwtest_pool$statistic, res_pdwtest_pool$statistic))) stop("statistics do not match!")
if (!isTRUE(all.equal(res_dwtest_pool$p.value,   res_pdwtest_pool$p.value))) stop("p-values do not match!")

if (!isTRUE(all.equal(res_dwtest_fe$statistic, res_pdwtest_fe$statistic))) stop("statistics do not match!")
if (!isTRUE(all.equal(res_dwtest_fe$p.value,   res_pdwtest_fe$p.value))) stop("p-values do not match!")

# test for passing of arguments in ellipsis (...)
res_dwtest_pool_alt2  <- lmtest::dwtest(lm_pool, alternative = "two.sided")
res_pdwtest_pool_alt2 <- pdwtest(plm_pool, alternative = "two.sided")
if (!isTRUE(all.equal(res_dwtest_pool_alt2$statistic, res_pdwtest_pool_alt2$statistic))) stop("statistics do not match! Arg 'alternative' likely not respected")

# simple run tests
pdwtest(inv ~ value + capital, data = Grunfeld)
pdwtest(inv ~ value + capital, data = Grunfeld, model = "random", effect = "twoways")
pdwtest(inv ~ value + capital, data = Grunfeld, model = "random", effect = "twoways", alternative = "two.sided")
# exact = T (but not exact = TRUE) fails up to at least rev. 408
pdwtest(inv ~ value + capital, data = Grunfeld, model = "pooling", effect = "individual", alternative = "two.sided", exact = TRUE)
# pdwtest(inv ~ value + capital, data = Grunfeld, model = "pooling", effect = "individual", alternative = "two.sided", exact = T)
# pdwtest(plm_pool, alternative = "two.sided", exact = T)
##  Error in if (exact) { : argument is not interpretable as logical 