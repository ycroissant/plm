## Test of pvcm
#
# residuals should be of class c("pseries", "numeric) [since rev. 713]
#

library(plm)
data("Produc", package = "plm")
zw <- pvcm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc, model = "within")
zr <- pvcm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc, model = "random")

# time effect as run test
zw.ti <- pvcm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc, model = "within", effect = "time")
zr.ti <- pvcm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc, model = "random", effect = "time")


print(zw$residuals)
class(zw$residuals)
print(zw$coefficients)
class(zw$coefficients)
summary(zw)
pwaldtest(zw)

print(zr$residuals)
class(zr$residuals)
print(zr$coefficients)
class(zr$coefficients)
summary(zr)
pwaldtest(zr)


# run tests intercept-only models
zwint  <- pvcm(log(gsp) ~ 1, data = Produc, model = "within")
zwint2 <- pvcm(log(gsp) ~ 1, data = Produc[1:17, ], model = "within") # test with only one individual
summary(zwint) # gave multiple intercept summaries up until rev. 1199
stopifnot(dim(coef(zwint)) == c(48, 1)) 
# pwaldtest(zwint) # errors rightfully, and since rev. 1200 also informatively

zrint  <- pvcm(log(gsp) ~ 1, data = Produc, model = "random")
# zrint2 <- pvcm(log(gsp) ~ 1, data = Produc[1:17, ], model = "random") # errors rightfully with informative msg
summary(zrint) # does not calculate Wald statistic (rightfully, as only intercept)
# pwaldtest(zrint) # errors rightfully, and since rev. 1202 also informatively


## replicate Greene (2018), p. 452, table 11.22/(2012), p. 419, table 11.14
form.greene <- log(gsp) ~ log(pc) + log(hwy) + log(water) + log(util) + log(emp) + unemp
greene.ols     <- plm( form.greene, data = Produc, model = "pooling")
greene.pvcm.re <- pvcm(form.greene, data = Produc, model = "random")
summary(greene.pvcm.re)

greene.pvcm.re$single.coefs[ , 1]



## Stata example:
## http://www.stata.com/manuals/xtxtrc.pdf
## replicate Stata's example:
##
#  dat <- haven::read_dta("http://www.stata-press.com/data/r15/invest2.dta")
#  mod.pvcm.re <- pvcm(invest ~ market + stock, data = dat, index = c("company", "time"), model = "random")
# 
# ### Coefficients:
# ##   (Intercept)      market       stock
# ##    -23.583612    0.080765    0.283989
# 
#  print(mod.pvcm.re$single.coefs)
#  ##  (Intercept)     market     stock
#  ## 1  -71.629273 0.10278480 0.3678493
#  ## 2   -9.819343 0.08423601 0.3092167
#  ## 3  -12.032683 0.02793844 0.1508282
#  ## 4    3.269523 0.04110890 0.1407172
#  ## 5  -27.706284 0.14775499 0.4513312
