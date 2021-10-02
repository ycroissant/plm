## Test of pvcm
#
# residuals should be of class c("pseries", "numeric) [since rev. 713]
#

library(plm)
data("Produc", package = "plm")
zw <- pvcm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc, model = "within")
zr <- pvcm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc, model = "random")

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
# zrint2 <- pvcm(log(gsp) ~ 1, data = Produc[1:17, ], model = "random") # only one individual -> errors -> catch case?
summary(zrint) # does not calculate Wald statistic (rightfully, as only intercept)
# pwaldtest(zrint) # errors rightfully, and since rev. 1202 also informatively

## Stata example:
## http://www.stata.com/manuals/xtxtrc.pdf
## replicate Stata's example:
# dat <- haven::read_dta("http://www.stata-press.com/data/r15/invest2.dta")
# pvcm(invest ~ market + stock, data = dat, index = c("company", "time"), model = "random")

