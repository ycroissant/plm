## Test of pvcm
#
# residuals should be of class c("pseries", "numeric) [since rev. 713]
#

library(plm)
data("Produc", package = "plm")
zw <- pvcm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc, model = "within")
zr <- pvcm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc, model = "random")

## IGNORE_RDIFF_BEGIN
print(zw$residuals)
## IGNORE_RDIFF_END
class(zw$residuals)
print(zw$coefficients)
class(zw$coefficients)
summary(zw)

## IGNORE_RDIFF_BEGIN
print(zr$residuals)
## IGNORE_RDIFF_END
class(zr$residuals)
print(zr$coefficients)
class(zr$coefficients)
summary(zr)


## Stata example:
## http://www.stata.com/manuals/xtxtrc.pdf
## replicate Stata's example:
# dat <- haven::read_dta("http://www.stata-press.com/data/r15/invest2.dta")
# pvcm(invest ~ market + stock, data = dat, index = c("company", "time"), model = "random")

