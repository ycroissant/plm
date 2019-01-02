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

print(zr$residuals)
class(zr$residuals)
print(zw$coefficients)
class(zw$coefficients)
summary(zr)