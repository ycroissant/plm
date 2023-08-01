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

## single unbiased coefficients and std.error
print(greene.pvcm.re$single.coefs)
print(greene.pvcm.re$single.std.error)

## replicate Poi (2003) with the Grunfeld data and inject values from 
## Stata's invest2.dta data set (remaining tiny diffs are due to invest2.dta
## having more digits, not justified by the original Grunfeld print of data)
data(Grunfeld) # need firm = 1, 4, 3, 8, 2
Gr.Poi.2003 <- Grunfeld[c(1:20, 61:80, 41:60, 141:160, 21:40), ]
Gr.Poi.2003$firm <- rep(1:5, each = 20)
Gr.Poi.2003[c(86, 98), "inv"] <- c(261.6, 645.2)
Gr.Poi.2003[c(92), "capital"] <- c(232.6)

mod.poi <- pvcm(inv ~ value + capital, data = Gr.Poi.2003, model = "random")
summary(mod.poi)
print(mod.poi$single.coefs)
print(mod.poi$single.std.error)


## Stata example / Poi (2003):
## http://www.stata.com/manuals/xtxtrc.pdf
## https://journals.sagepub.com/doi/pdf/10.1177/1536867X0300300307
## replicate Stata's example:
##
#  dat <- haven::read_dta("http://www.stata-press.com/data/r15/invest2.dta")
#  mod.poi.stata.dat <- pvcm(invest ~ market + stock, data = dat, index = c("company", "time"), model = "random")
# 
# ### Coefficients:
# ##   (Intercept)      market       stock
# ##    -23.583612    0.080765    0.283989
# 
#  print(mod.poi.stata.dat$single.coefs)
#  ##  (Intercept)     market     stock
#  ## 1  -71.629273 0.10278480 0.3678493
#  ## 2   -9.819343 0.08423601 0.3092167
#  ## 3  -12.032683 0.02793844 0.1508282
#  ## 4    3.269523 0.04110890 0.1407172
#  ## 5  -27.706284 0.14775499 0.4513312
#  print(mod.poi.stata.dat$single.std.error)
#  ##  (Intercept)     market      stock
#  ## 1   37.466633 0.01085665 0.03313519
#  ## 2   14.074958 0.01557613 0.03018058
#  ## 3   29.580826 0.01347698 0.02869037
#  ## 4    9.510794 0.01181795 0.03402793
#  ## 5   42.125236 0.01819023 0.05692992

## run test for a model with NA coeffs
pder.avail <- if(!requireNamespace("pder", quietly = TRUE)) FALSE else TRUE
if(pder.avail) {
  # from Croissant/Millo (2019), ex. 8.1, p. 189
  data("Dialysis", package = "pder")
  rndcoef <- pvcm(log(diffusion / (1 - diffusion)) ~ trend + trend:regulation, data = Dialysis, model="random")
  summary(rndcoef)
}

