## Replicate some IV regression results
## Replicate Baltagi (2013), Econometric Analysis of Panel Data, 5th edition, ch. 7.2 (p. 133)
## (same as Baltagi (2006), Estimating an econometric model of crime using panel data from North Carolina,
##                          Journal of Applied Econometrics 21(4), pp. 543-547.

library(plm)
data("Crime", package = "plm")

# replicates Table 7.1, column "Between"
form <- log(crmrte) ~ log(prbarr) + log(prbconv) + log(prbpris) + log(avgsen) + log(polpc) + log(density) + log(wcon) + log(wtuc) + log(wtrd) + log(wfir) + log(wser) + log(wmfg) + log(wfed) + log(wsta) + log(wloc) + log(pctymle) + log(pctmin) + region + smsa
be <- plm(form, data = Crime, model = "between")
summary(be)

# replicates Table 7.1, column "Fixed Effects"
fe <- plm(form, data = Crime, model = "within", effect = "twoways")
summary(fe)

# replicates Table 7.1, column "FE2SLS"
form_iv <- log(crmrte) ~ log(prbarr) + log(prbconv) + log(prbpris) + log(avgsen) + log(polpc) + log(density) + log(wcon) + log(wtuc) + log(wtrd) + log(wfir) + log(wser) + log(wmfg) + log(wfed) + log(wsta) + log(wloc) + log(pctymle) + log(pctmin) + region + smsa | . -log(prbarr) - log(polpc) + log(taxpc) + log(mix)
fe_iv <- plm(form_iv, data = Crime, model = "within", effect = "twoways", inst.method = "baltagi")
summary(fe_iv)

# replicates Table 7.1, column "BE2SLS"
be_iv <- plm(form_iv, data = Crime, model = "between")
summary(be_iv)

# replicates Table 7.1, column "EC2SLS"
## need to include time dummies!
form_re_iv <- log(crmrte) ~ log(prbarr) + log(prbconv) + log(prbpris) + log(avgsen) + log(polpc) + log(density) + log(wcon) + log(wtuc) + log(wtrd) + log(wfir) + log(wser) + log(wmfg) + log(wfed) + log(wsta) + log(wloc) + log(pctymle) + log(pctmin) + region + smsa + factor(year) | . -log(prbarr) - log(polpc) + log(taxpc) + log(mix)
re_iv <- plm(form_re_iv, data = Crime, model = "random", inst.method = "baltagi")
summary(re_iv)

# replicates (almost) Baltagi (2013), p. 137 ("G2SLS") (not in Table 7.1)
re2_iv <- plm(form_re_iv, data = Crime, model = "random", inst.method = "bvk")
summary(re2_iv)
cor(plm:::fitted_exp.plm(re2_iv), re2_iv$model[,1])^2 # overall R^2 as per Stata



re_iv3 <- plm(form_re_iv, data = Crime, model = "random", inst.method = "am")
summary(re_iv3)

re_iv4 <- plm(form_re_iv, data = Crime, model = "random", inst.method = "bms")
summary(re_iv4)
