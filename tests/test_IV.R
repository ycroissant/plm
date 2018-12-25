## Replicate some IV regression results
## Replicate Baltagi (2013), Econometric Analysis of Panel Data, 5th edition, ch. 7.2 (p. 133)
## (same as Baltagi (2006), Estimating an econometric model of crime using panel data from North Carolina,
##                          Journal of Applied Econometrics 21(4), pp. 543-547.
##
## NB: Crime data set: results can diverge slightly form the values printed in Baltagi
##     if logarithm computation is performed on the original variable. For the paper,
##     a data set with pre-computed logarithms (variables l*) was used and those 
##     logarithmic values diverge from what R's log() function gives.
##     -> see the two FE2SLS example which is computed in both ways


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
form_iv2 <- lcrmrte ~ lprbarr + lprbconv + lprbpris + lavgsen + lpolpc + ldensity + lwcon + lwtuc + lwtrd + lwfir + lwser + lwmfg + lwfed + lwsta + lwloc + lpctymle + lpctmin + region + smsa | . -lprbarr - lpolpc + ltaxpc + lmix
fe_iv <- plm(form_iv, data = Crime, model = "within", effect = "twoways", inst.method = "baltagi")
fe_iv2 <- plm(form_iv2, data = Crime, model = "within", effect = "twoways", inst.method = "baltagi")
summary(fe_iv)  # logs computed by R
summary(fe_iv2) # logs as in data set by Baltagi -> results match exactly

# ## felm example
# library(lfe)
# form_felm <- log(crmrte) ~  log(prbconv) + log(prbpris) + log(avgsen) + log(density) + log(wcon) + log(wtuc) + log(wtrd) + log(wfir) + log(wser) + log(wmfg) + log(wfed) + log(wsta) + log(wloc) + log(pctymle)  |
#   county + year |
#   (log(prbarr) + log(polpc) ~ log(prbpris) + log(avgsen) + log(density) + log(wcon) + log(wtuc) + log(wtrd) + log(wfir) + log(wser) + log(wmfg) + log(wfed) + log(wsta) + log(wloc) + log(pctymle) + log(taxpc) + log(mix))
# summary(felm(form_felm, data = Crime))

# replicates Table 7.1, column "BE2SLS"
be_iv <- plm(form_iv, data = Crime, model = "between")
summary(be_iv)

# replicates Table 7.1, column "EC2SLS"
## need to include time dummies!
form_re_iv  <- log(crmrte) ~ log(prbarr) + log(prbconv) + log(prbpris) + log(avgsen) + log(polpc) + log(density) + log(wcon) + log(wtuc) + log(wtrd) + log(wfir) + log(wser) + log(wmfg) + log(wfed) + log(wsta) + log(wloc) + log(pctymle) + log(pctmin) + region + smsa + factor(year) | . -log(prbarr) - log(polpc) + log(taxpc) + log(mix)
form_re_iv2 <- lcrmrte ~ lprbarr + lprbconv + lprbpris + lavgsen + lpolpc + ldensity + lwcon + lwtuc + lwtrd + lwfir + lwser + lwmfg + lwfed + lwsta + lwloc + lpctymle + lpctmin + region + smsa + factor(year) | . -lprbarr - lpolpc + ltaxpc + lmix
re_iv  <- plm(form_re_iv,  data = Crime, model = "random", inst.method = "baltagi")
re_iv2 <- plm(form_re_iv2, data = Crime, model = "random", inst.method = "baltagi")
summary(re_iv)
summary(re_iv2)

# replicates Baltagi (2013), p. 137 ("G2SLS") (not in Table 7.1)
re_iv_bvk  <- plm(form_re_iv,  data = Crime, model = "random", inst.method = "bvk")
re_iv_bvk2 <- plm(form_re_iv2, data = Crime, model = "random", inst.method = "bvk")
summary(re_iv_bvk)
summary(re_iv_bvk2)
cor(plm:::fitted_exp.plm(re_iv_bvk2), re_iv_bvk2$model[,1])^2 # overall R^2 as per Stata



re_iv_am <- plm(form_re_iv, data = Crime, model = "random", inst.method = "am")
summary(re_iv_am)

re_iv_bms <- plm(form_re_iv, data = Crime, model = "random", inst.method = "bms")
summary(re_iv_bms)
