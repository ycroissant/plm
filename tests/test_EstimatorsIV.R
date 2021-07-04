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

# not in table
fd_iv <- plm(form_iv, data = Crime, model = "fd", effect = "individual")
summary(fd_iv)

# replicates Table 7.1, column "EC2SLS"
## need to include time dummies!
form_re_iv  <- log(crmrte) ~ log(prbarr) + log(prbconv) + log(prbpris) + log(avgsen) + log(polpc) + log(density) + log(wcon) + log(wtuc) + log(wtrd) + log(wfir) + log(wser) + log(wmfg) + log(wfed) + log(wsta) + log(wloc) + log(pctymle) + log(pctmin) + region + smsa + factor(year) | . -log(prbarr) - log(polpc) + log(taxpc) + log(mix)
form_re_iv2 <- lcrmrte ~ lprbarr + lprbconv + lprbpris + lavgsen + lpolpc + ldensity + lwcon + lwtuc + lwtrd + lwfir + lwser + lwmfg + lwfed + lwsta + lwloc + lpctymle + lpctmin + region + smsa + factor(year) | . -lprbarr - lpolpc + ltaxpc + lmix
re_iv  <- plm(form_re_iv,  data = Crime, model = "random", inst.method = "baltagi")
re_iv2 <- plm(form_re_iv2, data = Crime, model = "random", inst.method = "baltagi")
summary(re_iv)
summary(re_iv2)

# replicates Baltagi (2013), p. 137/Baltagi (2021), p. 165 ("G2SLS"), table 7.3 (not in Table 7.1)
re_iv_bvk  <- plm(form_re_iv,  data = Crime, model = "random", inst.method = "bvk")
re_iv_bvk2 <- plm(form_re_iv2, data = Crime, model = "random", inst.method = "bvk")
summary(re_iv_bvk)
summary(re_iv_bvk2)
cor(plm:::fitted_exp.plm(re_iv_bvk2), re_iv_bvk2$model[ , 1])^2 # overall R^2 as per Stata



## Hausman-Taylor estimator:
## replicates Baltagi (2005, 2013), table 7.4; Baltagi (2021), table 7.5
# (chisq values in Baltagi (2021) are not those of the models but of Hausman test
#  between the models! plm's summary replicates chisq values of the models as 
#  given by Stata and printed in Baltagi (2021), tables 7.6, 7.7)
#
# Table 7.5 claims to replicate Baltagi/Khanti-Akom (1990), table II, but values
# for all models but within are largely different (even the GLS case!), making
# the book reproducible but not the paper (likely the paper is in error!).
data("Wages", package = "plm")
pWages <- pdata.frame(Wages, index = 595)

form_wage <- lwage ~ wks + south + smsa + married + exp + I(exp ^ 2) + 
  bluecol + ind + union + sex + black + ed

form_wage_iv <- lwage ~ wks + south + smsa + married + exp + I(exp ^ 2) + 
  bluecol + ind + union + sex + black + ed |
  bluecol + south + smsa + ind + sex + black |
  wks + married + union + exp + I(exp ^ 2)

gls <- plm(form_wage, data = pWages, model = "random")
summary(gls)

within <- plm(form_wage, data = pWages, model = "within")
summary(within)

ht <- plm(form_wage_iv,
          data = pWages,
          random.method = "ht", model = "random", inst.method = "baltagi")
summary(ht)

am <- plm(form_wage_iv, 
          data = pWages,
          random.method = "ht", model = "random", inst.method = "am")
summary(am)

bms <- plm(form_wage_iv,
          data = pWages,
          random.method = "ht", model = "random", inst.method = "bms")
summary(bms)

# texreg::screenreg(list(ht, am, bms))

phtest(within, ht) # 5.2577 -> match Baltagi (2021), p. 175 for statistic but
                   #           df are different (9 vs. 3), Baltagi explains why df = 3.

phtest(ht, am) # 14.66 -> close to Baltagi's 17.74 (df = 12 vs. 13)




### IV estimators ##
form_wage_iv2 <- lwage ~ wks + married + exp + I(exp ^ 2) + bluecol |
                   wks + exp + bluecol |
                   wks + married +  exp + I(exp ^ 2)

## balanced one-way individual
IVbvk <- plm(form_wage_iv2,
          data = pWages,
          model = "random", inst.method = "bvk")
summary(IVbvk)

IVbalt <- plm(form_wage_iv2,
          data = pWages,
          model = "random", inst.method = "baltagi")
summary(IVbalt)

IVam <- plm(form_wage_iv2, 
          data = pWages,
          model = "random", inst.method = "am")
summary(IVam)

IVbms <- plm(form_wage_iv2,
           data = pWages,
           model = "random", inst.method = "bms")
summary(IVbms)

# texreg::screenreg(list("BVK" = IVbvk, "Baltagi" = IVbalt, "AM" = IVam, "BMS" = IVbms),
#                  digits = 5)

## unbalanced one-way individual

pWages_ubal <- pWages[-c(2:7, 79:82, 500:505), ]
pdim(pWages_ubal)
IVbvk_ubal <- plm(form_wage_iv2,
             data = pWages_ubal,
             model = "random", inst.method = "bvk")
summary(IVbvk_ubal)

IVbalt_ubal <- plm(form_wage_iv2,
              data = pWages_ubal,
              model = "random", inst.method = "baltagi")
summary(IVbalt_ubal)

IVam_ubal <- plm(form_wage_iv2, 
            data = pWages_ubal,
            model = "random", inst.method = "am")
summary(IVam_ubal)

IVbms_ubal <- plm(form_wage_iv2,
             data = pWages_ubal,
             model = "random", inst.method = "bms")
summary(IVbms_ubal)

# texreg::screenreg(list("BVK ui" = IVbvk_ubal, "Baltagi ui" = IVbalt_ubal, "AM ui" = IVam_ubal, "BMS ui" = IVbms_ubal),
#                  digits = 5)


## balanced one-way time
# gives identical results for "am" and "bms" results are identical to "baltagi",
# likely because  function StarX is not symmetric in effect
IVbvkt <- plm(form_wage_iv2,
             data = pWages,
             model = "random", inst.method = "bvk", effect = "time")
summary(IVbvkt)

IVbaltt <- plm(form_wage_iv2,
              data = pWages,
              model = "random", inst.method = "baltagi", effect = "time")
summary(IVbaltt)

IVamt <- plm(form_wage_iv2, 
            data = pWages,
            model = "random", inst.method = "am", effect = "time")
summary(IVamt)

IVbmst <- plm(form_wage_iv2,
             data = pWages,
             model = "random", inst.method = "bms", effect = "time")
summary(IVbmst)

# texreg::screenreg(list("BVK t" = IVbvkt, "Baltagi t" = IVbaltt, "AM t" = IVamt, "BMS t" = IVbmst),
#                  digits = 5)


IVbvktw <- plm(form_wage_iv2,
              data = pWages,
              model = "random", inst.method = "bvk", effect = "twoways")
summary(IVbvktw)

# IVbalttw <- plm(form_wage_iv2,
#                data = pWages,
#                model = "random", inst.method = "baltagi", effect = "twoways")
# summary(IVbalttw)
# 
# IVamtw <- plm(form_wage_iv2,
#              data = pWages,
#              model = "random", inst.method = "am", effect = "twoways")
# summary(IVamtw)
# 
# IVbmstw <- plm(form_wage_iv2,
#               data = pWages,
#               model = "random", inst.method = "bms", effect = "twoways")
# summary(IVbmstw)
# 
# texreg::screenreg(list("BVK tw" = IVbvktw, "Baltagi tw" = IVbalttw, "AM tw" = IVamtw, "BMS tw" = IVbmstw),
#                   digits = 5)
