#### Replicate results of various sources
####
#### (1): Baltagi (2013)
#### (2): STATA's FE estimator




#### (1) ####
#### compare OLS, FE and RE estimators to Baltagi's results
# Baltagi (2013), Econometric Analysis of Panel Data, 5th edition, Wiley & Sons
#
#
#    oneway:  sec. 2.6, example 1 p. 27, table 2.1
#    twoways: sec. 3.6, example 1 p. 51, table 3.1
#
# = Baltagi (2005), p. 21 (oneway), p. 43 (twoways) 

# Table 2.1 Grunfeld’s Data. One-way Error Component Results
#             β1        β2      ρ     σμ    σν
#--------------------------------------------------
# OLS       0.116     0.231
#          (0.006)*  (0.025)*
#
# Between   0.135     0.032
#          (0.029)   (0.191)
#
# Within    0.110     0.310
#          (0.012)   (0.017)
#
# WALHUS    0.110     0.308   0.73  87.36   53.75
#          (0.011)   (0.017)
#
# AMEMIYA   0.110     0.308   0.71  83.52   52.77
#          (0.010)   (0.017)
#
# SWAR      0.110     0.308   0.72  84.20   52.77
#          (0.010)   (0.017)
#
# IMLE      0.110     0.308   0.70  80.30   52.49
#          (0.010)   (0.017)
library(plm)
data("Grunfeld", package = "plm")
# oneway
plm_grunfeld_pooled     <- plm(inv ~ value + capital, data=Grunfeld, model="pooling")
plm_grunfeld_be         <- plm(inv ~ value + capital, data=Grunfeld, model="between")
plm_grunfeld_fe         <- plm(inv ~ value + capital, data=Grunfeld, model="within")
plm_grunfeld_re_walhus  <- plm(inv ~ value + capital, data=Grunfeld, model="random", random.method="walhus")
plm_grunfeld_re_amemiya <- plm(inv ~ value + capital, data=Grunfeld, model="random", random.method="amemiya")
plm_grunfeld_re_swar    <- plm(inv ~ value + capital, data=Grunfeld, model="random", random.method="swar")


summary(plm_grunfeld_pooled    )
summary(plm_grunfeld_be        )
summary(plm_grunfeld_fe        )
summary(plm_grunfeld_re_walhus )
summary(plm_grunfeld_re_swar   )
summary(plm_grunfeld_re_amemiya)

# Table 3.1 Grunfeld’s Data. Two-way Error Component Results
# "For the random effects estimators, both the SWAR and
#  WALHUS report negative estimates of σ2
#  λ and this is replaced by zero."
#
#             β1        β2      σμ    σλ    σν
#-------------------------------------------------
# OLS       0.116     0.231
#          (0.006)∗  (0.025)∗
#
# Within    0.118     0.358
#          (0.014)   (0.023)
#
# WALHUS    0.110     0.308   87.31   -0     55.33
#          (0.010)   (0.017)
#
# AMEMIYA   0.111     0.324   89.26 15.78   51.72
#          (0.011)   (0.019)
#
# SWAR      0.110     0.308   84.23   -0     51.72
#          (0.011)   (0.017)
#
# IMLE      0.110     0.309   80.41 3.87    52.35
#          (0.010)   (0.020)


# twoways
plm_grunfeld_pooled_tw  <- plm(inv ~ value + capital, data=Grunfeld, model="pooling", effect = "twoways")
plm_grunfeld_fe_tw      <- plm(inv ~ value + capital, data=Grunfeld, model="within",  effect = "twoways")
plm_grunfeld_re_walhus  <- plm(inv ~ value + capital, data=Grunfeld, model="random", random.method="walhus",  effect = "twoways")
plm_grunfeld_re_amemiya <- plm(inv ~ value + capital, data=Grunfeld, model="random", random.method="amemiya", effect = "twoways")
plm_grunfeld_re_swar    <- plm(inv ~ value + capital, data=Grunfeld, model="random", random.method="swar",    effect = "twoways")

summary(plm_grunfeld_pooled_tw )
summary(plm_grunfeld_fe_tw     )
summary(plm_grunfeld_re_walhus )
summary(plm_grunfeld_re_amemiya)
summary(plm_grunfeld_re_swar   )


#### (2) ####
####### replicate STATA's fixed effects estimator, R-squared, F statistic ###
## http://www.stata.com/manuals14/xtxtreg.pdf [example 2 on p. 14]
# library(plm)
# library(haven)
# nlswork <- read_dta("http://www.stata-press.com/data/r14/nlswork.dta") # large file
# nlswork$race <- factor(nlswork$race) # convert
# nlswork$race2 <- factor(ifelse(nlswork$race == 2, 1, 0)) # need this variable for example
# nlswork$grade <- as.numeric(nlswork$grade)
# pnlswork <- pdata.frame(nlswork, index=c("idcode", "year"), drop.index=F)
# 
# form_nls_ex2 <- formula(ln_wage ~ grade + age + I(age^2) + ttl_exp + I(ttl_exp^2) + tenure + I(tenure^2) + race2 + not_smsa + south)
# 
# plm_fe_nlswork <- plm(form_nls_ex2, data = pnlswork, model = "within")

# STATA's results:
#
# R-sq:                                          
#   within  = 0.1727                               
#   between = 0.3505                               
#   overall = 0.262

# F(8,23386) = 610.12
# Prob > F = 0.0000

# ln_wage                       Coef.       Std. Err.
#-------------------------------------------------------
# grade                           0         (omitted)
# age                         .0359987      .0033864    
# c.age#c.age                -.000723       .0000533
# ttl_exp                     .0334668      .0029653    
# c.ttl_exp#c.ttl_exp         .0002163      .0001277    
# tenure                      .0357539      .0018487    
# c.tenure#c.tenure          -.0019701      .000125   
#   race
# black                           0         (omitted)
# not_smsa                   -.0890108      .0095316   
# south                      -.0606309      .0109319
# _cons                      1.03732        .0485546

# resambles STATA (ex. 2, p. 14)
# => coefficients, std.errors, R^2 (=R-sq within), F => correct
# (NB: STATA outputs an "artificial" constant for FE models, see below)
#summary(plm_fe_nlswork)

# STATA outputs a constant for the FE model which is computed as the weighted average of the individual constants
# see http://www.stata.com/support/faqs/statistics/intercept-in-fixed-effects-model/
# However, STATA also outputs std.err, t-test and p-value for the artificial constant
# gretl mimics STATA: see gretl user's guide example p. 160-161 (example 18.1)
# http://gretl.sourceforge.net/gretl-help/gretl-guide.pdf
# http://lists.wfu.edu/pipermail/gretl-devel/2013-May/004459.html
#const_fe_STATA_gretl <- weighted.mean(fixef(plm_fe_nlswork) , as.numeric(table(index(plm_fe_nlswork)[[1]])))

# RE estimator
# note STATA 14 uses by default a different method compared to plm's Swamy-Arora variance component estimator
# This is why in comparison with web examples from STATA the random effects coefficients slightly differ
#plm_re_nlswork <- plm(form_nls_ex2, data = pnlswork, model = "random")


