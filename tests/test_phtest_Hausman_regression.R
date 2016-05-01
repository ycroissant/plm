#### Hausman test (original version and regression-based version)
##
##
## (1) comparision to Baltagi (2013), sec. 4.3.1, example 1 (pp. 81-82)
## (2) comparision to Baltagi (2013), sec. 4.3.2, example 2 (pp. 82-83)
## (3) comparision to STATA


################################## (1) ##################################
# Baltagi (2013), Econometric Analysis of Panel Data, 5th edition, Wiley & Sons
# Sec 4.3.1, p. 81 (example 1):
#
#### statistics are: 2.131 for regression based;
#                    2.33  for original Hausman
#
#### vcov within * 10^-3:
#
# 0.14058    -0.077468
#             0.3011788
#
#### vcov between * 10^-3:
#
# 0.82630142  -3.7002477
#             36.4572431

options(digits = 10)
library(plm)
data("Grunfeld")
Grunfeldpdata <- pdata.frame(Grunfeld, index = c("firm", "year"), drop.index = FALSE, row.names = TRUE)
fe_grun  <- plm(inv ~ value + capital, data=Grunfeldpdata, model="within")
be_grun  <- plm(inv ~ value + capital, data=Grunfeldpdata, model="between")
re_grun  <- plm(inv ~ value + capital, data=Grunfeldpdata, model="random")

# regression-based Hausman test

phtest(inv ~ value + capital, Grunfeldpdata)               # replicates Baltagi's m1 = 2.33
phtest(fe_grun, re_grun)                                   # same as above, replicates Baltagi's m1 = 2.33
phtest(be_grun, re_grun)                                   # replicates Baltagi's m2 = 2.131 [values m1 and m2 coincide in this case]
phtest(inv ~ value + capital, Grunfeldpdata, method="aux") # replicates Baltagi's m3 = 2.131

phtest(inv ~ value + capital, Grunfeldpdata, method="aux", vcov = vcovHC) # no comparison value

# replicates variance-covariance matrices
vcov(fe_grun)*1000
vcov(be_grun)*1000


################################## (2) ##################################
# Baltagi (2013), Econometric Analysis of Panel Data, 5th edition, Wiley & Sons
# Sec 4.3.2, p. 82-83 (example 2):
### Baltagi's Gasoline example
data("Gasoline", package = "plm")
form <- lgaspcar ~ lincomep + lrpmg + lcarpcap
wi <- plm(form, data = Gasoline, model = "within")
be <- plm(form, data = Gasoline, model = "between")
re <- plm(form, data = Gasoline, model = "random")

phtest(wi, re) # replicates Baltagi's m1 = 302.8
phtest(form, data = Gasoline) # same as above (m1)

phtest(be, re) # replicates Baltagi's m2 = 27.45
phtest(form, data = Gasoline, method = "aux") # chisq = 26.495054, replicates _almost_ Baltagi's m3 = 26.507

# replicates variance-covariance matrices
#
# vcov in Baltagi within:
# 0.539 0.029 -0.205
#       0.194  0.009
#              0.088
#
# vcov in Baltagi between:
# 2.422 -1.694 -1.056
#        1.766  0.883
#               0.680
vcov(wi)*100
vcov(be)*100



################################## (3) ##################################
### comparision to STATA:
# Hausman test with STATA example 2, pp. 5-6 in http://www.stata.com/manuals14/xtxtregpostestimation.pdf
#
# Results of phtest differ, most likely because RE model differs slightly from STATA's RE model as the
# default RE model in STATA uses a slightly different implementation of Swamy-Arora method
# [see http://www.stata.com/manuals14/xtxtreg.pdf]
#
# STATA:
# chi2(8)   = (b-B)'[(V_b-V_B)^(-1)](b-B)
#           =      149.43
# Prob>chi2 =      0.0000

# library(haven)
# nlswork <- read_dta("http://www.stata-press.com/data/r14/nlswork.dta") # large file
# nlswork$race     <- factor(nlswork$race) # convert
# nlswork$race2    <- factor(ifelse(nlswork$race == 2, 1, 0)) # need this variable for example
# nlswork$grade    <- as.numeric(nlswork$grade)
# nlswork$age2     <- (nlswork$age)^2
# nlswork$tenure2  <- (nlswork$tenure)^2
# nlswork$ttl_exp2 <- (nlswork$ttl_exp)^2
# 
# pnlswork <- pdata.frame(nlswork, index=c("idcode", "year"), drop.index=F)
# 
# form_nls_ex2 <- formula(ln_wage ~ grade + age + age2 + ttl_exp + ttl_exp2 + tenure + tenure2 + race2 + not_smsa + south)
# 
# plm_fe_nlswork <- plm(form_nls_ex2, data = pnlswork, model = "within")
# plm_be_nlswork <- plm(form_nls_ex2, data = pnlswork, model = "between")
# plm_re_nlswork <- plm(form_nls_ex2, data = pnlswork, model = "random")
# 
# summary(plm_re_nlswork)
# 
# ### STATA: chi2(8) = 149.43
# phtest(plm_fe_nlswork, plm_re_nlswork)              # chisq = 176.39, df = 8,  p-value < 2.2e-16
# phtest(plm_be_nlswork, plm_re_nlswork)              # chisq = 141.97, df = 10, p-value < 2.2e-16
# phtest(form_nls_ex2, data = pnlswork, method="aux") # chisq = 627.46, df = 8,  p-value < 2.2e-16 [this resulted in an error for SVN revisions 125 - 141]
# phtest(form_nls_ex2, data = nlswork,  method="aux") # same on data.frame
# phtest(form_nls_ex2, data = pnlswork, method="aux", vcov = vcovHC) # chisq = 583.56, df = 8, p-value < 2.2e-16
# # phtest(form_nls_ex2, data = pnlswork, method="aux", vcov = function(x) vcovHC(x, method="white2", type="HC3")) # computationally too heavy!
