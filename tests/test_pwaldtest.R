#### Testfile for pwaldtest()
#
# see also tests/test_pwaldtest_vcovG_attr_cluster.R for the attribute 'cluster' of the furnished vcovs
options(scipen = 999)
options(digits = 8)
library(plm)
data("Grunfeld", package="plm")
gp <- plm(inv ~ value + capital, data = Grunfeld, model = "pooling")
gi <- plm(inv ~ value + capital, data = Grunfeld,
          effect = "individual", model = "within")
gt <- plm(inv ~ value + capital, data = Grunfeld,
          effect = "time", model = "within")
gd <- plm(inv ~ value + capital, data = Grunfeld,
          effect = "twoways", model = "within")
gre<- plm(inv ~ value + capital, data = Grunfeld,
          effect = "individual", model = "random")

# Chisq
plm:::pwaldtest(gp, test = "Chisq")
plm:::pwaldtest(gi, test = "Chisq")
plm:::pwaldtest(gt, test = "Chisq")
plm:::pwaldtest(gd, test = "Chisq")
plm:::pwaldtest(gre, test = "Chisq")

# F
plm:::pwaldtest(gp, test = "F")
plm:::pwaldtest(gi, test = "F")
plm:::pwaldtest(gt, test = "F")
plm:::pwaldtest(gd, test = "F")
plm:::pwaldtest(gre, test = "F")


# Gretl uses Stata's small sample adjustment
  g <- pdim(gi)$nT$n # no of individuals
  n <- pdim(gi)$nT$N # no of total obs
  k <- length(coefficients(gi))
  adj_k1 <- (g/(g-1) * (n-1)/(n-k-1)) # k <- k + 1 because Stata and Gretl have the intercept in the FE model
  adj    <- (g/(g-1) * (n-1)/(n-k))
  adj_gd <- (g/(g-1) * (n-1)/(n-k-1-19)) # Gretl has time dummies, not demeaning by time (20 periods for Grunfeld data)
# gd: k anpassen=  adj_ 
  vcov_mat_adj_gp  <- adj_k1  * plm::vcovHC(gp)
  vcov_mat_adj_gi  <- adj_k1  * plm::vcovHC(gi)
  vcov_mat_adj_gd  <- adj_gd  * plm::vcovHC(gd)
  vcov_mat_adj_gre <- adj_k1  * plm::vcovHC(gre)
  vcov_mat_adj_gt  <- adj_k1  * plm::vcovHC(gt)

# Chisq - robust - formula
plm:::pwaldtest(gp, test = "Chisq", .vcov = vcovHC)
plm:::pwaldtest(gi, test = "Chisq", .vcov = vcovHC)
plm:::pwaldtest(gt, test = "Chisq", .vcov = vcovHC)
plm:::pwaldtest(gd, test = "Chisq", .vcov = vcovHC)
plm:::pwaldtest(gre, test = "Chisq", .vcov = vcovHC)

# Chisq - robust - matrix
plm:::pwaldtest(gp, test = "Chisq", .vcov = vcovHC(gp))
plm:::pwaldtest(gi, test = "Chisq", .vcov = vcovHC(gi))
plm:::pwaldtest(gt, test = "Chisq", .vcov = vcovHC(gt))
plm:::pwaldtest(gd, test = "Chisq", .vcov = vcovHC(gd))
plm:::pwaldtest(gre, test = "Chisq", .vcov = vcov_mat_adj_gre) # replicates Gretl: Chi-square(2) = 70.1267

# F - robust 
plm:::pwaldtest(gp, test = "F", .vcov = vcov_mat_adj_gp) # replicates Gretl: F(2, 9) = 51.59060
plm:::pwaldtest(gi, test = "F", .vcov = vcov_mat_adj_gi) # replicates Gretl: F(2, 9) = 28.3096
plm:::pwaldtest(gi, test = "F", .vcov = function(x) vcovHC(x, cluster = "time")) # cluster on time, df2 = 19
plm:::pwaldtest(gt, test = "F", .vcov = vcov_mat_adj_gt)
plm:::pwaldtest(gd, test = "F", .vcov = vcov_mat_adj_gd) # replicates Gretl: F(2, 9) = 60.0821
plm:::pwaldtest(gre, test = "F", .vcov = vcov_mat_adj_gre)


# F - robust - matrix
plm:::pwaldtest(gp, test = "F", .vcov = vcovHC(gp))
plm:::pwaldtest(gi, test = "F", .vcov = vcovHC(gi))
plm:::pwaldtest(gi, test = "F", .vcov = function(x) vcovHC(x, cluster = "time")) # cluster on time, df2 = 19
plm:::pwaldtest(gt, test = "F", .vcov = vcovHC(gt))
plm:::pwaldtest(gd, test = "F", .vcov = vcovHC(gd))
plm:::pwaldtest(gre, test = "F", .vcov = vcovHC(gre))


############### compare to other statistics packages:

## package 'lfe'
# library(lfe)
# data("Grunfeld", package = "plm")
# gi_lfe <- felm(inv ~ value + capital | firm, data = Grunfeld)
# gi_lfe_cluster <- felm(inv ~ value + capital | firm, data = Grunfeld, clustervar="firm")
# summary(gi_lfe)
# summary(gi_lfe_cluster)
# lfe::waldtest(gi_lfe, R = names(coef(gi_lfe))) # df1 = 2, df2 = 188
# lfe::waldtest(gi_lfe_cluster, R = names(coef(gi_lfe_cluster))) # chi2: 54.03250, F. 27.01625, df1 = 2, df2 = 9
# gi_lfe_cluster$clustervcv #  # this vcov is not identical to vcovHC, so results do not match


### Stata ####
# See http://www.stata.com/manuals14/xtxtreg.pdf
# example 2 vs. example 3 (p 14 and 16):
# F(8, 23386) = 610.12 - normal
# F(8, 4696)  = 273.86 - robust

# commented because it needs extra library 'foreign'
# library(plm)
# library(haven)
# nlswork <- read_dta("http://www.stata-press.com/data/r14/nlswork.dta") # large file
# nlswork$race <- factor(nlswork$race) # convert
# nlswork$race2 <- factor(ifelse(nlswork$race == 2, 1, 0)) # need this variable for example
# nlswork$grade <- as.numeric(nlswork$grade)
# pnlswork <- pdata.frame(nlswork, index=c("idcode", "year"), drop.index=F)
# 
# form_nls_ex2 <- formula(ln_wage ~ grade + age + I(age^2) + ttl_exp + I(ttl_exp^2) + tenure + I(tenure^2) + race2 + not_smsa + south)
# plm_fe_nlswork <- plm(form_nls_ex2, data = pnlswork, model = "within")
# 
# plm:::pwaldtest(plm_fe_nlswork, test = "F")                 # replicates Stata: F(8, 23386) = 610.12 - normal
# plm:::pwaldtest(plm_fe_nlswork, test = "F", .vcov = vcovHC) # replicates Stata: F(8, 4696)  = 273.86 - robust



### replicate Gretl ####
# library(foreign);library(plm)
# wagepan<-read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/wagepan.dta")
# pwagepan <- pdata.frame(wagepan, index = c("nr", "year"))
# pdim(pwagepan)
# 
# mod_fe_ind <- plm(lwage ~ exper + hours + married + expersq, data = pwagepan, model = "within", effect = "individual")
# 
# plm:::pwaldtest(mod_fe_ind, test="F")
# plm:::pwaldtest(mod_fe_ind, test="F", .vcov = function(x) vcovHC(x)) # 121.4972
# 
# # Gretl uses Stata's small sample adjustment
# g <- pdim(mod_fe_ind)$nT$n # no of individuals
# n <- pdim(mod_fe_ind)$nT$N # no of total obs
# k <- length(coefficients(mod_fe_ind))
# k <- k+1 # + 1 because Stata and Gretl have the intercept in the FE model
# adj <- (g/(g-1) * (n-1)/(n-k))
# vcov_mat_adj <- adj * plm::vcovHC(mod_fe_ind)
# print(plm:::pwaldtest(mod_fe_ind, test="F", .vcov = vcov_mat_adj), digits = 12) # replicate Gretl: F(4, 544) = 121.163


# Reference: Gretl (2016b)
#
# Gretl, wagepan data, fixed effects (oneway, HAC SEs)
# Model 1: Fixed-effects, using 4360 observations
# Included 545 cross-sectional units
# Time-series length = 8
# Dependent variable: lwage
# Robust (HAC) standard errors
# 
#              coefficient    std. error    t-ratio    p-value 
#   -----------------------------------------------------------
#   const       1.30069       0.0550817     23.61     2.15e-085 ***
#   exper       0.137331      0.0108430     12.67     2.12e-032 ***
#   hours      −0.000136467   2.13715e-05   −6.385    3.67e-010 ***
#   married     0.0481248     0.0213232      2.257    0.0244    **
#   expersq    −0.00532076    0.000692182   −7.687    7.09e-014 ***
# 
# Mean dependent var   1.649147   S.D. dependent var   0.532609
# Sum squared resid    459.8591   S.E. of regression   0.347371
# LSDV R-squared       0.628105   Within R-squared     0.196125
# Log-likelihood      −1283.082   Akaike criterion     3664.165
# Schwarz criterion    7166.910   Hannan-Quinn         4900.376
# rho                  0.065436   Durbin-Watson        1.546260
# 
# Joint test on named regressors -
#   Test statistic: F(4, 544) = 121.163
#   with p-value = P(F(4, 544) > 121.163) = 7.19472e-074
# 
# Robust test for differing group intercepts -
#   Null hypothesis: The groups have a common intercept
#   Test statistic: Welch F(544, 1276.3) = 26.9623
#   with p-value = P(F(544, 1276.3) > 26.9623) = 0



# Model 1: Fixed-effects, using 200 observations
# Included 10 cross-sectional units
# Time-series length = 20
# Dependent variable: inv
# Robust (HAC) standard errors
# 
#              coefficient   std. error   t-ratio   p-value 
#   --------------------------------------------------------
#   const      −58.7439      27.6029      −2.128    0.0622   *
#   value        0.110124     0.0151945    7.248    4.83e-05 ***
#   capital      0.310065     0.0527518    5.878    0.0002   ***
# 
# Mean dependent var   145.9582   S.D. dependent var   216.8753
# Sum squared resid    523478.1   S.E. of regression   52.76797
# LSDV R-squared       0.944073   Within R-squared     0.766758
# Log-likelihood      −1070.781   Akaike criterion     2165.562
# Schwarz criterion    2205.142   Hannan-Quinn         2181.579
# rho                  0.663920   Durbin-Watson        0.684480
# 
# Joint test on named regressors -
#   Test statistic: F(2, 9) = 28.3096
#   with p-value = P(F(2, 9) > 28.3096) = 0.000131055
# 
# Robust test for differing group intercepts -
#   Null hypothesis: The groups have a common intercept
#   Test statistic: Welch F(9, 70.6) = 85.9578
#   with p-value = P(F(9, 70.6) > 85.9578) = 1.90087e-034


# Model 6: Fixed-effects, using 200 observations
# Included 10 cross-sectional units
# Time-series length = 20
# Dependent variable: inv
# Robust (HAC) standard errors
# 
#              coefficient   std. error   t-ratio   p-value 
#   --------------------------------------------------------
#   const      −32.8363      19.7826      −1.660    0.1313  
#   value        0.117716     0.0108244   10.88     1.77e-06 ***
#   capital      0.357916     0.0478484    7.480    3.77e-05 ***
#   dt_2       −19.1974      20.6986      −0.9275   0.3779  
#   dt_3       −40.6900      33.2832      −1.223    0.2526  
#   dt_4       −39.2264      15.7365      −2.493    0.0343   **
#   dt_5       −69.4703      26.9988      −2.573    0.0300   **
#   dt_6       −44.2351      17.3723      −2.546    0.0314   **
#   dt_7       −18.8045      17.8475      −1.054    0.3195  
#   dt_8       −21.1398      14.1648      −1.492    0.1698  
#   dt_9       −42.9776      12.5441      −3.426    0.0076   ***
#   dt_10      −43.0988      10.9959      −3.920    0.0035   ***
#   dt_11      −55.6830      15.2019      −3.663    0.0052   ***
#   dt_12      −31.1693      20.9169      −1.490    0.1704  
#   dt_13      −39.3922      26.4371      −1.490    0.1704  
#   dt_14      −43.7165      38.8786      −1.124    0.2899  
#   dt_15      −73.4951      38.2545      −1.921    0.0869   *
#   dt_16      −75.8961      36.7985      −2.062    0.0692   *
#   dt_17      −62.4809      49.4181      −1.264    0.2379  
#   dt_18      −64.6323      51.5621      −1.253    0.2416  
#   dt_19      −67.7180      43.7447      −1.548    0.1560  
#   dt_20      −93.5262      31.7263      −2.948    0.0163   **
# 
# Mean dependent var   145.9582   S.D. dependent var   216.8753
# Sum squared resid    452147.1   S.E. of regression   51.72452
# LSDV R-squared       0.951693   Within R-squared     0.798540
# Log-likelihood      −1056.132   Akaike criterion     2174.264
# Schwarz criterion    2276.512   Hannan-Quinn         2215.643
# rho                  0.658860   Durbin-Watson        0.686728
# 
# Joint test on named regressors -
#   Test statistic: F(2, 9) = 60.0821
#   with p-value = P(F(2, 9) > 60.0821) = 6.22231e-006
# 
# Robust test for differing group intercepts -
#   Null hypothesis: The groups have a common intercept
#   Test statistic: Welch F(9, 76.7) = 53.1255
#   with p-value = P(F(9, 76.7) > 53.1255) = 2.45306e-029



# Model 5: Pooled OLS, using 200 observations
# Included 10 cross-sectional units
# Time-series length = 20
# Dependent variable: inv
# Robust (HAC) standard errors
# 
#              coefficient   std. error   t-ratio   p-value 
#   --------------------------------------------------------
#   const      −42.7144      20.4252      −2.091    0.0660   *
#   value        0.115562     0.0158943    7.271    4.71e-05 ***
#   capital      0.230678     0.0849671    2.715    0.0238   **
# 
# Mean dependent var   145.9582   S.D. dependent var   216.8753
# Sum squared resid     1755850   S.E. of regression   94.40840
# R-squared            0.812408   Adjusted R-squared   0.810504
# F(2, 9)              51.59060   P-value(F)           0.000012
# Log-likelihood      −1191.802   Akaike criterion     2389.605
# Schwarz criterion    2399.500   Hannan-Quinn         2393.609
# rho                  0.956242   Durbin-Watson        0.209717


# Model 2: Random-effects (GLS), using 200 observations
# Included 10 cross-sectional units
# Time-series length = 20
# Dependent variable: inv
# Robust (HAC) standard errors
# 
#              coefficient   std. error     z       p-value 
#   --------------------------------------------------------
#   const      −57.8344      24.8432      −2.328   0.0199    **
#   value        0.109781     0.0137557    7.981   1.45e-015 ***
#   capital      0.308113     0.0549728    5.605   2.08e-08  ***
# 
# Mean dependent var   145.9582   S.D. dependent var   216.8753
# Sum squared resid     1841062   S.E. of regression   96.42765
# Log-likelihood      −1196.541   Akaike criterion     2399.083
# Schwarz criterion    2408.978   Hannan-Quinn         2403.087
# 
# 'Between' variance = 7089.8
# 'Within' variance = 2784.46
# theta used for quasi-demeaning = 0.861224
# corr(y,yhat)^2 = 0.806104
# 
# Joint test on named regressors -
#   Asymptotic test statistic: Chi-square(2) = 70.1267
#   with p-value = 5.91814e-016
# 
# Breusch-Pagan test -
#   Null hypothesis: Variance of the unit-specific error = 0
#   Asymptotic test statistic: Chi-square(1) = 798.162
#   with p-value = 1.35448e-175
# 
# Hausman test -
#   Null hypothesis: GLS estimates are consistent
#   Asymptotic test statistic: Chi-square(2) = 7.31971
#   with p-value = 0.0257363


