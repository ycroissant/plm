#### Testfile for Ftest()
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
# Chisq
plm:::Ftest(gp, test = "Chisq")
plm:::Ftest(gi, test = "Chisq")
plm:::Ftest(gt, test = "Chisq")
plm:::Ftest(gd, test = "Chisq")

# F
plm:::Ftest(gp, test = "F")
plm:::Ftest(gi, test = "F")
plm:::Ftest(gt, test = "F")
plm:::Ftest(gd, test = "F")

# Chisq - robust - formula
plm:::Ftest(gp, test = "Chisq", .vcov = vcovHC)
plm:::Ftest(gi, test = "Chisq", .vcov = vcovHC)
plm:::Ftest(gt, test = "Chisq", .vcov = vcovHC)
plm:::Ftest(gd, test = "Chisq", .vcov = vcovHC)

# Chisq - robust - matrix
plm:::Ftest(gp, test = "Chisq", .vcov = vcovHC(gp))
plm:::Ftest(gi, test = "Chisq", .vcov = vcovHC(gi))
plm:::Ftest(gt, test = "Chisq", .vcov = vcovHC(gt))
plm:::Ftest(gd, test = "Chisq", .vcov = vcovHC(gd))

# F - robust - formula
plm:::Ftest(gp, test = "F", .vcov = vcovHC) # replicates Gretl: F(2, 9)=57.90485
plm:::Ftest(gi, test = "F", .vcov = vcovHC) # replicates Gretl: F(2, 9)=31.7744
plm:::Ftest(gi, test = "F", .vcov = function(x) vcovHC(x, cluster = "time")) # cluster on time, df2 = 19
plm:::Ftest(gt, test = "F", .vcov = vcovHC)
plm:::Ftest(gd, test = "F", .vcov = vcovHC) # replicates Gretl: F(2, 9)=74.6338

# F - robust - matrix
plm:::Ftest(gp, test = "F", .vcov = vcovHC(gp)) # replicates Gretl: F(2, 9)=57.90485
plm:::Ftest(gi, test = "F", .vcov = vcovHC(gi)) # replicates Gretl: F(2, 9)=31.7744
plm:::Ftest(gi, test = "F", .vcov = function(x) vcovHC(x, cluster = "time")) # cluster on time, df2 = 19
plm:::Ftest(gt, test = "F", .vcov = vcovHC(gt))
plm:::Ftest(gd, test = "F", .vcov = vcovHC(gd)) # replicates Gretl: F(2, 9)=74.6338




############### compare to other statistics packages:

## package 'lfe'
# library(lfe)
# data("Grunfeld", package = "plm")
# gi_lfe <- felm(inv ~ value + capital | firm, data = Grunfeld)
# gi_lfe_cluster <- felm(inv ~ value + capital | firm, data = Grunfeld, clustervar="firm")
# summary(gi_lfe)
# summary(gi_lfe_cluster)
# waldtest(gi_lfe, R = names(coef(gi_lfe))) # df2 = 188
# waldtest(gi_lfe_cluster, R = names(coef(gi_lfe_cluster))) # chi2: 54.03250, F. 27.01625, df1 = 2, df2 = 9
# gi_lfe_cluster$clustervcv #  # this vcov is not identical to vcovHC, so results do not match
# 
# mod_felm_ind <- felm(lwage ~ exper + hours + married + expersq | nr, data = pwagepan, clustervar="nr")
# summary(mod_felm_ind)
# waldtest(mod_felm_ind, R = names(coef(mod_felm_ind)))
# round(waldtest(mod_felm_ind, R = names(coef(mod_felm_ind))), digits = 8)
# mod_felm_ind$clustervcv # this vcov is not identical to vcovHC, so results do not match

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
# plm:::Ftest(plm_fe_nlswork, test = "F")                 # replicates Stata: F(8, 23386) = 610.12 - normal
# plm:::Ftest(plm_fe_nlswork, test = "F", .vcov = vcovHC) # replicates Stata: F(8, 4696)  = 273.86 - robust



### Gretl ####
# library(foreign);library(plm)
# wagepan<-read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/wagepan.dta")
# pwagepan <- pdata.frame(wagepan, index = c("nr", "year"))
# pdim(pwagepan)
# 
# mod_fe_ind <- plm(lwage ~ exper + hours + married + expersq, data = pwagepan, model = "within", effect = "individual")
# 
# pdim(mod_fe_ind)
# plm:::Ftest(mod_fe_ind, test="F")
# plm:::Ftest(mod_fe_ind, test="F", .vcov = function(x) vcovHC(x)) # replicates Gretl: F(4, 544)=121.497



# Gretl, wagepan data, fixed effects (oneway, HAC SEs)
# Gretl (2016a) currently does use the normal degrees of freedom
# NB: need at least the development version of 2016b  (built date >= 2016-03-26)
#
# Model 1: Fixed-effects, using 4360 observations
# Included 545 cross-sectional units
# Time-series length = 8
# Dependent variable: lwage
# Robust (HAC) standard errors
# 
#              coefficient    std. error    t-ratio    p-value 
#   -----------------------------------------------------------
#   const       1.30069       0.0550059     23.65     1.47e-085 ***
#   exper       0.137331      0.0108281     12.68     1.79e-032 ***
#   hours      −0.000136467   2.13420e-05   −6.394    3.48e-010 ***
#   married     0.0481248     0.0212938      2.260    0.0242    **
#   expersq    −0.00532076    0.000691230   −7.698    6.58e-014 ***
# 
# Mean dependent var   1.649147   S.D. dependent var   0.532609
# Sum squared resid    459.8591   S.E. of regression   0.347371
# LSDV R-squared       0.628105   Within R-squared     0.196125
# Log-likelihood      −1283.082   Akaike criterion     3664.165
# Schwarz criterion    7166.910   Hannan-Quinn         4900.376
# rho                  0.065436   Durbin-Watson        1.546260
# 
# Joint test on named regressors -
#   Test statistic: F(4, 544) = 121.497
#   with p-value = P(F(4, 544) > 121.497) = 5.06079e-074
# 
# Robust test for differing group intercepts -
#   Null hypothesis: The groups have a common intercept
#   Test statistic: Welch F(544, 1276.3) = 26.9623
#   with p-value = P(F(544, 1276.3) > 26.9623) = 0


# Gretl, Grunfeld data, fixed effects (oneway, HAC SEs)
# Model 1: Fixed-effects, using 200 observations
# Included 10 cross-sectional units
# Time-series length = 20
# Dependent variable: inv
# Robust (HAC) standard errors
# 
#              coefficient   std. error   t-ratio   p-value 
#   --------------------------------------------------------
#   const      −58.7439      26.0545      −2.255    0.0506   *
#   value        0.110124     0.0143421    7.678    3.07e-05 ***
#   capital      0.310065     0.0497926    6.227    0.0002   ***
# 
# Mean dependent var   145.9582   S.D. dependent var   216.8753
# Sum squared resid    523478.1   S.E. of regression   52.76797
# LSDV R-squared       0.944073   Within R-squared     0.766758
# Log-likelihood      −1070.781   Akaike criterion     2165.562
# Schwarz criterion    2205.142   Hannan-Quinn         2181.579
# rho                  0.663920   Durbin-Watson        0.684480
# 
# Joint test on named regressors -
#   Test statistic: F(2, 9) = 31.7744
#   with p-value = P(F(2, 9) > 31.7744) = 8.34168e-005
# 
# Robust test for differing group intercepts -
#   Null hypothesis: The groups have a common intercept
#   Test statistic: Welch F(9, 70.6) = 85.9578
#   with p-value = P(F(9, 70.6) > 85.9578) = 1.90087e-034

# Gretl, Grunfeld data, fixed effects (oneway with time dummies, HAC SEs)
# Model 2: Fixed-effects, using 200 observations
# Included 10 cross-sectional units
# Time-series length = 20
# Dependent variable: inv
# Robust (HAC) standard errors
# 
#              coefficient   std. error    t-ratio   p-value 
#   ---------------------------------------------------------
#   const      −32.8363      17.7496       −1.850    0.0974   *
#   value        0.117716     0.00971202   12.12     7.07e-07 ***
#   capital      0.357916     0.0429311     8.337    1.59e-05 ***
#   dt_2       −19.1974      18.5714       −1.034    0.3282  
#   dt_3       −40.6900      29.8627       −1.363    0.2061  
#   dt_4       −39.2264      14.1193       −2.778    0.0215   **
#   dt_5       −69.4703      24.2241       −2.868    0.0185   **
#   dt_6       −44.2351      15.5870       −2.838    0.0195   **
#   dt_7       −18.8045      16.0133       −1.174    0.2704  
#   dt_8       −21.1398      12.7091       −1.663    0.1306  
#   dt_9       −42.9776      11.2550       −3.819    0.0041   ***
#   dt_10      −43.0988       9.86584      −4.368    0.0018   ***
#   dt_11      −55.6830      13.6396       −4.082    0.0027   ***
#   dt_12      −31.1693      18.7673       −1.661    0.1311  
#   dt_13      −39.3922      23.7202       −1.661    0.1311  
#   dt_14      −43.7165      34.8831       −1.253    0.2417  
#   dt_15      −73.4951      34.3232       −2.141    0.0609   *
#   dt_16      −75.8961      33.0168       −2.299    0.0471   **
#   dt_17      −62.4809      44.3395       −1.409    0.1924  
#   dt_18      −64.6323      46.2631       −1.397    0.1959  
#   dt_19      −67.7180      39.2491       −1.725    0.1185  
#   dt_20      −93.5262      28.4659       −3.286    0.0094   ***
# 
# Mean dependent var   145.9582   S.D. dependent var   216.8753
# Sum squared resid    452147.1   S.E. of regression   51.72452
# LSDV R-squared       0.951693   Within R-squared     0.798540
# Log-likelihood      −1056.132   Akaike criterion     2174.264
# Schwarz criterion    2276.512   Hannan-Quinn         2215.643
# rho                  0.658860   Durbin-Watson        0.686728
# 
# Joint test on named regressors -
#   Test statistic: F(2, 9) = 74.6338
#   with p-value = P(F(2, 9) > 74.6338) = 2.4936e-006
# 
# Robust test for differing group intercepts -
#   Null hypothesis: The groups have a common intercept
#   Test statistic: Welch F(9, 76.7) = 53.1255
#   with p-value = P(F(9, 76.7) > 53.1255) = 2.45306e-029

# Gretl, Grunfeld data, pooled OLS
# Model 1: Pooled OLS, using 200 observations
# Included 10 cross-sectional units
# Time-series length = 20
# Dependent variable: inv
# Robust (HAC) standard errors
# 
#              coefficient   std. error   t-ratio   p-value 
#   --------------------------------------------------------
#   const      −42.7144      19.2794      −2.216    0.0540   *
#   value        0.115562     0.0150027    7.703    2.99e-05 ***
#   capital      0.230678     0.0802008    2.876    0.0183   **
# 
# Mean dependent var   145.9582   S.D. dependent var   216.8753
# Sum squared resid     1755850   S.E. of regression   94.40840
# R-squared            0.812408   Adjusted R-squared   0.810504
# F(2, 9)              57.90485   P-value(F)           7.26e-06
# Log-likelihood      −1191.802   Akaike criterion     2389.605
# Schwarz criterion    2399.500   Hannan-Quinn         2393.609
# rho                  0.956242   Durbin-Watson        0.209717
