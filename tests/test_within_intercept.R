### Test of within_intercept in connection with fixef() and comparision to Stata and Gretl
#
# TODO: modify to accomodate unbalanced two-way models?
#  
# (1) balanced
# (2) unbalanced

# test in connection with fixef:
library(plm)
data("Grunfeld", package = "plm")

############# (1) balanced ##############
# oneway individual balanced
gi <- plm(inv ~ value + capital, data = Grunfeld, model = "within", effect = "individual")
f_level_gi <- fixef(gi, type = "level")
f_dmean_gi <- fixef(gi, type = "dmean")
int_gi <- within_intercept(gi)
int_manual_gi <- mean(fixef(gi))
individual_intercepts_gi <- int_gi + f_dmean_gi

# check consistency of functions fixef and within_intercept
# works
if (!isTRUE(all.equal(individual_intercepts_gi, f_level_gi, check.attributes = FALSE))) stop("within_intercept: something is wrong")
if (!isTRUE(all.equal(int_gi, int_manual_gi, check.attributes = FALSE))) stop("within_intercept: something is wrong")

# oneway time balanced
gt <- plm(inv ~ value + capital, data = Grunfeld, model = "within", effect = "time")
f_level_gt <- fixef(gt, type = "level")
f_dmean_gt <- fixef(gt, type = "dmean")
int_gt <- within_intercept(gt)
int_manual_gt <- mean(fixef(gt))
individual_intercepts_gt <- int_gt + f_dmean_gt

# check consistency of functions fixef and within_intercept
# works
if (!isTRUE(all.equal(individual_intercepts_gt, f_level_gt, check.attributes = FALSE))) stop("within_intercept: something is wrong")
if (!isTRUE(all.equal(int_gt, int_manual_gt, check.attributes = FALSE))) stop("within_intercept: something is wrong")

# two-way individual, time balanced
gtw <- plm(inv ~ value + capital, data = Grunfeld, model = "within", effect = "twoways")
f_level_tw_i <- fixef(gtw, type = "level", effect = "individual")
f_dmean_tw_i <- fixef(gtw, type = "dmean", effect = "individual")
f_level_tw_t <- fixef(gtw, type = "level", effect = "time")
f_dmean_tw_t <- fixef(gtw, type = "dmean", effect = "time")

int_tw <- within_intercept(gtw)

# In the balanced case, the mean of the level effects must be the same
# and the means are the overall intercept
int_manual_tw_i <- mean(f_level_tw_i)
int_manual_tw_t <- mean(f_level_tw_t) # identical to int_manual_tw_i
if (!isTRUE(all.equal(int_manual_tw_i, int_manual_tw_t, check.attributes = FALSE))) stop("within_intercept twoways: something is wrong")
if (!isTRUE(all.equal(int_tw,          int_manual_tw_t, check.attributes = FALSE))) stop("within_intercept twoways: something is wrong")

individual_intercepts_tw_i <- int_tw + f_dmean_tw_i
individual_intercepts_tw_t <- int_tw + f_dmean_tw_t

# check consistency of functions fixef and within_intercept
# works
if (!isTRUE(all.equal(individual_intercepts_tw_i, f_level_tw_i, check.attributes = FALSE))) stop("within_intercept twoways, individual: something is wrong")
if (!isTRUE(all.equal(individual_intercepts_tw_t, f_level_tw_t, check.attributes = FALSE))) stop("within_intercept twoways, time:       something is wrong")





############# (2) unbalanced tests ################
Grunfeld_unbalanced <- Grunfeld[-c(200), ]

# oneway individual unbalanced
gi_u <- plm(inv ~ value + capital, data = Grunfeld_unbalanced, model = "within", effect = "individual")
f_level_gi_u <- fixef(gi_u, type = "level")
f_dmean_gi_u <- fixef(gi_u, type = "dmean")


# in the one-way unbalanced case: is the overall intercept is the weighted mean of the effects
# (with the current fixef implementation) - this check also depends on how type = "dmean" is calculated in fixef
int_gi_u <- within_intercept(gi_u)
individual_intercepts_gi_u <- int_gi_u + f_dmean_gi_u

int_manual_gi_u <- weighted.mean(fixef(gi_u), as.numeric(table(index(gi_u)[[1]])))
mean(f_level_gi_u)
individual_intercepts_gi_u2 <- mean(f_level_gi_u) + f_dmean_gi_u

# check consistency of functions in themselves
# if (!isTRUE(all.equal(individual_intercepts_gi_u,  f_level_gi_u, check.attributes = FALSE))) stop("within_intercept, unbalanced: something is wrong")
# if (!isTRUE(all.equal(individual_intercepts_gi_u2, f_level_gi_u, check.attributes = FALSE))) stop("within_intercept, unbalanced: something is wrong")

#if (!isTRUE(all.equal(int_gi_u, int_manual_gi_u, check.attributes = FALSE))) stop("within_intercept, unbalanced: something is wrong")

# take weighted.mean:
# if (!isTRUE(all.equal(individual_intercepts_gi_u2, f_level_gi_u, check.attributes = FALSE))) stop("within_intercept, unbalanced: something is wrong")


# oneway time unbalanced
gt_u <- plm(inv ~ value + capital, data = Grunfeld_unbalanced, model = "within", effect = "time")
f_level_gt_u <- fixef(gt_u, type = "level")
f_dmean_gt_u <- fixef(gt_u, type = "dmean")
int_gt_u <- within_intercept(gt_u)
individual_intercepts_gt_u <- int_gt_u + f_dmean_gt_u


# fails for unbalanced models, likely due to fixef calc. for dmean: fixef - mean(fixef); need weighted mean?
# check consistency of functions in themselves
# if (!isTRUE(all.equal(individual_intercepts_gt_u, f_level_gt_u, check.attributes = FALSE))) stop("within_intercept, unbalanced: something is wrong")

## take weighted mean for "dmean" calc. instead:
# int_manual_gt_u <- weighted.mean(fixef(gt_u), as.numeric(table(index(gt_u)[[2]])))
# mean(f_level_gt_u)
# 
# int_gt_u <- within_intercept(gt_u)
# 
# if (!isTRUE(all.equal(int_gt_u, int_manual_gt_u, check.attributes = FALSE))) stop("within_intercept, unbalanced: something is wrong")
# 
# f_dmean_gt_u2 <- f_level_gt_u - int_manual_gt_u
# individual_intercepts_gt_u2 <- int_gt_u + f_dmean_gt_u2
# # taking weighted.mean: works!
# if (!isTRUE(all.equal(individual_intercepts_gt_u2, f_level_gt_u, check.attributes = FALSE))) stop("within_intercept, unbalanced: something is wrong")
# 



## twoways unbalanced
gtw_u <- plm(inv ~ value + capital, data = Grunfeld_unbalanced, model = "within", effect = "twoways")
f_level_tw_i_u <- fixef(gtw_u, type = "level", effect = "individual") # these effects are correct since rev. 278
f_level_tw_t_u <- fixef(gtw_u, type = "level", effect = "time")       # 
f_dmean_tw_i_u <- fixef(gtw_u, type = "dmean", effect = "individual")
f_dmean_tw_t_u <- fixef(gtw_u, type = "dmean", effect = "time")

int_tw_u <- within_intercept(gtw_u)

weights_gtw_i_u <- as.numeric(table(index(gtw_u)[[1]]))
weights_gtw_t_u <- as.numeric(table(index(gtw_u)[[2]]))

# in the unbalanced case, the means of the level fixed effects are not the same
# (like they were in the balanced case)
int_manual_tw_i_u <- mean(f_level_tw_i_u)
int_manual_tw_t_u <- mean(f_level_tw_t_u)
all.equal(int_manual_tw_i_u, int_manual_tw_t_u) # not equal
# ... but weighted means are
int_manual_tw_i_u2 <- crossprod(f_level_tw_i_u, weights_gtw_i_u) / sum(weights_gtw_i_u)
int_manual_tw_t_u2 <- crossprod(f_level_tw_t_u, weights_gtw_t_u) / sum(weights_gtw_t_u)


# TODO: within_intercept needs to be modified to handle the two-way unbalanced case?
individual_intercepts_tw_i_u <- int_tw_u + f_dmean_tw_i_u
individual_intercepts_tw_t_u <- int_tw_u + f_dmean_tw_t_u

individual_intercepts_tw_i_u2 <- int_manual_tw_i_u2 + f_dmean_tw_i_u
individual_intercepts_tw_t_u2 <- int_manual_tw_t_u2 + f_dmean_tw_t_u

mod_lm <- lm(inv ~ value + capital + factor(firm) + factor(year), data = Grunfeld_unbalanced)

# check consistency of functions fixef and within_intercept
# fails for unbalanced models, likely due to fixef calc. for dmean: fixef - mean(fixef); need weighted mean?

#if (!isTRUE(all.equal(individual_intercepts_tw_i_u2, f_level_tw_i_u, check.attributes = FALSE))) stop("within_intercept twoways, individual: something is wrong")
#if (!isTRUE(all.equal(individual_intercepts_tw_t_u2, f_level_tw_t_u, check.attributes = FALSE))) stop("within_intercept twoways, time:       something is wrong")
#if (!isTRUE(all.equal(individual_intercepts_tw_i_u, f_level_tw_i_u, check.attributes = FALSE))) stop("within_intercept twoways, individual: something is wrong")
#if (!isTRUE(all.equal(individual_intercepts_tw_t_u, f_level_tw_t_u, check.attributes = FALSE))) stop("within_intercept twoways, time:       something is wrong")
#if (!isTRUE(all.equal(int_tw_u, int_manual_tw_u, check.attributes = FALSE))) stop("within_intercept: something is wrong")



######### Test with reference case: balanced panel
## commented because it needs extra library 'foreign'
# library(foreign);library(plm)
# wagepan<-read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/wagepan.dta")
# pwagepan <- pdata.frame(wagepan, index = c("nr", "year"))
# pdim(pwagepan)
# 
# mod_fe_ind <- plm(lwage ~ exper + hours + married + expersq, data = pwagepan, model = "within", effect = "individual")
# summary(mod_fe_ind)
# # matches gretl, balanced panel, individual effect (see below)
# inter_mod_fe_ind <- plm:::within_intercept.plm(mod_fe_ind)
# print(inter_mod_fe_ind)
# mean(fixef(mod_fe_ind))
# 
# # matches Gretl robust SE
# inter_mod_fe_ind_robust <- plm:::within_intercept.plm(mod_fe_ind, vcov = function(x) plm::vcovHC(x, method="arellano", type="HC0"))
# print(inter_mod_fe_ind_robust)


# Some data to compare to:
# gretl: Data wagepan, individual effects, "normal" standard errors
#
# Model 1: Fixed-effects, using 4360 observations
# Included 545 cross-sectional units
# Time-series length = 8
# Dependent variable: lwage
# 
#              coefficient    std. error    t-ratio    p-value 
#   -----------------------------------------------------------
#   const       1.30069       0.0334564      38.88    8.95e-279 ***
#   exper       0.137331      0.00856279     16.04    4.56e-056 ***
#   hours      −0.000136467   1.33668e-05   −10.21    3.67e-024 ***
#   married     0.0481248     0.0181012       2.659   0.0079    ***
#   expersq    −0.00532076    0.000606304    −8.776   2.52e-018 ***
# 
# Mean dependent var   1.649147   S.D. dependent var   0.532609
# Sum squared resid    459.8591   S.E. of regression   0.347371
# LSDV R-squared       0.628105   Within R-squared     0.196125
# LSDV F(548, 3811)    11.74547   P-value(F)           0.000000
# Log-likelihood      −1283.082   Akaike criterion     3664.165
# Schwarz criterion    7166.910   Hannan-Quinn         4900.376
# rho                  0.065436   Durbin-Watson        1.546260
# 
# Joint test on named regressors -
#   Test statistic: F(4, 3811) = 232.447
#   with p-value = P(F(4, 3811) > 232.447) = 8.13484e-179
# 
# Test for differing group intercepts -
#   Null hypothesis: The groups have a common intercept
#   Test statistic: F(544, 3811) = 10.3148
#   with p-value = P(F(544, 3811) > 10.3148) = 0


# gretl: Data wagepan, individual effects, HAC standard errors
#
# Model 1: Fixed-effects, using 4360 observations
# Included 545 cross-sectional units
# Time-series length = 8
# Dependent variable: lwage
# Robust (HAC) standard errors
# Omitted due to exact collinearity: black hisp
# 
#              coefficient    std. error    t-ratio    p-value 
#   -----------------------------------------------------------
#   const       1.30069       0.0550059     23.65     1.82e-115 ***
#   exper       0.137331      0.0108281     12.68     3.92e-036 ***
#   hours      −0.000136467   2.13420e-05   −6.394    1.81e-010 ***
#   married     0.0481248     0.0212938      2.260    0.0239    **
#   expersq    −0.00532076    0.000691230   −7.698    1.76e-014 ***
# 
# Mean dependent var   1.649147   S.D. dependent var   0.532609
# Sum squared resid    459.8591   S.E. of regression   0.347371
# LSDV R-squared       0.628105   Within R-squared     0.196125
# Log-likelihood      −1283.082   Akaike criterion     3664.165
# Schwarz criterion    7166.910   Hannan-Quinn         4900.376
# rho                  0.065436   Durbin-Watson        1.546260
# 
# Joint test on named regressors -
#   Test statistic: F(4, 3811) = 121.497
#   with p-value = P(F(4, 3811) > 121.497) = 1.02521e-097
# 
# Robust test for differing group intercepts -
#   Null hypothesis: The groups have a common intercept
#   Test statistic: Welch F(544, 1276.3) = 27.3958
#   with p-value = P(F(544, 1276.3) > 27.3958) = 0


####
# Gretl, twoways, Grunfeld, balanced panel, normal SEs
#    -- Gretl does only time dummies, no sweeping out of time effect in the data
#       -> not comparable because constant becomes the reference year
# Model 2: Fixed-effects, using 200 observations
# Included 10 cross-sectional units
# Time-series length = 20
# Dependent variable: inv
# 
#              coefficient   std. error   t-ratio    p-value 
#   ---------------------------------------------------------
#   const      −32.8363      18.8753      −1.740    0.0837    *
#   value        0.117716     0.0137513    8.560    6.65e-015 ***
#   capital      0.357916     0.0227190   15.75     5.45e-035 ***
#   dt_2       −19.1974      23.6759      −0.8108   0.4186   
#   dt_3       −40.6900      24.6954      −1.648    0.1013   
#   dt_4       −39.2264      23.2359      −1.688    0.0932    *
#   dt_5       −69.4703      23.6561      −2.937    0.0038    ***
#   dt_6       −44.2351      23.8098      −1.858    0.0649    *
#   dt_7       −18.8045      23.6940      −0.7936   0.4285   
#   dt_8       −21.1398      23.3816      −0.9041   0.3672   
#   dt_9       −42.9776      23.5529      −1.825    0.0698    *
#   dt_10      −43.0988      23.6102      −1.825    0.0697    *
#   dt_11      −55.6830      23.8956      −2.330    0.0210    **
#   dt_12      −31.1693      24.1160      −1.292    0.1980   
#   dt_13      −39.3922      23.7837      −1.656    0.0995    *
#   dt_14      −43.7165      23.9697      −1.824    0.0699    *
#   dt_15      −73.4951      24.1829      −3.039    0.0028    ***
#   dt_16      −75.8961      24.3455      −3.117    0.0021    ***
#   dt_17      −62.4809      24.8643      −2.513    0.0129    **
#   dt_18      −64.6323      25.3495      −2.550    0.0117    **
#   dt_19      −67.7180      26.6111      −2.545    0.0118    **
#   dt_20      −93.5262      27.1079      −3.450    0.0007    ***





## Test unbalanced panel 
####### replicate Stata's fixed effects estimator, R-squared, F statistic ###
## http://www.stata.com/manuals14/xtxtreg.pdf [example 2 on p. 14, ex. 3 on p. 16]
#
# commented because it needs extra library 'foreign'
# 
# normal SE (ex. 2, p. 14)
# Stata's intercept (coefficient, Standard error)
#             _cons    1.03732  ,   .0485546
#
# robust SE (ex. 3, p. 16)
#             _cons    1.03732  ,   .0739644

# library(plm)
# library(haven)
# nlswork <- haven::read_dta("http://www.stata-press.com/data/r14/nlswork.dta") # large file
# nlswork$race <- factor(nlswork$race) # convert
# nlswork$race2 <- factor(ifelse(nlswork$race == 2, 1, 0)) # need this variable for example
# nlswork$grade <- as.numeric(nlswork$grade)
# pnlswork <- pdata.frame(nlswork, index=c("idcode", "year"), drop.index=F)
# 
# form_nls_ex2 <- formula(ln_wage ~ grade + age + I(age^2) + ttl_exp + I(ttl_exp^2) + tenure + I(tenure^2) + race2 + not_smsa + south)
# 
# plm_fe_nlswork <- plm(form_nls_ex2, data = pnlswork, model = "within", effect = "individual")
# 
# int_fe_nls_work <- within_intercept(plm_fe_nlswork) # matches Stata "normal" SE
# print(int_fe_nls_work)
# weighted.mean(fixef(plm_fe_nlswork), w = as.numeric(table(index(plm_fe_nlswork)[[1]])))
# summary(plm_fe_nlswork)
# summary(plm_fe_nlswork, vcov = vcovHC(plm_fe_nlswork, type="sss"))
# int_fe_nls_work_robust <- within_intercept(plm_fe_nlswork, vcov = function(x) vcovHC(x, type="sss"))  # matches Stata robust SE
# print(int_fe_nls_work_robust)

