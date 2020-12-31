#### Replicate results of various sources and additional run tests, compared to the corresponding .Rout.save
####
#### (1): Baltagi (2013)
#### (2): Stata's FE estimator
#### (3): test of unbalanced one-way RE Swamy/Arora on Hedonic data set

#### (1) ####
#### compare OLS, FE and RE estimators to Baltagi's results
# Baltagi (2013), Econometric Analysis of Panel Data, 5th edition, Wiley & Sons
#    oneway:  sec. 2.6, example 1 p. 27, table 2.1
#    twoways: sec. 3.6, example 1 p. 51, table 3.1
#
# = Baltagi (2005), p. 21 (oneway), p. 43 (twoways) 

# Table 2.1 Grunfeld's Data One-way Error Component Results
#           beta1     beta2   rho  sigma_me sigma_nu
#--------------------------------------------------
# [...]

library(plm)
data("Grunfeld", package = "plm")
Grunfeld_unbal <- Grunfeld[1:199, ]

#### oneway individual balanced
plm_grunfeld_pooled     <- plm(inv ~ value + capital, data=Grunfeld, model="pooling")
plm_grunfeld_be         <- plm(inv ~ value + capital, data=Grunfeld, model="between")
plm_grunfeld_fe         <- plm(inv ~ value + capital, data=Grunfeld, model="within")
plm_grunfeld_re_walhus  <- plm(inv ~ value + capital, data=Grunfeld, model="random", random.method="walhus")
plm_grunfeld_re_amemiya <- plm(inv ~ value + capital, data=Grunfeld, model="random", random.method="amemiya")
plm_grunfeld_re_swar    <- plm(inv ~ value + capital, data=Grunfeld, model="random", random.method="swar")
plm_grunfeld_re_nerlove <- plm(inv ~ value + capital, data=Grunfeld, model="random", random.method="nerlove")

summary(plm_grunfeld_pooled    )
summary(plm_grunfeld_be        )
summary(plm_grunfeld_fe        )
summary(plm_grunfeld_re_walhus )
summary(plm_grunfeld_re_swar   )
summary(plm_grunfeld_re_amemiya)
summary(plm_grunfeld_re_nerlove)

#### oneway time balanced
plm_grunfeld_be_time         <- plm(inv ~ value + capital, data=Grunfeld, model="between", effect = "time")
plm_grunfeld_fe_time         <- plm(inv ~ value + capital, data=Grunfeld, model="within",  effect = "time")
plm_grunfeld_re_walhus_time  <- plm(inv ~ value + capital, data=Grunfeld, model="random", random.method="walhus",  effect = "time")
plm_grunfeld_re_amemiya_time <- plm(inv ~ value + capital, data=Grunfeld, model="random", random.method="amemiya", effect = "time")
plm_grunfeld_re_swar_time    <- plm(inv ~ value + capital, data=Grunfeld, model="random", random.method="swar",    effect = "time")
plm_grunfeld_re_nerlove_time <- plm(inv ~ value + capital, data=Grunfeld, model="random", random.method="nerlove", effect = "time")

summary(plm_grunfeld_be_time        )
summary(plm_grunfeld_fe_time        )
summary(plm_grunfeld_re_walhus_time )
summary(plm_grunfeld_re_swar_time   )
summary(plm_grunfeld_re_amemiya_time)
summary(plm_grunfeld_re_nerlove_time)

#### oneway individual unbalanced
plm_grunfeld_be_unbal         <- plm(inv ~ value + capital, data=Grunfeld_unbal, model="between")
plm_grunfeld_fe_unbal         <- plm(inv ~ value + capital, data=Grunfeld_unbal, model="within")
plm_grunfeld_re_walhus_unbal  <- plm(inv ~ value + capital, data=Grunfeld_unbal, model="random", random.method="walhus")
plm_grunfeld_re_amemiya_unbal <- plm(inv ~ value + capital, data=Grunfeld_unbal, model="random", random.method="amemiya")
plm_grunfeld_re_swar_unbal    <- plm(inv ~ value + capital, data=Grunfeld_unbal, model="random", random.method="swar")
plm_grunfeld_re_nerlove_unbal <- plm(inv ~ value + capital, data=Grunfeld_unbal, model="random", random.method="nerlove")

summary(plm_grunfeld_be_unbal        )
summary(plm_grunfeld_fe_unbal        )
summary(plm_grunfeld_re_walhus_unbal )
summary(plm_grunfeld_re_swar_unbal   )
summary(plm_grunfeld_re_amemiya_unbal)
summary(plm_grunfeld_re_nerlove_unbal)

#### oneway time unbalanced
plm_grunfeld_be_time_unbal         <- plm(inv ~ value + capital, data=Grunfeld_unbal, model="between", effect = "time")
plm_grunfeld_fe_time_unbal         <- plm(inv ~ value + capital, data=Grunfeld_unbal, model="within",  effect = "time")
plm_grunfeld_re_walhus_time_unbal  <- plm(inv ~ value + capital, data=Grunfeld_unbal, model="random", random.method="walhus",  effect = "time")
plm_grunfeld_re_amemiya_time_unbal <- plm(inv ~ value + capital, data=Grunfeld_unbal, model="random", random.method="amemiya", effect = "time")
plm_grunfeld_re_swar_time_unbal    <- plm(inv ~ value + capital, data=Grunfeld_unbal, model="random", random.method="swar",    effect = "time")
plm_grunfeld_re_nerlove_time_unbal <- plm(inv ~ value + capital, data=Grunfeld_unbal, model="random", random.method="nerlove", effect = "time")

summary(plm_grunfeld_be_time_unbal        )
summary(plm_grunfeld_fe_time_unbal        )
summary(plm_grunfeld_re_walhus_time_unbal )
summary(plm_grunfeld_re_swar_time_unbal   )
summary(plm_grunfeld_re_amemiya_time_unbal)
summary(plm_grunfeld_re_nerlove_time_unbal)




# Table 3.1 Grunfeld's Data. Two-way Error Component Results
#  RE estimators: SWAR and WALHUS yield negative estimates of
#  sigma_lambda^2 and these are set to zero in the table.
#
#           beta1     beta2   rho  sigma_me sigma_nu
#-------------------------------------------------
# [...]

#### twoways balanced
plm_grunfeld_pooled_tw     <- plm(inv ~ value + capital, data=Grunfeld, model="pooling", effect = "twoways")
plm_grunfeld_fe_tw         <- plm(inv ~ value + capital, data=Grunfeld, model="within",  effect = "twoways")
plm_grunfeld_re_walhus_tw  <- plm(inv ~ value + capital, data=Grunfeld, model="random", random.method="walhus",  effect = "twoways")
plm_grunfeld_re_amemiya_tw <- plm(inv ~ value + capital, data=Grunfeld, model="random", random.method="amemiya", effect = "twoways")
plm_grunfeld_re_swar_tw    <- plm(inv ~ value + capital, data=Grunfeld, model="random", random.method="swar",    effect = "twoways")
plm_grunfeld_re_nerlove_tw <- plm(inv ~ value + capital, data=Grunfeld, model="random", random.method="nerlove", effect = "twoways")

## IGNORE_RDIFF_BEGIN
summary(plm_grunfeld_pooled_tw    )
summary(plm_grunfeld_fe_tw        )
summary(plm_grunfeld_re_walhus_tw )
summary(plm_grunfeld_re_amemiya_tw)
summary(plm_grunfeld_re_swar_tw   )
summary(plm_grunfeld_re_nerlove_tw)
## IGNORE_RDIFF_END

##### twoways unbalanced
plm_grunfeld_pooled_tw_unbal     <- plm(inv ~ value + capital, data=Grunfeld_unbal, model="pooling", effect = "twoways")
plm_grunfeld_fe_tw_unbal         <- plm(inv ~ value + capital, data=Grunfeld_unbal, model="within",  effect = "twoways")
plm_grunfeld_re_walhus_tw_unbal  <- plm(inv ~ value + capital, data=Grunfeld_unbal, model="random", random.method="walhus",  effect = "twoways")
plm_grunfeld_re_amemiya_tw_unbal <- plm(inv ~ value + capital, data=Grunfeld_unbal, model="random", random.method="amemiya", effect = "twoways")
plm_grunfeld_re_swar_tw_unbal    <- plm(inv ~ value + capital, data=Grunfeld_unbal, model="random", random.method="swar",    effect = "twoways")
plm_grunfeld_re_nerlove_tw_unbal <- plm(inv ~ value + capital, data=Grunfeld_unbal, model="random", random.method="nerlove", effect = "twoways")

## IGNORE_RDIFF_BEGIN
summary(plm_grunfeld_pooled_tw_unbal    )
summary(plm_grunfeld_fe_tw_unbal        )
summary(plm_grunfeld_re_walhus_tw_unbal )
summary(plm_grunfeld_re_amemiya_tw_unbal)
summary(plm_grunfeld_re_swar_tw_unbal   )
summary(plm_grunfeld_re_nerlove_tw_unbal)
## IGNORE_RDIFF_END

### "amemiya" and "swar" have the same idiosyncratic variance (both based on the within variance)
# if (!isTRUE(all.equal(ercomp(plm_grunfeld_re_amemiya)[["sigma2"]][["idios"]], ercomp(plm_grunfeld_re_swar)[["sigma2"]][["idios"]])))
#   stop("idiosyncratic variance for 'amemiya' and 'swar' differ!")
# 
# if (!isTRUE(all.equal(ercomp(plm_grunfeld_re_amemiya_time)[["sigma2"]][["idios"]], ercomp(plm_grunfeld_re_swar_time)[["sigma2"]][["idios"]])))
#   stop("idiosyncratic variance for 'amemiya' and 'swar' differ!")
# 
# if (!isTRUE(all.equal(ercomp(plm_grunfeld_re_amemiya_unbal)[["sigma2"]][["idios"]], ercomp(plm_grunfeld_re_swar_unbal)[["sigma2"]][["idios"]])))
#   stop("idiosyncratic variance for 'amemiya' and 'swar' differ!")
# 
# if (!isTRUE(all.equal(ercomp(plm_grunfeld_re_amemiya_time_unbal)[["sigma2"]][["idios"]], ercomp(plm_grunfeld_re_swar_time_unbal)[["sigma2"]][["idios"]])))
#   stop("idiosyncratic variance for 'amemiya' and 'swar' differ!")
# 
# if (!isTRUE(all.equal(ercomp(plm_grunfeld_re_amemiya_tw)[["sigma2"]][["idios"]], ercomp(plm_grunfeld_re_swar_tw)[["sigma2"]][["idios"]])))
#   stop("idiosyncratic variance for 'amemiya' and 'swar' differ!")
# 
# if (!isTRUE(all.equal(ercomp(plm_grunfeld_re_amemiya_tw_unbal)[["sigma2"]][["idios"]], ercomp(plm_grunfeld_re_swar_tw_unbal)[["sigma2"]][["idios"]])))
#   stop("idiosyncratic variance for 'amemiya' and 'swar' differ!")



#### (2) ####
####### replicate Stata's fixed effects estimator, R-squared, F statistic ###
## http://www.stata.com/manuals/xtxtreg.pdf [example 2 on p. 14]
# library(plm)
# library(haven)
# nlswork <- read_dta("http://www.stata-press.com/data/r14/nlswork.dta") # large file
# nlswork$race <- factor(nlswork$race) # convert
# nlswork$race2 <- factor(ifelse(nlswork$race == 2, 1, 0)) # need this variable for example
# nlswork$grade <- as.numeric(nlswork$grade)
# pnlswork <- pdata.frame(nlswork, index=c("idcode", "year"), drop.index=FALSE)
# 
# form_nls_ex2 <- formula(ln_wage ~ grade + age + I(age^2) + ttl_exp + I(ttl_exp^2) + tenure + I(tenure^2) + race2 + not_smsa + south)
# 
# plm_fe_nlswork <- plm(form_nls_ex2, data = pnlswork, model = "within")

# Stata's results:
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

# resambles Stata (ex. 2, p. 14)
# => coefficients, std.errors, R^2 (=R-sq within), F => correct
# (NB: Stata outputs an "artificial" constant for FE models, see below)
#summary(plm_fe_nlswork)

# Stata outputs a constant for the FE model which is computed as the weighted average of the individual constants
# see http://www.stata.com/support/faqs/statistics/intercept-in-fixed-effects-model/
# However, Stata also outputs std.err, t-test and p-value for the artificial constant
# gretl mimics Stata: see gretl user's guide example p. 160-161 (example 18.1)
# http://gretl.sourceforge.net/gretl-help/gretl-guide.pdf
# http://lists.wfu.edu/pipermail/gretl-devel/2013-May/004459.html
#within.intercept(plm_fe_nlswork)
#const_fe_Stata_gretl <- weighted.mean(fixef(plm_fe_nlswork) , as.numeric(table(index(plm_fe_nlswork)[[1]])))

# RE estimator
# note Stata 14 uses by default a different method compared to plm's Swamy-Arora variance component estimator
# This is why in comparison with web examples from Stata the random effects coefficients slightly differ
#plm_re_nlswork <- plm(form_nls_ex2, data = pnlswork, model = "random")

#### (3) ####
## Test of unbalanced random effects estimator on Hedonic data of Harrison/Rubinfeld (1978)

## NB: Baltagi's text book, table 9.1 uses the Stata results, the original paper Baltagi/Chang (1994) what EViews and plm yields
##     However, the standard error of plm do not match exactly EViews and the paper. We don't know what exactly
##     EViews or Baltagi/Chang (1994) did (the paper mentions "approximate" standard errors).
##     A detailed explanation what EViews does is here: http://forums.eviews.com/viewtopic.php?f=4&t=18629#p59506

# scaling of variables in dataset Hedonic is a little bit different to Baltagi/Chang (1994) and Baltagi's text book, table 9.1
# see below for scaling as in Baltagi/Chang (1994)
data("Hedonic", package = "plm")
pHedonic <- pdata.frame(Hedonic, index = "townid")
form <- formula(mv ~ crim + zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + blacks + lstat)
summary(plm(form, data = pHedonic, model = "random"))

## do (weired) scaling of variables as in Baltagi/Chang (1994)
Hedonic$mv2      <- Hedonic$mv
Hedonic$crim2    <- Hedonic$crim / 100
Hedonic$zn2      <- Hedonic$zn / 1000
Hedonic$indus2   <- Hedonic$indus / 100
Hedonic$chas2    <- (as.numeric(Hedonic$chas)-1) / 10
Hedonic$nox2     <- Hedonic$nox / 100
Hedonic$rm2      <- Hedonic$rm / 100
Hedonic$age2     <- Hedonic$age / 1000
Hedonic$dis2     <- Hedonic$dis / 10
Hedonic$rad2     <- Hedonic$rad / 10
Hedonic$tax2     <- Hedonic$tax / 1000
Hedonic$ptratio2 <- Hedonic$ptratio / 100
Hedonic$lstat2   <- Hedonic$lstat / 10

pHedonic2 <- pdata.frame(Hedonic, index = "townid")
form2 <- formula(mv2 ~ crim2 + zn2 + indus2 + chas2 + nox2 + rm2 + age2 + dis2 + rad2 + tax2 + ptratio2 + blacks + lstat2)
summary(plm(form2, data = pHedonic2, model = "random"))


# pcce(., model = "mg") amd pmg(., model = "cmg") estimate  the same model but
# in a different way - coefficients need to match
data("Produc", package = "plm")
form <- log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp
pccemgmod   <- pcce(form, data = Produc, model = "mg")
pmgccemgmod <- pmg (form, data = Produc, model = "cmg")
common <- intersect(names(pccemgmod[["coefficients"]]), names(pmgccemgmod[["coefficients"]]))
coef_pccemgmod   <- round(pccemgmod[["coefficients"]][common],   digits = 7)
coef_pmgccemgmod <- round(pmgccemgmod[["coefficients"]][common], digits = 7)
stopifnot(all.equal(coef_pccemgmod, coef_pmgccemgmod, tolerance = 1E-04))
## IGNORE_RDIFF_BEGIN
print(summary(pccemgmod))
print(summary(pmgccemgmod))


# run and output tests for pcce/pmg with model = 'p'/'mg'/'dmg'
print(summary(pcce(form, data = Produc, model = "p")))
print(summary(pmg (form, data = Produc, model = "mg")))
print(summary(pmg (form, data = Produc, model = "dmg")))
print(summary(pmg (form, data = Produc, model = "cmg", trend = TRUE)))
print(summary(pmg (form, data = Produc, model = "mg",  trend = TRUE)))
print(summary(pmg (form, data = Produc, model = "dmg", trend = TRUE)))
##IGNORE_RDIFF_END

## further run tests without intercept
plm(inv ~ 0 + value + capital + year, data = Grunfeld, model = "between")
plm(inv ~ 0 + value + capital + year, data = Grunfeld, model = "random")
plm(inv ~ 0 + value + capital + year, data = Grunfeld, model = "within")
