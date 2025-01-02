library("plm")
data("EmplUK", package = "plm")

# Arellano/Bond 1991, Table 4, column (a1) [non-robust SEs]
ab.a1 <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
              + lag(log(capital), 0:2) + lag(log(output), 0:2) | lag(log(emp), 2:99),
              data = EmplUK, effect = "twoways", model = "onestep")

(s.ab.a1  <- summary(ab.a1, robust = FALSE)) # as tabulated by Arellano/Bond
(s.ab.a1r <- summary(ab.a1, robust = TRUE))

### Arellano-Bond test, Sargan test and Wald test yield different results for onestep
## xtabond2 result for robust mtest for col. a1
#  xtabond2 n L.n L2.n w L.w L(0/2).(k ys) yr*, gmm(L.n) iv(w L.w L(0/2).(k ys) yr*) nolevel robust
#Arellano-Bond test for AR(1) in first differences: z = -3.60 Pr > z = 0.000
#Arellano-Bond test for AR(2) in first differences: z = -0.52 Pr > z = 0.606


# Arellano/Bond 1991, Table 4, column (a2) [non-robust SEs]
ab.a2 <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
              + lag(log(capital), 0:2) + lag(log(output), 0:2) | lag(log(emp), 2:99),
              data = EmplUK, effect = "twoways", model = "twosteps")
(s.ab.a2  <- summary(ab.a2, robust = FALSE)) # as tabulated by Arellano/Bond
(s.ab.a2r <- summary(ab.a2, robust = TRUE))

## Arellano and Bond (1991), table 4 col. b
ab.b <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
           + log(capital) + lag(log(output), 0:1) | lag(log(emp), 2:99),
            data = EmplUK, effect = "twoways", model = "twosteps")
(s.ab.b  <- summary(ab.b, robust = FALSE)) # as tabulated by Arellano/Bond
(s.ab.br <- summary(ab.b, robust = TRUE))


ab.b.collapse <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
           + log(capital) + lag(log(output), 0:1) | lag(log(emp), 2:99),
           data = EmplUK, effect = "twoways", model = "twosteps", collapse = TRUE)
summary(ab.b.collapse, robust = TRUE) # default

ab.b.ind <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
           + log(capital) + lag(log(output), 0:1) | lag(log(emp), 2:99),
            data = EmplUK, effect = "individual", model = "twosteps")
summary(ab.b.ind, robust = TRUE) # default


## Blundell and Bond (1998) table 4 (cf DPD for OX p.12 col.4)
## not quite...
## Maybe due to this: "The original implementation of system GMM
## (Blundell and Bond 1998) used H = I." (from Roodman 2009, p. 117)
z2 <- pgmm(log(emp) ~ lag(log(emp), 1)+ lag(log(wage), 0:1) +
           lag(log(capital), 0:1) | lag(log(emp), 2:99) +
           lag(log(wage), 3:99) + lag(log(capital), 2:99),
           data = EmplUK, effect = "twoways", model = "onestep", 
           transformation = "ld")
summary(z2, robust = TRUE)

z2b <- pgmm(log(emp) ~ lag(log(emp), 1)+ lag(log(wage), 0:1) +
           lag(log(capital), 0:1) | lag(log(emp), 2:99) +
           lag(log(wage), 3:99) + lag(log(capital), 2:99),
           data = EmplUK, effect = "individual", model = "onestep", 
           transformation = "ld")
summary(z2b, robust = TRUE)


z3 <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
           + log(capital) + lag(log(output), 0:1) | lag(log(emp), 2:99),
           data = EmplUK, effect = "twoways", model = "twosteps", transformation = "ld")
summary(z3)

z3col <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
              + log(capital) + lag(log(output), 0:1) | lag(log(emp), 2:99),
              data = EmplUK, effect = "twoways", model = "twosteps", collapse = TRUE, transformation = "ld")
summary(z3col)


z3ind <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
              + log(capital) + lag(log(output), 0:1) | lag(log(emp), 2:99),
              data = EmplUK, effect = "individual", model = "twosteps", transformation = "ld")
summary(z3ind)

z3indcol <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
                 + log(capital) + lag(log(output), 0:1) | lag(log(emp), 2:99),
                 data = EmplUK, effect = "individual", model = "twosteps", transformation = "ld")
summary(z3indcol)



# Baltagi (2005, 2013/2021), Table 8.1
# Interesting note: Baltagi (2005, 3rd), table 8.1 has different values compared 
# to Baltagi (2013/2021, 5th/6th) for the two-step GMM case where the difference 
# stems from using xtabond2 and collapsed instruments in the newer editions 
# (as opposed to xtabond and not mentioning of collapsed instruments in older edition).

data("Cigar", package = "plm")
Cigar$real_c     <- Cigar$sales * Cigar$pop / Cigar$pop16
Cigar$real_p     <- Cigar$price / Cigar$cpi * 100
Cigar$real_pimin <- Cigar$pimin / Cigar$cpi * 100
Cigar$real_ndi   <- Cigar$ndi   / Cigar$cpi


# Baltagi (2005, 3rd edition), table 8.1 [one-step not contained in later editions anymore]
# GMM-one-step
form_cig_GMMonestep <- log(real_c) ~ lag(log(real_c)) + log(real_p) + log(real_pimin) + log(real_ndi) | lag(log(real_c), 2:99)
gmm_onestep <- pgmm(form_cig_GMMonestep, data = Cigar, effect = "twoways", model = "onestep")
(sgmm_onestep <- summary(gmm_onestep, robust = FALSE))
round(sgmm_onestep$coefficients[ , 1], 3)
round(sgmm_onestep$coefficients[ , 3], 3) 

# matches coefficients in table 8.1: 0.84, -0.377, -0.016, 0.14
# but not t-statistics
#  t-statistics in parenthesis:      (52.0) (11.7) (0.4) (3.8)
#### TODO: maybe matrix is a different one?
# Keane/Neal (2016). p. 529 replicate by 
# xtabond2 logc l.logc logp logpn logy i.yr, gmm(l.logc) iv(logp logpn logy i.yr) nolevel
# What they call DIFF-GMM is “GMM-one-step” in Baltagi (2005)
# and give 3 digits of coefficients:
#                                   0.843  −0.377  −0.016 0.139
#                                   (52.66)  (−11.81)  (−0.39) (3.88)

# individual onestep (without time dummies)
# Keane/Neal (2016). p. 529 also give individual model (without time dummies) by
# xtabond2 logc l.logc logp logpn logy, gmm(l.logc) iv(logp logpn logy) nolevel
gmm_onestep_ind <- pgmm(form_cig_GMMonestep, data = Cigar, effect = "individual", model = "onestep")

(sgmm_onestep_ind <- summary(gmm_onestep_ind, robust = FALSE))

round(sgmm_onestep_ind$coefficients[ , 1], 3)
round(sgmm_onestep_ind$coefficients[ , 3], 3)
## -> matches coefficients
# TODO: statistic
# 0.891  −0.152  0.050  −0.071
# (47.04) (−5.49) (1.85) (−6.09)


# Replicate OLS and 2-way FE from Baltagi table 8.1
form_cig_OLS <- log(real_c) ~ lag(log(real_c)) + log(real_p) + log(real_pimin) + log(real_ndi)
summary(plm(form_cig_OLS, data= Cigar, model="pooling"))
summary(plm(form_cig_OLS, data= Cigar, model="within", effect = "twoways"))


# two-step GMM
#
## Table 8.1, 8.2 in Baltagi (2021): Coefs (z-stat) 0.70 (10.2) −0.396 (6.0) −0.105 (1.3) 0.13 (3.5) 
# 
# Stata xtabond2 lnc L.(lnc) lnrp lnrpn lnrdi dum3 dum8 dum10-dum29, gmm(L.(lnc), collapse)
#           iv(lnrp lnrpn lndrdi dum3 dum8 dum10-29) noleveleq robust nomata twostep
# No of obs 1288, no of groups = 46, balanced, 28 obs/group, no of instruments = 53

year.d <- contr.treatment(levels(factor(Cigar$year)))
year.d <- cbind("63" = c(1, rep(0, nrow(year.d)-1)), year.d) # add base level
colnames(year.d) <- paste0("year", colnames(year.d)) # make colnames
year.d <- cbind("year" = rownames(year.d), as.data.frame(year.d)) # add column with year as numeric (to enable merge in next step) and make final data frame

pCigar <- merge(Cigar, year.d, by = "year", sort = FALSE, all.x = TRUE, all.y = FALSE)
pCigar <- pCigar[ , c(2,1, 3:length(names(pCigar)))] # order columns
pCigar <- pdata.frame(pCigar, index = c("state", "year"))

# not quite (need to add IV instruments!?):
form2021 <- formula(log(real_c) ~ lag(log(real_c)) + log(real_p) + log(real_pimin) + log(real_ndi) +
  # year63 + year64 +
  year65 + 
  # year66 + year67 + year68 + year69 + 
  year70 + 
  # year71 + 
  year72 + year73 + year74 + year75 + year76 + year77 + 
  year78 + year79 + year80 + year81 + year82 + year83 + 
  year84 + year85 + year86 + year87 + year88 + year89 + 
  year90 + year91
#  + year92
 | lag(log(real_c), 2:99)
)

gmm_twostep <- pgmm(form2021, data = pCigar, effect = "individual", model = "twosteps", transformation = "d", collapse = TRUE)
summary(gmm_twostep)


## Table 8.1, 8.2 in Baltagi (2005) "GMM-two-step"
# Coefs (z-stat) 0.80 (3.7) −0.379 (8.0) −0.020 (0.4) 0.24 (0.9)
# with xtbond (not xtbond2) and a different set of time dummies:
# xtabond lnc lnrp lnrpn lnrdi dum3-dum29, lag(1) twostep
form2005 <- formula(log(real_c) ~ lag(log(real_c)) + log(real_p) + log(real_pimin) + log(real_ndi) +
# + year63 + year64 
  year65 + 
  year66 + year67 + year68 + year69 + 
  year70 + 
  year71 + 
  year72 + year73 + year74 + year75 + year76 + year77 + 
  year78 + year79 + year80 + year81 + year82 + year83 + 
  year84 + year85 + year86 + year87 + year88 + year89 + 
  year90 + year91
 # year92 
  | lag(log(real_c), 2:99))

gmm_twostep_2005 <- pgmm(form2005, data = pCigar, effect = "individual", model = "twosteps", transformation = "d")
summary(gmm_twostep_2005)
round(coefficients(gmm_twostep_2005)[1:4], 3)
## -> only nearly replicates

pdynmc.avail <- if (!requireNamespace("pdynmc", quietly = TRUE)) FALSE else TRUE

if(pdynmc.avail) {
  
  library(pdynmc)
  ## from PDF R-Journal (https://journal.r-project.org/archive/2021/RJ-2021-035/RJ-2021-035.pdf)
  dat <- EmplUK
  dat[ , c(4:7)] <- log(dat[ , c(4:7)])
  names(dat)[4:7] <- c("n", "w", "k", "ys") # n = emp, w = wage, k = capital, ys = output
  
  ### Sargan test and Wald test yield a different results for onestep
  ab.a1.pdynmc <- pdynmc(
    dat = dat, varname.i = "firm", varname.t = "year",
    use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
    include.y = TRUE, varname.y = "n", lagTerms.y = 2,
    fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
    varname.reg.fur = c("w", "k", "ys"), lagTerms.reg.fur = c(1,2,2),
    include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
    w.mat = "iid.err", std.err = "unadjusted",
    estimation = "onestep", opt.meth = "none")
  summary(ab.a1.pdynmc)
  
  ab.a1r.pdynmc <- pdynmc(
    dat = dat, varname.i = "firm", varname.t = "year",
    use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
    include.y = TRUE, varname.y = "n", lagTerms.y = 2,
    fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
    varname.reg.fur = c("w", "k", "ys"), lagTerms.reg.fur = c(1,2,2),
    include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
    w.mat = "iid.err", std.err = "corrected",
    estimation = "onestep", opt.meth = "none")
  summary(ab.a1r.pdynmc)
  
  ab.a2.pdynmc <- pdynmc(
    dat = dat, varname.i = "firm", varname.t = "year",
    use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
    include.y = TRUE, varname.y = "n", lagTerms.y = 2,
    fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
    varname.reg.fur = c("w", "k", "ys"), lagTerms.reg.fur = c(1,2,2),
    include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
    w.mat = "iid.err", std.err = "unadjusted",
    estimation = "twostep", opt.meth = "none")
  summary(ab.a2.pdynmc)
  
  ab.a2r.pdynmc <- pdynmc(
    dat = dat, varname.i = "firm", varname.t = "year",
    use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
    include.y = TRUE, varname.y = "n", lagTerms.y = 2,
    fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
    varname.reg.fur = c("w", "k", "ys"), lagTerms.reg.fur = c(1,2,2),
    include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
    w.mat = "iid.err", std.err = "corrected",
    estimation = "twostep", opt.meth = "none")
  summary(ab.a2r.pdynmc)
  
  ### check coefs and SE pgmm vs. pdynmc ###
  
  ## one-step GMM
  # check coefficients
  stopifnot(isTRUE(all.equal(round( s.ab.a1[[1L]][ , 1], 5), round( ab.a1.pdynmc$coefficients[1:10], 5), check.attributes = FALSE)))
  
  # check standard errors (non-robust and robust)
  stopifnot(isTRUE(all.equal(round( s.ab.a1[[1L]][ , 2], 5), round( ab.a1.pdynmc$stderr$step1[1:10], 5), check.attributes = FALSE)))
  stopifnot(isTRUE(all.equal(round(s.ab.a1r[[1L]][ , 2], 5), round(ab.a1r.pdynmc$stderr$step1[1:10], 5), check.attributes = FALSE)))
  
  ## two-steps GMM
  # check coefficients
  stopifnot(isTRUE(all.equal(round( s.ab.a2[[1L]][ , 1], 5), round( ab.a2.pdynmc$coefficients[1:10], 5), check.attributes = FALSE)))
  
  # check standard errors (non-robust and robust)
  stopifnot(isTRUE(all.equal(round( s.ab.a2[[1L]][ , 2], 5), round( ab.a2.pdynmc$stderr$step2[1:10], 5), check.attributes = FALSE)))
  stopifnot(isTRUE(all.equal(round(s.ab.a2r[[1L]][ , 2], 5), round(ab.a2r.pdynmc$stderr$step2[1:10], 5), check.attributes = FALSE)))
  
  ## Baltagi table 8.1
  data("Cigar", package = "plm")
  Cigar$real_c     <- log((Cigar$sales*Cigar$pop) / Cigar$pop16)
  Cigar$real_p     <- log(Cigar$price/Cigar$cpi * 100)
  Cigar$real_pimin <- log(Cigar$pimin/Cigar$cpi * 100)
  Cigar$real_ndi   <- log(Cigar$ndi/Cigar$cpi)
  
  ## TODO: does not replicate literature yet
  c1 <- pdynmc(dat = Cigar, varname.i = "state", varname.t = "year",
               use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
               include.y = TRUE, varname.y = "real_c", lagTerms.y = 1,
               fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
               varname.reg.fur = c("real_p", "real_pimin", "real_ndi"), lagTerms.reg.fur = c(0,0,0),
               include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
               w.mat = "iid.err", std.err = "unadjusted", estimation = "onestep",
               opt.meth = "none")
  summary(c1)
}
