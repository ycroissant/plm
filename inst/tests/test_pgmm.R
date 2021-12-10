library("plm")
data("EmplUK", package = "plm")
## Arellano and Bond (1991), table 4 col. b
z1 <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
           + log(capital) + lag(log(output), 0:1) | lag(log(emp), 2:99),
            data = EmplUK, effect = "twoways", model = "twosteps")
summary(z1, robust = TRUE) # default


z1col <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
           + log(capital) + lag(log(output), 0:1) | lag(log(emp), 2:99),
           data = EmplUK, effect = "twoways", model = "twosteps", collapse = TRUE)
summary(z1col, robust = TRUE) # default

z1ind <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
           + log(capital) + lag(log(output), 0:1) | lag(log(emp), 2:99),
            data = EmplUK, effect = "individual", model = "twosteps")
summary(z1ind, robust = TRUE) # default

z1indcol <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
            + log(capital) + lag(log(output), 0:1) | lag(log(emp), 2:99),
            data = EmplUK, effect = "individual", model = "twosteps")
summary(z1indcol, robust = TRUE) # default


## Blundell and Bond (1998) table 4 (cf DPD for OX p.12 col.4)
## not quite...
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


### further run tests with various argument values
summary(z1, robust = FALSE)
summary(z1col, robust = FALSE)
summary(z1ind, robust = FALSE)
summary(z1indcol, robust = FALSE)

summary(z2, robust = FALSE)
summary(z2b, robust = FALSE)

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
Cigar$real_c     <- Cigar$sales * Cigar$pop/Cigar$pop16
Cigar$real_p     <- Cigar$price/Cigar$cpi * 100
Cigar$real_pimin <- Cigar$pimin/Cigar$cpi * 100
Cigar$real_ndi   <- Cigar$ndi/Cigar$cpi

form_cig <- log(real_c) ~ lag(log(real_c)) + log(real_p) + log(real_pimin) + log(real_ndi) | lag(log(real_c), 2:99)

# Baltagi (2005, 3rd edition), table 8.1 
# one-step GMM
gmm_onestep <- pgmm(form_cig, data = Cigar, effect = "twoways", model = "onestep")
# matches table 8.1: 0.84, -0.377, -0.016, 0.14
summary(gmm_onestep)

# two-step GMM
#
# Table 8.1, 8.2 in Baltagi (2021): Coefs (z-stat) 0.70 (10.2) −0.396 (6.0) −0.105 (1.3) 0.13 (3.5) 
# 
# Stata xtabond2 lnc L.(lnc) lnrp lnrpn lnrdi dum3 dum8 dum10-dum29, gmm(L.(lnc), collapse)
#           iv(lnrp lnrpn lndrdi dum3 dum8 dum10-29) noleveleq robust nomata twostep
# No of obs 1288, no of groups = 48, balanced, no of instruments = 53

year.d <- contr.treatment(levels(factor(Cigar$year)))
year.d <- cbind("63" = c(1, rep(0, nrow(year.d)-1)), year.d)
colnames(year.d) <- paste0("year_", colnames(year.d))
year.d <- cbind("year" = rownames(year.d), as.data.frame(year.d))

Cigar <- merge(Cigar, year.d)
pCigar <- pdata.frame(Cigar, index = c("state", "year"))

# not quite (need to add IV instruments!?):
gmm_twostep <- pgmm(log(real_c) ~ lag(log(real_c)) + log(real_p) + log(real_pimin) + log(real_ndi) 
                # + year_63 + year_64 
                + year_65 + 
                # year_66 + year_67 + year_68 + year_69 + 
                  year_70 + 
                # year_71 + 
                year_72 + year_73 + year_74 + year_75 + year_76 + year_77 + 
                year_78 + year_79 + year_80 + year_81 + year_82 + year_83 + 
                year_84 + year_85 + year_86 + year_87 + year_88 + year_89 + 
                year_90 + year_91 
                # + year_92
                | lag(log(real_c), 2:99)
                , data = pCigar, effect = "individual", model = "twosteps", transformation = "d", collapse = TRUE)
summary(gmm_twostep)
