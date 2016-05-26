### Test of within_intercept in connection with fixef() and comparision to Stata and Gretl

# test in connection with fixef:
library(plm)
data("Grunfeld", package = "plm")
gi <- plm(inv ~ value + capital, data = Grunfeld, model = "within")
gt <- plm(inv ~ value + capital, data = Grunfeld, model = "within", effect = "time")
gd <- plm(inv ~ value + capital, data = Grunfeld, model = "within", effect = "twoways")


f_level             <- fixef(gi, type = "level")
f_level_robust_mat  <- fixef(gi, type = "level", .vcov = vcovHC(gi)) # .vcov is matrix
f_level_robust_func <- fixef(gi, type = "level", .vcov = vcovHC)     # .vcov is function

print(attr(f_level,             "se"))
print(attr(f_level_robust_func, "se"))

print(summary(f_level),             digits = 8)
print(summary(f_level_robust_func), digits = 8)


f_level_t             <- fixef(gt, type = "level")
f_level_t_robust_func <- fixef(gt, type = "level", .vcov = vcovHC)     # .vcov is function

print(attr(f_level_t,             "se"))
print(attr(f_level_t_robust_func, "se"))

print(summary(f_level_t),             digits = 8)
print(summary(f_level_t_robust_func), digits = 8)

f_level_d             <- fixef(gd, type = "level")
f_level_d_robust_func <- fixef(gd, type = "level", .vcov = vcovHC)     # .vcov is function

print(attr(f_level_d,             "se"))
print(attr(f_level_d_robust_func, "se"))

print(summary(f_level_d),             digits = 8)
print(summary(f_level_d_robust_func), digits = 8)

# consistency check:
# compare summary.plm to summary.fixef( , type = "dfirst")
mod_pool <- plm(inv ~ value + capital + factor(firm), data = Grunfeld, model = "pooling")
sum_mod_pool <- summary(mod_pool)
f_dfirst <- fixef(gi, type = "dfirst")
sum_f_dfirst <- summary(f_dfirst)

if (!isTRUE(all.equal(sum_mod_pool[["coefficients"]][-c(1:3) , "Estimate"], sum_f_dfirst[ , "Estimate"], check.attributes = FALSE)))
  stop("estimates diverge: summary.plm vs. summary.fixef(..., type = \"dfirst\")")

if (!isTRUE(all.equal(sum_mod_pool[["coefficients"]][-c(1:3) , "Std. Error"], sum_f_dfirst[ , "Std. Error"], check.attributes = FALSE)))
  stop("standard errors diverge: summary.plm vs. summary.fixef(..., type = \"dfirst\")")

if (!isTRUE(all.equal(sum_mod_pool[["coefficients"]][-c(1:3) , "t-value"], sum_f_dfirst[ , "t-value"], check.attributes = FALSE)))
  stop("t-values diverge: summary.plm vs. summary.fixef(..., type = \"dfirst\")")

if (!isTRUE(all.equal(sum_mod_pool[["coefficients"]][-c(1:3) , "Pr(>|t|)"], sum_f_dfirst[ , "Pr(>|t|)"], check.attributes = FALSE)))
  stop("p-values diverge: summary.plm vs. summary.fixef(..., type = \"dfirst\")")


## compare to package lfe:
# library(lfe)
# mod_felm <- felm(inv ~ value + capital | firm, data = Grunfeld)
# summary(mod_felm)
## Standard errors are bootstrapped in lfe -> different for every new call
# fe_lfe <- getfe(mod_felm, se = TRUE, bN = 50)
# print(fe_lfe)
# sum_f_level <- summary(f_level)
# print(sum_f_level)
