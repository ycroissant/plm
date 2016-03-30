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


f_level             <- fixef(gt, type = "level")
f_level_robust_func <- fixef(gt, type = "level", .vcov = vcovHC)     # .vcov is function

print(attr(f_level,             "se"))
print(attr(f_level_robust_func, "se"))

print(summary(f_level),             digits = 8)
print(summary(f_level_robust_func), digits = 8)

f_level             <- fixef(gd, type = "level")
f_level_robust_func <- fixef(gd, type = "level", .vcov = vcovHC)     # .vcov is function

print(attr(f_level,             "se"))
print(attr(f_level_robust_func, "se"))

print(summary(f_level),             digits = 8)
print(summary(f_level_robust_func), digits = 8)
