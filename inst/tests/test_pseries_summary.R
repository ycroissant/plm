### test of summary.pseries, print.summary.pseries

library(plm)
data("Grunfeld", package = "plm")
Grunfeld$logi <- c(rep(TRUE, 50), rep(FALSE, 50), rep(TRUE, 50), rep(FALSE, 50))
Grunfeld$char <- c(rep("A", 50), rep("B", 50), rep("C", 50), rep("D", 50))
Grunfeld$fac  <- factor(c(rep("fac_A", 50), rep("fac_B", 50), rep("fac_C", 50), rep("fac_D", 50)))

####### base R
class(Grunfeld$inv)
class(Grunfeld$logi)
class(Grunfeld$char)
class(Grunfeld$fac)

sum <- summary(Grunfeld)

sum_num  <- summary(Grunfeld$inv)
sum_logi <- summary(Grunfeld$logi)
sum_char <- summary(Grunfeld$char)
sum_fac  <- summary(Grunfeld$fac)

str(sum)

str(sum_num)
str(sum_logi)
str(sum_char)
str(sum_fac)

print(sum)

print(sum_num)
print(sum_logi)
print(sum_char)
print(sum_fac)

####### pdata.frame, pseries
pGrunfeld <- pdata.frame(Grunfeld)

class(pGrunfeld$inv)
class(pGrunfeld$logi)
class(pGrunfeld$char)
class(pGrunfeld$fac)

psum <- summary(pGrunfeld)

psum_num  <- summary(pGrunfeld$inv)
psum_logi <- summary(pGrunfeld$logi)
psum_char <- summary(pGrunfeld$char)
psum_fac  <- summary(pGrunfeld$fac)

str(psum)

str(psum_num)
str(psum_logi)
str(psum_char)
str(psum_fac)

print(psum)

print(psum_num)
print(psum_logi)
print(psum_char)
print(psum_fac)
