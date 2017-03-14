library("plm")
data("Grunfeld", package = "plm")
Grunfeld$const <- 5
Grunfeld$allNA <- NA
Grunfeld$non_finite <- 6
Grunfeld$non_finite[1:50] <- -Inf
Grunfeld$non_finite[51] <- 6.5
Grunfeld$non_finite2 <- 7
Grunfeld$non_finite2[1:40] <- NaN
Grunfeld$non_finite2[41] <- 7.5


# # plm()'s behaviour
# plm(inv ~ value + capital + const, data = Grunfeld)         # works fine
# plm(inv ~ value + capital + allNA, data = Grunfeld)         # errors with informative msg: 0 (non-NA) cases
# plm(inv ~ value + capital + non_finite, data = Grunfeld)    # errors with informative msg: NA/NaN/Inf in 'x'
# plm(inv ~ value + capital + non_finite2, data = Grunfeld)   # works fine
# plm(inv ~ value + capital, data = Grunfeld[-c(1:40), ])     # check: same result as above line
# 
# 
# # compare to behaviour of lm()
# lm(inv ~ value + capital + const, data = Grunfeld)         # works fine
# lm(inv ~ value + capital + allNA, data = Grunfeld)         # errors with informative msg: 0 (non-NA) cases
# lm(inv ~ value + capital + non_finite, data = Grunfeld)    # errors with informative msg: NA/NaN/Inf in 'x'
# lm(inv ~ value + capital + non_finite2, data = Grunfeld)   # works fine
# lm(inv ~ value + capital, data = Grunfeld[-c(1:40), ])     # check: same result as above line


pGrun_const <- pdata.frame(Grunfeld)
pGrun_const <- pdata.frame(Grunfeld, drop.const.series = TRUE) # allNA series is also constant
pGrun_allNA <- pdata.frame(Grunfeld, drop.NA.series = TRUE)
pGrun_const_allNA <- pdata.frame(Grunfeld, drop.const.series = TRUE, drop.NA.series = TRUE)
pGrun_const_allNA_non_finiteFALSE <- pdata.frame(Grunfeld, drop.const.series = TRUE, drop.NA.series = TRUE, replace.non.finite = FALSE)
pGrun_non_finite <- pdata.frame(Grunfeld, replace.non.finite = TRUE)

