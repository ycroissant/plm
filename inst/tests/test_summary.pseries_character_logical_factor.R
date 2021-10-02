### some of summary.pseries error'ed or gave warnings pre rev. 445

library("plm")
data("Grunfeld", package = "plm")

Grunfeld$char <- rep("ab", 200)
Grunfeld$logi <- rep(c(T, F), 100)
Grunfeld$fac <- factor(rep(c("a", "b"), 100))

pGrund <- pdata.frame(Grunfeld, stringsAsFactors = FALSE)

# summary on original data
summary(Grunfeld$inv)   # numeric
summary(Grunfeld$char)  # character
summary(Grunfeld$logi)  # logical
summary(Grunfeld$fac)   # factor

# summary.pseries
summary(pGrund$inv)
summary(pGrund$char)
summary(pGrund$logi)
summary(pGrund$fac)


summary(Grunfeld)
summary(pGrund)
