## Test of unbalanced random effects estimator on Hedonic data of Harrison/Rubinfeld (1978)

## NB: Baltagi's text book, table 9.1 uses the Stata results, the original paper Baltagi/Chang (1994) what EViews and plm yields

library(plm)
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
