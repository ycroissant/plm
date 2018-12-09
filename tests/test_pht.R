
# replicates Baltagi (2005, 2013), table 7.4
library("plm")
data("Wages", package = "plm")
ht <- pht(lwage ~ wks + south + smsa + married + exp + I(exp^2) +
            bluecol + ind + union + sex + black + ed | 
            sex + black + bluecol + south + smsa + ind,
          data = Wages, model = "ht", index = 595)
summary(ht)


ht2 <- pht(lwage ~ wks + south + smsa + married + exp + I(exp^2) +
            bluecol + ind + union + sex + black + ed | 
            sex + black + bluecol + south + smsa + ind,
          data = Wages, model = "am", index = 595)
summary(ht2)

ht3 <- pht(lwage ~ wks + south + smsa + married + exp + I(exp^2) +
            bluecol + ind + union + sex + black + ed | 
            sex + black + bluecol + south + smsa + ind,
          data = Wages, model = "bms", index = 595)
summary(ht3)

ht4 <- plm(lwage ~ wks + south + smsa + married + exp + I(exp^2) +
            bluecol + ind + union + sex + black + ed | 
            sex + black + bluecol + south + smsa + ind, model = "ht",
           data = Wages, index = 595)
summary(ht4)

# estimating with pht and plm(., model = "ht") should give the same results
stopifnot(isTRUE(all.equal(coef(ht4), coef(ht))))

# estimating with pht and plm(., model = "random", random.method = "ht", inst.method = "baltagi") should give the same results
ht5 <- plm(lwage ~ wks + south + smsa + married + exp + I(exp ^ 2) + 
             bluecol + ind + union + sex + black + ed |
             bluecol + south + smsa + ind + sex + black |
             wks + married + union + exp + I(exp ^ 2),
          model = "random", random.method = "ht", inst.method = "baltagi",
           data = Wages, index = 595)
summary(ht5)

ht6 <- plm(lwage ~ wks + south + smsa + married + exp + I(exp ^ 2) + 
             bluecol + ind + union + sex + black + ed |
             bluecol + south + smsa + ind + sex + black |
             wks + married + union + exp + I(exp ^ 2),
           model = "random", random.method = "ht", inst.method = "am",
           data = Wages, index = 595)
summary(ht6)

ht7 <- plm(lwage ~ wks + south + smsa + married + exp + I(exp ^ 2) + 
             bluecol + ind + union + sex + black + ed |
             bluecol + south + smsa + ind + sex + black |
             wks + married + union + exp + I(exp ^ 2),
           model = "random", random.method = "ht", inst.method = "bms",
           data = Wages, index = 595)
summary(ht7)

stopifnot(isTRUE(all.equal(coef(ht5), coef(ht))))
stopifnot(isTRUE(all.equal(coef(ht6), coef(ht2))))
stopifnot(isTRUE(all.equal(coef(ht7), coef(ht3))))


