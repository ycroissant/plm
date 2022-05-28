library(plm)

# pcce(., model = "mg") and pmg(., model = "cmg") estimate the same model but
# in a different way - coefficients need to match
data("Produc", package = "plm")
pProduc <- pdata.frame(Produc)
form <- log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp
pccemgmod   <- pcce(form, data = pProduc, model = "mg")
pmgccemgmod <- pmg (form, data = pProduc, model = "cmg")
common <- intersect(names(pccemgmod[["coefficients"]]), names(pmgccemgmod[["coefficients"]]))
coef_pccemgmod   <- round(pccemgmod[["coefficients"]][common],   digits = 7)
coef_pmgccemgmod <- round(pmgccemgmod[["coefficients"]][common], digits = 7)
stopifnot(all.equal(coef_pccemgmod, coef_pmgccemgmod, tolerance = 1E-04))
stopifnot(all.equal(pccemgmod$residuals, pmgccemgmod$residuals, check.attributes = FALSE, tolerance = 1E-05))

((summary(pccemgmod)))
((summary(pmgccemgmod)))

#### pcce ####

# further tests, incl. residuals
((summary(pccepmod <- pcce(form, data = pProduc, model = "p"))))

residuals(pccemgmod, type = "defactored") # default
residuals(pccemgmod, type = "standard")

residuals(pccepmod, type = "defactored") # default
residuals(pccepmod, type = "standard")


# run tests with unbalanced data
pProduc_unbal <- pProduc[-c(3, 50:52, 502, 800), ]

pccemgmod_unbal <- pcce(form, data = pProduc_unbal, model = "mg")
pccepmod_unbal  <- pcce(form, data = pProduc_unbal, model = "p")

((summary(pccemgmod_unbal)))
((summary(pccepmod_unbal)))

residuals(pccemgmod_unbal, type = "defactored") # default
residuals(pccemgmod_unbal, type = "standard")

residuals(pccepmod_unbal, type = "defactored") # default
residuals(pccepmod_unbal, type = "standard")

# transformed data
pccemgmod$tr.model$y
pccemgmod$tr.model$X

pccepmod$tr.model$y
pccepmod$tr.model$X

pccemgmod_unbal$tr.model$y
pccemgmod_unbal$tr.model$X

pccepmod_unbal$tr.model$y
pccepmod_unbal$tr.model$X

# vcovXX
vcovHC(pccemgmod)
vcovHC(pccepmod)

vcovHC(pccemgmod_unbal)
vcovHC(pccepmod_unbal)

vcovHC(pccemgmod, cluster = "time")
vcovHC(pccepmod,  cluster = "time")

vcovHC(pccemgmod_unbal, cluster = "time")
vcovHC(pccepmod_unbal,  cluster = "time")

vcovNW(pccemgmod)
vcovNW(pccepmod)

vcovNW(pccemgmod_unbal)
vcovNW(pccepmod_unbal)

vcovNW(pccemgmod, cluster = "time")
vcovNW(pccepmod,  cluster = "time")

vcovNW(pccemgmod_unbal, cluster = "time")
vcovNW(pccepmod_unbal,  cluster = "time")

vcovSCC(pccemgmod)
vcovSCC(pccepmod)

vcovSCC(pccemgmod_unbal)
vcovSCC(pccepmod_unbal)

vcovSCC(pccemgmod, cluster = "time")
vcovSCC(pccepmod,  cluster = "time")

vcovSCC(pccemgmod_unbal, cluster = "time")
vcovSCC(pccepmod_unbal,  cluster = "time")

########## with trend ##########
summary(pccemgmod_trend  <- pcce(form, data = pProduc, model = "mg",  trend = TRUE))
summary(pccepmod_trend   <- pcce(form, data = pProduc, model = "p",   trend = TRUE))

summary(pccemgmod_trend_unbal  <- pcce(form, data = pProduc_unbal, model = "mg",  trend = TRUE))
summary(pccepmod_trend_unbal   <- pcce(form, data = pProduc_unbal, model = "p",   trend = TRUE))


residuals(pccemgmod_trend, type = "defactored") # default
residuals(pccemgmod_trend, type = "standard")

residuals(pccepmod_trend, type = "defactored") # default
residuals(pccepmod_trend, type = "standard")

residuals(pccemgmod_trend_unbal, type = "defactored") # default
residuals(pccemgmod_trend_unbal, type = "standard")

residuals(pccepmod_trend_unbal, type = "defactored") # default
residuals(pccepmod_trend_unbal, type = "standard")


# transformed data
pccemgmod_trend$tr.model$y
pccemgmod_trend$tr.model$X

pccepmod_trend$tr.model$y
pccepmod_trend$tr.model$X

pccemgmod_trend_unbal$tr.model$y
pccemgmod_trend_unbal$tr.model$X

pccepmod_trend_unbal$tr.model$y
pccepmod_trend_unbal$tr.model$X

#### pmg ####

# run and output tests for pcce/pmg with model = 'p'/'mg'/'dmg'
print(summary(pmg_cmg <- pmg(form, data = pProduc, model = "cmg")))
print(summary(pmg_mg  <- pmg(form, data = pProduc, model = "mg")))
print(summary(pmg_dmg <- pmg(form, data = pProduc, model = "dmg")))
print(summary(pmg_cmg_trend <- pmg(form, data = pProduc, model = "cmg", trend = TRUE)))
print(summary(pmg_mg_trend  <- pmg(form, data = pProduc, model = "mg",  trend = TRUE)))
print(summary(pmg_dmg_trend <- pmg(form, data = pProduc, model = "dmg", trend = TRUE)))

print(summary(pmg_cmg_unbal <- pmg(form, data = pProduc_unbal, model = "cmg")))
print(summary(pmg_mg_unbal  <- pmg(form, data = pProduc_unbal, model = "mg")))
print(summary(pmg_dmg_unbal <- pmg(form, data = pProduc_unbal, model = "dmg")))
print(summary(pmg_cmg_unbal_trend <- pmg(form, data = pProduc_unbal, model = "cmg", trend = TRUE)))
print(summary(pmg_mg_unbal_trend  <- pmg(form, data = pProduc_unbal, model = "mg",  trend = TRUE)))
print(summary(pmg_dmg_unbal_trend <- pmg(form, data = pProduc_unbal, model = "dmg", trend = TRUE)))

