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

vcovNW(pccemgmod)
vcovNW(pccepmod)

vcovSCC(pccemgmod)
vcovSCC(pccepmod)

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

