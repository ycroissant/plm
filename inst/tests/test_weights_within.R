## Weighted fixed effects (within) models must match lm() with dummies and the
## same weights: the within transformation takes deviations from weighted
## means (GH issue #5). Weights vary within individuals here; with weights
## constant within individuals, weighted and unweighted means coincide.
library(plm)
data("Grunfeld", package = "plm")
set.seed(1)
Grunfeld$w <- runif(nrow(Grunfeld), 0.5, 2)
set.seed(42)
Grunfeld_unbal <- Grunfeld[-sample(nrow(Grunfeld), 23), ]

for(d in list(Grunfeld, Grunfeld_unbal)) {
  pdf <- pdata.frame(d, index = c("firm", "year"))
  ## one-way individual
  plm_id <- plm(inv ~ value + capital, data = d, model = "within", weights = w)
  lm_id  <- lm(inv ~ value + capital + factor(firm), data = d, weights = w)
  stopifnot(isTRUE(all.equal(coef(plm_id), coef(lm_id)[2:3])))
  stopifnot(isTRUE(all.equal(vcov(plm_id), vcov(lm_id)[2:3, 2:3])))
  cf <- coef(lm_id)
  stopifnot(isTRUE(all.equal(as.numeric(fixef(plm_id)), as.numeric(cf[1] + c(0, cf[-(1:3)])))))
  stopifnot(isTRUE(all.equal(as.numeric(attr(fixef(plm_id, type = "dfirst"), "se")),
                             as.numeric(coef(summary(lm_id))[-(1:3), 2]))))
  stopifnot(isTRUE(all.equal(as.numeric(predict(plm_id, newdata = pdf)), as.numeric(fitted(lm_id)))))
  ## one-way time
  plm_ti <- plm(inv ~ value + capital, data = d, model = "within", effect = "time", weights = w)
  lm_ti  <- lm(inv ~ value + capital + factor(year), data = d, weights = w)
  stopifnot(isTRUE(all.equal(coef(plm_ti), coef(lm_ti)[2:3])))
  cf <- coef(lm_ti)
  stopifnot(isTRUE(all.equal(as.numeric(fixef(plm_ti)), as.numeric(cf[1] + c(0, cf[-(1:3)])))))
  ## two-ways
  plm_tw <- plm(inv ~ value + capital, data = d, model = "within", effect = "twoways", weights = w)
  lm_tw  <- lm(inv ~ value + capital + factor(firm) + factor(year), data = d, weights = w)
  stopifnot(isTRUE(all.equal(coef(plm_tw), coef(lm_tw)[2:3])))
  stopifnot(isTRUE(all.equal(vcov(plm_tw), vcov(lm_tw)[2:3, 2:3])))
  Xb <- as.vector(as.matrix(d[ , c("value", "capital")]) %*% coef(lm_tw)[2:3])
  stopifnot(isTRUE(all.equal(as.numeric(fixef(plm_tw, effect = "twoways")), as.numeric(fitted(lm_tw)) - Xb)))
  stopifnot(isTRUE(all.equal(as.numeric(predict(plm_tw, newdata = pdf)), as.numeric(fitted(lm_tw)))))
}

## unit weights and rescaled weights do not change the estimates
fe    <- plm(inv ~ value + capital, data = Grunfeld, model = "within")
fe_w1 <- plm(inv ~ value + capital, data = Grunfeld, model = "within", weights = rep(1, 200))
stopifnot(isTRUE(all.equal(coef(fe), coef(fe_w1))))
fe_w  <- plm(inv ~ value + capital, data = Grunfeld, model = "within", weights = w)
fe_w3 <- plm(inv ~ value + capital, data = Grunfeld, model = "within", weights = 3 * w)
stopifnot(isTRUE(all.equal(coef(fe_w), coef(fe_w3))))

## zero weights: same coefficients and effects as dropping the rows
Grunfeld0 <- Grunfeld
Grunfeld0$w[c(3, 77)] <- 0
fe_0 <- plm(inv ~ value + capital, data = Grunfeld0, model = "within", weights = w)
fe_d <- plm(inv ~ value + capital, data = Grunfeld[-c(3, 77), ], model = "within", weights = w)
stopifnot(isTRUE(all.equal(coef(fe_0), coef(fe_d))))
stopifnot(isTRUE(all.equal(fixef(fe_0), fixef(fe_d), check.attributes = FALSE)))
