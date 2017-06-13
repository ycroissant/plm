library(plm)
data("Produc", package = "plm")
pProduc <- pdata.frame(Produc, index = c("state", "year", "region"))
form  <- log(gsp) ~ log(pc) + log(emp) + log(hwy) + log(water) + log(util) + unemp
summary(plm(form, data = pProduc, model = "random", effect = "nested"))
summary(plm(form, data = pProduc, model = "random", effect = "nested", random.method = "walhus"))
summary(plm(form, data = pProduc, model = "random", effect = "nested", random.method = "amemiya"))
