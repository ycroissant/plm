library("plm")
data("Grunfeld", package = "plm")
Grunfeld$fac <- factor(1:200)
pGrun <- pdata.frame(Grunfeld)


pGrunpser  <- plm:::pseriesfy.pdata.frame(pGrun)
pGrunpser2 <- plm:::pseriesfy.pdata.frame.collapse(pGrun)
class(pGrunpser)
class(pGrunpser2)
identical(pGrunpser, pGrunpser2)

lapply(pGrunpser,  class)
lapply(pGrunpser2, class)

lapply(pGrunpser,  names)
lapply(pGrunpser2, names)

form <- inv ~ value + capital
plm(form, pGrunpser,  model = "within")
plm(form, pGrunpser2, model = "within")


# pwld <- pdata.frame(wlddev, index = c("iso3c", "year"))
# microbenchmark::microbenchmark(pseriesfy.pdata.frame(pwld),
#                                pseriesfy.pdata.frame.collapse(pwld), times = 100)
