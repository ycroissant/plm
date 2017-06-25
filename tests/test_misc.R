library(plm)
data("Grunfeld", package = "plm")
Grunfeld_unbal <- Grunfeld[1:199, ]
# ercomp(plm(inv ~ value, Grunfeld, model = "random"))
# ercomp(plm(inv ~ value, Grunfeld, model = "random", random.method = "amemiya"))
# ercomp(plm(inv ~ value + capital, Grunfeld_unbal, model = "random"))


# these resulted in errors pre rev. 523 due to missing drop = FALSE
plm(inv ~ value, Grunfeld_unbal, model = "random", random.method = "amemiya")
plm(inv ~ value, Grunfeld_unbal, model = "random", random.method = "amemiya", effect = "time")
