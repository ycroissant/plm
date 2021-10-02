## run tests for cipstest()
library(plm)
data("Produc", package = "plm")
Produc <- pdata.frame(Produc, index=c("state", "year"))

##IGNORE_RDIFF_BEGIN

# truncated = FALSE (default)
cipstest(Produc$gsp, type = "trend", model = "cmg")
cipstest(Produc$gsp, type = "drift", model = "cmg")
cipstest(Produc$gsp, type = "none",  model = "cmg")

cipstest(Produc$gsp, type = "trend", model = "mg")
cipstest(Produc$gsp, type = "drift", model = "mg")
cipstest(Produc$gsp, type = "none",  model = "mg")

cipstest(Produc$gsp, type = "trend", model = "dmg")
cipstest(Produc$gsp, type = "drift", model = "dmg")
cipstest(Produc$gsp, type = "none",  model = "dmg")

# truncated = TRUE
cipstest(Produc$gsp, type = "trend", model = "cmg", truncated = TRUE)
cipstest(Produc$gsp, type = "drift", model = "cmg", truncated = TRUE)
cipstest(Produc$gsp, type = "none",  model = "cmg", truncated = TRUE)

cipstest(Produc$gsp, type = "trend", model = "mg", truncated = TRUE)
cipstest(Produc$gsp, type = "drift", model = "mg", truncated = TRUE)
cipstest(Produc$gsp, type = "none",  model = "mg", truncated = TRUE)

cipstest(Produc$gsp, type = "trend", model = "dmg", truncated = TRUE)
cipstest(Produc$gsp, type = "drift", model = "dmg", truncated = TRUE)
cipstest(Produc$gsp, type = "none",  model = "dmg", truncated = TRUE)

##IGNORE_RDIFF_END
