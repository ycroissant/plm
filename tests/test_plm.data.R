# testfile to check if the deprecated function plm.data() is correctly
# reworked by using pdata.frame()
# 
# Usefull especially if future changes to pdata.frame() affect the "plm.dim"
# object onces created by plm.data()

library(plm)

# use a data set that has index variables not in first two columns, because
# plm.dim objects always have them in the fist two colums (id, time)
data("Hedonic", package = "plm")
pHed <- pdata.frame(Hedonic, index = "townid")
plm:::pos.index(pHed) # gives position of indexes

pHed_new_plm.data <- plm.data(Hedonic, indexes = "townid")
pHed_old_plm.data <- plm:::plm.data_depr_orig(Hedonic, indexes = "townid")

if (!identical(pHed_new_plm.data, pHed_old_plm.data))
  stop("plm.data stub function using pdata.frame() does not recreate ('identical()') the original plm.dim object once produced by the original plm.data() ")

if (!isTRUE(all.equal(pHed_new_plm.data, pHed_old_plm.data)))
  stop("plm.data stub function using pdata.frame() does not recreate ('all.equal()') the original plm.dim object once produced by the original plm.data() ")

# introduce constant columns
Hedonic_const <- Hedonic
Hedonic_const$constantNr <- 1
Hedonic_const$constantStr <- "constant"
Hedonic_const <- Hedonic_const[ , c("constantNr", setdiff(names(Hedonic), c("constantNr", "constantStr")), "constantStr")]
pHed_const_new_plm.data <- plm.data(Hedonic_const, indexes = "townid")
pHed_const_old_plm.data <- plm:::plm.data_depr_orig(Hedonic_const, indexes = "townid")
if (!isTRUE(all.equal(pHed_const_new_plm.data, pHed_const_old_plm.data)))
  stop("plm.data stub function using pdata.frame() does not recreate ('all.equal()') the original plm.dim object once produced by the original plm.data() ")

class(pHed_const_new_plm.data)
class(pHed_const_old_plm.data)
names(pHed_const_new_plm.data)
names(pHed_const_old_plm.data)
lapply(pHed_const_new_plm.data, class)
lapply(pHed_const_old_plm.data, class)
