# testfile to check if the deprecated function plm.data() is correctly
# reworked by using pdata.frame()
# 
# Usefull especially if future changes to pdata.frame() affect the "plm.dim"
# object onces created by plm.data()


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
