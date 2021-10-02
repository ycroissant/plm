# Test of coercering a data.frame and a pdata.frame to matrix
# Currently (in at least rev. 195), there is a difference between the both.

library(plm)
data("Grunfeld", package = "plm")

pGrunfeld <- pdata.frame(Grunfeld)

matGrunfeld      <- as.matrix(Grunfeld)
matpGrunfeld     <- as.matrix(pGrunfeld)
mat_df_pGrunfeld <- as.matrix(as.data.frame(pGrunfeld))

## IGNORE_RDIFF_BEGIN
class(matGrunfeld)
class(matpGrunfeld)
class(mat_df_pGrunfeld)
## IGNORE_RDIFF_END

lapply(matGrunfeld,  function(x) class(x)) # all numeric
lapply(matpGrunfeld, function(x) class(x)) # all character

# Also all character in (at least) rev. 195, albeit pdata.frame was coerced to data.frame first
lapply(mat_df_pGrunfeld, function(x) class(x))
