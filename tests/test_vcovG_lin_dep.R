# Currently (in at least rev. 195), plm() and summary() can deal with linear dependent
# columns (by silently dropping them), but vcovG framework gets a hiccup,
# see the example below

library(plm)
data(Cigar)
Cigar.p <- pdata.frame(Cigar)
Cigar.p[ , "fact1"] <- c(0,1)
Cigar.p[ , "fact2"] <- c(1,0)

# linear dependent columns are silently dropped in these functions, thus they work
mod_fe <- plm(price ~ cpi + fact1 + fact2, data = Cigar.p, model = "within")
summary(mod_fe) # works

# in vcovG (at least) rev. 195, the column is not dropped leading to failing function due
# to the non-invertible matrix
# vcovHC(mod_fe)

# detect linear dependence
detect_lin_dep(model.matrix(mod_fe))
