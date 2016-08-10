# Currently (in at least rev. 195), plm() and summary() can deal with linear dependent
# columns (by silently dropping them), but vcovG framework had a hiccup pre rev. 302
# see the example below

library(plm)
data(Cigar)
Cigar.p <- pdata.frame(Cigar)
Cigar.p[ , "fact1"] <- c(0,1)
Cigar.p[ , "fact2"] <- c(1,0)

# linear dependent columns are silently dropped in these functions, thus they work
mod_fe_lin_dep <- plm(price ~ cpi + fact1 + fact2, data = Cigar.p, model = "within") # contains lin dep columns
mod_fe_no_lin_dep <- plm(price ~ cpi + fact1, data = Cigar.p, model = "within")      # does not contain lin dep columns
summary(mod_fe_lin_dep)    # works with linear dep columns
summary(mod_fe_no_lin_dep) 

# detect linear dependence
detect_lin_dep(model.matrix(mod_fe_lin_dep))
detect_lin_dep(model.matrix(mod_fe_no_lin_dep))
mod_fe_lin_dep$aliased
mod_fe_no_lin_dep$aliased

# failed in vcovG up to rev. 301;
# fixed in rev. 302 by taking care of aliased coefficients
# the linear dependent column is not dropped leading to failing function due
# to the non-invertible matrix
vcovHC(mod_fe_lin_dep)
vcovHC(mod_fe_no_lin_dep)
if (!identical(vcovHC(mod_fe_lin_dep), vcovHC(mod_fe_no_lin_dep))) {
  stop("vcvov w/ linear dependent columns and the corresponding one w/o are not identical")
}

## test for vcovBK because that is separate in the vcovG framework
vcovBK(mod_fe_lin_dep)
vcovBK(mod_fe_no_lin_dep)
if (!identical(vcovBK(mod_fe_lin_dep), vcovBK(mod_fe_no_lin_dep))) {
  stop("vcvov w/ linear dependent columns and the corresponding one w/o are not identical")
}



# just run test for for pgmm models (as vcovHC.pgmm methods use vcovHC.plm)
# (no linear dependence involved here)
data("EmplUK", package="plm")
ar <- pgmm(dynformula(log(emp) ~ log(wage) + log(capital) + log(output),
           list(2, 1, 2, 2)), data = EmplUK, effect = "twoways",
           model = "twosteps", gmm.inst = ~ log(emp),
           lag.gmm = list(c(2, 99)))
vcovHC(ar)
plm:::vcovHC.pgmm(ar)
