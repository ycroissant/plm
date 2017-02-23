
# tests for experimental, non-exported methods:
#   * residuals_overall_exp.plm
#   * fitted_exp.plm
library(plm)
data("Grunfeld", package = "plm")

# random - balanced
re_id_bal   <- plm(inv ~ value + capital, model = "random", effect = "individual", data = Grunfeld)
re_time_bal <- plm(inv ~ value + capital, model = "random", effect = "time",       data = Grunfeld)
re2_bal     <- plm(inv ~ value + capital, model = "random", effect = "twoways",    data = Grunfeld)

# random - unbalanced
re_id_unbal   <- plm(inv ~ value + capital, model = "random", effect = "individual", data = Grunfeld[1:199, ])
re_time_unbal <- plm(inv ~ value + capital, model = "random", effect = "time",       data = Grunfeld[1:199, ])
re2_unbal     <- plm(inv ~ value + capital, model = "random", effect = "twoways",    data = Grunfeld[1:199, ])

# fixed - balanced
fe_id_bal   <- plm(inv ~ value + capital, model = "within", effect = "individual", data = Grunfeld)
fe_time_bal <- plm(inv ~ value + capital, model = "within", effect = "time",       data = Grunfeld)
fe2_bal     <- plm(inv ~ value + capital, model = "within", effect = "twoways",    data = Grunfeld)

# fixed - unbalanced
fe_id_unbal   <- plm(inv ~ value + capital, model = "within", effect = "individual", data = Grunfeld[1:199, ])
fe_time_unbal <- plm(inv ~ value + capital, model = "within", effect = "time",       data = Grunfeld[1:199, ])
fe2_unbal     <- plm(inv ~ value + capital, model = "within", effect = "twoways",    data = Grunfeld[1:199, ])

# between
be_bal   <- plm(inv ~ value + capital, model = "between", data = Grunfeld)
be_unbal <- plm(inv ~ value + capital, model = "between", data = Grunfeld[1:199, ])

# pooling
pool_bal   <- plm(inv ~ value + capital, model = "pooling", data = Grunfeld)
pool_unbal <- plm(inv ~ value + capital, model = "pooling", data = Grunfeld[1:199, ])

# fd
fd_bal   <- plm(inv ~ value + capital, model = "fd", data = Grunfeld)
fd_unbal <- plm(inv ~ value + capital, model = "fd", data = Grunfeld[1:199, ])

### Tests ###

# random - balanced
if (!isTRUE(all.equal(re_id_bal$model[,1],   fitted_exp.plm(re_id_bal)   + residuals_overall_exp.plm(re_id_bal),   check.attributes = F))) stop("model random not equal")
if (!isTRUE(all.equal(re_time_bal$model[,1], fitted_exp.plm(re_time_bal) + residuals_overall_exp.plm(re_time_bal), check.attributes = F))) stop("model random not equal")
if (!isTRUE(all.equal(re2_bal$model[,1],     fitted_exp.plm(re2_bal)     + residuals_overall_exp.plm(re2_bal),     check.attributes = F))) stop("model random not equal")

# random - unbalanced
if (!isTRUE(all.equal(re_id_unbal$model[,1],   fitted_exp.plm(re_id_unbal)   + residuals_overall_exp.plm(re_id_unbal),   check.attributes = F))) stop("model random not equal")
if (!isTRUE(all.equal(re_time_unbal$model[,1], fitted_exp.plm(re_time_unbal) + residuals_overall_exp.plm(re_time_unbal), check.attributes = F))) stop("model random not equal")
if (!isTRUE(all.equal(re2_unbal$model[,1],     fitted_exp.plm(re2_unbal)     + residuals_overall_exp.plm(re2_unbal),     check.attributes = F))) stop("model random not equal")

# fixed - balanced
if (!isTRUE(all.equal(fe_id_bal$model[,1],   fitted_exp.plm(fe_id_bal)   + residuals_overall_exp.plm(fe_id_bal),   check.attributes = F))) stop("model within not equal")
if (!isTRUE(all.equal(fe_time_bal$model[,1], fitted_exp.plm(fe_time_bal) + residuals_overall_exp.plm(fe_time_bal), check.attributes = F))) stop("model within not equal")
if (!isTRUE(all.equal(fe2_bal$model[,1],     fitted_exp.plm(fe2_bal)     + residuals_overall_exp.plm(fe2_bal),     check.attributes = F))) stop("model within not equal")

# fixed - unbalanced
if (!isTRUE(all.equal(fe_id_unbal$model[,1],   fitted_exp.plm(fe_id_unbal)   + residuals_overall_exp.plm(fe_id_unbal),   check.attributes = F))) stop("model within not equal")
if (!isTRUE(all.equal(fe_time_unbal$model[,1], fitted_exp.plm(fe_time_unbal) + residuals_overall_exp.plm(fe_time_unbal), check.attributes = F))) stop("model within not equal")
if (!isTRUE(all.equal(fe2_unbal$model[,1],     fitted_exp.plm(fe2_unbal)     + residuals_overall_exp.plm(fe2_unbal),     check.attributes = F))) stop("model within not equal")

# between
if (!isTRUE(all.equal(pmodel.response(be_bal),   as.numeric(fitted_exp.plm(be_bal)   + residuals_overall_exp.plm(be_bal)),   check.attributes = F))) stop("model be not equal")
if (!isTRUE(all.equal(pmodel.response(be_unbal), as.numeric(fitted_exp.plm(be_unbal) + residuals_overall_exp.plm(be_unbal)), check.attributes = F))) stop("model be not equal")

# pooling
if (!isTRUE(all.equal(pool_bal$model[,1],   fitted_exp.plm(pool_bal)   + residuals_overall_exp.plm(pool_bal),   check.attributes = F))) stop("model pool not equal")
if (!isTRUE(all.equal(pool_unbal$model[,1], fitted_exp.plm(pool_unbal) + residuals_overall_exp.plm(pool_unbal), check.attributes = F))) stop("model pool not equal")

# fd
if (!isTRUE(all.equal(pmodel.response(fd_bal),   as.numeric(fitted_exp.plm(fd_bal)   + residuals_overall_exp.plm(fd_bal)),   check.attributes = F))) stop("model fd not equal")
if (!isTRUE(all.equal(pmodel.response(fd_unbal), as.numeric(fitted_exp.plm(fd_unbal) + residuals_overall_exp.plm(fd_unbal)), check.attributes = F))) stop("model fd not equal")

