# Test of fitted.plm
# 
# 1) Does fitted.plm run with various inputs?
# 2) Does fitted.plm run / issue a warning if a coefficients gets (silently) dropped in estimated model
#
# NB: Correctness of calculation in fitted.plm is not checked currently in this file

library(plm)
data(Grunfeld)
form <- inv ~ value + capital

mod_pool <- plm(form, data = Grunfeld, model = "pooling")
mod_fe   <- plm(form, data = Grunfeld, model = "within")
mod_re   <- plm(form, data = Grunfeld, model = "random")
mod_be   <- plm(form, data = Grunfeld, model = "between")


######## 1) General tests with various inputs ######## 
plm:::fitted.plm(mod_pool)
plm:::fitted.plm(mod_fe)
plm:::fitted.plm(mod_re)
plm:::fitted.plm(mod_be)

plm:::fitted.plm(mod_pool, model = "pooling")
plm:::fitted.plm(mod_fe,   model = "pooling")
plm:::fitted.plm(mod_re,   model = "pooling")
plm:::fitted.plm(mod_be,   model = "pooling")

plm:::fitted.plm(mod_pool, model = "within")
plm:::fitted.plm(mod_fe,   model = "within")
plm:::fitted.plm(mod_re,   model = "within")
plm:::fitted.plm(mod_be,   model = "within")

# some fail in v1.4-0 and v1.5-14:
# "Error in ercomp.plm(object) : ercomp only relevant for random models"
# plm:::fitted.plm(mod_pool, model = "random") 
# plm:::fitted.plm(mod_fe,   model = "random")
plm:::fitted.plm(mod_re,   model = "random")
# plm:::fitted.plm(mod_be,   model = "random")

plm:::fitted.plm(mod_pool, model = "between")
plm:::fitted.plm(mod_fe,   model = "between")
plm:::fitted.plm(mod_re,   model = "between")
plm:::fitted.plm(mod_be,   model = "between")


######## 2) Testcase with dropped coefficient in estimated model ######
# add linear dependend columns to dataset. Those columns get dropped in estimation
Grunfeld$lin_dep_col <- Grunfeld$capital
Grunfeld$lin_dep_col2 <- 2 * Grunfeld$lin_dep_col
form2 <- update.formula(form, . ~ . + lin_dep_col)
form3 <- update.formula(form, . ~ . + lin_dep_col + lin_dep_col2)

mod_pool_droppedCoef <- plm(form2, data = Grunfeld, model="pooling")
mod_fe_droppedCoef   <- plm(form2, data = Grunfeld, model="within")
mod_re_droppedCoef   <- plm(form2, data = Grunfeld, model="random")
mod_be_droppedCoef   <- plm(form2, data = Grunfeld, model="between")

mod_pool_dropped2Coef <- plm(form3, data = Grunfeld, model="pooling")

# These fail due to dropped coefficients [and some due to an additional error with ercomp.plm]
# [and due to the current (v1.5-14) implementation
#  of fitted.plm they give an error in some instances]
#
# A warning message about the reason (dropped coef in estimated model) is
# implemented in fitted.plm, yet uncommented (v1.5-14).


# plm:::fitted.plm(mod_pool_droppedCoef)
# plm:::fitted.plm(mod_fe_droppedCoef)
# plm:::fitted.plm(mod_re_droppedCoef)
# plm:::fitted.plm(mod_be_droppedCoef)
# 
# plm:::fitted.plm(mod_pool_droppedCoef, model = "pooling")
# plm:::fitted.plm(mod_fe_droppedCoef,   model = "pooling")
# plm:::fitted.plm(mod_re_droppedCoef,   model = "pooling")
# plm:::fitted.plm(mod_be_droppedCoef,   model = "pooling")
# 
# plm:::fitted.plm(mod_pool_droppedCoef, model = "within") # no error, but all NA values returned
# plm:::fitted.plm(mod_fe_droppedCoef,   model = "within")
# plm:::fitted.plm(mod_re_droppedCoef,   model = "within") # no error, but all NA values returned
# plm:::fitted.plm(mod_be_droppedCoef,   model = "within") # no error, but all NA values returned
# 
# plm:::fitted.plm(mod_pool_droppedCoef, model = "random") # "Error in ercomp.plm(object) : ercomp only relevant for random models"
# plm:::fitted.plm(mod_fe_droppedCoef,   model = "random") # "Error in ercomp.plm(object) : ercomp only relevant for random models"
# plm:::fitted.plm(mod_re_droppedCoef,   model = "random")
# plm:::fitted.plm(mod_be_droppedCoef,   model = "random") # "Error in ercomp.plm(object) : ercomp only relevant for random models"
