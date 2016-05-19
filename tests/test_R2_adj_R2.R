## Comparision of lm()'s adjusted R-squared and plm()'s R-squared
## for pooling-Models


require(plm)
data("Grunfeld")

##### with intercept
### lm
mod_lm <- lm(inv ~ value + capital, Grunfeld)
r2_lm     <- summary(mod_lm)$r.squared
r2_adj_lm <- summary(mod_lm)$adj.r.squared

### plm
mod_plm <- plm(inv ~ value + capital , data=Grunfeld, model = "pooling")
r2_plm     <- summary(mod_plm)$r.squared[1] # R^2
r2_adj_plm <- summary(mod_plm)$r.squared[2] # adj. R^2

if (!isTRUE(all.equal(r2_lm,     r2_plm,     check.attributes = F))) stop("r squares differ (with intercept)")
if (!isTRUE(all.equal(r2_adj_lm, r2_adj_plm, check.attributes = F))) stop("adjusted r squareds differ (with intercept)")

##### without intercept
## lm - see summary.lm's source how the checking for the presence of an intercept is done
mod_wo_int_lm <- lm(inv ~ value + capital -1 , Grunfeld)
r2_wo_int_lm     <- summary(mod_wo_int_lm)$r.squared
r2_adj_wo_int_lm <- summary(mod_wo_int_lm)$adj.r.squared

## plm
mod_wo_int_plm <- plm(inv ~ value + capital -1, data=Grunfeld, model = "pooling")
r2_wo_int_plm     <- summary(mod_wo_int_plm)$r.squared[1] # R^2
r2_adj_wo_int_plm <- summary(mod_wo_int_plm)$r.squared[2] # adj. R^2


#### fails in rev 261
#if (!isTRUE(all.equal(r2_wo_int_lm,     r2_wo_int_plm,     check.attributes = F))) stop("r squareds differ")
#if (!isTRUE(all.equal(r2_adj_wo_int_lm, r2_adj_wo_int_plm, check.attributes = F))) stop("adjusted r squareds differ")



##### test if 'model' argument works correctly - does not in rev. 261 ###

# take pooling model as input and calculate r-squared for corresponding within model
wi <- plm(inv ~ value + capital, data = Grunfeld, model = "within")
re <- plm(inv ~ value + capital, data = Grunfeld, model = "random")
# if(!isTRUE(all.equal(r.squared(wi), r.squared(a_plm, model = "within")))) stop("r squareds differ")
# if(!isTRUE(all.equal(r.squared(re), r.squared(a_plm, model = "random")))) stop("r squareds differ")
# if(!isTRUE(all.equal(r.squared(a_plm), r.squared(re, model = "pooling")))) stop("r squareds differ")
# if(!isTRUE(all.equal(r.squared(a_plm), r.squared(wi, model = "pooling")))) stop("r squareds differ")



