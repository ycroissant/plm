## Comparision of lm()'s adjusted R-squared and plm()'s R-squared
## for pooling-Models


require(plm)
data("Grunfeld")

### lm
a_lm <- lm(inv ~ value + capital, Grunfeld)
r2_lm     <- summary(a_lm)$r.squared
r2_adj_lm <- summary(a_lm)$adj.r.squared

### plm
a_plm <- plm(inv ~ value + capital , data=Grunfeld, model = "pooling")
r2_plm     <- summary(a_plm)$r.squared[1] # R^2
r2_adj_plm <- summary(a_plm)$r.squared[2] # adj. R^2

if (!isTRUE(all.equal(r2_lm, r2_plm, check.attributes = F))) warning("r squared's differ")
if (!isTRUE(all.equal(r2_adj_lm, r2_adj_plm, check.attributes = F))) warning("r squared's differ")




##### without intercept
## lm - see summary.lm's source how the checking for the presence of an intercept is done
a2_lm <- lm(inv ~ value + capital -1 , Grunfeld)
r2_lm2     <- summary(a2_lm)$r.squared
r2_adj_lm2 <- summary(a2_lm)$adj.r.squared

## plm
a2_plm <- plm(inv ~ value + capital -1, data=Grunfeld, model = "pooling")
r2_plm2     <- summary(a2_plm)$r.squared[1] # R^2
r2_adj_plm2 <- summary(a2_plm)$r.squared[2] # adj. R^2

#### fails in rev 189
#if (!isTRUE(all.equal(r2_lm2, r2_plm2, check.attributes = F))) warning("adj. r squared's differ")
#if (!isTRUE(all.equal(r2_adj_lm2, r2_adj_plm2, check.attributes = F))) warning("adj. r squared's differ")



##### test if 'model' argument works correctly - does not in rev. 189 ###

# take pooling model as input and calculate r-squared for corresponding within model
wi <- plm(inv ~ value + capital, data = Grunfeld, model = "within")
re <- plm(inv ~ value + capital, data = Grunfeld, model = "random")
# if(!isTRUE(all.equal(r.squared(wi), r.squared(a_plm, model = "within")))) stop("r squared's differ")
# if(!isTRUE(all.equal(r.squared(re), r.squared(a_plm, model = "random")))) stop("r squared's differ")
# if(!isTRUE(all.equal(r.squared(a_plm), r.squared(re, model = "pooling")))) stop("r squared's differ")
# if(!isTRUE(all.equal(r.squared(a_plm), r.squared(wi, model = "pooling")))) stop("r squared's differ")



