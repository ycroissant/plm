# tests if model is re-producable from plm_object$model (model.frame in plm_object)
# => reproduction works

library(plm)
data("Grunfeld", package="plm") # requires package plm


# generate dataset with NA in dependent and independent variable
Grunfeld_NA_dep_var <- Grunfeld
Grunfeld_NA_dep_var[1, ]$inv <- NA
pGrunfeld_NA_dep_var <- pdata.frame(Grunfeld_NA_dep_var)

Grunfeld_NA_indep_var <- Grunfeld
Grunfeld_NA_indep_var[1, ]$value <- NA
pGrunfeld_NA_indep_var <- pdata.frame(Grunfeld_NA_indep_var)

#### input more NAs in dep var
pGrunfeld_NA_dep_var_more <- pGrunfeld_NA_dep_var
pGrunfeld_NA_dep_var_more[c(1:10, 21:30), ]$inv <- NA



# generate dataset with NA row
Grunfeld_NA_row <- Grunfeld
Grunfeld_NA_row[1, c("inv", "value", "capital")] <- NA
pGrunfeld_NA_row <- pdata.frame(Grunfeld_NA_row)


form <- formula(inv ~ value + capital)

# 200 rows
nrow(Grunfeld)
nrow(Grunfeld_NA_dep_var)
nrow(Grunfeld_NA_row)


plm_fe                 <- plm(form, data=Grunfeld, model="within")
plm_fe_NA_dep_var      <- plm(form, data=pGrunfeld_NA_dep_var, model="within")
plm_fe_NA_dep_var_more <- plm(form, data=pGrunfeld_NA_dep_var_more, model="within")
plm_fe_NA_dep_var_tw   <- plm(form, data=pGrunfeld_NA_dep_var, model="within", effect = "twoways")

plm_re                 <- plm(form, data=Grunfeld, model="random")
plm_re_NA_dep_var      <- plm(form, data=pGrunfeld_NA_dep_var, model="random")
plm_re_NA_dep_var_more <- plm(form, data=pGrunfeld_NA_dep_var_more, model="random")
# plm_re_NA_dep_var_tw   <- plm(form, data=pGrunfeld_NA_dep_var, model="random", effect = "twoways") # not implemented
# plm_re_NA_tw   <- plm(form, data=Grunfeld, model="random", effect = "twoways") # est. variance of time effect < 0


if (nrow(plm_fe$model) != 200) stop("should be 200 rows") # 200 (correct)
if (nrow(plm_fe_NA_dep_var$model) != 199) stop("should be 199 rows") # 199 (correct)
if (nrow(plm_fe_NA_dep_var_more$model) != 180) stop("should be 180 rows") # 180 (correct)
if (nrow(plm_fe_NA_dep_var_tw$model) != 199) stop("should be 199 rows") # 199 (correct)

if (nrow(plm_re$model) != 200) stop("should be 200 rows") # 200 (correct)
if (nrow(plm_re_NA_dep_var$model) != 199) stop("should be 199 rows") # 199 (correct)
if (nrow(plm_re_NA_dep_var_more$model) != 180) stop("should be 180 rows") # 180 (correct)
#nrow(plm_fe_NA_dep_var_tw$model) # not implemented



###### re-produce FE model
plm_fe_NA_dep_var2 <- plm(form, data=plm_fe_NA_dep_var$model, model="within")

# coefficients are the same
if(!all(plm_fe_NA_dep_var$coefficients == plm_fe_NA_dep_var2$coefficients)) stop("coefficients diverge")

# model.frames in plm_objects are the same
if(!all(plm_fe_NA_dep_var$model == plm_fe_NA_dep_var2$model)) stop("model.frames diverge")
if(!all.equal(plm_fe_NA_dep_var$model, plm_fe_NA_dep_var2$model, check.attributes = FALSE)) stop("model.frames diverge")
#compare::compare(as.data.frame(plm_fe_NA_dep_var$model), as.data.frame(plm_fe_NA_dep_var2$model), ignoreAttrs = TRUE) # TRUE




###### re-produce FE model with more NAs
plm_fe_NA_dep_var_more2 <- plm(form, data=plm_fe_NA_dep_var_more$model, model="within")

# coefficients are the same
if (!all(plm_fe_NA_dep_var_more$coefficients == plm_fe_NA_dep_var_more2$coefficients)) stop("coefficients diverge")

# model.frame in plm_object is same
if (!all(plm_fe_NA_dep_var_more$model == plm_fe_NA_dep_var_more2$model)) stop("model.frames diverge")
if (!all.equal(plm_fe_NA_dep_var_more$model, plm_fe_NA_dep_var_more2$model, check.attributes = FALSE)) stop("model.frames diverge")
#compare::compare(as.data.frame(plm_fe_NA_dep_var_more$model), as.data.frame(plm_fe_NA_dep_var_more2$model), ignoreAttrs = TRUE) # TRUE



###### re-produce for twoway FE model
plm_fe_NA_dep_var_tw2 <- plm(form, data=plm_fe_NA_dep_var_tw$model, model="within", effect = "twoways")

# coefficients are the same
if (!all(plm_fe_NA_dep_var_tw$coefficients == plm_fe_NA_dep_var_tw2$coefficients)) stop("coefficients diverge")

# model.frame in plm_object is same
if (!all(plm_fe_NA_dep_var_tw$model == plm_fe_NA_dep_var_tw$model)) stop("model.frames diverge")
if (!all.equal(plm_fe_NA_dep_var_tw$model, plm_fe_NA_dep_var_tw2$model, check.attributes = FALSE)) stop("model.frames diverge")
#compare::compare(as.data.frame(plm_fe_NA_dep_var_tw$model), as.data.frame(plm_fe_NA_dep_var_tw2$model), ignoreAttrs = TRUE) # TRUE






###### re-produce RE model
plm_re_NA_dep_var2 <- plm(form, data=plm_re_NA_dep_var$model, model="random")

# coefficients are the same
if (!all(plm_re_NA_dep_var$coefficients == plm_re_NA_dep_var2$coefficients)) stop("coefficients diverge")

# model.frames in plm_objects are the same
if (!all(plm_re_NA_dep_var$model == plm_re_NA_dep_var2$model)) stop("model.frames diverge")
if (!all.equal(plm_re_NA_dep_var$model, plm_re_NA_dep_var2$model, check.attributes = FALSE)) stop("model.frames diverge")
#compare::compare(as.data.frame(plm_re_NA_dep_var$model), as.data.frame(plm_re_NA_dep_var2$model), ignoreAttrs = TRUE) # TRUE




###### re-produce RE model with more NAs
plm_re_NA_dep_var_more2 <- plm(form, data=plm_re_NA_dep_var_more$model, model="random")

# coefficients are the same
if (!all(plm_re_NA_dep_var_more$coefficients == plm_re_NA_dep_var_more2$coefficients)) stop("coefficients diverge")

# model.frame in plm_object is same
if (!all(plm_re_NA_dep_var_more$model == plm_re_NA_dep_var_more2$model)) stop("model.frames diverge")
if (!all.equal(plm_re_NA_dep_var_more$model, plm_re_NA_dep_var_more2$model, check.attributes = FALSE)) stop("model.frames diverge")
#compare::compare(as.data.frame(plm_re_NA_dep_var_more$model), as.data.frame(plm_re_NA_dep_var_more2$model), ignoreAttrs = TRUE) # TRUE



###### re-produce for twoway RE model - not implemented
# plm_re_NA_dep_var_tw2 <- plm(form, data=plm_re_NA_dep_var_tw$model, model="within", effect = "twoways")
# 
# # coefficients are the same
# if(!all(plm_re_NA_dep_var_tw$coefficients == plm_re_NA_dep_var_tw2$coefficients)) stop("coefficients diverge")
# 
# # model.frame in plm_object is same
# if(!all(plm_re_NA_dep_var_tw$model == plm_re_NA_dep_var_tw$model)) stop("model.frames diverge")
# if(!all.equal(plm_re_NA_dep_var_tw$model, plm_re_NA_dep_var_tw2$model, check.attributes = FALSE)) stop("model.frames diverge")
#compare::compare(as.data.frame(plm_re_NA_dep_var_tw$model), as.data.frame(plm_re_NA_dep_var_tw2$model), ignoreAttrs = TRUE) # TRUE

