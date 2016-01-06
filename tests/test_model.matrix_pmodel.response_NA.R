## Tests for correct construction in case of NAs of model.matrix[.pFormula|.plm] and pmodel.response.[pFormula|.plm]

# see, if NA dropping in construction of model.matrix and pmodel.response is done correctly.

# 1) model.matrix[.pFormula|.plm] 
# 2) pmodel.response.[pFormula|.plm]


library(plm)
data(Grunfeld, package="plm")
form <- formula(inv ~ value + capital)
plm_pool       <- plm(form, data=Grunfeld, model="pooling")
plm_fe         <- plm(form, data=Grunfeld, model="within")
plm_fe_tw      <- plm(form, data=Grunfeld, model="within", effect = "twoways")
plm_re         <- plm(form, data=Grunfeld, model="random")
plm_re_time    <- plm(form, data=Grunfeld, model="random", effect = "time")
plm_re_nerlove <- plm(form, data=Grunfeld, model="random", random.method = "nerlove")



# create Grunfeld pdata.frame
pGrunfeld <- pdata.frame(Grunfeld, index = c("firm", "year"))

# generate dataset with NA in dependent variable
Grunfeld_NA_dep_var <- Grunfeld
Grunfeld_NA_dep_var[1, ]$inv <- NA
pGrunfeld_NA_dep_var <- pdata.frame(Grunfeld_NA_dep_var)

Grunfeld_NA_indep_var <- Grunfeld
Grunfeld_NA_indep_var[1, ]$value <- NA
pGrunfeld_NA_indep_var <- pdata.frame(Grunfeld_NA_indep_var)

# generate dataset with NA row
Grunfeld_NA_row <- Grunfeld
Grunfeld_NA_row[1, c("inv", "value", "capital")] <- NA
pGrunfeld_NA_row <- pdata.frame(Grunfeld_NA_row)

# pdim on pdata.frame and plm object
pdim(pGrunfeld_NA_row) # balanced - ok, because (p)data.frame
pdim(plm_fe_NA_row      <- plm(form, data=pGrunfeld_NA_row, model="within")) # unbalanced - ok
pdim(plm_fe_NA_row_time <- plm(form, data=pGrunfeld_NA_row, model="within", effect = "time")) # unbalanced - ok
pdim(plm_fe_NA_row_tw   <- plm(form, data=pGrunfeld_NA_row, model="within", effect = "twoway")) # unbalanced - ok

pdim(pGrunfeld_NA_dep_var) # balanced - ok, because (p)data.frame
pdim(plm_fe_NA_dep_var      <- plm(form, data=pGrunfeld_NA_dep_var, model="within")) # unbalanced - ok
pdim(plm_fe_NA_dep_var_time <- plm(form, data=pGrunfeld_NA_dep_var, model="within", effect = "time")) # unbalanced - ok
pdim(plm_fe_NA_dep_var_tw   <- plm(form, data=pGrunfeld_NA_dep_var, model="within", effect = "twoway")) # unbalanced - ok

pdim(pGrunfeld_NA_indep_var) # balanced, because (p)data.frame
pdim(plm_fe_NA_indep_var      <- plm(form, data=pGrunfeld_NA_indep_var, model="within")) # unbalanced - ok
pdim(plm_fe_NA_indep_var_time <- plm(form, data=pGrunfeld_NA_indep_var, model="within", effect = "time")) # unbalanced - ok
pdim(plm_fe_NA_indep_var_tw   <- plm(form, data=pGrunfeld_NA_indep_var, model="within", effect = "twoway")) # unbalanced - ok



##### inspect row numbers in model.frame, model.matrix for various data with and without NAs ####


nrow(plm:::model.matrix.plm(plm_fe_NA_row)) # 199 rows - ok
# nrow(plm:::model.matrix.pFormula(form, data=pGrunfeld_NA_row, model="within")) # error
# Error in tapply(z, effect, "mean") : arguments must have same length

nrow(plm:::model.matrix.plm(plm_fe_NA_dep_var)) # 199 - ok

nrow(plm:::model.matrix.pFormula(form, data=pGrunfeld_NA_dep_var, model="within")) # NOT OK: 200, but should be 199
nrow(model.matrix(pFormula(form), data=pGrunfeld_NA_dep_var, model="within")) # NOT OK: 200, but should be 199

nrow(plm:::model.matrix.pFormula(form, data=pGrunfeld_NA_dep_var, model="pooling")) # NOT OK: 200, but should be 199
nrow(model.matrix(pFormula(form), data=pGrunfeld_NA_dep_var, model="within")) # NOT OK: 200, but should be 199

# NOT OK: 200, but should be 199
# wrong transformation applied: balanced transformation instead of unbalanced
# due to the not sanitized data argument in model.matrix.pFormula
# -> see following comparision to model.matrix from plm interface
nrow(plm:::model.matrix.pFormula(form, data=pGrunfeld_NA_dep_var, model="within", effect = "twoways"))

  # NOT OK: Should be equal, but is not due to wrong transformation
  # drop first row b/c not in model.matrix.plm()
  all.equal(plm:::model.matrix.pFormula(form, data=pGrunfeld_NA_dep_var, model="within", effect = "twoways")[-1, ],
            plm:::model.matrix.plm(plm_fe_NA_dep_var_tw), check.attributes = FALSE)


########### 1) model.matrix[.pFormula|.plm] ###########


# pooling and within models work if data is a pdata.frame
modmat_pFormula_pdataframe_pool    <- plm:::model.matrix.pFormula(form, data=pGrunfeld, model="pooling") # works
modmat_pFormula_pdataframe_fe      <- plm:::model.matrix.pFormula(form, data=pGrunfeld, model="within")  # works
modmat_pFormula_pdataframe_fe_time <- plm:::model.matrix.pFormula(form, data=pGrunfeld, model="within", effect = "time")  # works
modmat_pFormula_pdataframe_fe_tw   <- plm:::model.matrix.pFormula(form, data=pGrunfeld, model="within", effect = "twoway")  # works


# RE fails due to theta = NULL in model.matrix.pFormula (also model.matrix.pFormula needs facilities for random.method (ercomp(, method)))
# modmat_pFormula_pdataframe_re      <- plm:::model.matrix.pFormula(form, data=pGrunfeld, model="random") # error
# modmat_pFormula_pdataframe_re_time <- plm:::model.matrix.pFormula(form, data=pGrunfeld, model="random", effect = "time") # error
# modmat_pFormula_pdataframe_fe_tw   <- plm:::model.matrix.pFormula(form, data=pGrunfeld, model="random", effect = "twoway") # error

# Error:
# Error in plm:::model.matrix.pFormula(form, data = pGrunfeld, model = "random") : 
#   dims [product 600] do not match the length of object [0]




####### Tests for removal of rows in model.matrix.pFormula if dependent var contains NAs


# 200 rows resulting form model.matrix.default - ok for this data set
nrow(model.matrix(inv ~ value + capital, data=Grunfeld))
# 200 rows,  - ok for this data set
nrow(plm:::model.matrix.pFormula(pFormula(inv ~ value + capital), data=pdata.frame(Grunfeld)))


# 199 rows resulting from model.matrix.default - ok
# NA in dependent variable detected and thus row in model.matrix dropped
nrow(stats::model.matrix(inv ~ value + capital, data=Grunfeld_NA_dep_var))

# 199 rows in model.frame of estimated plm_model$model - ok
nrow(plm(inv ~ value + capital, data=pdata.frame(Grunfeld_NA_dep_var))$model)


# NOT OK: 200 returend, 199 rows should result from model.matrix.pFormula
# NA in dependent variable _not_ detected and thus row in model.matrix _not_ dropped
# This is due to the Formula package which is does not behave as stats::model.matrix.default does
# for NA handling in dependent variable
nrow(plm:::model.matrix.pFormula(pFormula(inv ~ value + capital), data=pdata.frame(Grunfeld_NA_dep_var)))

# 200 returend - ok
# NA in independent variable is detected and thus row in model.matrix is dropped
nrow(plm:::model.matrix.pFormula(pFormula(inv ~ value + capital), data=pdata.frame(Grunfeld_NA_indep_var)))

# 199 returned - ok
# NA row is detected and thus dropped
nrow(plm:::model.matrix.pFormula(pFormula(inv ~ value + capital), data=pdata.frame(Grunfeld_NA_row)))





####### some sanity checks - see if various interfaces yield the same result ######
modmat_plm_pool <- model.matrix(plm_pool)
modmat_plm_fe   <- model.matrix(plm_fe)
modmat_plm_re   <- model.matrix(plm_re)
modmat_plm_re_time   <- model.matrix(plm_re_time)
modmat_plm_re_nerlove   <- model.matrix(plm_re_nerlove)

# delete rownames to enable comparison with all.equal()
rownames(modmat_plm_pool) <- NULL
rownames(modmat_plm_fe) <- NULL
rownames(modmat_plm_re) <- NULL
rownames(modmat_plm_re_time) <- NULL
rownames(modmat_plm_re_nerlove) <- NULL

# rownames(modmat_pFormula_pool) <- NULL
# rownames(modmat_pFormula_fe) <- NULL
rownames(modmat_pFormula_pdataframe_pool) <- NULL
rownames(modmat_pFormula_pdataframe_fe) <- NULL
# rownames(modmat_pFormula_pdataframe_re) <- NULL
# rownames(modmat_pFormula_pdataframe_re_time) <- NULL
# rownames(modmat_pFormula_pdataframe_re_nerlove) <- NULL

#### Tests

# w/o any NAs
### interfaces: plm vs. pFormula
if (!all.equal(modmat_plm_pool,       modmat_pFormula_pdataframe_pool)) stop("FAIL!")
if (!all.equal(modmat_plm_fe,         modmat_pFormula_pdataframe_fe)) stop("FAIL!")
#if (!all.equal(modmat_plm_re,         modmat_pFormula_pdataframe_re)) stop("FAIL!")
#if (!all.equal(modmat_plm_re_time,    modmat_pFormula_pdataframe_re_time)) stop("FAIL!")
#if (!all.equal(modmat_plm_re_nerlove, modmat_pFormula_pdataframe_re_nerlove)) stop("FAIL!")




# add more Tests here



########### 2) pmodel.response.[pFormula|.plm] ###########
# pmodel.response on regular data.frame (not pdata.frame)
# plm:::pmodel.response.pFormula(form, data = Grunfeld, model = "pooling") # warning still in v1.5-14/rev. 175
# plm:::pmodel.response.pFormula(form, data = Grunfeld, model = "within")  # fails
# plm:::pmodel.response.pFormula(form, data = Grunfeld, model = "random")  # fails

# pooling and within modls work on pdata.frame with fix in v1.5-14/rev. 175
resp_pFormula_pool  <- plm:::pmodel.response.pFormula(form, data = pGrunfeld, model = "pooling")
resp_pFormula_fe    <- plm:::pmodel.response.pFormula(form, data = pGrunfeld, model = "within")
resp_pFormula_fe_tw <- plm:::pmodel.response.pFormula(form, data = pGrunfeld, model = "within", effect = "twoways")
# resp_pFormula_re    <- plm:::pmodel.response.pFormula(form, data = pGrunfeld, model = "random") # error
# still fails, likely due to theta = NULL in RE model
# resp_pFormula_re <- plm:::pmodel.response.pFormula(form, data = pGrunfeld, model = "random")
#
# Error in model.matrix.pFormula(pFormula(formula), data = data, model = model,  : 
#   dims [product 200] do not match the length of object [0]



# dataset with NA in response variable yields error for FE and RE model
#  -> in the called function model.matrix.pFormula, the indexes are extracted from data (a pdata.frame)
#     and do not necessarily match the length of the extracted response (X, a model.matrix), because
#     in X, NAs are already dropped

# resp_pFormula_NA_depvar_fe   <- plm:::pmodel.response.pFormula(form, data = pGrunfeld_NA_dep_var, model = "within") # error
# resp_pFormula_NA_depvar_re   <- plm:::pmodel.response.pFormula(form, data = pGrunfeld_NA_dep_var, model = "random") # error
#
#Error in model.matrix.pFormula(pFormula(formula), data = data, model = model,  : 
#  dims [product 199] do not match the length of object [0] 

# pmodel.repsonse.plm
resp_plm_NA_depvar_pool  <- plm:::pmodel.response.plm(plm(form, data = pGrunfeld_NA_dep_var, model = "pooling"))
resp_plm_NA_depvar_fe    <- plm:::pmodel.response.plm(plm(form, data = pGrunfeld_NA_dep_var, model = "within"))
resp_plm_NA_depvar_fe_tw <- plm:::pmodel.response.plm(plm(form, data = pGrunfeld_NA_dep_var, model = "within", effect = "twoway"))
resp_plm_NA_depvar_re    <- plm:::pmodel.response.plm(plm(form, data = pGrunfeld_NA_dep_var, model = "random"))

resp_plm_NA_indepvar_pool  <- plm:::pmodel.response.plm(plm(form, data = pGrunfeld_NA_indep_var, model = "pooling"))
resp_plm_NA_indepvar_fe    <- plm:::pmodel.response.plm(plm(form, data = pGrunfeld_NA_indep_var, model = "within"))
resp_plm_NA_indepvar_fe_tw <- plm:::pmodel.response.plm(plm(form, data = pGrunfeld_NA_indep_var, model = "within", effect = "twoway")) # correct transformation
resp_plm_NA_indepvar_re    <- plm:::pmodel.response.plm(plm(form, data = pGrunfeld_NA_indep_var, model = "random"))


# pmodel.repsonse.pFormula with NA in dependent variable
resp_pFormula_NA_depvar_pool    <- plm:::pmodel.response.pFormula(form, data = pGrunfeld_NA_dep_var, model = "pooling")

# NOT OK: errors
#resp_pFormula_NA_depvar_fe    <- plm:::pmodel.response.pFormula(form, data = pGrunfeld_NA_dep_var, model = "within")
#resp_pFormula_NA_depvar_fe_tw <- plm:::pmodel.response.pFormula(form, data = pGrunfeld_NA_dep_var, model = "within", effect = "twoway")
#resp_pFormula_NA_depvar_re    <- plm:::pmodel.response.pFormula(form, data = pGrunfeld_NA_dep_var, model = "random")

# pmodel.repsonse.pFormula with NA in _in_dependent variable
resp_pFormula_NA_indepvar_pool  <- plm:::pmodel.response.pFormula(form, data = pGrunfeld_NA_indep_var, model = "pooling")
resp_pFormula_NA_indepvar_fe    <- plm:::pmodel.response.pFormula(form, data = pGrunfeld_NA_indep_var, model = "within")
resp_pFormula_NA_indepvar_fe_tw <- plm:::pmodel.response.pFormula(form, data = pGrunfeld_NA_indep_var, model = "within", effect = "twoway")
# resp_pFormula_NA_indepvar_re    <- plm:::pmodel.response.pFormula(form, data = pGrunfeld_NA_indep_var, model = "random") # error





#### some sanity checks ###
resp_plm_pool  <- pmodel.response(plm_pool)
resp_plm_fe    <- pmodel.response(plm_fe)
resp_plm_fe_tw <- pmodel.response(plm_fe_tw)
resp_plm_re    <- pmodel.response(plm_re)


# remove names to enable comparison
names(resp_plm_pool) <- NULL
names(resp_plm_fe) <- NULL
names(resp_plm_fe_tw) <- NULL
names(resp_plm_re) <- NULL

names(resp_pFormula_pool) <- NULL
names(resp_pFormula_fe) <- NULL
names(resp_pFormula_fe_tw) <- NULL
# names(resp_pFormula_re) <- NULL

names(resp_plm_NA_depvar_pool)  <- NULL
names(resp_plm_NA_depvar_fe)    <- NULL
names(resp_plm_NA_depvar_fe_tw) <- NULL
names(resp_plm_NA_depvar_re)    <- NULL

names(resp_pFormula_NA_depvar_pool)  <- NULL
# names(resp_pFormula_NA_depvar_fe)    <- NULL
# names(resp_pFormula_NA_depvar_fe_tw) <- NULL
# names(resp_pFormula_NA_depvar_re)    <- NULL


names(resp_plm_NA_indepvar_pool)  <- NULL
names(resp_plm_NA_indepvar_fe)    <- NULL
names(resp_plm_NA_indepvar_fe_tw) <- NULL
names(resp_plm_NA_indepvar_re)    <- NULL

names(resp_pFormula_NA_indepvar_pool)  <- NULL
names(resp_pFormula_NA_indepvar_fe)    <- NULL
names(resp_pFormula_NA_indepvar_fe_tw) <- NULL
# names(resp_pFormula_NA_indepvar_re)    <- NULL

##### interfaces: pFormula vs. plm
if (!all.equal(resp_pFormula_pool,  resp_plm_pool)) stop("Fail! resp_pFormula_pool != resp_plm_pool")
if (!all.equal(resp_pFormula_fe,    resp_plm_fe)) stop("Fail! resp_pFormula_fe != resp_plm_fe")
if (!all.equal(resp_pFormula_fe_tw, resp_plm_fe_tw)) stop("Fail! resp_pFormula_fe_tw != resp_plm_fe_tw")
#if (!all.equal(resp_pFormula_re,    resp_plm_re)) stop("Fail! resp_pFormula_re != resp_plm_re")

# with NA in dependent variable
if (!all.equal(resp_plm_NA_depvar_pool,  resp_pFormula_NA_depvar_pool)) stop("Fail! resp_plm_NA_depvar_pool != resp_pFormula_NA_depvar_pool")
#if (!all.equal(resp_plm_NA_depvar_fe,    resp_pFormula_NA_depvar_fe)) stop("Fail! resp_plm_NA_depvar_fe != resp_pFormula_NA_depvar_fe")
#if (!all.equal(resp_plm_NA_depvar_fe_tw, resp_pFormula_NA_depvar_fe_tw)) stop("Fail! resp_plm_NA_depvar_fe_tw != resp_pFormula_NA_depvar_fe_tw")
#if (!all.equal(resp_plm_NA_depvar_re,    resp_pFormula_NA_depvar_re)) stop("Fail! resp_plm_NA_depvar_re != resp_pFormula_NA_depvar_re")


# NOT OK: with NA in _in_dependent variable, fails due to different length of objects
# if (!all(resp_plm_NA_indepvar_pool  == resp_pFormula_NA_indepvar_pool)) stop("Fail! resp_plm_NA_indepvar_pool != resp_pFormula_NA_indepvar_pool")
# if (!all(resp_plm_NA_indepvar_fe    == resp_pFormula_NA_indepvar_fe)) stop("Fail! resp_plm_NA_indepvar_fe != resp_pFormula_NA_indepvar_fe")
# if (!all(resp_plm_NA_indepvar_fe_tw == resp_pFormula_NA_indepvar_fe_tw)) stop("Fail! resp_plm_NA_indepvar_fe_tw != resp_pFormula_NA_indepvar_fe_tw")
# if (!all(resp_plm_NA_indepvar_re    == resp_pFormula_NA_indepvar_re)) stop("Fail! resp_plm_NA_indepvar_re != resp_pFormula_NA_indepvar_re")

if (!all(resp_plm_NA_indepvar_pool  == resp_pFormula_NA_indepvar_pool[-1])) stop("Fail! resp_plm_NA_indepvar_pool != resp_pFormula_NA_indepvar_pool")
# NOT ok - fail due to differnt numbers
# if (!all(resp_plm_NA_indepvar_fe    == resp_pFormula_NA_indepvar_fe[-1])) stop("Fail! resp_plm_NA_indepvar_fe != resp_pFormula_NA_indepvar_fe")
# if (!all(resp_plm_NA_indepvar_fe_tw == resp_pFormula_NA_indepvar_fe_tw[-1])) stop("Fail! resp_plm_NA_indepvar_fe_tw != resp_pFormula_NA_indepvar_fe_tw")
# if (!all(resp_plm_NA_indepvar_re    == resp_pFormula_NA_indepvar_re[-1])) stop("Fail! resp_plm_NA_indepvar_re != resp_pFormula_NA_indepvar_re")