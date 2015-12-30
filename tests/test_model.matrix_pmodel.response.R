## Tests for model.matrix[.pFormula|.plm] and pmodel.response.[pFormula|.plm]

# 1) model.matrix[.pFormula|.plm] 
# 2) pmodel.response.[pFormula|.plm]


library(plm)
data(Grunfeld, package="plm")
form <- formula(inv ~ value + capital)
plm_pool <- plm(form, data=Grunfeld, model="pooling")
plm_fe   <- plm(form, data=Grunfeld, model="within")
plm_re   <- plm(form, data=Grunfeld, model="random")

########### 1) model.matrix[.pFormula|.plm] ###########

# still warning in v1.5-14 / 2015-12-30
# modmat_pFormula_pool <- plm:::model.matrix.pFormula(form, data=Grunfeld, model="pooling")

# these still fail as of v1.5-14 / 2015-12-30
# modmat_pFormula_fe <- plm:::model.matrix.pFormula(form, data=Grunfeld, model="within")
# modmat_pFormula_re <- plm:::model.matrix.pFormula(form, data=Grunfeld, model="random")

# Error in tapply(z, effect, "mean") : arguments must have same length
# Warning message:
# In is.na(id) : is.na() applied to non-(list or vector) of type 'NULL'


# pooling and within models work pdata.frame
pGrunfeld <- pdata.frame(Grunfeld, index = c("firm", "year"))

modmat_pFormula_pool2 <- plm:::model.matrix.pFormula(form, data=pGrunfeld, model="pooling") # works w/o warning
modmat_pFormula_fe2   <- plm:::model.matrix.pFormula(form, data=pGrunfeld, model="within")  # works w/o warning
# plm:::model.matrix.pFormula(form, data=pGrunfeld, model="random")  # still fails in v1.5-14

# Error:
# Error in plm:::model.matrix.pFormula(form, data = pGrunfeld, model = "random") : 
#   dims [product 600] do not match the length of object [0]


#### some sanity checks ###
modmat_plm_pool <- model.matrix(plm_pool)
modmat_plm_fe <- model.matrix(plm_fe)

rownames(modmat_plm_pool) <- NULL
rownames(modmat_plm_fe) <- NULL
rownames(modmat_pFormula_fe2) <- NULL
# rownames(modmat_pFormula_pool) <- NULL
rownames(modmat_pFormula_pool2) <- NULL

# all.equal(modmat_plm_pool, modmat_pFormula_pool)
# all.equal(modmat_pFormula_pool, modmat_pFormula_pool2)

# add more here



########### 2) pmodel.response.[pFormula|.plm] ###########
# plm:::pmodel.response.pFormula(form, data = Grunfeld, model = "pooling") # warning still in v1.5-14/rev. 175
# plm:::pmodel.response.pFormula(form, data = Grunfeld, model = "within")  # fails
# plm:::pmodel.response.pFormula(form, data = Grunfeld, model = "random")  # fails

# pooling and within modls work on pdata.frame with fix in v1.5-14/rev. 175
resp_pFormula_pool <- plm:::pmodel.response.pFormula(form, data = pGrunfeld, model = "pooling") 
resp_pFormula_fe   <- plm:::pmodel.response.pFormula(form, data = pGrunfeld, model = "within")

# still fails
# resp_pFormula_re <- plm:::pmodel.response.pFormula(form, data = pGrunfeld, model = "random")
#
# Error in model.matrix.pFormula(pFormula(formula), data = data, model = model,  : 
#   dims [product 200] do not match the length of object [0]

#### some sanity checks ###
resp_plm_pool <- pmodel.response(plm_pool)
resp_plm_fe <- pmodel.response(plm_fe)
resp_plm_re <- pmodel.response(plm_re)

names(resp_plm_pool) <- NULL
names(resp_plm_fe) <- NULL
names(resp_plm_re) <- NULL

names(resp_pFormula_pool) <- NULL
names(resp_pFormula_fe) <- NULL
# names(resp_pFormula_re) <- NULL

all.equal(resp_pFormula_pool, resp_plm_pool)
all.equal(resp_pFormula_fe, resp_plm_fe)
# all.equal(resp_pFormula_re, resp_plm_re)

