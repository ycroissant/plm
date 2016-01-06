## Tests for model.matrix[.pFormula|.plm] and pmodel.response.[pFormula|.plm|.data.frame]

# commented lines do not run in v1.5-15

# 1) model.matrix[.pFormula|.plm] 
# 2) pmodel.response.[pFormula|.plm|.data.frame]



library(plm)
data(Grunfeld, package="plm")
form <- formula(inv ~ value + capital)
plm_pool <- plm(form, data=Grunfeld, model="pooling")
plm_fe   <- plm(form, data=Grunfeld, model="within")
plm_re   <- plm(form, data=Grunfeld, model="random")

########### 1) model.matrix[.pFormula|.plm] ###########

# still warning in v1.5-15
# modmat_pFormula_df_pool <- plm:::model.matrix.pFormula(form, data=Grunfeld, model="pooling")

# these still fail as of v1.5-15
# modmat_pFormula_df_fe <- plm:::model.matrix.pFormula(form, data=Grunfeld, model="within")
# modmat_pFormula_df_re <- plm:::model.matrix.pFormula(form, data=Grunfeld, model="random")

# Error in tapply(z, effect, "mean") : arguments must have same length
# Warning message:
# In is.na(id) : is.na() applied to non-(list or vector) of type 'NULL'


# pooling and within models work pdata.frame
pGrunfeld <- pdata.frame(Grunfeld, index = c("firm", "year"))

modmat_pFormula_pdf_pool <- plm:::model.matrix.pFormula(form, data=pGrunfeld, model="pooling") # works
modmat_pFormula_pdf_fe   <- plm:::model.matrix.pFormula(form, data=pGrunfeld, model="within")  # works
#modmat_pFormula_re2   <- plm:::model.matrix.pFormula(form, data=pGrunfeld, model="random")  # still fails in v1.5-15

# Error:
# Error in plm:::model.matrix.pFormula(form, data = pGrunfeld, model = "random") : 
#   dims [product 600] do not match the length of object [0]


#### some sanity checks if various interfaces yield the same result ###
modmat_plm_pool <- model.matrix(plm_pool)
modmat_plm_fe   <- model.matrix(plm_fe)
modmat_plm_re   <- model.matrix(plm_re)


# delete rownames first to enable comparision
# [some results are based on data.frame w/o rownames, some on pdata.frame w/ rownames]
rownames(modmat_plm_pool) <- NULL
rownames(modmat_plm_fe) <- NULL
rownames(modmat_plm_re) <- NULL

# rownames(modmat_pFormula_df_pool) <- NULL
# rownames(modmat_pFormula_df_fe) <- NULL
# rownames(modmat_pFormula_df_re) <- NULL

rownames(modmat_pFormula_pdf_pool) <- NULL
rownames(modmat_pFormula_pdf_fe) <- NULL
# rownames(modmat_pFormula_pdf_re) <- NULL

##### interfaces: plm vs. pFormula with data.frame
#all.equal(modmat_plm_pool, modmat_pFormula_df_pool)
#all.equal(modmat_plm_fe,   modmat_pFormula_df_fe)
#all.equal(modmat_plm_re,   modmat_pFormula_df_re)

##### interfaces: plm vs. pFormula with pdata.frame
if(!all.equal(modmat_plm_pool, modmat_pFormula_pdf_pool)) stop("model.matrix's are not the same")
if(!all.equal(modmat_plm_fe,   modmat_pFormula_pdf_fe)) stop("model.matrix's are not the same")
#if(!all.equal(modmat_plm_re,   modmat_pFormula_pdf_re)) stop("model.matrix's are not the same")




########### 2) pmodel.response.[pFormula|.plm|.data.frame] ###########
# plm:::pmodel.response.pFormula(form, data = Grunfeld, model = "pooling") # warning still in v1.5-14/rev. 175
# plm:::pmodel.response.pFormula(form, data = Grunfeld, model = "within")  # fails
# plm:::pmodel.response.pFormula(form, data = Grunfeld, model = "random")  # fails

# pooling and within models work on pdata.frame with fix in v1.5-14/rev. 175
resp_pFormula_pool <- plm:::pmodel.response.pFormula(form, data = pGrunfeld, model = "pooling") 
resp_pFormula_fe   <- plm:::pmodel.response.pFormula(form, data = pGrunfeld, model = "within")

# still fails
# resp_pFormula_re <- plm:::pmodel.response.pFormula(form, data = pGrunfeld, model = "random")
#
# Error in model.matrix.pFormula(pFormula(formula), data = data, model = model,  : 
#   dims [product 200] do not match the length of object [0]

# warnings and sometimes error when applied on data.frame [needs to be without the index columns!]
#resp_df_pool <- plm:::pmodel.response.data.frame(Grunfeld[ , -c(1:2) ], model = "pooling") # warning
#resp_df_fel  <- plm:::pmodel.response.data.frame(Grunfeld[ , -c(1:2) ], model = "within") # error and warning
#resp_df_re   <- plm:::pmodel.response.data.frame(Grunfeld[ , -c(1:2) ], model = "random") # warning on data.frame


# on pdata.frame [needs to be without the index columns!]
resp_pdf_pool <- plm:::pmodel.response.data.frame(pGrunfeld[ , -c(1:2) ], model = "pooling") # works
resp_pdf_fe   <- plm:::pmodel.response.data.frame(pGrunfeld[ , -c(1:2)], model = "within")   # works
#resp_pdf_re   <- plm:::pmodel.response.data.frame(pGrunfeld[ , -c(1:2)], model = "random") # error


#### some sanity checks if various interfaces yield the same result ###
resp_plm_pool <- pmodel.response(plm_pool)
resp_plm_fe   <- pmodel.response(plm_fe)
resp_plm_re   <- pmodel.response(plm_re)

# delete names first to enable comparision
# [some results are based on data.frame w/o rownames, some on pdata.frame w/ rownames]
names(resp_plm_pool) <- NULL
names(resp_plm_fe) <- NULL
names(resp_plm_re) <- NULL

names(resp_pFormula_pool) <- NULL
names(resp_pFormula_fe) <- NULL
# names(resp_pFormula_re) <- NULL

names(resp_pdf_pool) <- NULL
names(resp_pdf_fe) <- NULL
# names(resp_pdf_re) <- NULL


# compare interface pFormula with plm
if(!all.equal(resp_pFormula_pool, resp_plm_pool)) stop("responses not equal")
if(!all.equal(resp_pFormula_fe,   resp_plm_fe)) stop("responses not equal")
#if(!all.equal(resp_pFormula_re, resp_plm_re)) stop("responses not equal")

# compare interface data.frame with plm
if(!all.equal(resp_pdf_pool, resp_plm_pool)) stop("responses not equal")
if(!all.equal(resp_pdf_fe,   resp_plm_fe)) stop("responses not equal")
#if(!all.equal(resp_pdf_re, resp_plm_re)) stop("responses not equal")
