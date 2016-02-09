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



# pooling and within models work pdata.frame [albeit one should input a model.frame of class pdata.frame]
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


##### interfaces: plm vs. pFormula with pdata.frame
if(!isTRUE(all.equal(modmat_plm_pool, modmat_pFormula_pdf_pool, check.attributes = FALSE))) stop("model.matrix's are not the same")
if(!isTRUE(all.equal(modmat_plm_fe,   modmat_pFormula_pdf_fe,   check.attributes = FALSE))) stop("model.matrix's are not the same")
#if(!isTRUE(all.equal(modmat_plm_re,   modmat_pFormula_pdf_re,   check.attributes = FALSE))) stop("model.matrix's are not the same")




########### 2) pmodel.response.[pFormula|.plm|.data.frame] ###########

# pooling and within models work on a pdata.frame [the plain pdata.frame is coerced to a model.frame
# internally in pmodel.response.pFormula]
resp_pFormula_pool <- plm:::pmodel.response.pFormula(form, data = pGrunfeld, model = "pooling") 
resp_pFormula_fe   <- plm:::pmodel.response.pFormula(form, data = pGrunfeld, model = "within")

# still fails
# resp_pFormula_re <- plm:::pmodel.response.pFormula(form, data = pGrunfeld, model = "random")
#
# Error in model.matrix.pFormula(pFormula(formula), data = data, model = model,  : 
#   dims [product 200] do not match the length of object [0]


### pmodel.response.data.frame on data.frame/pdata.frame
## the 'data' data.frame for pmodel.response.data.frame must be a model.frame created by plm's model.frame
## it needs to be a model.frame because then it is ensured we find the response variable in the fist column
pGrunfeld_mf <- model.frame(pFormula(form), data = pGrunfeld)

resp_pdf_mf_pool <- plm:::pmodel.response.data.frame(pGrunfeld_mf, model = "pooling") # works
resp_pdf_mf_fe   <- plm:::pmodel.response.data.frame(pGrunfeld_mf, model = "within")   # works
#resp_pdf_mf_re   <- plm:::pmodel.response.data.frame(pGrunfeld_mf, model = "random") # error, likely due to missing arguments




#### some sanity checks if various interfaces yield the same result ###
resp_plm_pool <- pmodel.response(plm_pool)
resp_plm_fe   <- pmodel.response(plm_fe)
resp_plm_re   <- pmodel.response(plm_re)



# compare interface pFormula with plm
if(!isTRUE(all.equal(resp_pFormula_pool, resp_plm_pool, check.attributes = FALSE))) stop("responses not equal")
if(!isTRUE(all.equal(resp_pFormula_fe,   resp_plm_fe, check.attributes = FALSE))) stop("responses not equal")
#if(!isTRUE(all.equal(resp_pFormula_re, resp_plm_re, check.attributes = FALSE))) stop("responses not equal")


# compare interface data.frame with model.frame with plm
if(!isTRUE(all.equal(resp_pdf_mf_pool, resp_plm_pool, check.attributes = FALSE))) stop("responses not equal")
if(!isTRUE(all.equal(resp_pdf_mf_fe,   resp_plm_fe, check.attributes = FALSE))) stop("responses not equal")
#if(!isTRUE(all.equal(resp_pdf_mf_re, resp_plm_re, check.attributes = FALSE))) stop("responses not equal")




