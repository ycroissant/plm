### (1) Comparision of extraction of data.frame and pdata.frame and
### (2) class 'pseries' of estimated_model$model



### (1) Comparision of extraction of data.frame and pdata.frame ###
# -> everything is ok in rev. 189+

# from ?pdata.frame: "The "[" method behaves as for data.frame, except that the extraction is also applied to the index attribute."
library(plm)
data(Grunfeld, package="plm")
class(Grunfeld)
pGrunfeld <- pdata.frame(Grunfeld, index = c("firm", "year"), drop.index = F)
class(pGrunfeld)

nrow(Grunfeld[Grunfeld$inv == 317.60, ])    # 1 row and ...
class(Grunfeld[Grunfeld$inv == 317.60, ])   # ... it is a data.frame

class(pGrunfeld[pGrunfeld$inv == 317.60, ]) # should be classes 'pdata.frame' and 'data.frame' like it is for a data.frame
if (!all(class(pGrunfeld[pGrunfeld$inv == 317.60, ]) == c("pdata.frame", "data.frame"))) stop("wrong classes")
nrow(pGrunfeld[pGrunfeld$inv == 317.60, ])  # operation works on pdata.frame as well




### (2) class 'pseries' of estimated_model$model [fixed in rev. 242] ###
mod <- plm(inv ~ value + capital, data=pGrunfeld, model = "pooling")
class(mod$model) # 'pseries' appeared twice before rev. 242
if (!all(class(mod$model$inv) == c("pseries", "numeric"))) stop("wrong classes (or too few/many")
if (!(length(class(mod$model$inv)) == 2 && class(mod$model$inv) == c("pseries", "numeric"))) warning("class(es) are wrong!")


df <- as.data.frame(mod$model)
class(df)
class(df$inv) # 'pseries' apperead two before rev. 242
if (!(length(class(df$inv)) == 2 && class(df$inv) == c("pseries", "numeric"))) warning("class(es) are wrong!")



# pdata.frame extraction by [.pdata.frame uses data.frame's extraction ([.data.frame) which drops the "pindex" class for the index
# test if it is still present
class(attr(pGrunfeld, "index"))
class(attr(pGrunfeld[1, ], "index"))
if(!all(class(attr(pGrunfeld[1, ], "index")) == c("pindex", "data.frame")))
  stop("class \"pindex\" missing for index after subsetting a pdata.frame")

# for pseries
class(attr(pGrunfeld$inv, "index"))
class(attr(pGrunfeld[1, ]$inv, "index"))
if(!all(class(attr(pGrunfeld[1, ]$inv, "index")) == c("pindex", "data.frame")))
  stop("class \"pindex\" missing for index after subsetting a pseries")



## pdata.frame: Check order of attributes after subsetting [R's "[.data.frame" does not preserve order of attributes]
attrib_names_before_subsetting_pdataframe <- names(attributes(pGrunfeld))
attrib_names_after_subsetting_pdataframe  <- names(attributes(pGrunfeld[1:2, ]))
if (!isTRUE(all.equal(attrib_names_before_subsetting_pdataframe, attrib_names_after_subsetting_pdataframe)))
  stop("order of attributes has changed after subsetting a pdata.frame")

## pseries: pdata.frame: Check order of attributes after subsetting [R's "[.data.frame" does not preserve order of attributes]
attrib_names_before_subsetting_pseries <- names(attributes(pGrunfeld$inv))
attrib_names_after_subsetting_pseries  <- names(attributes(pGrunfeld[1:2, ]$inv))
if (!isTRUE(all.equal(attrib_names_before_subsetting_pseries, attrib_names_after_subsetting_pseries)))
  stop("order of attributes has changed after subsetting a pseries")


