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
nrow(pGrunfeld[pGrunfeld$inv == 317.60, ])  # operation works on pdata.frame as well




### (2) class 'pseries' of estimated_model$model [cosmetic, present in (at least) rev. 189] ###
mod <- plm(inv ~ value + capital, data=pGrunfeld, model = "pooling")
class(mod$model)
class(mod$model$inv) # 'pseries' appears twice

# if (!(length(class(mod$model$inv)) == 2 && class(mod$model$inv) == c("pseries", "numeric"))) warning("class(es) are wrong!")

df <- as.data.frame(mod$model)
class(df)
class(df$inv) # 'pseries' is still here twice

# if (!(length(class(df$inv)) == 2 && class(df$inv) == c("pseries", "numeric"))) warning("class(es) are wrong!")
