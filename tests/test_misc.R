library(plm)
data("Grunfeld", package = "plm")
Grunfeld_unbal <- Grunfeld[1:199, ]
# ercomp(plm(inv ~ value, Grunfeld, model = "random"))
# ercomp(plm(inv ~ value, Grunfeld, model = "random", random.method = "amemiya"))
# ercomp(plm(inv ~ value + capital, Grunfeld_unbal, model = "random"))


# these resulted in errors pre rev. 523 due to missing drop = FALSE
plm(inv ~ value, Grunfeld_unbal, model = "random", random.method = "amemiya")
plm(inv ~ value, Grunfeld_unbal, model = "random", random.method = "amemiya", effect = "time")


# test case for illegal pseries in pmerge's return value:
# up to rev. 675, pmerge produced a data.frame with a column declared to be a pseries but with lacking index,
# and there should be no 'pseries' in the resulting data.frame in first place
pGrunfeld <- pdata.frame(Grunfeld)
df_after_pmerge <- plm:::pmerge(pGrunfeld$inv, pGrunfeld$value)
if (inherits(df_after_pmerge$ind, "pseries") & is.null(attr(df_after_pmerge$ind, "index"))) stop("illegal pseries (no index) produced by pmerge")
if ("pseries" %in% unlist(lapply(df_after_pmerge, class))) stop("pmerge returned a column with pseries")
if (!"data.frame" == class(df_after_pmerge)) stop("pmerge did not return a pure data.frame according to class()")


# test case for illegal pseries from pmodel.response for some models: index is not correct
form <- formula(inv ~ value + capital)
if (!plm:::has.index(pmodel.response(form, data = pGrunfeld, model = "pooling"))) stop("pmodel.response's return value does not have an index")
if (!plm:::has.index(pmodel.response(form, data = pGrunfeld, model = "within"))) stop("pmodel.response's return value does not have an index")
if (!plm:::has.index(pmodel.response(form, data = pGrunfeld, model = "Between"))) stop("pmodel.response's return value does not have an index")

### TODO: no or illegal index for FD and between models
# if (!plm:::has.index(pmodel.response(form, data = pGrunfeld, model = "fd"))) stop("pmodel.response's return value does not have an index")
# if (!plm:::has.index(pmodel.response(form, data = pGrunfeld, model = "between"))) stop("pmodel.response's return value does not have an index")

