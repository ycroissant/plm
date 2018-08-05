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


# pmodel.response: test case for illegal pseries
form <- formula(inv ~ value + capital)
if (!is.pseries(pmodel.response(form, data = pGrunfeld, model = "pooling"))) stop("pmodel.response's return value is not a valid pseries")
if (!is.pseries(pmodel.response(form, data = pGrunfeld, model = "within"))) stop("pmodel.response's return value is not a valid pseries")
if (!is.pseries(pmodel.response(form, data = pGrunfeld, model = "Between"))) stop("pmodel.response's return value is not a valid pseries")
if (!is.pseries(pmodel.response(plm(form, data = pGrunfeld, model = "random")))) stop("pmodel.response's return value is not a valid pseries")
# for FD and between models, it should be a numeric as a pseries does not make sense due to the data compression
if (inherits(pmodel.response(form, data = pGrunfeld, model = "fd"), "pseries")) stop("pmodel.response's return value shall not be a pseries for fd models")
if (inherits(pmodel.response(form, data = pGrunfeld, model = "between"), "pseries")) stop("pmodel.response's return value shall not be a pseries for between models")
if (plm:::has.index(pmodel.response(plm(form, data = pGrunfeld, model = "fd")))) stop("pmodel.response's return value shall not have an index for fd models")
if (plm:::has.index(pmodel.response(plm(form, data = pGrunfeld, model = "between")))) stop("pmodel.response's return value shall not have an index for between models")


# residuals.plm: test case for illegal pseries 
if (!is.pseries(residuals(plm(form, data = pGrunfeld, model = "pooling")))) stop("residuals.plm's return value is not a valid pseries")
if (!is.pseries(residuals(plm(form, data = pGrunfeld, model = "within")))) stop("residuals.plm's return value is not a valid pseries")
if (!is.pseries(residuals(plm(form, data = pGrunfeld, model = "random")))) stop("residuals.plm's return value is not a valid pseries")
# for FD and between models, it should be a numeric as a pseries does not make sense due to the data compression
if (inherits(residuals(plm(form, data = pGrunfeld, model = "fd")), "pseries")) stop("residuals.plm's return value shall not be a pseries for fd models")
if (inherits(residuals(plm(form, data = pGrunfeld, model = "between")), "pseries")) stop("residuals.plm's return value shall not be a pseries for between models")
if (plm:::has.index(residuals(plm(form, data = pGrunfeld, model = "fd")))) stop("residuals.plm's return value shall not have an index for fd models")
if (plm:::has.index(residuals(plm(form, data = pGrunfeld, model = "between")))) stop("residuals.plm's return value shall not have an index for between models")


# fitted.plm: test case for illegal pseries
if (!is.pseries(fitted(plm(form, data = pGrunfeld, model = "pooling")))) stop("fitted.plm's return value is not a valid pseries")
if (!is.pseries(fitted(plm(form, data = pGrunfeld, model = "within")))) stop("fitted.plm's return value is not a valid pseries")
if (!is.pseries(fitted(plm(form, data = pGrunfeld, model = "random")))) stop("fitted.plm's return value is not a valid pseries")
# for FD and between models, it should be a numeric as a pseries does not make sense due to the data compression
if (inherits(fitted(plm(form, data = pGrunfeld, model = "fd")), "pseries")) stop("fitted.plm's return value shall not be a pseries for fd models")
if (inherits(fitted(plm(form, data = pGrunfeld, model = "between")), "pseries")) stop("fitted.plm's return value shall not be a pseries for between models")
if (plm:::has.index(fitted(plm(form, data = pGrunfeld, model = "fd")))) stop("fitted.plm's return value shall not have an index for fd models")
if (plm:::has.index(fitted(plm(form, data = pGrunfeld, model = "between")))) stop("fitted.plm's return value shall not have an index for between models")

## WLS
p <- plm(inv ~ value, Grunfeld, model = "pooling")
pwls <- plm(inv ~ value + capital, data = Grunfeld, weights = Grunfeld$capital, model = "pooling")

if (!is.null(p$weights)) stop("element 'weights' in plm object albeit it should not be there")
if (is.null(pwls$weights)) stop("element 'weights' missing in plm object")
