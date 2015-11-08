## plm does not respect argument na.action [in CRAN release v1.4-0 and SVN rev. up to (at least) 155]
## compare lm()'s and plm()'s behaviour
## plm always uses na.action = na.omit and disrespects na.action = na.exclude (na.exclude performs no padding)

library(plm)
data("Grunfeld", package = "plm")
form <- formula(inv ~ value + capital)

# set some arbitrary value to NA, so it is left out of the estimation
Grunfeld_1NA <- Grunfeld
line_no <- 6L
Grunfeld_1NA[line_no, "inv"] <- NA


############ lm ############

# lm and na.action set to default [usually na.omit]
lm_gr_1NA <- lm(form, data=Grunfeld_1NA)
nobs(lm_gr_1NA)
length(residuals(lm_gr_1NA))
residuals(lm_gr_1NA)[line_no]

# lm and na.omit
lm_gr_na_omit <- lm(form, data=Grunfeld_1NA, na.action = na.omit)
nobs(lm_gr_na_omit)
length(residuals(lm_gr_na_omit))  # should be equal to no. of obs used (199)
residuals(lm_gr_na_omit)[line_no] # element #line_no should be non-NA (a real data point)
is.na(residuals(lm_gr_na_omit)[line_no])

# lm and na.exclude
lm_gr_na_exclude <- lm(form, data=Grunfeld_1NA, na.action = na.exclude)
nobs(lm_gr_na_exclude)
length(residuals(lm_gr_na_exclude))  # should be equal to length of original data, due to padding with NA values (200)
residuals(lm_gr_na_exclude)[line_no] # element #line_no should be NA, due to padding performed
is.na(residuals(lm_gr_na_exclude)[line_no])

# Should be TRUE [199 + 1 == 200]
length(residuals(lm_gr_na_omit)) + 1 == length(residuals(lm_gr_na_exclude))



############ plm ############

# plm and na.action set to default [usually na.omit]
plm_gr_1NA <- plm(form, data=Grunfeld_1NA, model="pooling")
nobs(plm_gr_1NA)
length(residuals(plm_gr_1NA))
residuals(plm_gr_1NA)[line_no]
is.na(residuals(plm_gr_1NA)[line_no])


# plm and na.omit
plm_gr_na_omit <- plm(form, data=Grunfeld_1NA, model="pooling", na.action = na.omit)
nobs(plm_gr_na_omit)
length(residuals(plm_gr_na_omit))
residuals(plm_gr_na_omit)[line_no]
is.na(residuals(plm_gr_na_omit)[line_no])

# plm and na.exclude
plm_gr_na_exclude <- plm(form, data=Grunfeld_1NA, model="pooling", na.action = na.exclude)
nobs(plm_gr_na_exclude)
length(residuals(plm_gr_na_exclude))
residuals(plm_gr_na_exclude)[line_no]
is.na(residuals(plm_gr_na_exclude)[line_no])

# Should be TRUE [199 + 1 == 200], but is not currently
length(residuals(plm_gr_na_omit)) + 1 == length(residuals(plm_gr_na_exclude))
