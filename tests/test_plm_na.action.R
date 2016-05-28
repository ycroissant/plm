## plm does not respect argument na.action for padding residuals:
##  There is no element "na.action" in the plm object.
##  Putting it in, would allow for padding the residuals with NA to
##  match the number of rows of the original data by using the standard
##  framework like it is in place for lm().
##
## However, many functions in the plm package (statistical tests, ...) rely
## on the non-padded residuals but extract the residuals by residuals(plm_object)
## or the shortcut resid(plm_object), instead of using plm_object$residuals which
## give the "correct" residuals for such tests.
## 
## There is an experimental branch with an added na.action element and changed
## functions to use plm_object$residuals where appropriate, but likely not all
## cases have been caught in that branch.
##
##
## compare lm()'s and plm()'s behaviour
## when residuals() is called on plm objects, na.action element is disrepected;
## na.action = na.omit drops NAs from residuals extracted by residuals 
## na.action = na.exclude performs NA padding to match original data length

library(plm)
data("Grunfeld", package = "plm")
form <- formula(inv ~ value + capital)

# set some arbitrary value to NA, so it is left out of the estimation
Grunfeld_1NA <- Grunfeld
line_no <- 6L
Grunfeld_1NA[line_no, "inv"] <- NA


############ lm ############

# lm and na.action set to default [usually na.omit]
lm_gr_1NA <- lm(form, data = Grunfeld_1NA)
nobs(lm_gr_1NA)
length(residuals(lm_gr_1NA))
residuals(lm_gr_1NA)[line_no]

# lm and na.omit
lm_gr_na_omit <- lm(form, data = Grunfeld_1NA, na.action = na.omit)
nobs(lm_gr_na_omit)
length(residuals(lm_gr_na_omit))  # should be equal to no. of obs used (199)
residuals(lm_gr_na_omit)[line_no] # element #line_no should be non-NA (a real data point)
is.na(residuals(lm_gr_na_omit)[line_no])
lm_gr_na_omit$na.action
head(lm_gr_na_omit$model)
na.action(lm_gr_na_omit)


# lm and na.exclude
lm_gr_na_exclude <- lm(form, data = Grunfeld_1NA, na.action = na.exclude)
nobs(lm_gr_na_exclude)
length(residuals(lm_gr_na_exclude))  # should be equal to length of original data, due to padding with NA values (200)
length(lm_gr_na_exclude$residuals)   # but element "residuals" in lm object has only values without NA
residuals(lm_gr_na_exclude)[line_no] # element #line_no should be NA, due to padding performed
is.na(residuals(lm_gr_na_exclude)[line_no])
head(lm_gr_na_exclude$model)
na.action(lm_gr_na_exclude)

# lm and na.pass
# lm_gr_na_pass <- lm(form, data=Grunfeld_1NA, na.action = na.pass) # yields an error for lm: Error in lm.fit(....) : NA/NaN/Inf in 'y'

# Should be TRUE [199 + 1 == 200]
# if (!(length(residuals(lm_gr_na_omit)) + 1 == length(residuals(lm_gr_na_exclude))))
#   stop("in lm: na.action with na.omit and na.exclude not working correctly")



############ plm ############

# plm and na.action set to default [usually na.omit]
plm_gr_1NA <- plm(form, data = Grunfeld_1NA, model = "pooling")
nobs(plm_gr_1NA)
length(residuals(plm_gr_1NA))
residuals(plm_gr_1NA)[line_no]
is.na(residuals(plm_gr_1NA)[line_no])
plm_gr_1NA$na.action

# plm and na.omit
plm_gr_na_omit <- plm(form, data = Grunfeld_1NA, model = "pooling", na.action = na.omit)
nobs(plm_gr_na_omit)
length(residuals(plm_gr_na_omit))
residuals(plm_gr_na_omit)[line_no]
is.na(residuals(plm_gr_na_omit)[line_no])
plm_gr_na_omit$na.action
#if (is.null(plm_gr_na_omit$na.action)) stop("no na.action element found")

# plm and na.exclude
plm_gr_na_exclude <- plm(form, data = Grunfeld_1NA, model = "pooling", na.action = na.exclude)
nobs(plm_gr_na_exclude)
length(residuals(plm_gr_na_exclude))
residuals(plm_gr_na_exclude)[line_no]
is.na(residuals(plm_gr_na_exclude)[line_no])
plm_gr_na_exclude$na.action
#if (is.null(plm_gr_na_exclude$na.action)) stop("no na.action element found")

# plm and na.pass
# as opposed to lm, plm does not stop with na.pass
# NB: as na.pass returns the object unchanged, there is no attribute "na.action"
#     in this case for the data or for the plm object
plm_gr_na_pass <- plm(form, data = Grunfeld_1NA, model = "pooling", na.action = na.pass)
nobs(plm_gr_na_pass)
length(residuals(plm_gr_na_pass))
residuals(plm_gr_na_pass)[line_no]
is.na(residuals(plm_gr_na_pass)[line_no])
plm_gr_na_pass$na.action
# if (!is.null(plm_gr_na_pass$na.action))
#   stop("na.pass: na.action element found in plm object, albeit there should be non for na.action = na.pass")

# plm and na.pass without NAs
plm_gr_no_NA_na_pass <- plm(form, data = Grunfeld, model = "pooling", na.action = na.pass)
# if (!is.null(plm_gr_no_NA_na_pass$na.action))
#   stop("na.pass: na.action element found in plm object, albeit there should be non for na.action = na.pass")


# Should be TRUE [199 + 1 == 200]
length(residuals(plm_gr_na_omit)) + 1 == length(residuals(plm_gr_na_exclude))

# formal test:
 # if (!(length(residuals(plm_gr_na_omit)) + 1 == length(residuals(plm_gr_na_exclude))))
 #   stop("residuals not padded!")

# test with randomly missing data
Grunfeld_NA_rand <- Grunfeld
Grunfeld_NA_rand[sample(1:nrow(Grunfeld_NA_rand), size = 25), "inv"] <- NA
sum(is.na(Grunfeld_NA_rand$inv))
plm_gr_wi_na_exclude <- plm(form, data = Grunfeld_NA_rand, model = "within", na.action = na.exclude)
data_NA_structure <- is.na(Grunfeld_NA_rand$inv)
res_NA_structure  <- is.na(residuals(plm_gr_wi_na_exclude))
# if (!isTRUE(all.equal(data_NA_structure, res_NA_structure, check.attributes = FALSE)))
#   stop("na.exclude: NA pattern does not match NA pattern of original data")



## test with summary call etc.
## some show NA values
summary(plm_gr_1NA)
summary(plm_gr_na_omit)
summary(plm_gr_na_exclude)
summary(plm_gr_na_pass)
summary(plm_gr_no_NA_na_pass)
summary(plm_gr_wi_na_exclude)

class(residuals(plm_gr_1NA))
class(residuals(plm_gr_na_omit))
class(residuals(plm_gr_na_exclude))
class(residuals(plm_gr_na_pass))
class(residuals(plm_gr_no_NA_na_pass))
class(residuals(plm_gr_wi_na_exclude))

