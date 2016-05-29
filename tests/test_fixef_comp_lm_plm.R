library(plm)
data("Grunfeld", package = "plm")
Grunfeld_unbalanced <- Grunfeld[1:199, ]

### TODO:
## * test vcov, once the standard erros have been modifidied to 
## accommodate the unbalanced case (see (3))

## (1) tests of effects in balanced case
## (2) tests of effects in unbalanced case
## (3) tests of standard erros in balanced and unbalanced case

######### (1) balanced ############

# plm oneway individual balanced
plm_fe_oneway_ind <- plm(inv ~ value + capital, data = Grunfeld, model = "within", effect = "individual")
fixef_plm_oneway_ind_dfirst  <- fixef(plm_fe_oneway_ind, type = "dfirst", effect = "individual")

# plm oneway time balanced
plm_fe_oneway_time <- plm(inv ~ value + capital, data = Grunfeld, model = "within", effect = "time")
fixef_plm_oneway_time_dfirst <- fixef(plm_fe_oneway_time, type = "dfirst", effect = "time")

# plm twoways balanced
plm_fe_tw <- plm(inv ~ value + capital, data = Grunfeld, model = "within", effect = "twoways")
fixef_plm_tw_ind_dfirst  <- fixef(plm_fe_tw, type = "dfirst", effect = "individual")
fixef_plm_tw_time_dfirst <- fixef(plm_fe_tw, type = "dfirst", effect = "time")

# lm oneway individual balanced
lm_fe_oneway_ind <- lm(inv ~ value + capital + factor(firm), data = Grunfeld)
fixef_lm_oneway_ind_dfirst  <- lm_fe_oneway_ind$coefficients[4:12]

# lm oneway time balanced
lm_fe_oneway_time <- lm(inv ~ value + capital + factor(year), data = Grunfeld)
fixef_lm_oneway_time_dfirst <- lm_fe_oneway_time$coefficients[4:22]

# lm twoways balanced
lm_fe_tw <- lm(inv ~ value + capital + factor(firm) + factor(year), data = Grunfeld)
fixef_lm_tw_ind_dfirst  <- lm_fe_tw$coefficients[4:12]
fixef_lm_tw_time_dfirst <- lm_fe_tw$coefficients[13:31]


# Test residuals oneway individual and time; twoway balanced
if (!isTRUE(all.equal(residuals(lm_fe_oneway_ind), as.numeric(residuals(plm_fe_oneway_ind)), check.attributes = FALSE)))
  stop("one-way individual balanced model: residuals do not match (lm vs. plm)")
if (!isTRUE(all.equal(residuals(lm_fe_oneway_time), as.numeric(residuals(lm_fe_oneway_time)), check.attributes = FALSE)))
  stop("one-way time balanced model: residuals do not match (lm vs. plm)")
if (!isTRUE(all.equal(residuals(lm_fe_tw), as.numeric(residuals(plm_fe_tw)), check.attributes = FALSE)))
  stop("two-ways balanced model: residuals do not match (lm vs. plm)")


# Test balanced one-way and two-way dfirst fixefs: lm vs. plm
if (!isTRUE(all.equal(fixef_lm_oneway_ind_dfirst, as.numeric(fixef_plm_oneway_ind_dfirst), check.attributes = FALSE)))
  stop("individual effects do not match")

if (!isTRUE(all.equal(fixef_lm_oneway_time_dfirst, as.numeric(fixef_plm_oneway_time_dfirst), check.attributes = FALSE)))
  stop("time effects do not match")

if (!isTRUE(all.equal(fixef_lm_tw_ind_dfirst, as.numeric(fixef_plm_tw_ind_dfirst), check.attributes = FALSE)))
  stop("individual effects do not match")

if (!isTRUE(all.equal(fixef_lm_tw_time_dfirst, as.numeric(fixef_plm_tw_time_dfirst), check.attributes = FALSE)))
  stop("time effects do not match")




######### (2) unbalanced ############

# plm one-way individual unbalanced
plm_fe_oneway_ind_u <- plm(inv ~ value + capital, data = Grunfeld_unbalanced, model = "within", effect = "individual")
fixef_plm_oneway_ind_dfirst_u <- fixef(plm_fe_oneway_ind_u, type = "dfirst", effect = "individual")

# plm one-way time unbalanced
plm_fe_oneway_time_u <- plm(inv ~ value + capital, data = Grunfeld_unbalanced, model = "within", effect = "time")
fixef_plm_oneway_time_dfirst_u <- fixef(plm_fe_oneway_time_u, type = "dfirst", effect = "time")

# plm twoways unbalanced
plm_fe_tw_u <- plm(inv ~ value + capital, data = Grunfeld_unbalanced, model = "within", effect = "twoways")
fixef_plm_tw_ind_dfirst_u  <- fixef(plm_fe_tw_u, type = "dfirst", effect = "individual")
fixef_plm_tw_time_dfirst_u <- fixef(plm_fe_tw_u, type = "dfirst", effect = "time")


# lm oneway individual unbalanced
lm_fe_oneway_ind_u <- lm(inv ~ value + capital + factor(firm), data = Grunfeld_unbalanced)
fixef_lm_oneway_ind_dfirst_u  <-  lm_fe_oneway_ind_u$coefficients[4:12]

# lm oneway time unbalanced
lm_fe_oneway_time_u <- lm(inv ~ value + capital + factor(year), data = Grunfeld_unbalanced)
fixef_lm_oneway_time_dfirst_u <- lm_fe_oneway_time_u$coefficients[4:22]


# lm twoways unbalanced
lm_fe_tw_u <- lm(inv ~ value + capital + factor(firm) + factor(year), data = Grunfeld_unbalanced)
fixef_lm_tw_ind_dfirst_u  <- lm_fe_tw_u$coefficients[4:12]
fixef_lm_tw_time_dfirst_u <- lm_fe_tw_u$coefficients[13:31]



# Test residuals oneway individual and time; twoway unbalanced
if (!isTRUE(all.equal(residuals(lm_fe_oneway_ind_u), as.numeric(residuals(plm_fe_oneway_ind_u)), check.attributes = FALSE)))
  stop("one-way individual unbalanced model: residuals do not match (lm vs. plm)")
if (!isTRUE(all.equal(residuals(lm_fe_oneway_time_u), as.numeric(residuals(lm_fe_oneway_time_u)), check.attributes = FALSE)))
  stop("one-way time unbalanced model: residuals do not match (lm vs. plm)")
if (!isTRUE(all.equal(residuals(lm_fe_tw_u), as.numeric(residuals(plm_fe_tw_u)), check.attributes = FALSE)))
  stop("two-ways unbalanced model: residuals do not match (lm vs. plm)")


# Test unbalanced one-way and two-way dfirst fixefs: lm vs. plm
if (!isTRUE(all.equal(fixef_lm_oneway_ind_dfirst_u,  as.numeric(fixef_plm_oneway_ind_dfirst_u), check.attributes = FALSE)))
  stop("oneway individual unbalanced: dfirst fixefs do not match")

if (!isTRUE(all.equal(fixef_lm_oneway_time_dfirst_u, as.numeric(fixef_plm_oneway_time_dfirst_u), check.attributes = FALSE)))
  stop("oneway time unbalanced: dfirst fixefs do not match")

if (!isTRUE(all.equal(fixef_lm_tw_ind_dfirst_u,  as.numeric(fixef_plm_tw_ind_dfirst_u), check.attributes = FALSE)))
  stop("two-ways individual unbalanced: dfirst fixefs do not match")

if (!isTRUE(all.equal(fixef_lm_tw_time_dfirst_u,  as.numeric(fixef_plm_tw_time_dfirst_u), check.attributes = FALSE)))
  stop("two-ways individual unbalanced: dfirst fixefs do not match")


# TODO: mean(fixef) oder weighted.mean(fixef) -> auch in doc! -> siehe andere Testdatei


######### (3) Test of standard errors, balanced and unbalanced ############

# oneway ind, time balanced
sum_lm_fe_oneway_ind  <- summary(lm_fe_oneway_ind)
sum_lm_fe_oneway_time <- summary(lm_fe_oneway_time)
sum_plm_fixef_lm_oneway_ind_dfirst  <- summary(fixef_plm_oneway_ind_dfirst)
sum_plm_fixef_lm_oneway_time_dfirst <- summary(fixef_plm_oneway_time_dfirst)

# twoways ind, time balanced
sum_lm_fe_tw <- summary(lm_fe_tw)
sum_plm_fixef_lm_tw_ind_dfirst  <- summary(fixef_plm_tw_ind_dfirst)
sum_plm_fixef_lm_tw_time_dfirst <- summary(fixef_plm_tw_time_dfirst)

# oneway ind, time unbalanced
sum_lm_fe_oneway_ind_u  <- summary(lm_fe_oneway_ind_u)
sum_lm_fe_oneway_time_u <- summary(lm_fe_oneway_time_u)
sum_plm_fixef_lm_oneway_ind_dfirst_u  <- summary(fixef_plm_oneway_ind_dfirst_u)
sum_plm_fixef_lm_oneway_time_dfirst_u <- summary(fixef_plm_oneway_time_dfirst_u)

# twoways ind, time unbalanced
sum_lm_fe_tw_u <- summary(lm_fe_tw_u)
sum_plm_fixef_lm_tw_ind_dfirst_u  <- summary(fixef_plm_tw_ind_dfirst_u)
sum_plm_fixef_lm_tw_time_dfirst_u <- summary(fixef_plm_tw_time_dfirst_u)

# one-way balanced
if (!isTRUE(all.equal(sum_lm_fe_oneway_ind[["coefficients"]][c(4:12) , "Std. Error"], sum_plm_fixef_lm_oneway_ind_dfirst[ , "Std. Error"], check.attributes = FALSE)))
  stop("standard errors diverge: summary.plm vs. summary.fixef(..., type = \"dfirst\")")
if (!isTRUE(all.equal(sum_lm_fe_oneway_time[["coefficients"]][c(4:22) , "Std. Error"], sum_plm_fixef_lm_oneway_time_dfirst[ , "Std. Error"], check.attributes = FALSE)))
  stop("standard errors diverge: summary.plm vs. summary.fixef(..., type = \"dfirst\")")

# one-way unbalanced
if (!isTRUE(all.equal(sum_lm_fe_oneway_ind_u[["coefficients"]][c(4:12) , "Std. Error"], sum_plm_fixef_lm_oneway_ind_dfirst_u[ , "Std. Error"], check.attributes = FALSE)))
  stop("standard errors diverge: summary.plm vs. summary.fixef(..., type = \"dfirst\")")
if (!isTRUE(all.equal(sum_lm_fe_oneway_time_u[["coefficients"]][c(4:22) , "Std. Error"], sum_plm_fixef_lm_oneway_time_dfirst_u[ , "Std. Error"], check.attributes = FALSE)))
  stop("standard errors diverge: summary.plm vs. summary.fixef(..., type = \"dfirst\")")

# two-ways balanced
if (!isTRUE(all.equal(sum_lm_fe_tw[["coefficients"]][c(4:12) , "Std. Error"], sum_plm_fixef_lm_tw_ind_dfirst[ , "Std. Error"], check.attributes = FALSE)))
  stop("standard errors diverge: summary.plm vs. summary.fixef(..., type = \"dfirst\")")
if (!isTRUE(all.equal(sum_lm_fe_tw[["coefficients"]][c(13:31) , "Std. Error"], sum_plm_fixef_lm_tw_time_dfirst[ , "Std. Error"], check.attributes = FALSE)))
  stop("standard errors diverge: summary.plm vs. summary.fixef(..., type = \"dfirst\")")

# two-ways unbalanced - does not yet match
# if (!isTRUE(all.equal(sum_lm_fe_tw_u[["coefficients"]][c(4:12) , "Std. Error"], sum_plm_fixef_lm_tw_ind_dfirst_u[ , "Std. Error"], check.attributes = FALSE)))
#   stop("standard errors diverge: summary.plm vs. summary.fixef(..., type = \"dfirst\")")
# if (!isTRUE(all.equal(sum_lm_fe_tw_u[["coefficients"]][c(13:31) , "Std. Error"], sum_plm_fixef_lm_tw_time_dfirst_u[ , "Std. Error"], check.attributes = FALSE)))
#   stop("standard errors diverge: summary.plm vs. summary.fixef(..., type = \"dfirst\")")

