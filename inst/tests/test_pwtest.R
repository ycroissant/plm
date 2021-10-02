### Testfile for pwtest()


###  panelmodel interface did not respect the effect parameter in pre rev. 200, i. e.
###  for a supplied panelmodel effect="individual" and effect="time" delivered the same
###  result for CRAN version 1.4-0 / r-forge pre rev. 200

require(plm)
data("Produc", package="plm")
formula <- log(gsp)~log(pcap)+log(pc)+log(emp)+unemp


## formula interface
default_pwtest_formula <- pwtest(formula, data = Produc) # == effect = "individual"
ind_pwtest_formula     <- pwtest(formula, data = Produc, effect = "individual")
time_pwtest_formula    <- pwtest(formula, data = Produc, effect = "time")

## panelmodel interface
pool_prodc <- plm(formula, data=Produc, model="pooling")

default_pwtest_panelmodel <- pwtest(pool_prodc) # == effect = "individual"
ind_pwtest_panelmodel     <- pwtest(pool_prodc, effect="individual")
time_pwtest_panelmodel    <- pwtest(pool_prodc, effect="time")

if (!identical(default_pwtest_formula,    ind_pwtest_formula))    stop("pwtest.formula default effect != effect = 'individual'!")
if (!identical(default_pwtest_panelmodel, ind_pwtest_panelmodel)) stop("pwtest.panelmodel default effect != effect = 'individual'!")

if (!identical(ind_pwtest_panelmodel,  ind_pwtest_formula))  stop("pwtest with effect = 'individual': formula and panelmodel interface results differ!")
if (!identical(time_pwtest_panelmodel, time_pwtest_formula)) stop("pwtest with effect = 'individual': formula and panelmodel interface results differ!")
if (identical(ind_pwtest_panelmodel, time_pwtest_panelmodel)) stop("pwtest results for effect='individual' and effect='time' are identical!")


### test if the error messages points to the correct possible values of 'effect' argument (only 'individual' or 'time' are allowed)
# pwtest(formula, data=Produc, effect = "individualXX")
# pwtest(pool_prodc, data=Produc, effect = "individualXX")

### test if wrong input model is detected
# fe_prodc <- plm(formula, data=Produc, model="within")
# pwtest(fe_prodc)