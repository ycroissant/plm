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
if (inherits(df_after_pmerge$ind, "pseries") && is.null(attr(df_after_pmerge$ind, "index"))) stop("illegal pseries (no index) produced by pmerge")
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

## aneweytest
data("RiceFarms", package = "plm")
aneweytest(log(goutput) ~ log(seed) + log(totlabor) + log(size), RiceFarms, index = "id")

## piest
pirice <- piest(log(goutput) ~ log(seed) + log(totlabor) + log(size), RiceFarms, index = "id")
summary(pirice)

## mtest
data("EmplUK", package = "plm")
ar <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1) +
             lag(log(capital), 0:2) + lag(log(output), 0:2) | lag(log(emp), 2:99),
           data = EmplUK, effect = "twoways", model = "twosteps")
mtest(ar, order = 1)
mtest(ar, order = 2, vcov = vcovHC)

## pcdtest
pcdtest(inv ~ value + capital, data = Grunfeld,
        index = c("firm", "year"))

## test on two-way fixed effects homogeneous model
pcdtest(inv ~ value + capital, data = Grunfeld, model = "within",
        effect = "twoways", index = c("firm", "year"))

## test on panelmodel object
g <- plm(inv ~ value + capital, data = Grunfeld, index = c("firm", "year"))
pcdtest(g)

## scaled LM test
pcdtest(g, test = "sclm")

## test on pseries
pGrunfeld <- pdata.frame(Grunfeld)
pcdtest(pGrunfeld$value)

## local test
## define neighbours for individual 2: 1, 3, 4, 5 in lower triangular matrix
w <- matrix(0, ncol= 10, nrow=10)
w[2,1] <- w[3,2] <- w[4,2] <- w[5,2] <- 1
pcdtest(g, w = w)


## cortab
pGrunfeld <- pdata.frame(Grunfeld)
grp <- c(rep(1, 100), rep(2, 50), rep(3, 50)) # make 3 groups
cortab(pGrunfeld$value, grouping = grp, groupnames = c("A", "B", "C"))

## ercomp
data("Produc", package = "plm")
ercomp(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc,
       method = "walhus", effect = "time")
z <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
         data = Produc, random.method = "walhus",
         effect = "time", model = "random")
ercomp(z)
ercomp(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc,
       method = "amemiya", effect = "twoways")

## index
data("Grunfeld", package = "plm")
Gr <- pdata.frame(Grunfeld, index = c("firm", "year"))
m <- plm(inv ~ value + capital, data = Gr)
index(Gr, "firm")
index(Gr, "time")
index(Gr$inv, c(2, 1))
index(m, "id")

# with additional group index
data("Produc", package = "plm")
pProduc <- pdata.frame(Produc, index = c("state", "year", "region"))
index(pProduc, 3)
index(pProduc, "region")
index(pProduc, "group")

## is.pbalanced
Grunfeld_missing_period <- Grunfeld[-2, ]
is.pbalanced(Grunfeld_missing_period)     # check if balanced: FALSE
pdim(Grunfeld_missing_period)$balanced    # same
pGrunfeld_missing_period <- pdata.frame(Grunfeld_missing_period)
is.pbalanced(Grunfeld_missing_period)
is.pbalanced(pGrunfeld_missing_period$inv)

## is.pconsecutive
is.pconsecutive(Grunfeld)
is.pconsecutive(Grunfeld, index=c("firm", "year"))

# delete 2nd row (2nd time period for first individual)
# -> non consecutive 
Grunfeld_missing_period <- Grunfeld[-2, ]
is.pconsecutive(Grunfeld_missing_period)
all(is.pconsecutive(Grunfeld_missing_period)) # FALSE

# delete rows 1 and 2 (1st and 2nd time period for first individual)
# -> consecutive
Grunfeld_missing_period_other <- Grunfeld[-c(1,2), ]
is.pconsecutive(Grunfeld_missing_period_other) # all TRUE

# delete year 1937 (3rd period) for _all_ individuals
Grunfeld_wo_1937 <- Grunfeld[Grunfeld$year != 1937, ]
is.pconsecutive(Grunfeld_wo_1937) # all FALSE

# pdata.frame interface
pGrunfeld <- pdata.frame(Grunfeld)
pGrunfeld_missing_period <- pdata.frame(Grunfeld_missing_period)
is.pconsecutive(pGrunfeld) # all TRUE
is.pconsecutive(pGrunfeld_missing_period) # first FALSE, others TRUE

# panelmodel interface (first, estimate some models)
mod_pGrunfeld <- plm(inv ~ value + capital, data = Grunfeld)
mod_pGrunfeld_missing_period <- plm(inv ~ value + capital, data = Grunfeld_missing_period)

is.pconsecutive(mod_pGrunfeld)
is.pconsecutive(mod_pGrunfeld_missing_period)

nobs(mod_pGrunfeld) # 200
nobs(mod_pGrunfeld_missing_period) # 199

# pseries interface
pinv <- pGrunfeld$inv
pinv_missing_period <- pGrunfeld_missing_period$inv

is.pconsecutive(pinv)
is.pconsecutive(pinv_missing_period)

# default method for arbitrary vectors or NULL
inv <- Grunfeld$inv
inv_missing_period <- Grunfeld_missing_period$inv
is.pconsecutive(inv, id = Grunfeld$firm, time = Grunfeld$year)
is.pconsecutive(inv_missing_period, id = Grunfeld_missing_period$firm, 
                time = Grunfeld_missing_period$year)

# only id and time are needed for evaluation
is.pconsecutive(NULL, id = Grunfeld$firm, time = Grunfeld$year)

## is.pseries
Em <- pdata.frame(EmplUK)
z <- Em$output

class(z) # pseries as indicated by class
is.pseries(z) # and confirmed by check

# destroy index of pseries and re-check
attr(z, "index") <- NA
is.pseries(z) # now FALSE

## model.frame, model.matrix
pGrunfeld <- pdata.frame(Grunfeld)

# then make a model frame from a formula and a pdata.frame
form <- inv ~ value
mf <- model.frame(pGrunfeld, form)

# then construct the (transformed) model matrix (design matrix)
# from model frame
modmat <- model.matrix(mf, model = "within")

## retrieve model frame and model matrix from an estimated plm object
fe_model <- plm(form, data = pGrunfeld, model = "within")
model.frame(fe_model)
model.matrix(fe_model)

# same as constructed before
all.equal(mf, model.frame(fe_model), check.attributes = FALSE) # TRUE
all.equal(modmat, model.matrix(fe_model), check.attributes = FALSE) # TRUE


## pmodel.response

form <- inv ~ value + capital
mf <- model.frame(pGrunfeld, form)
# construct (transformed) response of the within model
resp <- pmodel.response(form, data = mf, model = "within", effect = "individual")
# retrieve (transformed) response directly from model frame
resp_mf <- pmodel.response(mf, model = "within", effect = "individual")

# retrieve (transformed) response from a plm object, i.e., an estimated model
fe_model <- plm(form, data = pGrunfeld, model = "within")
pmodel.response(fe_model)

# same as constructed before
all.equal(resp, pmodel.response(fe_model), check.attributes = FALSE) # TRUE



## nobs
z <- plm(log(gsp)~log(pcap)+log(pc)+log(emp)+unemp,data=Produc,
         model="random", subset = gsp > 5000)

nobs(z)       # total observations used in estimation
pdim(z)$nT$N  # same information
pdim(z)       # more information about the dimensions (no. of individuals and time periods)

# illustrate difference between nobs and pdim for first-difference model
data("Grunfeld", package = "plm")
fdmod <- plm(inv ~ value + capital, data = Grunfeld, model = "fd")
nobs(fdmod)      # 190
pdim(fdmod)$nT$N # 200

## pgmm
## Arellano and Bond (1991), table 4 col. b 
z1 <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
           + log(capital) + lag(log(output), 0:1) | lag(log(emp), 2:99),
           data = EmplUK, effect = "twoways", model = "twosteps")
summary(z1, robust = FALSE)

## Blundell and Bond (1998) table 4 (cf. DPD for OX p. 12 col. 4)
z2 <- pgmm(log(emp) ~ lag(log(emp), 1)+ lag(log(wage), 0:1) +
             lag(log(capital), 0:1) | lag(log(emp), 2:99) +
             lag(log(wage), 2:99) + lag(log(capital), 2:99),
           data = EmplUK, effect = "twoways", model = "onestep", 
           transformation = "ld")
summary(z2, robust = TRUE)

# Same with the old formula or dynformula interface
# Arellano and Bond (1991), table 4, col. b
z1 <- pgmm(log(emp) ~ log(wage) + log(capital) + log(output),
             lag.form = list(2,1,0,1), data = EmplUK,
             effect = "twoways", model = "twosteps",
             gmm.inst = ~log(emp), lag.gmm = list(c(2,99)))
summary(z1, robust = FALSE)

## Blundell and Bond (1998) table 4 (cf DPD for OX p. 12 col. 4)
z2 <- pgmm(dynformula(log(emp) ~ log(wage) + log(capital), list(1,1,1)),
             data = EmplUK, effect = "twoways", model = "onestep",
             gmm.inst = ~log(emp) + log(wage) + log(capital),
             lag.gmm = c(2,99), transformation = "ld")
summary(z2, robust = TRUE)

## pht (deprecated)
# deprecated way with pht() for HT
data("Wages", package = "plm")
ht <- pht(lwage ~ wks + south + smsa + married + exp + I(exp^2) +
         bluecol + ind + union + sex + black + ed |
         sex + black + bluecol + south + smsa + ind,
         data = Wages, model = "ht", index = 595)
summary(ht)

am <- pht(lwage ~ wks + south + smsa + married + exp + I(exp^2) +
         bluecol + ind + union + sex + black + ed |
         sex + black + bluecol + south + smsa + ind,
         data = Wages, model = "am", index = 595)
summary(am)

## pldv
pder.avail <- if (!requireNamespace("pder", quietly = TRUE)) FALSE else TRUE
if(pder.avail) {
data("Donors", package = "pder")
pDonors <- pdata.frame(Donors, index = "id")
modA <- pldv(donation ~ treatment +  prcontr, data = pDonors,
             model = "random", method = "bfgs")
summary(modA)
modB <- pldv(donation ~ treatment * prcontr - prcontr, data = pDonors,
           model = "random", method = "bfgs")
summary(modB)
invisible(NULL)
}

## pwartest
pwartest(log(emp) ~ log(wage) + log(capital), data = EmplUK)
pwartest(log(emp) ~ log(wage) + log(capital), data = EmplUK, type = "HC3")

## pwfdtest
pwfdtest(log(emp) ~ log(wage) + log(capital), data = EmplUK)
pwfdtest(log(emp) ~ log(wage) + log(capital), data = EmplUK, h0 = "fe")
pwfdtest(log(emp) ~ log(wage) + log(capital), data = EmplUK, type = "HC3", h0 = "fe")

mod <- plm(log(emp) ~ log(wage) + log(capital), data = EmplUK, model = "fd")
pwfdtest(mod)
pwfdtest(mod, h0 = "fe")
pwfdtest(mod, type = "HC3", h0 = "fe")

# pwtest
pwtest(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc)
pwtest(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc, effect = "time")

## panelmodel interface
# first, estimate a pooling model, than compute test statistics
form <- formula(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp)
pool_prodc <- plm(form, data = Produc, model = "pooling")
pwtest(pool_prodc) # == effect="individual"
pwtest(pool_prodc, effect="time")
