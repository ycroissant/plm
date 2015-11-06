### Wooldridge's Tests
###
### pwfdtest()
### pwartest()

# attempt to replicate results in Drukker (2003)

# Drukker, David M. (2003), Testing for serial correlation in linear panel-data models,
#                           The Stata Journal (2003) 3, Number 2, pp. 168–177.
#          online: http://www.stata-journal.com/sjpdf.html?articlenum=st0039
library(plm)
library(haven)
nlswork_r8 <- read_dta("http://www.stata-press.com/data/r8/nlswork.dta")
pnlswork_r8 <- pdata.frame(nlswork_r8, index=c("idcode", "year"), drop.index=F)
pdim(pnlswork_r8)

pnlswork_r8$age2 <- (pnlswork_r8$age)^2
pnlswork_r8$tenure2 <- (pnlswork_r8$tenure)^2

form_nls <- formula(ln_wage ~ age + age2 + ttl_exp  + tenure + tenure2 + south)

fe_nls <- plm(form_nls, data = pnlswork_r8, model = "within")
fd_nls <- plm(form_nls, data = pnlswork_r8, model = "fd")

# results of regression differ, likely because the model matrices used differ (cf. number of obs)
# in STATA example : Number of obs: 10528 used in regression
summary(fd_nls)

# both result in error in plm v1.4-0 (CRAN) and dev ver as of 2015-11-05
pwfdtest(fd_nls)
pwfdtest(form_nls, data=pnlswork_r8)




data(Grunfeld, package = "plm")
Grunfeldpdata <- pdata.frame(Grunfeld, index = c("firm", "year"), drop.index = FALSE, row.names = TRUE)
form_grun <- formula(inv ~ value + capital)
fd_grun  <- plm(form_grun, data=Grunfeldpdata, model="fd")
fe_grun  <- plm(form_grun, data=Grunfeldpdata, model="within")

# pwfdtest() runs with Grunfeld data
pwfdtest(fd_grun) 
pwfdtest(form_grun, data=Grunfeldpdata)

# pwfdtest() has problem with this dataset
pwfdtest(fd_nls)
pwfdtest(form_nls, data=nlswork_r8)

pwfdtest(fe_nls)  # calling pwfdtest() on a FE model => problem in v1.4-0. Should this case be caught? Similar to calling pwartest on a FD model (see below)?

# pwartest()
pwartest(fe_grun) # no problem
pwartest(fe_nls)  # no problem
pwartest(fd_nls)  # adopt this error message (slightly modified) for pwfdtest(FE model)?

# make a short and unblanced version of Grunfeld data
g_short_unbalanced <- Grunfeld[1:20, ]
g_short_unbalanced[11:20, ]$firm <- 2L
g_short_unbalanced <- g_short_unbalanced[-3, ]
pg_short_unbalanced <- pdata.frame(g_short_unbalanced, index = c("firm", "year"), drop.index = FALSE, row.names = TRUE)
fd_grun_unbalanced  <- plm(form_grun, data=pg_short_unbalanced, model="fd")
fe_grun_unbalanced  <- plm(form_grun, data=pg_short_unbalanced, model="within")

pwfdtest(fd_grun) # no problem
pwfdtest(fd_grun_unbalanced) # no problem

pwfdtest(fe_grun) # calling pfdtest on a FE model => problem. Should this case be caught? Similar to calling pwartest on a FD model (see below)?
pwartest(fe_grun) # no problem
pwartest(fd_grun) # adopt this error message (slightly modified) for pwfdtest(fe_grun)?

