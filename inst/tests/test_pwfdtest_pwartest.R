### Wooldridge's Tests
###
### pwfdtest()
### pwartest()

# attempt to replicate results in Drukker (2003)

# Drukker, David M. (2003), Testing for serial correlation in linear panel-data models,
#                           The Stata Journal (2003) 3, Number 2, pp. 168–177.
#          online: http://www.stata-journal.com/sjpdf.html?articlenum=st0039
# library(plm)
# library(haven)
# nlswork_r8 <- read_dta("http://www.stata-press.com/data/r8/nlswork.dta")
# 
# nlswork_r8$age2 <- (nlswork_r8$age)^2
# nlswork_r8$tenure2 <- (nlswork_r8$tenure)^2
# 
# 
# pnlswork_r8 <- pdata.frame(nlswork_r8, index=c("idcode", "year"), drop.index=F)
# pdim(pnlswork_r8)
# 
# pnlswork_r8$age2 <- (pnlswork_r8$age)^2
# pnlswork_r8$tenure2 <- (pnlswork_r8$tenure)^2
# 
# form_nls <- formula(ln_wage ~ age + age2 + ttl_exp  + tenure + tenure2 + south)
# 
# fe_nls <- plm(form_nls, data = pnlswork_r8, model = "within")
# fd_nls <- plm(form_nls, data = pnlswork_r8, model = "fd")
# 
# # results of regression differ, likely because the model matrices used differ (cf. number of obs)
# # in Stata example : Number of obs: 10528 used in regression
# summary(fd_nls)
# 
# pwfdtest(fd_nls)
# pwfdtest(form_nls, data = pnlswork_r8)
# 
# # calc. FD model via pre-diff'ed data:
# pnlswork_r8$d.ln_wage <- diff(pnlswork_r8$ln_wage)
# pnlswork_r8$d.age     <- diff(pnlswork_r8$age)
# pnlswork_r8$d.age2    <- diff(pnlswork_r8$age2)
# pnlswork_r8$d.ttl_exp <- diff(pnlswork_r8$ttl_exp)
# pnlswork_r8$d.tenure  <- diff(pnlswork_r8$tenure)
# pnlswork_r8$d.tenure2 <- diff(pnlswork_r8$tenure2)
# pnlswork_r8$d.south   <- diff(pnlswork_r8$south)
# 
# form_nls.d <- formula(d.ln_wage ~ 0 + d.age + d.age2 + d.ttl_exp  + d.tenure + d.tenure2 + d.south)
# fd_nls.d <- plm(form_nls.d, data = pnlswork_r8, model = "pooling")
# summary(fd_nls.d) # replicates Drukker (2003), p. 170
# 
# pwfdtest(form_nls.d, data = pnlswork_r8)
# 
# pwfdtest(fd_nls)
# pwfdtest(form_nls, data=nlswork_r8)
# pwfdtest(form_nls, data=pnlswork_r8)
#
# pwartest(fe_nls)
## pwartest(fd_nls) # errors rightfully and informatively
## pwfdtest(fe_nls) # errors rightfully and informatively

library("plm")
data("Grunfeld", package = "plm")
Grunfeldpdata <- pdata.frame(Grunfeld, index = c("firm", "year"), drop.index = FALSE, row.names = TRUE)
form_grun <- formula(inv ~ value + capital)
fd_grun  <- plm(form_grun, data=Grunfeldpdata, model="fd")
fe_grun  <- plm(form_grun, data=Grunfeldpdata, model="within")

# pwfdtest() runs with Grunfeld data
pwfdtest(fd_grun) 
pwfdtest(form_grun, data=Grunfeldpdata)

## pwfdtest(fe_grun) ## errors rightfully and informatively
pwartest(fe_grun) # valid
## pwartest(fd_grun) # errors rightfully and informatively


## make a short and unbalanced version of Grunfeld data
g_short_unbalanced <- Grunfeld[1:20, ]
g_short_unbalanced[11:20, ]$firm <- 2L
g_short_unbalanced <- g_short_unbalanced[-3, ]
pg_short_unbalanced <- pdata.frame(g_short_unbalanced, index = c("firm", "year"), drop.index = FALSE, row.names = TRUE)
fd_grun_unbalanced  <- plm(form_grun, data=pg_short_unbalanced, model="fd")
fe_grun_unbalanced  <- plm(form_grun, data=pg_short_unbalanced, model="within")

pwfdtest(fd_grun_unbalanced)

## pwfdtest(fe_grun_unbalanced) # calling pwfdtest() on a FE model errors rightfully
pwartest(fe_grun_unbalanced)

