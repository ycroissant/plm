### Wooldridge's Tests
###
### pwfdtest()
### pwartest()

library(plm)



data("Grunfeld", package = "plm")
Grunfeldpdata <- pdata.frame(Grunfeld, index = c("firm", "year"), drop.index = FALSE, row.names = TRUE)
form_grun <- formula(inv ~ value + capital)
fd_grun  <- plm(form_grun, data=Grunfeldpdata, model="fd")
fe_grun  <- plm(form_grun, data=Grunfeldpdata, model="within")

# pwfdtest() runs with Grunfeld data
pwfdtest(fd_grun) 
pwfdtest(form_grun, data=Grunfeldpdata)


# make a short and unblanced version of Grunfeld data
g_short_unbalanced <- Grunfeld[1:20, ]
g_short_unbalanced[11:20, ]$firm <- 2L
g_short_unbalanced <- g_short_unbalanced[-3, ]
pg_short_unbalanced <- pdata.frame(g_short_unbalanced, index = c("firm", "year"), drop.index = FALSE, row.names = TRUE)
fd_grun_unbalanced  <- plm(form_grun, data=pg_short_unbalanced, model="fd")
fe_grun_unbalanced  <- plm(form_grun, data=pg_short_unbalanced, model="within")

pwfdtest(fd_grun) # ok
pwfdtest(fd_grun_unbalanced) # ok

# ## These two are puposed to error with informative error message
# # pwfdtest(fe_grun)
# # pwartest(fd_grun)
pwartest(fe_grun) # ok


## pwfdtest errored for data where an individual had only 1 observation in original data
data("EmplUK", package = "plm")
pData <- pdata.frame(EmplUK)
pData <- pData[-c(1:6), ] # individual 1 has 1 observation left

# errored, fixed in rev. 831
pwfdtest(log(emp) ~ log(wage) + log(capital), data = pData)

# test with individual 1 which has 2 missing observations
pData2 <- pdata.frame(EmplUK[-c(3:4), ]) 
pwfdtest(log(emp) ~ log(wage) + log(capital), data = pData2)


# attempt to replicate results in Drukker (2003)

# Drukker, David M. (2003), Testing for serial correlation in linear panel-data models,
#                           The Stata Journal (2003) 3, Number 2, pp. 168–177.
#          online: http://www.stata-journal.com/sjpdf.html?articlenum=st0039
#
# library(haven)
# nlswork_r8 <- read_dta("http://www.stata-press.com/data/r8/nlswork.dta")
# pnlswork_r8 <- pdata.frame(nlswork_r8, index=c("idcode", "year"), drop.index=F)
# pdim <- pdim(pnlswork_r8)
# pdim$Tint$Ti
# 
# 
# pnlswork_r8$age2 <- (pnlswork_r8$age)^2
# pnlswork_r8$tenure2 <- (pnlswork_r8$tenure)^2
# 
# form_nls <- formula(ln_wage ~ age + age2 + ttl_exp  + tenure + tenure2 + south)
# 
# fe_nls <- plm(form_nls, data = pnlswork_r8, model = "within")
# fd_nls <- plm(form_nls, data = pnlswork_r8, model = "fd")

# # results of FD regression differ, likely because the model matrices used differ (cf. number of obs)
# # in Stata example : Number of obs: 10528 used in regression
# # plm's FD model does not perform differencing based on time but on rows
# summary(fd_nls)
# 
# # these resulted in an error, fixed in rev. 831
# pwfdtest(fd_nls)
# pwfdtest(form_nls, data=pnlswork_r8)

# # pwartest()
# pwartest(fe_grun) # no problem
# pwartest(fe_nls)  # no problem
# pwartest(fd_nls)  # 


