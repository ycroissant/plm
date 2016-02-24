# library(foreign);library(plm)
# jtrain <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/jtrain.dta")
# 
# # Define panel data (for 1987 and 1988 only)
# jtrain.87.88 <- subset(jtrain,year<=1988)
# jtrain.p<-pdata.frame(jtrain.87.88, index=c("fcode","year"))
# 
# # no variation and all NAs
# pvar(jtrain.p)
# 
# # only all NAs
# pvar(jtrain.p[ , c(20:26)])
