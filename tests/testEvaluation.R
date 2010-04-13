library(plm)
## testing whether parameters in variables work (bug by Ulrich Morawtz)
data("Produc", package="plm")
estimation_method <- "within"
estimation_effect <- "individual"
zz <- plm(log(gsp)~log(pcap)+log(pc)+log(emp)+unemp,
          data=Produc, 
          index=c("state","year") , method=estimation_method , 
          effect=estimation_effect
          )
print(summary(zz))
