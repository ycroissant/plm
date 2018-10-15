## Test of various models involving first-differences

library(plm)
data("Produc", package = "plm")


# plm with intercept
fd_plm <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc, model = "fd")
# plm without intercept
fd_plm2 <- plm(log(gsp) ~ 0 + log(pcap) + log(pc) + log(emp) + unemp, data = Produc, model = "fd")

# pggls with intercept
fd_pggls <- pggls(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc, model = "fd")
  
# pggls without intercept
fd_pggls2 <- pggls(log(gsp) ~ 0 + log(pcap) + log(pc) + log(emp) + unemp, data = Produc, model = "fd")


summary(fd_plm)
summary(fd_plm2)
summary(fd_pggls)
summary(fd_pggls2)
vcovHC(fd_plm)
vcovHC(fd_plm2)
## vcovHC does not run pggls models
# vcovHC(fd_pggls)
# vcovHC(fd_pggls2)
