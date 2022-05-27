## Test of various models involving first-differences

library(plm)
data("Produc", package = "plm")
pProduc <- pdata.frame(Produc)
pProduc_unbal <- pdata.frame(Produc[-c(2, 5, 10, 50:60), ])

# plm with intercept
fd_plm       <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, model = "fd", data = pProduc)
fd_plm_unbal <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, model = "fd", data = pProduc_unbal)

# plm without intercept
fd_plm2       <- plm(log(gsp) ~ 0 + log(pcap) + log(pc) + log(emp) + unemp, model = "fd", data = pProduc)
fd_plm2_unbal <- plm(log(gsp) ~ 0 + log(pcap) + log(pc) + log(emp) + unemp, model = "fd", data = pProduc_unbal)

# pggls with intercept
fd_pggls       <- pggls(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, model = "fd", data = pProduc)
fd_pggls_unbal <- pggls(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, model = "fd", data = pProduc_unbal)

# pggls without intercept
fd_pggls2       <- pggls(log(gsp) ~ 0 + log(pcap) + log(pc) + log(emp) + unemp, model = "fd", data = pProduc)
fd_pggls2_unbal <- pggls(log(gsp) ~ 0 + log(pcap) + log(pc) + log(emp) + unemp, model = "fd", data = pProduc_unbal)


summary(fd_plm)
summary(fd_plm_unbal)
summary(fd_plm2)
summary(fd_plm2_unbal)
summary(fd_pggls)
summary(fd_pggls_unbal)
summary(fd_pggls2)
summary(fd_pggls2_unbal)
vcovHC(fd_plm)
vcovHC(fd_plm2)

## vcovHC does not run on pggls models/not implemented
# vcovHC(fd_pggls)
# vcovHC(fd_pggls2)
