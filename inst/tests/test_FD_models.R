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

### vcovXX on FD models
data("Grunfeld", package = "plm")
pGrun <- pdata.frame(Grunfeld)
# one group with only one observation > gets first-differenced away
pGrun1 <- pGrun[-c(61:200), ]
pGrun1 <- pGrun1[-c(2:20), ]
pdim(pGrun1)
pdim(pGrun1)$Tint
mod <- plm(inv ~ value + capital, data = pGrun1, model="fd")
vcovHC(mod)
vcovBK(mod)
diff(pGrun1$inv)
 
## pggls run test
mod.pggls <- pggls(inv ~ value + capital, data = pGrun1, model="fd")


# one time period with only one observation
pGrun2 <- pGrun[-c(21, 41, 61, 81, 101, 121, 141, 161, 181), ]
pdim(pGrun2)
pdim(pGrun2)$Tint
mod2 <- plm(inv ~ value + capital, data = pGrun2, model="fd")
vcovHC(mod2)
vcovBK(mod2) # vcovBK on this model fixed January 2025
 
mod2.pggls <- pggls(inv ~ value + capital, data = pGrun2, model="fd")



# data with one time period per group -> first-differenced away -> empty model
#pGrun3 <- pGrun[c(1, 21, 41, 61, 81, 101, 121, 141, 161, 181), ]
#mod3 <- plm(inv ~ value + capital, data = pGrun3, model="fd")