library(plm)
data("Produc", package = "plm")

pProduc <- pdata.frame(Produc)
pProduc_unbal <- pdata.frame(Produc[-c(2, 5, 10, 50:60), ])

## balanced
zz_default <- pggls(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
               data = pProduc)

summary(zz_default) # is within, check if correctly identified in print output

zz_wi <- pggls(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
               data = pProduc, model = "within")
summary(zz_wi)

zz_pool <- pggls(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
                 data = pProduc, model = "pooling")
summary(zz_pool)

zz_fd <- pggls(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
               data = pProduc, model = "fd")
summary(zz_fd)

zz_wi_t <- pggls(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
               data = pProduc, model = "within", effect = "time")
summary(zz_wi_t)

zz_pool_t <- pggls(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
                 data = pProduc, model = "pooling", effect = "time")
summary(zz_pool_t)

## effect = "time" for FD model not supported as senseless (individ. dimension
## does not have a natural order)

## unbalanced
zz_default_unbal <- pggls(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
                    data = pProduc_unbal)

summary(zz_default_unbal) # is within, check if correctly identified in print output

zz_wi_unbal <- pggls(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
               data = pProduc_unbal, model = "within")
summary(zz_wi_unbal)

zz_pool_unbal <- pggls(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
                 data = pProduc_unbal, model = "pooling")
summary(zz_pool_unbal)

zz_fd_unbal <- pggls(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
                data = pProduc_unbal, model = "fd")
summary(zz_fd_unbal)

zz_wi_t_unbal <- pggls(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
                 data = pProduc_unbal, model = "within", effect = "time")
summary(zz_wi_t_unbal)

zz_pool_t_unbal <- pggls(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
                   data = pProduc_unbal, model = "pooling", effect = "time")
summary(zz_pool_t_unbal)

## effect = "time" for FD model not supported as senseless (individ. dimension
## does not have a natural order)

## do not run as additional package is needed
# library(wooldridge)
# data("jtrain", package = "wooldridge")
# pjtrain <- pdata.frame(jtrain, index = c("fcode", "year"))
# 
# ## no examples in Wooldridge (2002/2010), Ch. 10 for the RE GLS, FE GLS, FD GLS models
# pggls(lscrap ~ d88 + d89 + union + grant + grant_1, data = pjtrain, model = "pooling")
# pggls(lscrap ~ d88 + d89 + union + grant + grant_1, data = pjtrain, model = "within")
# pggls(lscrap ~ d88 + d89 + union + grant + grant_1, data = pjtrain, model = "fd") # errored in 1.6-5, run in 1.6-6 (but gives nointercept), with intercept in 1.7-0 (rev. 746)
#
# 
# summary(plm(lscrap ~ d88 + d89 + union + grant + grant_1, data = pjtrain, model = "random")) # W (2010), p. 297
# summary(plm(lscrap ~ d88 + d89 + union + grant + grant_1, data = pjtrain, model = "within")) # W (2010), p. 307
# summary(plm(lscrap ~       d89 + union + grant + grant_1, data = pjtrain, model = "fd"))     # W (2010), p. 319

## Results in Wooldridge, Ch. 10 for plm random, p. 297
## 0.415   intercept
## -0.093  d88
## -0.270  d89
## +0.548  union
## -0.215  grant
## -0.377  grant_t-1