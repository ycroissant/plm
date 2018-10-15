## do not run as additional package is needed

# library(plm)
# library(wooldridge)
# data("jtrain")
# pjtrain <- pdata.frame(jtrain, index = c("fcode", "year"))
# 
# ## no examples in Wooldridge (2002/2010), Ch. 10 for the RE GLS, FE GLS, FD GLS models
# pggls(lscrap ~ d88 + d89 + union + grant + grant_1, data = pjtrain, model = "random")
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