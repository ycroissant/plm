# Testfile for plm::bgtest()
#
# (1) check if inputs are passed correctly for formula and panelmodel interface
# (2) compare results to lmtest::bgtest()


# Estimate some models first
library(plm)
data("Grunfeld", package = "plm")
g_re <- plm(inv ~ value + capital, data = Grunfeld, model = "random")
g_fe <- plm(inv ~ value + capital, data = Grunfeld, model = "within")
g_pool <- plm(inv ~ value + capital, data = Grunfeld, model = "pooling")
g_pool_lm <- lm(inv ~ value + capital, data = Grunfeld)
g_fe_lm <- lm(inv ~ factor(firm) + value + capital, data = Grunfeld)



# Results should all be the same for same model (statistics, df, p-value)

  # compare plm::bgtests panelmodel interface to formula interface
  plm::pbgtest(inv ~ value + capital, order=1, data=Grunfeld) # default is pooling model
  plm::pbgtest(inv ~ value + capital, order=1, data=Grunfeld, model="pooling")
  plm::pbgtest(g_pool, order=1)
  
  plm::pbgtest(inv ~ value + capital, order=1, model="within", data=Grunfeld)
  plm::pbgtest(g_fe, order=1)
  
  plm::pbgtest(inv ~ value + capital, order=1, model="random", data=Grunfeld)
  plm::pbgtest(g_re, order=1)




  # compare plm::pbgtest to lmtest::bgtest
  # Hint: for lm::bgtest(), if no order argument is supplied, order=1 is default,
  #      while plm::pbgtest() assues mininum number of obs over time (typically != 1)

  # panelmodel interface
  plm::pbgtest(g_pool, order = 1)
  lmtest::bgtest(g_pool)
  lmtest::bgtest(g_pool_lm)
  
  plm::pbgtest(g_pool, order = 1, type="F")
  lmtest::bgtest(g_pool,          type="F")
  lmtest::bgtest(g_pool_lm,       type="F")

  
    

  ## formula interface
  plm:::pbgtest( inv ~ value + capital, data = Grunfeld, order=1)
  lmtest::bgtest(inv ~ value + capital, data = Grunfeld, order=1)

  plm::pbgtest(  inv ~ value + capital, data = Grunfeld, order=1, type="F")
  lmtest::bgtest(inv ~ value + capital, data = Grunfeld, order=1, type="F")

# Use of order.by:
  
  # order.by as vector
  plm::pbgtest(g_pool,      order = 1, order.by=g_pool$model$capital)
  lmtest::bgtest(g_pool,    order = 1, order.by=g_pool$model$capital)
  lmtest::bgtest(g_pool_lm, order = 1, order.by=g_pool_lm$model$capital)
  
  plm::pbgtest(  inv ~ value + capital, data=Grunfeld, order = 1, order.by=g_pool$model$capital)
  lmtest::bgtest(inv ~ value + capital, data=Grunfeld, order = 1, order.by=g_pool$model$capital)
  lmtest::bgtest(inv ~ value + capital, data=Grunfeld, order = 1, order.by=g_pool_lm$model$capital)  
  
  
  
  plm::pbgtest(g_pool,      order = 1, order.by=g_pool$model$capital,    type="F")
  lmtest::bgtest(g_pool,    order = 1, order.by=g_pool$model$capital,    type="F")
  lmtest::bgtest(g_pool_lm, order = 1, order.by=g_pool_lm$model$capital, type="F")
  
  plm::pbgtest(  inv ~ value + capital, data=Grunfeld, order = 1, order.by=g_pool$model$capital,    type="F")
  lmtest::bgtest(inv ~ value + capital, data=Grunfeld, order = 1, order.by=g_pool$model$capital,    type="F")
  lmtest::bgtest(inv ~ value + capital, data=Grunfeld, order = 1, order.by=g_pool_lm$model$capital, type="F")
  
  
  
  
  # order.by as formula
  # when order.by is given as formula, also supply data= [requirement of lmtest::bgtest()]
  plm::pbgtest(g_pool,      order.by=~capital, order = 1, data=Grunfeld)
  lmtest::bgtest(g_pool,    order.by=~capital,            data=Grunfeld)
  lmtest::bgtest(g_pool_lm, order.by=~capital,            data=Grunfeld)
  
  plm::pbgtest(  inv ~ value + capital,    order.by=~capital, order = 1, data=Grunfeld, model="pooling")
  plm::pbgtest(  inv ~ value + capital,    order.by=~capital, order = 1, data=Grunfeld) # default is pooling model
  lmtest::bgtest(inv ~ value + capital,    order.by=~capital,            data=Grunfeld)
  lmtest::bgtest(inv ~ value + capital,    order.by=~capital,            data=Grunfeld)
  


  plm::pbgtest(g_pool,      order.by=~capital, order = 1, data=Grunfeld, type="F")
  lmtest::bgtest(g_pool,    order.by=~capital,            data=Grunfeld, type="F")
  lmtest::bgtest(g_pool_lm, order.by=~capital,            data=Grunfeld, type="F")

  plm::pbgtest(  inv ~ value + capital,     order.by=~capital, order = 1, data=Grunfeld, type="F", model="pooling")
  plm::pbgtest(  inv ~ value + capital,     order.by=~capital, order = 1, data=Grunfeld, type="F") # default is pooling model
  lmtest::bgtest(inv ~ value + capital,     order.by=~capital,            data=Grunfeld, type="F")
  lmtest::bgtest(inv ~ value + capital,     order.by=~capital,            data=Grunfeld, type="F")


  plm::pbgtest(inv ~ value + capital, order=1, model="within", data=Grunfeld, order.by=~capital)
  plm::pbgtest(g_fe,                  order=1,                 data=Grunfeld, order.by=~capital)
  plm::pbgtest(inv ~ value + capital, order=1, model="within", data=Grunfeld, order.by=~g_fe$model$capital)
  plm::pbgtest(g_fe,                  order=1,                 data=Grunfeld, order.by=~g_fe$model$capital)
  

  plm::pbgtest(inv ~ value + capital, order=1, model="random", data=Grunfeld, order.by=~capital)
  plm::pbgtest(g_re,                  order=1,                 data=Grunfeld, order.by=~capital)
  plm::pbgtest(inv ~ value + capital, order=1, model="random", data=Grunfeld, order.by=g_re$model$capital)
  plm::pbgtest(g_re,                  order=1,                 data=Grunfeld, order.by=g_re$model$capital)
  
  
  