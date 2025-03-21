library(plm)
data("Grunfeld", package = "plm")

pgrangertest(inv ~ value, data = Grunfeld)
pgrangertest(inv ~ value, data = Grunfeld, order = 2L)
pgrangertest(inv ~ value, data = Grunfeld, order = 2L, test = "Zbar")


# unbalanced
unbal <- pgrangertest(inv ~ value, data = Grunfeld[1:199, ], order = 2L)
unbal$indgranger

# varying lag order
bal_varorder <- pgrangertest(inv ~ value, data = Grunfeld, order = c(rep(2L, 9), 3L))
bal_varorder$indgranger
unbal_varorder <- pgrangertest(inv ~ value, data = Grunfeld[1:199, ], order = c(rep(2L, 9), 3L))
unbal_varorder$indgranger


## Demo data from Dumitrescu/Hurlin (2012) supplement:
## http://www.runmycode.org/companion/view/42
## The data are in the following format: 20 x 20
## First 20 columns are the x series for the 10 individual
## next 20 columns are the y series for the 10 individuals
## -> need to convert to 'long' format first

# demodat <- readxl::read_excel("data/Granger_Data_demo_long.xls")
# demodat <- data.frame(demodat)
# pdemodat <- pdata.frame(demodat)

# pgrangertest(y ~ x, data = pdemodat, order = 1L)
# pgrangertest(y ~ x, data = pdemodat, order = 1L, test = "Zbar")
# 
# pgrangertest(y ~ x, data = pdemodat, order = 2L)
# pgrangertest(y ~ x, data = pdemodat, order = 2L, test = "Zbar")