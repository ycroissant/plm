library(plm)
data("Grunfeld", package = "plm")

pgrangertest(inv ~ value, data = Grunfeld)
pgrangertest(inv ~ value, data = Grunfeld, order = 2L)
pgrangertest(inv ~ value, data = Grunfeld, order = 2L, test = "Zbar")


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