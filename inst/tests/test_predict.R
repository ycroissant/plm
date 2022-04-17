library(plm)
data("Grunfeld", package = "plm")

Grunfeld.p <- pdata.frame(Grunfeld)

fit.plm.fe.id       <- plm(inv ~ value + capital, data = Grunfeld, model = "within")
fit.plm.fe.id.unbal <- plm(inv ~ value + capital, data = Grunfeld[-c(198:200), ], model = "within")

## ensure predict with newdata = original data gives same result
(pred_orig1 <- predict(fit.plm.fe.id))
(pred_orig2 <- predict(fit.plm.fe.id, newdata = Grunfeld.p))
stopifnot(isTRUE(all.equal(pred_orig1, pred_orig2, check.attributes = FALSE)))


# generate 40 new observations of two firms used for prediction:
# firm 1 with years 1935:1964 and firm 2 with years 1935:1944
set.seed(42L)
new.value   <- runif(40, min = min(Grunfeld$value),   max = max(Grunfeld$value))
new.capital <- runif(40, min = min(Grunfeld$capital), max = max(Grunfeld$capital))

newdata <- data.frame(firm = c(rep(1, 30), rep(2, 10)),
                      year = c(1935:(1935+29), 1935:(1935+9)),
                      value = new.value, capital = new.capital)

newdata.p <- pdata.frame(newdata, index = c("firm", "year"))

(pred.plm.fe.id         <- predict(fit.plm.fe.id, newdata = newdata.p))
(pred.plm.fe.id.na.fill <- predict(fit.plm.fe.id, newdata = newdata.p, na.fill = TRUE))
(pred.plm.fe.id.df      <- predict(fit.plm.fe.id, newdata = newdata))


## one-way individual out-of-sample prediction
newdata.oos <- data.frame(firm = c(rep(1, 30), rep(11, 10)),
                          year = c(1935:(1935+29), 1935:(1935+9)),
                          value = new.value, capital = new.capital)

newdata.oos.p <- pdata.frame(newdata.oos, index = c("firm", "year"))

(pred.plm.fe.id.oos <- predict(fit.plm.fe.id, newdata = newdata.oos.p))
(pred.plm.fe.id.oos <- predict(fit.plm.fe.id, newdata = newdata.oos.p, na.fill = TRUE))

(pred.plm.fe.id.df.oos <- predict(fit.plm.fe.id, newdata = newdata.oos))



## one-way individual out-of-sample prediction - more complex pattern
new.value2   <- runif(55, min = min(Grunfeld$value),   max = max(Grunfeld$value))
new.capital2 <- runif(55, min = min(Grunfeld$capital), max = max(Grunfeld$capital))

newdata.oos2 <- data.frame(firm = c(rep(1, 30), rep(11, 10), rep(2, 15)),
                           year = c(1935:(1935+29), 1935:(1935+9), 1935:(1935+14)),
                           value = new.value2, capital = new.capital2)

newdata.oos2.p <- pdata.frame(newdata.oos2, index = c("firm", "year"))

(pred.plm.fe.id.oos2         <- predict(fit.plm.fe.id, newdata = newdata.oos2.p)) # sorted by ind. due to pdata.frame sorting (stacked)
(pred.plm.fe.id.oos2.na.fill <- predict(fit.plm.fe.id, newdata = newdata.oos2.p, na.fill = TRUE))

(pred.plm.fe.id.df.oos2 <- predict(fit.plm.fe.id, newdata = newdata.oos2))

(pred.plm.fe.id.unbal.oos2         <- predict(fit.plm.fe.id.unbal, newdata = newdata.oos2.p)) # sorted by ind. due to pdata.frame sorting (stacked)
(pred.plm.fe.id.unbal.oos2.na.fill <- predict(fit.plm.fe.id.unbal, newdata = newdata.oos2.p, na.fill = TRUE))

(pred.plm.fe.id.unbal.df.oos2 <- predict(fit.plm.fe.id, newdata = newdata.oos2))


# one-way - time
fit.plm.fe.ti       <- plm(inv ~ value + capital, data = Grunfeld, model = "within", effect = "time")
fit.plm.fe.ti.unbal <- plm(inv ~ value + capital, data = Grunfeld[-c(198:200), ], model = "within", effect = "time")

firsttwoyears <- c(1, 2, 21, 22, 41, 42, 61, 62, 81, 82,  101, 102, 121, 122, 141, 142, 161, 162, 181, 182)
newdata.ti <- data.frame(value = Grunfeld$value, capital = Grunfeld$capital, firm = Grunfeld$firm, year = c(rep(c(1935, 1936), 100)))[firsttwoyears, ]
newdata.ti.p <- pdata.frame(newdata.ti, index = c("firm", "year"))

(pred.plm.fe.ti    <- predict(fit.plm.fe.ti,   newdata = newdata.ti.p))
(pred.plm.fe.ti.df <- predict(fit.plm.fe.ti,   newdata = newdata.ti))

(pred.plm.fe.ti.unbal    <- predict(fit.plm.fe.ti.unbal,   newdata = newdata.ti.p))
(pred.plm.fe.ti.unbal.df <- predict(fit.plm.fe.ti.unbal,   newdata = newdata.ti))

## two-ways
fit.plm.fe.tw       <- plm(inv ~ value + capital, data = Grunfeld, model = "within", effect = "twoways")
fit.plm.fe.tw.unbal <- plm(inv ~ value + capital, data = Grunfeld[-c(198:200), ], model = "within", effect = "twoways")

newdata.tw <- data.frame(value = Grunfeld$value, capital = Grunfeld$capital, firm = Grunfeld$firm, year = c(rep(c(1935, 1936), 100)))[firsttwoyears, ]
newdata.tw.p <- pdata.frame(newdata.tw, index = c("firm", "year"))

(pred.plm.fe.tw    <- predict(fit.plm.fe.tw,   newdata = newdata.tw.p))
(pred.plm.fe.tw.df <- predict(fit.plm.fe.tw,   newdata = Grunfeld))

(pred.plm.fe.tw.unbal    <- predict(fit.plm.fe.tw.unbal,   newdata = newdata.tw.p))
(pred.plm.fe.tw.unbal.df <- predict(fit.plm.fe.tw.unbal,   newdata = Grunfeld))


# two-ways - out-of-sample prediction
(pred.plm.fe.tw.oos2         <- predict(fit.plm.fe.tw,   newdata = newdata.oos2.p))
(pred.plm.fe.tw.oos2.na.fill <- predict(fit.plm.fe.tw,   newdata = newdata.oos2.p, na.fill = TRUE))

(pred.plm.fe.tw.oos2.df <- predict(fit.plm.fe.tw,   newdata = newdata.oos2))

(pred.plm.fe.tw.unbal.oos2         <- predict(fit.plm.fe.tw.unbal,   newdata = newdata.oos2.p))
(pred.plm.fe.tw.unbal.oos2.na.fill <- predict(fit.plm.fe.tw.unbal,   newdata = newdata.oos2.p, na.fill = TRUE))

(pred.plm.fe.tw.unbal.oos2.df <- predict(fit.plm.fe.tw.unbal,   newdata = newdata.oos2))


### test versus  fixest::predict (fixest::predict does not do out-of-sample)
fixest.avail <- if(!requireNamespace("fixest", quietly = TRUE)) FALSE else TRUE

if(fixest.avail) {
  suppressPackageStartupMessages(library(fixest))
  
  # one-way individual
  fit.feols.fe.id <- feols(inv ~ value + capital | firm, data = Grunfeld)
  pred.feols.fe.id <- predict(fit.feols.fe.id, newdata = newdata)
  stopifnot(isTRUE(all.equal(pred.feols.fe.id, as.numeric(pred.plm.fe.id), check.attributes = FALSE)))
  
  # one-way individual - out-of-sample prediction
  pred.feols.fe.id.oos2 <- predict(fit.feols.fe.id, newdata = newdata.oos2.p)
  stopifnot(isTRUE(all.equal(pred.feols.fe.id.oos2, as.numeric(pred.plm.fe.id.oos2), check.attributes = FALSE)))

  # one-way individual unbalanced - out-of-sample prediction
  fit.feols.fe.id.unbal <- feols(inv ~ value + capital | firm, data = Grunfeld[-c(198:200), ])
  pred.feols.fe.id.unbal.oos2 <- predict(fit.feols.fe.id.unbal, newdata = newdata.oos2.p)
  stopifnot(isTRUE(all.equal(pred.feols.fe.id.unbal.oos2, as.numeric(pred.plm.fe.id.unbal.oos2), check.attributes = FALSE)))
  
  # one-way time
  fit.feols.fe.ti <- feols(inv ~ value + capital | year, Grunfeld)
  pred.feols.fe.ti <- predict(fit.feols.fe.ti, newdata = newdata.ti)
  stopifnot(isTRUE(all.equal(pred.feols.fe.ti, as.numeric(pred.plm.fe.ti), check.attributes = FALSE)))
  
  # one-way time unbalanced
  fit.feols.fe.ti.unbal <- feols(inv ~ value + capital | year, data = Grunfeld[-c(198:200), ])
  pred.feols.fe.ti.unbal <- predict(fit.feols.fe.ti.unbal, newdata = newdata.ti.p)
  stopifnot(isTRUE(all.equal(pred.feols.fe.ti.unbal, as.numeric(pred.plm.fe.ti.unbal), check.attributes = FALSE)))
  
  # twoways
  fit.feols.fe.tw <- feols(inv ~ value + capital | firm + year, Grunfeld)
  pred.feols.fe.tw <- predict(fit.feols.fe.tw, newdata = newdata.tw)
  stopifnot(isTRUE(all.equal(pred.feols.fe.tw, as.numeric(pred.plm.fe.tw), check.attributes = FALSE)))
  
  # twoways - out-of-sample prediction
  pred.feols.fe.tw.oos2 <- predict(fit.feols.fe.tw, newdata = newdata.oos2.p)
  stopifnot(isTRUE(all.equal(pred.feols.fe.tw.oos2, as.numeric(pred.plm.fe.tw.oos2), check.attributes = FALSE)))
  
  # twoways unbalanced - out-of-sample prediction
  fit.feols.fe.tw.unbal <- feols(inv ~ value + capital | firm + year, Grunfeld[-c(198:200), ])
  pred.feols.fe.tw.unbal.oos2 <- predict(fit.feols.fe.tw.unbal, newdata = newdata.oos2.p)
  stopifnot(isTRUE(all.equal(pred.feols.fe.tw.unbal.oos2, as.numeric(pred.plm.fe.tw.unbal.oos2), check.attributes = FALSE)))
}


#### corner cases

# NA in newdata
datNA <- data.frame(id = c(1,1,2,2,3), time = c(1,2,1,2,1), 
                   y = c(1,3,5,10,8), x = c(1, NA, 3,4,5))
datNA.p <- pdata.frame(datNA)

pool <- plm(y ~ x, data = datNA.p, index = c("id", "time"), model="pooling")

(predNA <- predict(pool, datNA.p))
(index(predNA)) # index may have only 4 rows left as one row is removed due to NA in data
stopifnot(nrow(index(predNA)) == 4L)

(predict(pool, datNA))

# FE model without intercept in formula
fe0 <- plm(y ~ 0 + x, data = datNA.p, index = c("id", "time"), model="pooling")
(pred.fe0.NA <- predict(fe0, datNA.p))


#### random effects
fit.plm.re.id <- plm(inv ~ value + capital, data = Grunfeld, model = "random")

(predict(fit.plm.re.id))

(pred.plm.re    <- predict(fit.plm.re.id, newdata = newdata.p))
(pred.plm.re.df <- predict(fit.plm.re.id, newdata = newdata))
