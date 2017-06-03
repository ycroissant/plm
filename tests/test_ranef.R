## test of ranef()
library(plm)
data("Cigar", package = "plm")

# note: the formulae used in estimation are different
obj_bal_ind  <- plm(sales ~ pop + price, data = Cigar, model = "random", effect = "individual") # gives positive estimate for theta
obj_bal_time <- plm(price ~ pop + sales, data = Cigar, model = "random", effect = "time") # gives positive estimate for theta
summary(obj_bal_ind)
summary(obj_bal_time)

ranef(obj_bal_ind)
ranef(obj_bal_time)

### unbalanced one-way tests

data("Grunfeld", package = "plm")
Grunfeld199 <- Grunfeld[1:199, ]

mod_unbal_id <- plm(inv ~ value + capital, data = Grunfeld199, model = "random", effect = "individual")
mod_unbal_time <- plm(inv ~ value + capital, data = Grunfeld199, model = "random", effect = "time")
ranef(mod_unbal_id)
ranef(mod_unbal_time)

Cigar_unbal <- Cigar[1:(nrow(Cigar)-1), ]

## same formula this time
obj_unbal_ind  <- plm(sales ~ pop, data = Cigar_unbal, model = "random", effect = "individual") # gives positive estimate for theta
obj_unbal_time <- plm(sales ~ pop, data = Cigar_unbal, model = "random", effect = "time") # gives positive estimate for theta
summary(obj_unbal_ind)
summary(obj_unbal_time)

ranef(obj_unbal_ind)
ranef(obj_unbal_time)

## two-way balanced
obj_bal_tw  <- plm(sales ~ pop + price, data = Cigar, model = "random", effect = "twoways") # gives positive estimate for theta
summary(obj_bal_tw)
ranef(obj_bal_tw)
ranef(obj_bal_tw, effect = "individual") # same as line before
ranef(obj_bal_tw, effect = "time")
