## test of ranef()
library(plm)
data("Cigar", package = "plm")

# note: the formulae used in estimation are different
obj_bal_ind  <- plm(sales ~ pop + price, data = Cigar, model = "random", effect = "individual") # gives positive estimate for theta
obj_bal_time <- plm(price ~ pop + sales, data = Cigar, model = "random", effect = "time") # gives positive estimate for theta
summary(obj_bal_ind)
summary(obj_bal_time)

plm:::ranef.plm(obj_bal_ind)
plm:::ranef.plm(obj_bal_time)
