### Test of summary.plm with user specified vcov (arg '.vcov' as matrix and as function)

options(scipen = 999)

library(plm)
data("Grunfeld", package = "plm")
gi <- plm(inv ~ value + capital, data = Grunfeld, model = "within")

sum_obj      <- summary(gi)
sum_obj_mat  <- summary(gi, .vcov = vcovHC(gi))            # arg '.vcov' is matrix
mat <- vcovHC(gi)
sum_obj_mat2 <- summary(gi, .vcov = mat)                   # arg '.vcov' is matrix
sum_obj_fun  <- summary(gi, .vcov = vcovHC)                # arg '.vcov' is function
sum_obj_fun2 <- summary(gi, .vcov = function(x) vcovHC(x, method="white2")) # arg '.vcov' is function with args

if (identical(sum_obj,      sum_obj_mat)) stop("summary.plm objects within and without robust vcov must be non-identical")
if (!identical(sum_obj_mat$coefficients, sum_obj_fun$coefficients))
  stop("summary.plm object's coefs, SE, t stat and p-values for .vcov = matrix and for .vcov = function not identical")
# Note: objects with vcov = matrix and vcov = function are non-identical, because attr(object$rvcov, "name") differs

# Visualise output with various rvcov names
print(sum_obj)      # without furnished vcov
print(sum_obj_mat)  # .vcov = matrix prints short info that a furnished vcov is used
print(sum_obj_mat2) # .vcov = matrix prints short info that a furnished vcov is used
print(sum_obj_fun)  # .vcov = function prints additional info about the vcov function used
print(sum_obj_fun2) # .vcov = function with args prints additional info about the vcov function used

