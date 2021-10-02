# TODO: add all cases: all model objects with all combinations of available model and effect values
#                      request all combinations from all model objects

library(plm)
data("Grunfeld", package = "plm")

fe2_bal   <- plm(inv ~ value + capital, model = "within", effect = "twoways", data = Grunfeld)
fe2_unbal <- plm(inv ~ value + capital, model = "within", effect = "twoways", data = Grunfeld[1:199, ])

model.matrix(fe2_bal,    model = "pooling", effect = "individual")
model.matrix(fe2_bal,    model = "pooling", effect = "time")
model.matrix(fe2_bal,    model = "pooling", effect = "twoways")

model.matrix(fe2_unbal,    model = "pooling", effect = "individual")
model.matrix(fe2_unbal,    model = "pooling", effect = "time")
model.matrix(fe2_unbal,    model = "pooling", effect = "twoways")

# this resulted pre rev. 298 in an error due to the effect argument saved in
# plm_object$args$effect = "twoways" which gets interpreted by model.matrix.pFormula
# and "applied" to model = "pooling"
model.matrix(fe2_unbal,    model = "pooling")
pmodel.response(fe2_unbal, model = "pooling")



# these still result in errors

#### "mean"
# in model.matrix.pFormula: add something for "mean" cases two-ways balanced/unbalanced?
# meaningful?
# model.matrix(fe2_unbal, model = "mean")
# model.matrix(fe2_bal,   model = "mean")

#### "random"
# model.matrix(fe2_unbal, model = "random")