# Tests for function detect_lin_dep()

library(plm)
data(Cigar)
Cigar[ , "fact1"] <- c(0,1)
Cigar[ , "fact2"] <- c(1,0)
Cigar.p <- pdata.frame(Cigar)

pform <- pFormula(price ~ 0 + cpi + fact1 + fact2)

# for the pooling model.matrix, there is no linear dependence
# (because the intercept is left out in this case in the formula)
detect_lin_dep(model.matrix(pform, data = Cigar.p, model = "pooling"))

# linear dependence occury after FE transformation
# [after transformation fact1 == -1 * fact2]
detect_lin_dep(model.matrix(pform, data = Cigar.p, model = "within"))


# test with NA matrix and empty matrix
detect_lin_dep(matrix(NA))                     # NA matrix
detect_lin_dep(matrix(NA, nrow = 0, ncol = 0)) # empty matrix

# linear dependent column(s) are silently dropped in plm estimation, thus this works
mod_fe <- plm(pform, data = Cigar.p, model = "within")
detect_lin_dep(model.matrix(mod_fe))

# tests with suppressed printing
detect_lin_dep(matrix(NA), suppressPrint = TRUE)                     # NA matrix
detect_lin_dep(matrix(NA, nrow = 0, ncol = 0), suppressPrint = TRUE) # empty matrix
detect_lin_dep(model.matrix(pform, data = Cigar.p, model = "pooling"), suppressPrint = TRUE)
detect_lin_dep(model.matrix(pform, data = Cigar.p, model = "within"), suppressPrint = TRUE)
detect_lin_dep(model.matrix(mod_fe), suppressPrint = TRUE)

# test for (p)data.frame interface
df <- as.data.frame(model.matrix(mod_fe))
detect_lin_dep(df)
detect_lin_dep(Cigar)
Cigar.p$price2 <- 2*Cigar.p$price
detect_lin_dep(Cigar.p)
detect_lin_dep(Cigar.p, suppressPrint = TRUE)
