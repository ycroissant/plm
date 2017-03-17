# Tests for functions:
#  * detect_lin_dep
#  * alias

library(plm)
data("Cigar", package = "plm")
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

mod_fe <- plm(pform, data = Cigar.p, model = "within")
detect_lin_dep(mod_fe)

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




######## alias.plm, alias.pFormula ######
lmmod1 <- lm(pform, data = Cigar.p)
alias(lmmod1)

plm_fe <- plm(pform, data = Cigar.p, model = "within")
plm_re <- plm(pform, data = Cigar.p, model = "random")
plm_re_wal <- plm(pform, data = Cigar.p, model = "random", random.method = "walhus")
plm_fd <- plm(pform, data = Cigar.p, model = "fd")
plm_pool <- plm(pform, data = Cigar.p, model = "pooling")

names(plm_fe$model)
summary(plm_fe)
alias(plm_fe)
alias(plm_re)
alias(plm_re_wal)
alias(plm_fd)
alias(plm_pool)

# Test variation of parameters
# alias.lm(object, complete = TRUE, partial = FALSE, partial.pattern = FALSE, ...)
alias(plm_fe, complete = FALSE)
alias(plm_fe, partial = TRUE)
alias(plm_fe, partial.pattern = TRUE)

alias(pform, Cigar.p, model = "within")
alias(pform, Cigar.p, model = "random")
alias(pform, Cigar.p, model = "random", random.method = "walhus")
# alias(pform, Cigar.p, model = "within", inst.method = "bvk") # should give informative error

alias(pform, Cigar.p, model = "fd")
alias(pform, Cigar.p, model = "pooling")

alias(pform, Cigar.p, model = "within", complete = FALSE)
alias(pform, Cigar.p, model = "within", partial = TRUE)
alias(pform, Cigar.p, model = "within", partial.pattern = TRUE)


# Tests for names of plm_object$aliased
if (!isTRUE(all.equal(names(mod_fe$aliased), colnames(model.matrix(mod_fe)))))
  stop("Names not correct (not like colnames of model.matrix: ", paste0(names(mod_fe$aliased), collapse = ", "))
