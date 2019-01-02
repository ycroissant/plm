# Tests for functions:
#  * detect.lindep
#  * alias
# YC 2017/10/09 :  RE model par defaut pb because the between model is empty


library(plm)
data("Cigar", package = "plm")
Cigar[ , "fact1"] <- c(0,1)
Cigar[ , "fact2"] <- c(1,0)
Cigar.p <- pdata.frame(Cigar)

#pform <- pFormula(price ~ 0 + cpi + fact1 + fact2)
form <- price ~ 0 + cpi + fact1 + fact2
mf <- model.frame(Cigar.p, form)

# for the pooling model.matrix, there is no linear dependence
# (because the intercept is left out in this case in the formula)
#MM detect.lindep(model.matrix(pform, data = Cigar.p, model = "pooling"))

detect.lindep(model.matrix(mf, model = "pooling"))

## check if backward compatibility works
detect_lin_dep(model.matrix(mf, model = "pooling"))

# linear dependence occury after FE transformation
# [after transformation fact1 == -1 * fact2]
#MM detect.lindep(model.matrix(pform, data = Cigar.p, model = "within"))

detect.lindep(model.matrix(mf, model = "within"))
#MM mod_fe <- plm(pform, data = Cigar.p, model = "within")
mod_fe <- plm(form, data = Cigar.p, model = "within")
detect.lindep(mod_fe)

# test with NA matrix and empty matrix
detect.lindep(matrix(NA))                     # NA matrix
detect.lindep(matrix(NA, nrow = 0, ncol = 0)) # empty matrix

# linear dependent column(s) are silently dropped in plm estimation, thus this works
#mod_fe <- plm(pform, data = Cigar.p, model = "within")
mod_fe <- plm(form, data = Cigar.p, model = "within")
detect.lindep(model.matrix(mod_fe))

# tests with suppressed printing
detect.lindep(matrix(NA), suppressPrint = TRUE)                     # NA matrix
detect.lindep(matrix(NA, nrow = 0, ncol = 0), suppressPrint = TRUE) # empty matrix
#MM detect.lindep(model.matrix(pform, data = Cigar.p, model = "pooling"), suppressPrint = TRUE)
#MM detect.lindep(model.matrix(pform, data = Cigar.p, model = "within"), suppressPrint = TRUE)
detect.lindep(model.matrix(mf, model = "pooling"), suppressPrint = TRUE)
detect.lindep(model.matrix(mf, model = "within"), suppressPrint = TRUE)
detect.lindep(model.matrix(mod_fe), suppressPrint = TRUE)

# test for (p)data.frame interface
df <- as.data.frame(model.matrix(mod_fe))
detect.lindep(df)
detect.lindep(Cigar)
Cigar.p$price2 <- 2*Cigar.p$price
detect.lindep(Cigar.p)
detect.lindep(Cigar.p, suppressPrint = TRUE)



######## alias.plm, alias.pFormula ######
#MM lmmod1 <- lm(pform, data = Cigar.p)
lmmod1 <- lm(form, data = Cigar.p)

alias(lmmod1)

#MM plm_fe <- plm(pform, data = Cigar.p, model = "within")
plm_fe <- plm(form, data = Cigar.p, model = "within")
#YC plm_re <- plm(pform, data = Cigar.p, model = "random")                                  # The between model is very special, as there is no interept and cpi is constant
#MM plm_re_wal <- plm(pform, data = Cigar.p, model = "random", random.method = "walhus")
#MM plm_fd <- plm(pform, data = Cigar.p, model = "fd")
#MM plm_pool <- plm(pform, data = Cigar.p, model = "pooling")
plm_re_wal <- plm(form, data = Cigar.p, model = "random", random.method = "walhus")
plm_fd <- plm(form, data = Cigar.p, model = "fd")
plm_pool <- plm(form, data = Cigar.p, model = "pooling")

names(plm_fe$model)
summary(plm_fe)
alias(plm_fe)
#YC alias(plm_re)
alias(plm_re_wal)
alias(plm_fd)
alias(plm_pool)

# Test variation of parameters
# alias.lm(object, complete = TRUE, partial = FALSE, partial.pattern = FALSE, ...)
alias(plm_fe, complete = FALSE)
alias(plm_fe, partial = TRUE)
alias(plm_fe, partial.pattern = TRUE)

#MM alias(pform, Cigar.p, model = "within")
alias(mf, model = "within")
#YC alias(pform, Cigar.p, model = "random")
#MM alias(pform, Cigar.p, model = "random", random.method = "walhus")
alias(mf, model = "random", random.method = "walhus")
# alias(pform, Cigar.p, model = "within", inst.method = "bvk") # should give informative error

#MM alias(pform, Cigar.p, model = "fd")
#MM alias(pform, Cigar.p, model = "pooling")

alias(mf, model = "fd")
alias(mf, model = "pooling")

#MM alias(pform, Cigar.p, model = "within", complete = FALSE)
#MM alias(pform, Cigar.p, model = "within", partial = TRUE)
#MM alias(pform, Cigar.p, model = "within", partial.pattern = TRUE)

alias(mf, model = "within", complete = FALSE)
alias(mf, model = "within", partial = TRUE)
alias(mf, model = "within", partial.pattern = TRUE)


# Tests for names of plm_object$aliased
if (!isTRUE(all.equal(names(mod_fe$aliased), colnames(model.matrix(mod_fe)))))
  stop("Names not correct (not like colnames of model.matrix: ", paste0(names(mod_fe$aliased), collapse = ", "))
