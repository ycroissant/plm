## Test if pdata.frame() drops the unused levels of factors

library(plm)



### test for dropping of unused levels ###
data("Grunfeld", package = "plm")
Grunfeld$fac_var <- factor(rep(c("a", "b"), 100))
levels(Grunfeld$fac_var) <- c("a", "b", "unused") # is unused level
pGrun_unused_level <- pdata.frame(Grunfeld, drop.unused.levels = FALSE)
pGrun_unused_level_default <- pdata.frame(Grunfeld) # test default behaviour
if (!isTRUE(all.equal(levels(pGrun_unused_level$fac_var), c("a", "b", "unused")))) stop("unused levels dropped but should be kept")
if (!isTRUE(all.equal(levels(pGrun_unused_level_default$fac_var), c("a", "b", "unused")))) stop("unused levels dropped but should be kept")

# index var with unused levels (rather: variable from which the index var is derived)
Grunfeld$firm <- factor(Grunfeld$firm)
levels(Grunfeld$firm) <- c(levels(Grunfeld$firm), "unused")
pGrunfeld_unused_level_index <- pdata.frame(Grunfeld, drop.unused.levels = FALSE)
if (!isTRUE(all.equal(levels(pGrunfeld_unused_level_index$firm),
                      as.character(1:10)))) stop("unused level not dropped from index variable but should by in any case")

# make sure the index variable in attribute is withoud unused levels
iind <- index(pGrunfeld_unused_level_index)[[1]]
if (!isTRUE(all.equal(levels(iind), levels(droplevels(iind))))) stop("unused level in index var (in attributes)!")


######## test of dropping unused level in factor (non index variable)
df <- data.frame(id = c(1,1,2), time = c(1,2,1),
                 f = factor(c("a", "a", "b")),
                 f2 = factor(c(1,2,3), levels = c(1,2,3,4)), # level 4 is unsed
                 n = c(1:3))
pdf <- pdata.frame(df, drop.unused.levels = TRUE)

levels(df$f2)
levels(pdf$f2)

if (!isTRUE(all.equal(levels(pdf$f2), c("1", "2", "3")))) stop("used levels in non-index factor not dropped")

### test unused level in index variable
dfindex <- data.frame(id = c(1,1,2), 
                      time = factor(c(1,2,1), levels = c(1,2,9)), # level 9 is unused
                      f = factor(c("a", "a", "b")),
                      f2 = factor(c(1,2,3), levels = c(1,2,3,4)), # level 4 is unsed
                      n = c(1:3))
pdfindex <- pdata.frame(dfindex, drop.unused.levels = FALSE)

levels(dfindex$time)
levels(pdfindex$time)
if (!isTRUE(all.equal(levels(pdfindex$time), c("1", "2")))) stop("used levels in index not dropped")
