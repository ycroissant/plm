## Test if pdata.frame() drops the unused levels of factor

library(plm)

### test unused level in factor (non index variable)
df <- data.frame(id = c(1,1,2), time = c(1,2,1),
                 f = factor(c("a", "a", "b")),
                 f2 = factor(c(1,2,3), levels = c(1,2,3,4)), # level 4 is unsed
                 n = c(1:3))
pdf <- pdata.frame(df)

levels(df$f2)
levels(pdf$f2)

if (!all(levels(pdf$f2) == c("1", "2", "3"))) stop("used levels in non-index factor not dropped")

### test unused level in index variable
dfindex <- data.frame(id = c(1,1,2), 
                      time = factor(c(1,2,1), levels = c(1,2,9)), # level 9 is unused
                      f = factor(c("a", "a", "b")),
                      f2 = factor(c(1,2,3), levels = c(1,2,3,4)), # level 4 is unsed
                      n = c(1:3))
pdfindex <- pdata.frame(dfindex)

levels(dfindex$f2)
levels(pdfindex$f2)
if (!all(levels(pdfindex$f2) == c("1", "2", "3"))) stop("used levels in index not dropped")
