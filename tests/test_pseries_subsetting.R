#
# Test subsetting for pseries objects
#
# Note:
#  currently, (2016-07-11, rev. 324), we do not have a special subsetting method
#  for pseries defined (see for test of pseries features after subsetting further down)


library(plm)

# data.frame with factor
df <- data.frame(id = c(1,1,2), time = c(1,2,1), f = factor(c("a", "a", "b")), n = c(1:3))
df$f
levels(df$f) <- c("a","b","c")
df$f[1]
df$f[1,drop=F]
df$f[1,drop=T]
df$f[drop=T]
df$f[0]

# pdata.frame with c("pseries", "factor")
pdf <- pdata.frame(df)
pdf$f
levels(pdf$f) <- c("a","b","c")
pdf$f[1]
pdf$f[drop=T]
pdf$f[0]
attr(pdf$f[0], "index")

pf <- pdf$f

if (!all(levels(pdf$f[1,drop=F]) == c("a","b","c"))) stop("subsetting for c(\"pseries\", \"factor\") (with drop=F) not correct")
if (!all(class(pdf$f[1]) == c("pseries", "factor"))) stop("classes not correct after subsetting pseries")

if (!levels(pdf$f[1,drop=T]) == "a") stop("subsetting for c(\"pseries\", \"factor\") with drop=T not correct - unused levels not dropped")
if (!all(levels(pdf$f[drop=T]) == c("a", "b"))) stop("subsetting for c(\"pseries\", \"factor\") with drop=T not correct - unused levels not dropped")

### activate these tests once the subsetting method for pseries is defined.
#if (is.null(attr(pdf$f[1], "index"))) stop("no index after subsetting")
#if (!nrow(attr(pdf$f[1], "index")) == 1) stop("wrong index after subsetting")



lapply(df, attributes)
lapply(pdf, attributes)

lapply(df, class)
lapply(pdf, class)
