# test of pseriesfy() (turns all columns of a pdata.frame into pseries)

library("plm")
data("Grunfeld", package = "plm")
Grunfeld$fac <- factor(1:200)
pGrun <- pdata.frame(Grunfeld)

options("plm.fast" = FALSE)
pGrunpser1.1 <- pseriesfy(pGrun)

## Run tests only if package 'collapse' is available
## (as they are 'Suggests' dependencies)
collapse.avail <- if (!requireNamespace("collapse", quietly = TRUE)) FALSE else TRUE

if(collapse.avail) {

options("plm.fast" = TRUE)
pGrunpser2.1 <- pseriesfy(pGrun)
options("plm.fast" = FALSE)

# Tests for base R vs. collapse version
class(pGrunpser1.1)
class(pGrunpser2.1)

stopifnot(identical(pGrunpser1.1, pGrunpser2.1))

lapply(pGrunpser1.1, class)
lapply(pGrunpser2.1, class)

lapply(pGrunpser1.1, names)
lapply(pGrunpser2.1, names)

form <- inv ~ value + capital
plm(form, pGrunpser1.1, model = "within")
plm(form, pGrunpser2.1, model = "within")

# apply again to an already pseriesfy-ed pdata.frame (result should not change)
options("plm.fast" = FALSE)
pGrunpser1.2 <- pseriesfy(pGrunpser1.1)
options("plm.fast" = TRUE)
pGrunpser2.2 <- pseriesfy(pGrunpser2.1)
options("plm.fast" = FALSE)

class(pGrunpser1.2)
class(pGrunpser2.2)

lapply(pGrunpser1.2, class)
lapply(pGrunpser2.2, class)

lapply(pGrunpser1.2, names)
lapply(pGrunpser2.2, names)

stopifnot(identical(pGrunpser1.1, pGrunpser1.2))
stopifnot(identical(pGrunpser2.1, pGrunpser2.2))

stopifnot(identical(pGrunpser1.2, pGrunpser2.2))

with(pGrun,        lag(value)) # dispatches to base R's lag
with(pGrunpser1.1, lag(value)) # dispatches to plm's lag() respect. panel structure
invisible(NULL)
}

### benchmark: collapse version about 10x faster
# library(collapse)
# data("wlddev", package = "collapse")
# pwld <- pdata.frame(wlddev, index = c("iso3c", "year"))
# options("plm.fast" = FALSE)
# microbenchmark::microbenchmark(pseriesfy(pwld), times = 100, unit = "us")
# options("plm.fast" = TRUE)
# microbenchmark::microbenchmark(pseriesfy(pwld), times = 100, unit = "us")
# options("plm.fast" = FALSE)
