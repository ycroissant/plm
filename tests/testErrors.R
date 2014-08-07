### Testing problematic and erroneous data
library(plm)

### NA in the individual index: should give an informative error
ind <- 1:100
ind[4] <- NA
T <- 4
                           # balanced panel of length 4
alpha <- rnorm(length(ind))
                           # fixed effects
eps <- rnorm(T*length(ind))
                           # idiosyncratic effect
x <- runif(length(ind))
y <- x + alpha + eps
dat <- data.frame(y, x, ind=rep(ind, T), t=rep(1:T, each=length(ind)))
data <- plm.data(dat, index=c("ind", "t"))
a <- try(m <- plm(y ~ x, data=data, model="random"))
                           # should give an error: NA in the individual index
