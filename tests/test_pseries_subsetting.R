# Test subsetting for pseries objects
#
# Note:
#  currently, we do not have a special subsetting method activated for pseries 
#  in file tool_pdata.frame.R is a commented [.pseries but see the hints there as it interferes with loaded dplyr

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

df$f[integer()]
df$f[NA]


# pdata.frame with c("pseries", "factor")
pdf <- pdata.frame(df)
pdf$f
levels(pdf$f) <- c("a","b","c")
pdf$f[1]
pdf$f[1, drop=T]
pdf$f[drop=T]
pdf$f[0]
attr(pdf$f[0], "index")

pdf$f[0]
pdf$f[integer()]
str(pdf$f[integer()])

str( df$f[NA])
str(pdf$f[NA])



df$f[integer()]
pdf$f[integer()]


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



############### test pseries subsetting ("[.pseries") ################
####  "[.pseries" is in pdata.frame.R
data("EmplUK", package = "plm")
(plm(log(emp) ~ log(wage) + log(capital), data = EmplUK, model = "fd"))

data("Grunfeld", package = "plm")
Grunfeld$fac <- factor(c("a", "b", "c", "d"))
pGrunfeld <- pdata.frame(Grunfeld)

pseries <- pGrunfeld$inv
pfac <- pGrunfeld$fac
fac <- Grunfeld$fac

pseries[1]
pseries[c(1,2)]
pseries[-c(1,2)]
# # this also checks for the both indexes having the same levels after subsetting
# # (unused levels in index are dropped):
#### if(!isTRUE(all.equal(index(pseries[c(1)]),    index(pGrunfeld[c(1), ])))) stop("indexes not the same")
#### if(!isTRUE(all.equal(index(pseries[c(1,2)]),  index(pGrunfeld[c(1,2), ])))) stop("indexes not the same")
#### if(!isTRUE(all.equal(index(pseries[-c(1,2)]), index(pGrunfeld[-c(1,2), ])))) stop("indexes not the same")

# subsetting when there are no names (in this case (dummy) names are used in the subsetting code)
pseries_nn <- unname(pGrunfeld$inv)
pfac_nn <- unname(pGrunfeld$fac)
fac_nn <- unname(Grunfeld$fac)

pseries_nn[1]
pseries_nn[c(1,2)]
pseries_nn[-c(1,2)]
# # # this also checks for the both indexes having the same levels after subsetting
# # # (unused levels in index are dropped):
#### if(!isTRUE(all.equal(index(pseries_nn[c(1)]),    index(pGrunfeld[c(1), ])))) stop("indexes not the same")
#### if(!isTRUE(all.equal(index(pseries_nn[c(1,2)]),  index(pGrunfeld[c(1,2), ])))) stop("indexes not the same")
#### if(!isTRUE(all.equal(index(pseries_nn[-c(1,2)]), index(pGrunfeld[-c(1,2), ])))) stop("indexes not the same")



# subsetting with character
pseries["10-1946"]
pseries[c("10-1935", "10-1946")]

# character subsetting works for plain numeric:
series <- Grunfeld$inv
names(series) <- names(pseries)
names(fac) <- names(pfac)
series["10-1946"]

#### if(!isTRUE(all.equal(index(pseries["10-1946"]),               index(pGrunfeld["10-1946", ])))) stop("indexes not the same")
#### if(!isTRUE(all.equal(index(pseries[c("10-1935", "10-1946")]), index(pGrunfeld[c("10-1935", "10-1946"), ])))) stop("indexes not the same")


### For c("pseries", "factor") perform additional tests of 'drop' argument
pfac[1, drop = TRUE]   # only level "a" should be left
pfac[1:3][drop = TRUE] # only level "a", "b", "c" should be left

fac[1, drop = TRUE]
fac[1:3][drop = TRUE]

pfac["nonExist"] # should be NA and levels "a" to "d"
fac["nonExist"]

pfac["nonExist"][drop = TRUE] # should be NA and no level left
fac["nonExist"][drop = TRUE]

# check subsetting with NA:
#### if(!isTRUE(all.equal(as.numeric(pseries[NA]), series[NA], check.attributes = FALSE))) stop("subsetting with NA not the same for pseries")

# assign first operand's attributes:
pseries[1:(length(pseries)-1)] + pseries[2:length(pseries)]
head(index(pseries[1:(length(pseries)-1)]))
head(index(pseries[2:length(pseries)]))


# ... just as in base R for names:
i1 <- 1:100; i2 <- 1:100
names(i1) <- 1:100; names(i2) <- LETTERS[(0:99 %% 26) + 1]
head(i1)
head(i2)
# names are taken from first operand in base R numerics,
# so for pseries it is ok to assign the index of the first operand
names(i1 + i2)
names(i2 + i1)


## These are ok (give (about) same error msg for plain numeric as for pseries numeric)
# pseries[1, ] # Error in x[...] : incorrect number of dimensions
# series[1, ]  # Error during wrapup: incorrect number of dimensions

# subsetting with NA
# entries and names are all NA
pseries[NA]
attr(pseries[NA], "index") #  same as in pdata.frame's index when pdata.frame is indexed by NA
str(pseries[NA])
series[NA]

# subsetting with integer()
pseries[integer()]
class(pseries[integer()]) # c("pseries", "numeric")
class(attr(pseries[integer()], "index"))
str(pseries[integer()], "index")

series[integer()]
str(series[integer()])

# subsetting with 0
pseries[0]
class(pseries[0]) # c("pseries", "numeric")
class(attr(pseries[0], "index"))
str(pseries[0], "index")

series[0]
str(series[0])
