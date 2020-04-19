## test of groupGernerics for 'pseries' objects work
## test of propagation
##
## see further down below (5) for an example of R's behaviour for a wrapping class "myclass" without group Generics
## see also (6) for a case which cannot be worked around (best to my knowledge)
library(plm)
data("Grunfeld", package = "plm")
Grunfeld[ , "integer"] <- rep(c(1L, 2L, 3L, 4L), 25)
Grunfeld[ , "logi"]    <- rep(c(TRUE, FALSE, TRUE, FALSE), 25)
Grunfeld[ , "complex"] <- rep(c(1+0i, 2+1i), 100)
Grunfeld[ , "char"] <- rep(c("a1", "b2"), 100)
Grunfeld[ , "fac"] <- factor(rep(c("a", "b"), 100))
pGrunfeld <- pdata.frame(Grunfeld, stringsAsFactors = FALSE)

############### (1) general checks if group generics and propagation works ###########

# check Ops: integer -> numeric
stopifnot(all.equal(c("pseries", class(Grunfeld$integer / 33)), class(pGrunfeld$integer / 33)))

# check Ops: logical -> numeric
stopifnot(all.equal(c("pseries", class(Grunfeld$logi + 1.1)), class(pGrunfeld$logi + 1.1)))
stopifnot(all.equal(c("pseries", class(-Grunfeld$logi)), class(-pGrunfeld$logi)))

stopifnot(all(class(diff(pGrunfeld$logi)) == c("pseries", "integer")))

## check Ops: non-vector result, result is matrix (may not be class c("pseries", "matrix"))
mdat <- matrix(c(1:200), nrow = 200, ncol = 1, byrow = TRUE)
stopifnot(all(class(pGrunfeld$integer * mdat) == "matrix"))
stopifnot(all(class(pGrunfeld$logi * mdat)    == "matrix"))
stopifnot(all(class(mdat * pGrunfeld$integer) == "matrix"))
stopifnot(all(class(mdat * pGrunfeld$logi)    == "matrix"))

# check Math: also with optional second argument (check calculation and class)
stopifnot(all.equal(log(Grunfeld$integer), as.numeric(log(pGrunfeld$integer))))
stopifnot(all.equal(c("pseries", class(log(Grunfeld$integer))), class(log(pGrunfeld$integer))))

stopifnot(all.equal(log(Grunfeld$integer, 20), as.numeric(log(pGrunfeld$integer, 20))))
stopifnot(all.equal(c("pseries", class(log(Grunfeld$integer, 20))), class(log(pGrunfeld$integer, 20))))


# check Complex
stopifnot(all(c("pseries", class(Re(Grunfeld$logi))) == class(Re(pGrunfeld$logi))))
stopifnot(all(c("pseries", class(Im(Grunfeld$logi))) == class(Im(pGrunfeld$logi))))
stopifnot(all(c("pseries", class(Conj(Grunfeld$logi))) == class(Re(pGrunfeld$logi))))
stopifnot(all(c("pseries", class(Conj(Grunfeld$complex))) == class(Conj(pGrunfeld$complex))))
# this is a downward propagation complex -> numeric
stopifnot(all(c("pseries", class(Re(Grunfeld$complex))) == class(Re(pGrunfeld$complex))))

############# (2) check of model estimation with dependent variable as integer #########
## During testing phase of the implementation of groupGenerics, it became apparent that
## non-correct implementation yields different results when an integer serves as dependent
## variable -> use an integer as test case
data("Produc", package = "plm")

## gsp is an integer
form <- log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp
zz <- plm(form, data = Produc, index=c("state","year"), model = "within")
print(summary(zz))

pProduc <- pdata.frame(Produc)
pProduc$gsp2 <- as.numeric(pProduc$gsp)

zz2 <- plm(update(form, log(gsp2) ~ . ), data = pProduc, index = c("state","year"), model = "within")
print(summary(zz2))

if (!isTRUE(all.equal(zz$coefficients, zz2$coefficients))) stop("estimation results not equal")


############# (3) assinging to a pdata.frame #############
## test for assigning by $<- to a pdata.frame
## pre rev. 634: decimals which had been integers previously were converted to integers with typeof == integer
##               and gave wrong results.

Grunfeld[ , "integer"] <- rep(c(1L, 2L, 3L, 4L), 25)
Grunfeld[ , "logi"]    <- rep(c(TRUE, FALSE, TRUE, FALSE), 25)
pGrunfeld <- pdata.frame(Grunfeld)

class(pGrunfeld[ , "integer"])
class(pGrunfeld[ , "integer"] / 3)

# assign: we don't (yet?) have methods for [<-.pdata.frame and [[<-.pdata.frame, so it dispatches to the respective data.frame methods
# This results in really assigning a pseries to the pdata.frame in case of [<- and [[<- as can be seen by lapply(pGrunfeld, class)
pGrunfeld[ , "int2double"] <- pGrunfeld$integer / 30 
pGrunfeld$int2double2      <- pGrunfeld$integer / 30 # this case by assigning with $<- is treated differently as we have "$<-.pdata.frame" defined
pGrunfeld[["int2double3"]] <- pGrunfeld$integer / 30

class(pGrunfeld[ , "int2double"])
class(pGrunfeld[ , "int2double2"])
class(pGrunfeld[ , "int2double3"])

typeof(pGrunfeld[ , "int2double"])
typeof(pGrunfeld[ , "int2double2"])
typeof(pGrunfeld[ , "int2double3"])

# check values
if(isTRUE(all.equal(as.numeric(pGrunfeld[ , "int2double"]),  rep(0, 200)))) stop("when assigning by [<-: double casted to integer (wrong result)")
if(isTRUE(all.equal(as.numeric(pGrunfeld$int2double2),       rep(0, 200)))) stop("when assigning by $<-: double casted to integer (wrong result)")
if(isTRUE(all.equal(as.numeric(pGrunfeld[ , "int2double3"]), rep(0, 200)))) stop("when assigning by [[<-: double casted to integer (wrong result)")

# check classes
if(!isTRUE(all.equal(class(pGrunfeld[ , "int2double"]),  c("pseries", "numeric")))) stop("when assigning by [<-: double casted to logical (wrong class)")
if(!isTRUE(all.equal(class(pGrunfeld$int2double2),       c("pseries", "numeric")))) stop("when assigning by $<-: double casted to logical (wrong class)")
if(!isTRUE(all.equal(class(pGrunfeld[ , "int2double3"]), c("pseries", "numeric")))) stop("when assigning by [[<-: double casted to logical (wrong class)")

## same with logicals:
pGrunfeld[ , "logi2double1"] <- pGrunfeld$logi / 10
pGrunfeld$logi2double2       <- pGrunfeld$logi / 10
pGrunfeld[["logi2double3"]]  <- pGrunfeld$logi / 10

class(pGrunfeld[ , "logi2double1"])
class(pGrunfeld[ , "logi2double2"])
class(pGrunfeld[ , "logi2double3"])

typeof(pGrunfeld[ , "logi2double1"])
typeof(pGrunfeld[ , "logi2double2"])
typeof(pGrunfeld[ , "logi2double3"])

# check values
if(!isTRUE(all.equal(as.numeric(pGrunfeld[ , "logi2double1"]), rep(c(0.1, 0.0), 100)))) stop("when assigning by [<-: double casted to logical (wrong result)")
if(!isTRUE(all.equal(as.numeric(pGrunfeld$logi2double2),       rep(c(0.1, 0.0), 100)))) stop("when assigning by $<-: double casted to logical (wrong result)")
if(!isTRUE(all.equal(as.numeric(pGrunfeld[ , "logi2double3"]), rep(c(0.1, 0.0), 100)))) stop("when assigning by [[<-: double casted to logical (wrong result)")

# check classes
if(!isTRUE(all.equal(class(pGrunfeld[ , "logi2double1"]), c("pseries", "numeric")))) stop("when assigning by [<-: double casted to logical (wrong class)")
if(!isTRUE(all.equal(class(pGrunfeld$logi2double2),       c("pseries", "numeric")))) stop("when assigning by $<-: double casted to logical (wrong class)")
if(!isTRUE(all.equal(class(pGrunfeld[ , "logi2double3"]), c("pseries", "numeric")))) stop("when assigning by [[<-: double casted to logical (wrong class)")


############## (4) test for various kinds of argument combinations in Ops.pseries

# e1: pseries, e2: not a pseries and vice versa
# -> result must be a pseries in both cases
e1e2_a <- `*`(pGrunfeld$integer, 4L)
e1e2_b <- `*`(4L, pGrunfeld$integer)
class(e1e2_a)
class(e1e2_b)
stopifnot(is.pseries(e1e2_a))
stopifnot(is.pseries(e1e2_b))
stopifnot(isTRUE(all.equal(e1e2_a, e1e2_b)))

# e1, e2: pseries with varying length
# -> result must have index of longer pseries (as the shorter pseries is recycled)
pGrunfeld_short <- pGrunfeld[4:5, ]
e1e2_c <- `*`(pGrunfeld$integer,       pGrunfeld_short$integer)
e1e2_d <- `*`(pGrunfeld_short$integer, pGrunfeld$integer)
length(e1e2_c)
length(e1e2_d)
index(e1e2_c)
index(e1e2_d)
nrow(index(e1e2_c))
nrow(index(e1e2_d))
stopifnot(is.pseries(e1e2_c))
stopifnot(is.pseries(e1e2_d))
stopifnot(isTRUE(all.equal(index(e1e2_c), index(pGrunfeld$integer))))
stopifnot(isTRUE(all.equal(index(e1e2_d), index(pGrunfeld$integer))))

# e1, e2: pseries with index of same length but different content
# -> result is assigned index of first operand
Gr <- Grunfeld
Gr$firm <- sort(rep(LETTERS[1:10], 20)) # make individual index different
pGr <- pdata.frame(Gr, stringsAsFactors = FALSE)
e1e2_e <- `*`(pGr$integer,       pGrunfeld$integer)
e1e2_f <- `*`(pGrunfeld$integer, pGr$integer)
index(e1e2_e)
index(e1e2_f)
stopifnot(is.pseries(e1e2_e))
stopifnot(is.pseries(e1e2_f))



############## (5) demonstration of R's behaviour for a wrapping class "myclass" without group generics
x <- c(1L, 2L, 3L)
class(x) # integer
mode(x)
typeof(x)

y <- x
class(y) <- c("myclass", class(y))
class(y) # c("myclass", "integer")
mode(y)
typeof(y)

x2 <- x / 10
class(x2) # numeric - propagated to higher class numeric
mode(x2)
typeof(x2)

y2 <- y / 10
class(y2) # c("myclass", "interger") - not propagated to c("myclass", "numeric")
mode(y2)
typeof(y2)
y2 # 0.1 0.2 0.3 - class is c("myclass", "integer") but result is decimals!

y3 <- y2
typeof(y3) # double
class(y3) <- setdiff(class(y3), "myclass")
class(y3) # integer
mode(y3)
typeof(y3) # integer
y3 # 0 0 0 - integers after class() <- "integer"

y4 <- y2
attr(y4, "class")
attr(y4, "class") <- NULL
class(y4)
mode(y4)
typeof(y4)
y4 # 0.1 0.2 0.3 numerics after attr(obj, "class") <- NULL

fac <- factor(x)
class(fac)
typeof(fac)
mode(fac)

logi <- c(TRUE, FALSE, TRUE)
class(logi) # logical
typeof(logi) # logical
class(logi) <- union("myclass", class(logi))
class(logi) # myclass logical
loginum <- logi - 1.5
class(loginum) # myclass logical
typeof(loginum) # double

############## (6) demonstrate case of R's behaviour which cannot be worked around even with without group generics
# dpois() (also dnorm() and likely more) does not strip unnecessary classes and custom attributes
# before it performes its operations
class(pGrunfeld$integer) #  "pseries" "integer"
set.seed(42)
res_dpois <- dpois(pGrunfeld$integer, sample(1:10, 200, replace = TRUE))
class(res_dpois) # "pseries" "integer"  <-- can do nothing about his
typeof(res_dpois) # double
str(res_dpois)
res_pmax <- pmax(res_dpois, .Machine[["double.eps"]])
# this errored for a while when no correction in remove_pseries_features() was in place:
if(isTRUE(all.equal(as.numeric(res_pmax), rep(.Machine[["double.eps"]], 200)))) {
  stop("pmax gives wrong result due wrong coercion (integer/numeric)")
}
