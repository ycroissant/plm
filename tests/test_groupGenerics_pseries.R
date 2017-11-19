## test of groupGernerics for 'pseries' objects work
## test of propagation
##
## see further down below (4) for an example of R's behaviour for a wrapping class "myclass" without group Generics
library(plm)
data("Grunfeld", package = "plm")
Grunfeld[ , "integer"] <- rep(c(1L, 2L, 3L, 4L), 25)
Grunfeld[ , "logi"]    <- rep(c(TRUE, FALSE, TRUE, FALSE), 25)
Grunfeld[ , "complex"] <- rep(c(1+0i, 2+1i), 100)
pGrunfeld <- pdata.frame(Grunfeld)

############### (1) general checks if group generics and propagation works ###########

# check Ops: integer -> numeric
stopifnot(all.equal(c("pseries", class(Grunfeld$integer / 33)), class(pGrunfeld$integer / 33)))

# check Ops. logi -> numeric
stopifnot(all.equal(c("pseries", class(Grunfeld$logi + 1.1)), class(pGrunfeld$logi + 1.1)))
stopifnot(all.equal(c("pseries", class(-Grunfeld$logi)), class(-pGrunfeld$logi)))

stopifnot(all(class(diff(pGrunfeld$logi)) == c("pseries", "integer")))

# check Math, also with optional second argument (check calculation and class)
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
## during testing phase of the implementation of groupGenerics, it became apparent that
## non-correct implementation yields different results when an integer is in the dependent
## variable -> so, have this as a test case
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
## pre rev. 634: decimals which where integers before were converted to integers with typof being integer
##               yielded wrong results.

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


############## (4) demonstration of R's behaviour for a wrapping class "myclass" without group generics
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
y2 # 0.1 0.2 0.3 - but decimal numbers!

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

