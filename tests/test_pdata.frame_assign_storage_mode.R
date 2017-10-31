library(plm)

## test for assigning by $<- to a pdata.frame
## pre rev. 634: decimals which where integers before were converted to integers with typof being integer
##               yielded wrong results.
##

## (1) shows R behaviour in general
## (2) formal test of assigning behaviour of $<- to pdata.frames

############# (1) #############

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


############# (2) #############

data(Grunfeld)
Grunfeld[ , "integer"] <- rep(c(1L, 2L, 3L, 4L), 25)
Grunfeld[ , "logi"]    <- rep(c(TRUE, FALSE, TRUE, FALSE), 25)
pG <- pdata.frame(Grunfeld)

class(pG[ , "integer"])
class(pG[ , "integer"] / 3) 

# assign: we don't have methods for [<-.pdata.frame and [[<-.pdata.frame, so it dispatches to the respective data.frame methods
# This results in really assigning a pseries to the pdata.frame in case of [<- and [[<- as can be seen by lapply(pG, class)
pG[ , "int2double"] <- pG$integer / 30 
pG$int2double2      <- pG$integer / 30 # this case by assigning with $<- is treated differently
pG[["int2double3"]] <- pG$integer / 30

class(pG[ , "int2double"])
class(pG[ , "int2double2"])
class(pG[ , "int2double3"])

typeof(pG[ , "int2double"])
typeof(pG[ , "int2double2"])
typeof(pG[ , "int2double3"])

if(isTRUE(all.equal(as.numeric(pG[ , "int2double"]),  rep(0, 200), check.attributes = FALSE))) stop("when assigning by [<-: double casted to integer (wrong result)")
if(isTRUE(all.equal(as.numeric(pG$int2double2),       rep(0, 200), check.attributes = FALSE))) stop("when assigning by $<-: double casted to integer (wrong result)")
if(isTRUE(all.equal(as.numeric(pG[ , "int2double3"]), rep(0, 200), check.attributes = FALSE))) stop("when assigning by [[<-: double casted to integer (wrong result)")

# check classes
#if(!isTRUE(all.equal(class(pG[ , "int2double"]), c("pseries", "numeric"), check.attributes = FALSE))) stop("when assigning by [<-: double casted to logical (wrong class)")
if(!isTRUE(all.equal(class(pG$int2double2),       c("pseries", "numeric"), check.attributes = FALSE))) stop("when assigning by $<-: double casted to logical (wrong class)")
#if(!isTRUE(all.equal(class(pG[ , "int2doubl3"]), c("pseries", "numeric"), check.attributes = FALSE))) stop("when assigning by [[<-: double casted to logical (wrong class)")


## same with logicals:

pG[ , "logi2double1"] <- pG$logi / 10
pG$logi2double2       <- pG$logi / 10
pG[["logi2double3"]]  <- pG$logi / 10

class(pG[ , "logi2double1"])
class(pG[ , "logi2double2"])
class(pG[ , "logi2double3"])

typeof(pG[ , "logi2double1"])
typeof(pG[ , "logi2double2"])
typeof(pG[ , "logi2double3"])

# check values
if(!isTRUE(all.equal(as.numeric(pG[ , "logi2double1"]), rep(c(0.1, 0.0), 100), check.attributes = FALSE))) stop("when assigning by [<-: double casted to logical (wrong result)")
if(!isTRUE(all.equal(as.numeric(pG$logi2double2),       rep(c(0.1, 0.0), 100), check.attributes = FALSE))) stop("when assigning by $<-: double casted to logical (wrong result)")
if(!isTRUE(all.equal(as.numeric(pG[ , "logi2double3"]), rep(c(0.1, 0.0), 100), check.attributes = FALSE))) stop("when assigning by [[<-: double casted to logical (wrong result)")

# check classes
#if(!isTRUE(all.equal(class(pG[ , "logi2double1"]), c("pseries", "numeric"), check.attributes = FALSE))) stop("when assigning by [<-: double casted to logical (wrong class)")
if(!isTRUE(all.equal(class(pG$logi2double2),       c("pseries", "numeric"), check.attributes = FALSE))) stop("when assigning by $<-: double casted to logical (wrong class)")
#if(!isTRUE(all.equal(class(pG[ , "logi2double3"]), c("pseries", "numeric"), check.attributes = FALSE))) stop("when assigning by [[<-: double casted to logical (wrong class)")


pseries_logi2double <- pG[ , "logi2double1"]

class(pseries_logi2double)
all.equal(pseries_logi2double, plm:::check_propagation_correct_class(pseries_logi2double), check.attributes = FALSE)
pseries_logi2double <- plm:::check_propagation_correct_class(pseries_logi2double)
class(pseries_logi2double)

class(pG$logi2complex <- pG$logi + 3i)
class(pG$logi2complex)
typeof(pG$logi2complex)
pseries_logi2complex <- pG$logi2complex

class(pseries_logi2complex)
all.equal(pseries_logi2complex, plm:::check_propagation_correct_class(pseries_logi2complex), check.attributes = FALSE)
pseries_logi2complex <- plm:::check_propagation_correct_class(pseries_logi2complex)
class(pseries_logi2complex)



