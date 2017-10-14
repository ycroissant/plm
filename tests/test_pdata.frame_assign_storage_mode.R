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


############# (2) #############

data(Grunfeld)
Grunfeld[ , "integer"] <- rep(c(1L, 2L, 3L, 4L), 25)
pG <- pdata.frame(Grunfeld)

class(pG[ , "integer"])
class(pG[ , "integer"] / 3) 

pG[ , "int2double"] <- pG$integer / 30
pG$int2double2      <- pG$integer / 30 # at least this case by assigning with $<- is treated differently
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

