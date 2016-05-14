# test if subsetting by rownames of a pdata.frame preserves index
#  (pre rev. 187/189 all entries were set to NA)
#  (pre rev. 251 subsetting a pdata.frame added extra information due to coercing rules of "[.data.frame")

library(plm)
data("Grunfeld")

pGrunfeld <- pdata.frame(Grunfeld)

# subsetting with [] with rownames - works
attr(pGrunfeld[c("1-1935"), ], which = "index")
attr(pGrunfeld[c("1-1935", "1-1936"), ], which = "index")

if (any(is.na(attr(pGrunfeld[c("1-1935"), ], which = "index")))) warning("FAIL: NA in index")
if (any(is.na(attr(pGrunfeld[c("1-1935", "1-1936"), ], which = "index")))) warning("FAIL: NA in index")


# subsetting with [] by line number works (indexes preserved)
if (!all(attr(pGrunfeld[c(1), ], which = "index") == c(1, 1935))) warning("wrong index!")
if (!all(attr(pGrunfeld[c(1,2), ], which = "index") == data.frame(firm = c(1,1), year = c(1935, 1936)))) warning("wrong index!")

if (any(is.na(attr(pGrunfeld[c(1), ], which = "index")))) warning("FAIL: NA in index")
if (any(is.na(attr(pGrunfeld[c(1,2), ], which = "index")))) warning("FAIL: NA in index")

# subsetting with [[]] works (indexes preserved)
attr(pGrunfeld[["inv"]], which = "index")
attr(pGrunfeld[[3]], which = "index")

if (any(is.na(attr(pGrunfeld[["inv"]], which = "index")))) warning("FAIL: NA in index")
if (any(is.na(attr(pGrunfeld[[3]], which = "index")))) warning("FAIL: NA in index")



############ subsetting changes the pdata.frame
##########(since rev. 251 only a tiny bit),
########## pre rev 251 a lot of unnecessary information was added to the pdata.frame by subsetting 

# this should yield a structurally identical pdata.frame as all rows are extracted:
Grunfeld2 <- Grunfeld[1:nrow(Grunfeld), ]
pGrunfeld2 <- pGrunfeld[1:nrow(pGrunfeld), ]

identical(Grunfeld, Grunfeld2)    # TRUE for data.frame
identical(pGrunfeld, pGrunfeld2)  # FALSE for pdata.frame

######### mode of index gets changed by subsetting!
all.equal(pGrunfeld, pGrunfeld2)
all.equal(pGrunfeld, pGrunfeld2, check.attributes = FALSE)
# compare::compare(pGrunfeld, pGrunfeld2, allowAll = TRUE)

# leave test commented as not yet addressed
# if (!identical(pGrunfeld, pGrunfeld2)) stop("not identical")



### compare object sizes
object.size(dput(pGrunfeld))  # 37392 bytes
object.size(dput(pGrunfeld2)) # 26200 bytes since rev. 251
                              # (was: 83072 bytes in pre rev.251, considerably larger!)




#### estimation on a subsetted pdata.frame failed pre rev. 251
pGrunfeld_sub <- pGrunfeld[c(23:99), ]

plm(inv ~ value + capital, data = pGrunfeld[c(23:99), ]) # failed pre rev.251

  # classes of index of pdata.frame and subsetted pdata.frame are the same 'pindex' and 'data.frame')
  class(attr(pGrunfeld, which="index"))
  class(attr(pGrunfeld$inv, which="index"))
  if (!all(class(attr(pGrunfeld, which="index")) == class(attr(pGrunfeld$inv, which="index")))) warning("classes differ!")
  
  # classes of index of columns of pdata.frame and subsetted pdata.frame must be the same 'pindex' and 'data.frame')
  class(attr(pGrunfeld$inv, which="index"))
  class(attr(pGrunfeld_sub$inv, which="index"))
  if (!all(class(attr(pGrunfeld$inv, which="index")) == class(attr(pGrunfeld_sub$inv, which="index")))) warning("classes differ!")


############ subsetting by [] doesn't mimic data.frame behavior in case of missing j
##this data.frame subsetting isn't documented clearly?:
# ?Extract.data.frame
# "When [ and [[ are used with a single vector index (x[i] or x[[i]]),
# they index the data frame as if it were a list."
# While X[2:4] works as described above, however X[[2:4]] ends up as an error.
##nonetheless, we should mimic what data.frame subsetting is doing
X <- head(Grunfeld, 25)
pX <- pdata.frame(X)
X[2:4]  ##data.frame returns columns 2, 3, 4
pX[2:4]  ##pdata.frame returns rows 2, 3, 4

