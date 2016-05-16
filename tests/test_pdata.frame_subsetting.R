# test if subsetting by rownames of a pdata.frame preserves index
#  (pre rev. 187/189 all entries were set to NA)
#  (pre rev. 251 subsetting a pdata.frame added extra information due to coercing rules of "[.data.frame")

library(plm)
data("Grunfeld")

pGrunfeld <- pdata.frame(Grunfeld)

# subsetting with [] with rownames - works
attr(pGrunfeld[c("1-1935"), ], which = "index")
attr(pGrunfeld[c("1-1935", "1-1936"), ], which = "index")

if (any(is.na(attr(pGrunfeld[c("1-1935"), ], which = "index")))) stop("FAIL: NA in index")
if (any(is.na(attr(pGrunfeld[c("1-1935", "1-1936"), ], which = "index")))) stop("FAIL: NA in index")


# subsetting with [] by line number works (indexes preserved)
if (!all(attr(pGrunfeld[c(1), ], which = "index") == c(1, 1935))) stop("wrong index!")
if (!all(attr(pGrunfeld[c(1,2), ], which = "index") == data.frame(firm = c(1,1), year = c(1935, 1936)))) stop("wrong index!")

if (any(is.na(attr(pGrunfeld[c(1), ], which = "index")))) stop("FAIL: NA in index")
if (any(is.na(attr(pGrunfeld[c(1,2), ], which = "index")))) stop("FAIL: NA in index")

# subsetting with [[]] works (indexes preserved)
attr(pGrunfeld[["inv"]], which = "index")
attr(pGrunfeld[[3]], which = "index")

if (any(is.na(attr(pGrunfeld[["inv"]], which = "index")))) stop("FAIL: NA in index")
if (any(is.na(attr(pGrunfeld[[3]], which = "index")))) stop("FAIL: NA in index")



############ subsetting used to change the pdata.frame
########## since rev.252 this is fully fixed (rev. 251 already fixed large parts of this),
########## pre rev 251 a lot of unnecessary information was added to the pdata.frame by subsetting 

# this should yield a structurally identical pdata.frame as all rows are extracted:
Grunfeld2 <- Grunfeld[1:nrow(Grunfeld), ]
pGrunfeld2 <- pGrunfeld[1:nrow(pGrunfeld), ]

identical(Grunfeld, Grunfeld2)    # TRUE for data.frame
identical(pGrunfeld, pGrunfeld2)  # TRUE for pdata.frame (was FALSE pre rev. 252)
if (!identical(pGrunfeld, pGrunfeld2))
  stop("pdata.frame not identical after \"subsetting\" with all rows (which should actually not do any subsetting))")

### compare object sizes
object.size(pGrunfeld)  # 37392 bytes
object.size(pGrunfeld2) # 37392 bytes since rev. 252 # (was: 83072 bytes in pre rev.251, considerably larger!)
                                                     # (was: 26200 bytes in rev. 251)
if (!object.size(pGrunfeld) == object.size(pGrunfeld2))
  print("pdata.frame not same object size after \"subsetting\" with all rows (which should actually not do any subsetting))")

# this is likely to be unnecessarily pedandic, because by default attrib.as.set is TRUE
# and from ?attributes "Attributes are not stored internally as a list and should be 
# thought of as a set and not a vector."
identical(Grunfeld, Grunfeld2,   attrib.as.set = FALSE)  # TRUE for data.frame
identical(pGrunfeld, pGrunfeld2, attrib.as.set = FALSE)  # FALSE for pdata.frame

# disply differences (if any) [with rev. 252 there should be no differences left]
all.equal(pGrunfeld, pGrunfeld2)
all.equal(pGrunfeld, pGrunfeld2, check.attributes = FALSE)
# compare::compare(pGrunfeld, pGrunfeld2, allowAll = TRUE)


# Unused levels from the index attribute of a pdata.frame shall be dropped
# (NB: unused levels are not dropped from the variables of the pdata.frame as this is standard R behaviour)
pGrunfeld_sub_id <- pGrunfeld[-c(1:20), ] # drop first individual (1st ind. is in first 20 rows)
if (!isTRUE(all.equal(levels(attr(pGrunfeld_sub_id, "index")[[1]]), levels(factor(2:10)))))
  stop("unused levels from index (individual) not dropped")

pGrunfeld_sub_year <- pGrunfeld[!pGrunfeld$year %in% "1936", ] # drop year 1936
if (!isTRUE(all.equal(levels(attr(pGrunfeld_sub_year, "index")[[2]]), levels(factor(c(1935, 1937:1954))))))
  stop("unused levels from index (time) not dropped")









#### test estimation by plm on a subsetted pdata.frame (failed pre rev. 251)
pGrunfeld_sub <- pGrunfeld[c(23:99), ]
plm(inv ~ value + capital, data = pGrunfeld[c(23:99), ]) # failed pre rev.251

  # classes of index of pdata.frame and subsetted pdata.frame are the same 'pindex' and 'data.frame')
  class(attr(pGrunfeld, which="index"))
  class(attr(pGrunfeld$inv, which="index"))
  if (!all(class(attr(pGrunfeld, which="index")) == class(attr(pGrunfeld$inv, which="index")))) stop("classes differ!")
  
  # classes of index of columns of pdata.frame and subsetted pdata.frame must be the same 'pindex' and 'data.frame')
  class(attr(pGrunfeld$inv, which="index"))
  class(attr(pGrunfeld_sub$inv, which="index"))
  if (!all(class(attr(pGrunfeld$inv, which="index")) == class(attr(pGrunfeld_sub$inv, which="index")))) stop("classes differ!")


############ further testing subsetting of pdata.frame and its index
# up to rev.254 subetting by [i] (with missing j) did not mimic data.frame behavior in case of missing j (j as in [i, j])
# fixed in rev.255
data("Grunfeld", package = "plm")
X <- Grunfeld
pX <- pdata.frame(X)

###### test dimensions of subsetted pdata.frame
if (!isTRUE(all.equal(dim(X[]), dim(pX[])))) stop("dimensions of data.frame and pdata.frame not equal after subsetting")
if (!isTRUE(all.equal(dim(X[ , ]), dim(pX[ ,])))) stop("dimensions of data.frame and pdata.frame not equal after subsetting")
if (!isTRUE(all.equal(dim(X[ , , ]), dim(pX[ , , ])))) stop("dimensions of data.frame and pdata.frame not equal after subsetting")
if (!isTRUE(all.equal(dim(X[ , , drop = TRUE]),  dim(pX[ , , drop = TRUE])))) stop("dimensions of data.frame and pdata.frame not equal after subsetting")
if (!isTRUE(all.equal(dim(X[ , , drop = FALSE]), dim(pX[ , , drop = FALSE])))) stop("dimensions of data.frame and pdata.frame not equal after subsetting")


if (!isTRUE(all.equal(dim(X[1:10, 2:4]),               dim(pX[1:10, 2:4])))) stop("dimensions of data.frame and pdata.frame not equal after subsetting")
if (!isTRUE(all.equal(dim(X[1:10, 2:4, drop = TRUE]),  dim(pX[1:10, 2:4, drop = TRUE])))) stop("dimensions of data.frame and pdata.frame not equal after subsetting")
if (!isTRUE(all.equal(dim(X[1:10, 2:4, drop = FALSE]), dim(pX[1:10, 2:4, drop = FALSE])))) stop("dimensions of data.frame and pdata.frame not equal after subsetting")

if (!isTRUE(all.equal(dim(X[1:10, , ]),              dim(pX[1:10, , ])))) stop("dimensions of data.frame and pdata.frame not equal after subsetting")
if (!isTRUE(all.equal(dim(X[1:10, , drop = TRUE]),   dim(pX[1:10, , drop = TRUE])))) stop("dimensions of data.frame and pdata.frame not equal after subsetting")
if (!isTRUE(all.equal(dim(X[1:10, , drop = FALSE]),  dim(pX[1:10, , drop = FALSE])))) stop("dimensions of data.frame and pdata.frame not equal after subsetting")


if (!isTRUE(all.equal(dim(X[1:10, ]),    dim(pX[1:10, ])))) stop("dimensions of data.frame and pdata.frame not equal after subsetting")
if (!isTRUE(all.equal(dim(X[1, ]),        dim(pX[1, ])))) stop("dimensions of data.frame and pdata.frame not equal after subsetting")

if (!isTRUE(all.equal(dim(X[1]),                 dim(pX[1])))) stop("dimensions of data.frame and pdata.frame not equal after subsetting")
if (!isTRUE(all.equal(dim(X[1, drop = TRUE]),    dim(pX[1, drop = TRUE])))) stop("dimensions of data.frame and pdata.frame not equal after subsetting")
if (!isTRUE(all.equal(dim(X[1, drop = FALSE]),   dim(pX[1, drop = FALSE])))) stop("dimensions of data.frame and pdata.frame not equal after subsetting")

if (!isTRUE(all.equal(dim(X[1:2]),               dim(pX[1:2])))) stop("dimensions of data.frame and pdata.frame not equal after subsetting")
if (!isTRUE(all.equal(dim(X[1:2, drop = TRUE]),  dim(pX[1:2, drop = TRUE])))) stop("dimensions of data.frame and pdata.frame not equal after subsetting")
if (!isTRUE(all.equal(dim(X[1:2, drop = FALSE]), dim(pX[1:2, drop = FALSE])))) stop("dimensions of data.frame and pdata.frame not equal after subsetting")

if (!isTRUE(all.equal(dim(X[ , 2:4]), dim(pX[ , 2:4])))) stop("dimensions of data.frame and pdata.frame not equal after subsetting")
if (!isTRUE(all.equal(dim(X[ , 2:4, drop = TRUE]),  dim(pX[ , 2:4, drop = TRUE])))) stop("dimensions of data.frame and pdata.frame not equal after subsetting")
if (!isTRUE(all.equal(dim(X[ , 2:4 ,drop = FALSE]), dim(pX[ , 2:4, drop = FALSE])))) stop("dimensions of data.frame and pdata.frame not equal after subsetting")

if (!isTRUE(all.equal(dim(X[ , 3]),               dim(pX[ , 3])))) stop("dimensions of data.frame and pdata.frame not equal after subsetting")
if (!isTRUE(all.equal(dim(X[ , 3, drop = TRUE]),  dim(pX[ , 3, drop = TRUE])))) stop("dimensions of data.frame and pdata.frame not equal after subsetting")
if (!isTRUE(all.equal(dim(X[ , 3, drop = FALSE]), dim(pX[ , 3, drop = FALSE])))) stop("dimensions of data.frame and pdata.frame not equal after subsetting")

if (!isTRUE(all.equal(dim(X[1, , ]),             dim(pX[1, , ])))) stop("dimensions of data.frame and pdata.frame not equal after subsetting")
if (!isTRUE(all.equal(dim(X[1, , drop = TRUE]),  dim(pX[1, , drop = TRUE])))) stop("dimensions of data.frame and pdata.frame not equal after subsetting")
if (!isTRUE(all.equal(dim(X[1, , drop = FALSE]), dim(pX[1, , drop = FALSE])))) stop("dimensions of data.frame and pdata.frame not equal after subsetting")


###### test dimensions of index of subsetted pdata.frame
if (!all(c(dim(pX[1:10 , 2:4])[1], 2L) == dim(attr(pX[1:10 , 2:4], "index")))) stop("index has wrong dimension after subsetting")
if (!all(c(dim(pX[1:10 ,    ])[1], 2L) == dim(attr(pX[1:10 ,    ], "index")))) stop("index has wrong dimension after subsetting")
if (!all(c(dim(pX[     , 2:4])[1], 2L) == dim(attr(pX[     , 2:4], "index")))) stop("index has wrong dimension after subsetting")

# NB: this is class c("pseries", "numeric), need length here
if (!all(c(length(pX[ , 3]), 2L) == dim(attr(pX[ , 3], "index")))) stop("index has wrong dimension after subsetting")

# NB: this is class c("pseries", "numeric), need length here
if (!all(c(length(pX[ , 3, drop = TRUE]), 2L) == dim(attr(pX[ , 3, drop = TRUE], "index")))) stop("index has wrong dimension after subsetting")

# need dim again here, because drop = FALSE
if (!all(c(dim(pX[ , 3, drop = FALSE])[1], 2L) == dim(attr(pX[ , 3, drop = FALSE], "index")))) stop("index has wrong dimension after subsetting")

# NB: this is a list! has no index anymore
length(pX[1, , drop = TRUE])
# NB: this a a pdata.frame (drop = FALSE)
if (!all(c(dim(pX[1, , drop = FALSE])[1], 2L) == dim(attr(pX[1, , drop = FALSE], "index")))) stop("index has wrong dimension after subsetting")


# case of [i]-indexing with missing j: index must be have full rows
# dim of pdata.frame: 25, 3
if (!all(c(dim(pX[2:4])[1], 2L)               == dim(attr(pX[2:4],               "index")))) stop("index has wrong dimension after subsetting")
if (!all(c(dim(pX[2:4, drop = TRUE])[1],  2L) == dim(attr(pX[2:4, drop = TRUE],  "index")))) stop("index has wrong dimension after subsetting")
if (!all(c(dim(pX[2:4, drop = FALSE])[1], 2L) == dim(attr(pX[2:4, drop = FALSE], "index")))) stop("index has wrong dimension after subsetting")

if (!all(c(dim(pX[1])[1], 2L)               == dim(attr(pX[1],               "index")))) stop("index has wrong dimension after subsetting")
if (!all(c(dim(pX[1, drop = TRUE])[1],  2L) == dim(attr(pX[1, drop = TRUE],  "index")))) stop("index has wrong dimension after subsetting")
if (!all(c(dim(pX[1, drop = FALSE])[1], 2L) == dim(attr(pX[1, drop = FALSE], "index")))) stop("index has wrong dimension after subsetting")

