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
identical(pGrunfeld, pGrunfeld2)  # TRUE for pdata.frame (was FALSE pre rev. 252)
if (!identical(pGrunfeld, pGrunfeld2))
  stop("pdata.frame not identical after \"subsetting\" with all rows (which should actually not do any subsetting))")

### compare object sizes
object.size(pGrunfeld)  # 37392 bytes
object.size(pGrunfeld2) # 37392 bytes since rev. 252 # (was: 83072 bytes in pre rev.251, considerably larger!)
                                                     # (was: 26200 bytes in rev. 251)
if (!object.size(pGrunfeld) == object.size(pGrunfeld2))
  print("pdata.frame not same object size after \"subsetting\" with all rows (which should actually not do any subsetting))")

# this is likely to be unnecessary pedandic, because by default attrib.as.set is TRUE
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
  if (!all(class(attr(pGrunfeld, which="index")) == class(attr(pGrunfeld$inv, which="index")))) warning("classes differ!")
  
  # classes of index of columns of pdata.frame and subsetted pdata.frame must be the same 'pindex' and 'data.frame')
  class(attr(pGrunfeld$inv, which="index"))
  class(attr(pGrunfeld_sub$inv, which="index"))
  if (!all(class(attr(pGrunfeld$inv, which="index")) == class(attr(pGrunfeld_sub$inv, which="index")))) warning("classes differ!")


############ subsetting by [] doesn't mimic data.frame behavior in case of missing j (j as in pdf[i, j])
##t his data.frame subsetting isn't documented clearly(?):
# ?Extract.data.frame
# "When [ and [[ are used with a single vector index (x[i] or x[[i]]),
# they index the data frame as if it were a list."
# While X[2:4] works as described above, however X[[2:4]] ends up as an error.
##nonetheless, we should mimic what data.frame subsetting is doing
X <- head(Grunfeld, 25)
pX <- pdata.frame(X)
X[2:4]  ##data.frame returns columns 2, 3, 4
pX[2:4]  ##pdata.frame returns rows 2, 3, 4 - should yield columns
"[.data.frame"(pX, 2:4)
plm:::"[.pdata.frame"(pX, 2:4)


