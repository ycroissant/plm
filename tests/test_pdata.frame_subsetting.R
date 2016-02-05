# test if subsetting by rownames of a pdata.frame preserves index
#  (pre rev. 187/189 all entries were set to NA)
#
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






#### subsetted pdata.frame can not be used for estimation in rev. 189
pGrunfeld_sub <- pGrunfeld[c(23:99), ]

  # classes of index of pdata.frame and subsetted pdata.frame are the same 'pindex' and 'data.frame')
  class(attr(pGrunfeld, which="index"))
  class(attr(pGrunfeld$inv, which="index"))
  if (!all(class(attr(pGrunfeld, which="index")) == class(attr(pGrunfeld$inv, which="index")))) warning("classes differ!")
  
  # However, classes index of columns of pdata.frame and subsetted pdata.frame diverge
  # (subsetted pdata.frame column misses class 'pindex' relative to full pdata.frame)
  class(attr(pGrunfeld$inv, which="index"))
  class(attr(pGrunfeld_sub$inv, which="index"))
#  if (!all(class(attr(pGrunfeld$inv, which="index")) == class(attr(pGrunfeld_sub$inv, which="index")))) warning("classes differ!")

# plm(inv ~ value + capital, data = pGrunfeld[c(23:99), ]) # fails in v 189

plm(inv ~ value + capital, data = pdata.frame(as.data.frame(pGrunfeld[c(23:99), ]))) # coercing to data.frame first => works


##subsetting by [] doesn't mimic data.frame behavior
##this data.frame subsetting isn't documented clearly:
# ?Extract.data.frame
# "When [ and [[ are used with a single vector index (x[i] or x[[i]]),
# they index the data frame as if it were a list."
# While X[2:4] works as described above, however X[[2:4]] ends up as an error.
##nonetheless, we should mimic what data.frame subsetting is doing
X <- head(Grunfeld, 25)
pX <- pdata.frame(X)
X[2:4]  ##data.frame returns columns 2, 3, 4
pX[2:4]  ##pdata.frame returns rows 2, 3, 4

