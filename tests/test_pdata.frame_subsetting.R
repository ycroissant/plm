# subsetting by rownames of a pdata.frame destroys index (sets all entries to NA)
#
# seems to be in function [.pdata.frame which includes some code for a lagacy pdata.frame format (old.pdata.frame)
library(plm)
data("Grunfeld")

pGrunfeld <- pdata.frame(Grunfeld)

# subsetting with [] with rownames - does not work
attr(pGrunfeld[c("1-1935"), ], which = "index")
attr(pGrunfeld[c("1-1935", "1-1936"), ], which = "index")

#if (any(is.na(attr(pGrunfeld[c("1-1935"), ], which = "index")))) warning("FAIL: NA in index")
#if (any(is.na(attr(pGrunfeld[c("1-1935", "1-1936"), ], which = "index")))) warning("FAIL: NA in index")


# subsetting with [] by line number works (indexes preserved)
attr(pGrunfeld[c(1), ], which = "index")
attr(pGrunfeld[c(1,2), ], which = "index")

#if (any(is.na(attr(pGrunfeld[c(1), ], which = "index")))) warning("FAIL: NA in index")
#if (any(is.na(attr(pGrunfeld[c(1,2), ], which = "index")))) warning("FAIL: NA in index")

# subsetting with [[]] works (indexes preserved)
attr(pGrunfeld[["inv"]], which = "index")
attr(pGrunfeld[[3]], which = "index")

#if (any(is.na(attr(pGrunfeld[["inv"]], which = "index")))) warning("FAIL: NA in index")
#if (any(is.na(attr(pGrunfeld[[3]], which = "index")))) warning("FAIL: NA in index")