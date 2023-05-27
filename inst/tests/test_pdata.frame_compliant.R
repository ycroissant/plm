library(plm)

set.seed(42)
A = c(runif(100))
B = c(runif(100))
C = c(runif(100))
df <- data.frame(A,B,C)
df$id <- floor((as.numeric(rownames(df))-1)/10)
df$t <- ave(df$A, df$id, FUN = seq_along)

form <- A ~ B + C

pdf <- pdata.frame(df, index = c("id","t"))
pdf_base <- subset(pdf, (as.numeric(pdf$t) < 8))

# library(dplyr)
# ## dplyr is not pdata.frame-aware/does not subset the index in row-filtering
# pdf_dplyr <- pdf %>% filter(as.numeric(t) < 8)

## emulate dplyr behaviour (no subsetting of index) so no dependency to dplyr for test is necessary
pdf_dplyr <- subset(df, (as.numeric(df$t) < 8))
pdf_dplyr <- pdata.frame(pdf_dplyr, index = c("id", "t"))
attr(pdf_dplyr, "index") <- index(pdf)

### Demonstration of issue
# lm(): results are the same
print(lm(form, data = pdf_base))
print(lm(form, data = pdf_dplyr))

# plm(): results differ for ill-conditioned pdata.frame:
print(plm(form, data = pdf_base,  method = "within"))
print(plm(form, data = pdf_dplyr, method = "within"))

# plm(): correct with well-behaved pdata.frame
pdf_dplyr_fixed <- pdata.frame(as.data.frame(pdf_dplyr), c("id", "t"))
print(plm(form, data = pdf_dplyr_fixed))

nrow(pdf_base)         # 70
nrow(index(pdf_base))  # 70
nrow(pdf_dplyr)        # 70
nrow(index(pdf_dplyr)) # 100 (should be 70 to be a proper pdata.frame)
nrow(pdf_dplyr_fixed)        # 70
nrow(index(pdf_dplyr_fixed)) # 70

plm:::is.pdata.frame(pdf_base)
plm:::is.pdata.frame(pdf_dplyr) ## ill-behaved pdata.frame is detected

## these should give a proper warning about ill-behaved pdata.frame
plm(  form, data = pdf_dplyr) # warns
pggls(form, data = pdf_dplyr) # warns
pcce( form, data = pdf_dplyr) # warns
pmg(  form, data = pdf_dplyr) # warns
pvcm( form, data = pdf_dplyr) # warns
pldv( form, data = pdf_dplyr) # warns
pgmm( A ~ B + C | lag(C, 2:10), data = pdf_dplyr) # warns

# test for re-creation of a compliant pdata.frame from a non-compliant one
pdf_dplyr_fixed2 <- pdata.frame(pdf_dplyr, index = c("id", "t"))
stopifnot(isTRUE(all.equal(pdf_dplyr_fixed, pdf_dplyr_fixed2)))


## dplyr::arrange compatibility via arrange.pdata.frame, arrange.pindex:
# https://github.com/ycroissant/plm/issues/46
# cannot have this automatically tested withouth making dplyr at least a Suggests-dependency
# avail.dplyr <- if(!requireNamespace("dplyr", quietly = TRUE)) FALSE else TRUE
# if(avail.dplyr) {
#   data("Produc", package = "plm")
#   library(dplyr)
#   p_orig <- pdata.frame(Produc, index = c("state", "year"))
#   p_subset <- p_orig[order(p_orig$year), ]
#   p_dplyr <- dplyr::arrange(p_orig, year, state)
#   head(p_subset)
#   head(index(p_subset))
#   head(p_dplyr)
#   head(index(p_dplyr))
#   
#   stopifnot(all.equal(p_dplyr, p_subset))
# }

