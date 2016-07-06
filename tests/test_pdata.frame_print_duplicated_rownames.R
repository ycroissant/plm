# Currently, duplicated row names are allowed for pdata frames.
# This leads to an error when printing pdata frames with duplicate
# row names, becase print.pdata.frame uses print.data.frame
#
# This is a testfile to check if the workaround works
library(plm)

df <- data.frame(id=c(1,2,11,11),
                 time=c(8,9,NA,NA),
                 a=c(1,2,3.1,3.2),
                 b=c(1,2,3.1,3.2))

# has duplicated row names, current workaround is to not print
# the original row names
pdf_w_duprownames <- pdata.frame(df, row.names = TRUE)
any(duplicated(rownames(pdf_w_duprownames)))
print(pdf_w_duprownames)

# should work in any case (no duplicated row names)
pdf_wo_duprownames <- pdata.frame(df, row.names = FALSE)
any(duplicated(rownames(pdf_wo_duprownames)))
print(pdf_wo_duprownames)
