library(plm)
data("Hedonic", package = "plm")

Hedonic2 <- Hedonic[order(Hedonic$mv), ] # sorted a different way
Hedonic3 <- Hedonic[order(Hedonic$townid, decreasing = TRUE), ] # in descending order

Hed <- pdata.frame(Hedonic, index = "townid", row.names = FALSE) # works 
pdim(Hed)

Hed2 <- pdata.frame(Hedonic2, index = "townid", row.names = FALSE) # wrong result, duplicate couples (time-id)
# pdim(Hed2) # fails, wrong result reveals by pdim()

Hed3 <- pdata.frame(Hedonic3, index = "townid", row.names = FALSE) # wrong result, duplicate couples (time-id)
# pdim(Hed3) # fails, wrong result reveals by pdim()

# Test of pdata.frame conversion if only an individual index in supplied


# require(plm)
# data(diamonds, package = "ggplot2")
# 
# class(diamonds$cut) # ordered factor
# 
# diamonds.p <- pdata.frame(diamonds, index = "cut") # wrong indexes created
# # pdim(diamonds.p) # fails
# 
# # order data set
# diamonds3_asc <- diamonds[order(diamonds$cut), ]
# diamonds3_asc.p <- pdata.frame(diamonds3_asc, index = "cut")
# pdim(diamonds3_asc.p) # works
# 
# diamonds3_desc <- diamonds[order(diamonds$cut, decreasing = T), ]
# diamonds3_desc.p <- pdata.frame(diamonds3_desc, index = "cut")
# # pdim(diamonds3_desc.p) # fails
# 
# 
# # try numeric index
# diamonds2 <- diamonds
# diamonds2$cut_num  <- as.numeric(diamonds2$cut) # make index numeric
# 
# diamonds2_asc  <- diamonds2[order(diamonds2$cut_num), ] # ascending order of index
# diamonds2_desc <- diamonds2[order(diamonds2$cut_num, decreasing = T), ] # descending order of index
# 
# head(diamonds2_asc)
# head(diamonds2_desc)
# 
# diamonds2_asc.p <- pdata.frame(diamonds2_asc, index = "cut_num")
# pdim(diamonds2_asc.p)
# 
# diamonds2_desc.p <- pdata.frame(diamonds2_desc, index = "cut_num") # wrong index created
# # pdim(diamonds2_desc.p) # fails
# 
# 
# # Some further tests about the blocks of individuals - does it depend on asc. block length?
# # -> no
# diamonds2_asc_short <- diamonds2_asc[-c(33940:nrow(diamonds2_asc)), ]
# diamonds2_asc_short.p <- pdata.frame(diamonds2_asc_short, index = "cut_num")
# pdim(diamonds2_asc_short.p)
# 
# diamonds2_asc_short2 <- diamonds2_asc[-c(6517:18517), ]
# diamonds2_asc_short2.p <- pdata.frame(diamonds2_asc_short2, index = "cut_num")
# pdim(diamonds2_asc_short2.p)
