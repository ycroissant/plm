# Test of conversion to pdata.frame if only an individual index in supplied
# bug fixed in rev. 204
 

library(plm)
data("Hedonic", package = "plm")

# insert test columns
Hedonic$str <- c(rep(c(letters), nrow(Hedonic)%/%length(letters)), letters[1:(nrow(Hedonic)%%length(letters))])
Hedonic$str_const <- rep("const", nrow(Hedonic))
temp <- pdata.frame(Hedonic, index = "townid")
Hedonic$fac <- factor(Hedonic$str)
Hedonic$fac_const <- rep(factor("fac_const"), nrow(Hedonic))
Hedonic$na <- rep(NA, nrow(Hedonic))
temp <- pdata.frame(Hedonic, index = "townid")
Hedonic$na2 <- rep(NA, nrow(Hedonic))

rm(temp)

Hedonic2 <- Hedonic[order(Hedonic$mv), ] # sorted a different way
Hedonic3 <- Hedonic[order(Hedonic$townid, decreasing = TRUE), ] # in descending order

Hed1 <- pdata.frame(Hedonic, index = "townid", stringsAsFactors = FALSE) # works 
pdim(Hed1)
head(Hed1)

Hed1_2.1 <- pdata.frame(Hedonic, index = "townid", stringsAsFactors = TRUE) # works 
pdim(Hed1_2.1)
head(Hed1_2.1)
#str(Hed1_2.1)
sapply(Hed1_2.1, function(x) class(x))

Hed1_2.2 <- pdata.frame(Hedonic, index = "townid", stringsAsFactors = FALSE) # works 
pdim(Hed1_2.2)
head(Hed1_2.2)
#str(Hed1_2.2)
sapply(Hed1_2.2, function(x) class(x))



Hed2 <- pdata.frame(Hedonic2, index = "townid")
pdim(Hed2)
head(Hed2)

Hed2_2 <- pdata.frame(Hedonic2, index = "townid")
pdim(Hed2_2)
head(Hed2_2)
head(Hedonic2)


Hed3 <- pdata.frame(Hedonic3, index = "townid")
pdim(Hed3)
head(Hed3)

Hed3_2 <- pdata.frame(Hedonic3, index = "townid")
pdim(Hed3_2)
head(Hed3_2)
head(Hedonic3)

# test for warning of duplicated couples
  data("Grunfeld", package = "plm")
  Grunfeld_dup <- rbind(Grunfeld, Grunfeld[200, ])
  ttC <-  tryCatch(pdata.frame(Grunfeld_dup), error=function(e) e, warning=function(w) w)
  if(!is(ttC,"warning") | ttC$message != "duplicate couples (id-time) in resulting pdata.frame\n to find out which, use e.g. table(index(your_pdataframe), useNA = \"ifany\")")
    stop("warning of duplicated couples not successful")


# test: character as individual index
Grunfeld.p <- pdata.frame(Grunfeld)
Grunfeld_charac <- Grunfeld
Grunfeld_charac$firm <- as.character(Grunfeld_charac$firm)
Grunfeld_charac.p <- pdata.frame(Grunfeld_charac)
Grunfeld_charac.p2 <- pdata.frame(Grunfeld_charac, stringsAsFactors = FALSE)
if(!identical(Grunfeld_charac.p, Grunfeld_charac.p2)) stop("pdata.frames not identical)")
pdim(Grunfeld_charac.p)
pdim(Grunfeld_charac.p2)


# test: character as individual index
Grunfeld_charac2 <- Grunfeld
Grunfeld_charac2$firm <- as.character(Grunfeld_charac2$firm)
Grunfeld_charac2$year <- as.character(Grunfeld_charac2$year)
Grunfeld_charac2.p <- pdata.frame(Grunfeld_charac2)
Grunfeld_charac2.p2 <- pdata.frame(Grunfeld_charac2, stringsAsFactors = FALSE)
if(!identical(Grunfeld_charac2.p, Grunfeld_charac2.p2)) stop("pdata.frames not identical)")
pdim(Grunfeld_charac2.p)
pdim(Grunfeld_charac2.p2)

# index with two variables
Grunfeld.p3 <- pdata.frame(Grunfeld, index = c("firm", "year"))
pdim(Grunfeld.p3)

# index is numeric
data("Wages", package = "plm")
Wag <- pdata.frame(Wages, 595)
pdim(Wag)

# test for warning about time index
ttC2 <-  tryCatch(pdata.frame(Wages, index=c(595, 3)), error=function(e) e, warning = function(w) w)
if(!is(ttC2,"warning") | ttC2$message != "The time index (second element of 'index' argument) will be ignored\n") stop("warning about unused time index not sent")

# test for error about length(index)>2
# Should result in error with informative message
#ttC3 <-  tryCatch(pdata.frame(Wages, index=c(595, 3, 5)), error=function(e) e, warning = function(w) w)
#if(!is(ttC3,"error") | ttC3$message != "'index' can be of length 2 at the most (one individual and one time index)") stop("error about length(index)>2 not sent")
#YC deprecated, the index can be now of length 3

#### larger data set ###
## commented because needs other package
# require(plm)
# library(ggplot2)
# data("diamonds", package = "ggplot2")
# 
# class(diamonds$cut) # ordered factor
# 
# diamonds.p <- pdata.frame(diamonds, index = "cut") # wrong indexes created
# pdim(diamonds.p)
# 
# # order data set
# diamonds3_asc <- diamonds[order(diamonds$cut), ]
# diamonds3_asc.p <- pdata.frame(diamonds3_asc, index = "cut")
# pdim(diamonds3_asc.p) # works
# 
# diamonds3_desc <- diamonds[order(diamonds$cut, decreasing = T), ]
# diamonds3_desc.p <- pdata.frame(diamonds3_desc, index = "cut")
# pdim(diamonds3_desc.p)
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
# pdim(diamonds2_desc.p)
# 
# 
# # Some further tests about the blocks of individuals
# # - does it depend on asc. block length?
# # -> no, works fine
# diamonds2_asc_short <- diamonds2_asc[-c(33940:nrow(diamonds2_asc)), ]
# diamonds2_asc_short.p <- pdata.frame(diamonds2_asc_short, index = "cut_num")
# pdim(diamonds2_asc_short.p)
# 
# diamonds2_asc_short2 <- diamonds2_asc[-c(6517:18517), ]
# diamonds2_asc_short2.p <- pdata.frame(diamonds2_asc_short2, index = "cut_num")
# pdim(diamonds2_asc_short2.p)
