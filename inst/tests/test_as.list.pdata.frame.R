library(plm)
data("Grunfeld", package = "plm")
pGrunfeld <- pdata.frame(Grunfeld)

# as.list.data.frame used on a pdata.frame strips the attributes (index, classes),
# thus, need as.list.pdata.frame function to make lapply usable for pdata.frame
# (otherwise as.list.data.frame is used and that does not work due to stripping the attributes)

# Default behaviour is keep.attributes = TRUE => behaves identical to as.list.data.frame (because it uses it)
# Do not change this default, because some code relies on it!
if (!identical(as.list.data.frame(pGrunfeld), as.list(pGrunfeld)))
  stop("as.list.pdata.frame(x, keep.attributes = FALSE) does not produce identical structure compared as.list.data.frame")


# test for keeping attributes (make a list of pseries objects)
expected_result_classes <- list(firm    = c("pseries", "factor"),
                                year    = c("pseries", "factor"),
                                inv     = c("pseries", "numeric"),
                                value   = c("pseries", "numeric"),
                                capital = c("pseries", "numeric"))

if (!identical(lapply(as.list(pGrunfeld, keep.attributes = TRUE), class), expected_result_classes)) stop("classes not correct")

if (!class(as.list(pGrunfeld)) == "list") stop("class is not list")
if (!class(as.list(pGrunfeld, keep.attributes = TRUE)) == "list") stop("class is not list")


# test operation with lapply
list_lags <- lapply(as.list(pGrunfeld, keep.attributes = TRUE), function(x) lag(x))
if (!all(class(list_lags[[1]]) == c("pseries", "factor"))) stop("wrong extracted class")
if (!all(class(list_lags[["value"]]) == c("pseries", "numeric"))) stop("wrong extracted class")
if (!identical(list_lags[["value"]], lag(pGrunfeld$value))) stop("lapply with function on pdata.frame produced incorrect results")

# set on subsetted pdata.frame
list_lags_sub <- lapply(as.list(pGrunfeld[1:50, ], keep.attributes = TRUE), function(x) lag(x))
if (!all(class(list_lags_sub[["value"]]) == c("pseries", "numeric"))) stop("wrong extracted class")
if (!identical(list_lags_sub[["value"]], lag(pGrunfeld[1:50, ]$value))) stop("lapply with function on pdata.frame produced incorrect results")

