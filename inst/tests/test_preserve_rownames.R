## testfile for preserving row.names in plm_object$model and as.data.frame


library(plm)
data("Grunfeld", package = "plm")

row.names(Grunfeld) # row.names are a sequence 1:nrow(Grunfeld) originally
gr <- plm(inv ~ value + capital, data=Grunfeld, model="pooling")
# same row.names and names as before estimation [i. e. original row.names are preserved]
if(!isTRUE(all.equal(row.names(gr$model),        row.names(Grunfeld)))) stop("original rownames not preserved in plm_obj$model")
if(!isTRUE(all.equal(row.names(model.frame(gr)), row.names(Grunfeld)))) stop("original rownames not preserved in model.frame(plm_obj)")
if(!isTRUE(all.equal(names(pmodel.response(gr)), row.names(Grunfeld)))) stop("original rownames not preserved in names(pmodel.response(plm_obj))")
if(!isTRUE(all.equal(names(residuals(gr)),       row.names(Grunfeld)))) stop("original rownames not preserved in names(residuals(plm_obj))")


# make a pdata.frame with "fancy" row.names (default)
# [i.e., combination of individual index and time index]
pGrunfeld <- pdata.frame(Grunfeld, index = c("firm", "year"))
row.names(pGrunfeld) # fancy row.names
gr_fancy_rownames <- plm(inv ~ value + capital, data=pGrunfeld, model="pooling")

# original row.names of pGrunfeld (i.e., "fancy" row.names) are preserved
if(!isTRUE(all.equal(row.names(gr_fancy_rownames$model), row.names(pGrunfeld)))) stop("original rownames not preserved in plm_obj$model")
if(!isTRUE(all.equal(row.names(model.frame(gr_fancy_rownames)), row.names(pGrunfeld)))) stop("original rownames not preserved in model.frame(plm_obj)")
if(!isTRUE(all.equal(names(pmodel.response(gr_fancy_rownames)), row.names(pGrunfeld)))) stop("original rownames not preserved in names(pmodel.response(plm_obj))")
if(!isTRUE(all.equal(names(residuals(gr_fancy_rownames)), row.names(pGrunfeld)))) stop("original rownames not preserved in names(residuals(plm_obj))")

# test with one missing observation

# set some arbitrary value to NA, so it is left out of the estimation
  # data.frame
  Grunfeld_1NA <- Grunfeld
  line_no <- 6L
  Grunfeld_1NA[line_no, "inv"] <- NA
  
  gr_1NA <- plm(inv ~ value + capital, data=Grunfeld_1NA, model="pooling")
  nobs(gr_1NA)       # 199
  nrow(Grunfeld_1NA) # 200
  if(!isTRUE(all.equal(row.names(gr_1NA$model),        row.names(Grunfeld_1NA[-line_no, ])))) stop("original rownames not preserved in plm_obj$model")
  if(!isTRUE(all.equal(row.names(model.frame(gr_1NA)), row.names(Grunfeld_1NA[-line_no, ])))) stop("original rownames not preserved in model.frame(plm_obj)")
  if(!isTRUE(all.equal(names(pmodel.response(gr_1NA)), row.names(Grunfeld_1NA[-line_no, ])))) stop("original rownames not preserved in names(pmodel.response(plm_obj))")
  if(!isTRUE(all.equal(names(residuals(gr_1NA)),       row.names(Grunfeld_1NA[-line_no, ])))) stop("original rownames not preserved in names(residuals(plm_obj))")
  

  # Should be TRUE
  # [199 + 1 == 200]
  # [199     == 199]
  if(!isTRUE(length(residuals(gr_1NA)) + 1 == length(row.names(Grunfeld_1NA)))) stop("length not correct")
  if(!isTRUE(length(residuals(gr_1NA))     == length(row.names(Grunfeld_1NA[-line_no, ])))) stop("length not correct")
  
  # pdata.frame
  pGrunfeld_1NA <- pGrunfeld
  line_no <- 6L
  pGrunfeld_1NA[line_no, "inv"] <- NA
  
  gr_fancy_rownames_1NA <- plm(inv ~ value + capital, data=pGrunfeld_1NA, model="pooling")
  nobs(gr_fancy_rownames_1NA)       # 199
  nrow(pGrunfeld_1NA) # 200
  if(!isTRUE(all.equal(row.names(gr_fancy_rownames_1NA$model),        row.names(pGrunfeld_1NA[-line_no, ])))) stop("original rownames not preserved in plm_obj$model")
  if(!isTRUE(all.equal(row.names(model.frame(gr_fancy_rownames_1NA)), row.names(pGrunfeld_1NA[-line_no, ])))) stop("original rownames not preserved in model.frame(plm_obj)")
  if(!isTRUE(all.equal(names(pmodel.response(gr_fancy_rownames_1NA)), row.names(pGrunfeld_1NA[-line_no, ])))) stop("original rownames not preserved in names(pmodel.response(plm_obj))")
  if(!isTRUE(all.equal(names(residuals(gr_fancy_rownames_1NA)),       row.names(pGrunfeld_1NA[-line_no, ])))) stop("original rownames not preserved in names(residuals(plm_obj))")
  
  # Should be TRUE
  # [199 + 1 == 200]
  # [199     == 199]
  if(!isTRUE(length(residuals(gr_fancy_rownames_1NA)) + 1 == length(row.names(pGrunfeld_1NA)))) stop("length not correct")
  if(!isTRUE(length(residuals(gr_fancy_rownames_1NA))     == length(row.names(pGrunfeld_1NA[-line_no, ])))) stop("length not correct")



# test as.data.frame.pdata.frame
row.names(as.data.frame(pGrunfeld))
row.names(as.data.frame(pGrunfeld, row.names = NULL))  # NULL is default, same as FALSE
row.names(as.data.frame(pGrunfeld, row.names = FALSE)) # same as NULL
row.names(as.data.frame(pGrunfeld, row.names = TRUE))  # fancy row names are added

# test data.frame - argument row.names has a different meaning here compared to as.data.frame.pdata.frame
row.names(data.frame(pGrunfeld))
row.names(data.frame(pGrunfeld, row.names = c(1:nrow(pGrunfeld))))
# row.names(data.frame(pGrunfeld, row.names = TRUE))  # error due to diff. meaning
# row.names(data.frame(pGrunfeld, row.names = FALSE)) # error due to diff. meaning
