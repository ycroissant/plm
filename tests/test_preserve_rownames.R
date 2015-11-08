## testfile for preserving row.names in plm_object$model and as.data.frame


library(plm)
data("Grunfeld", package = "plm")

row.names(Grunfeld) # row.names are a sequence 1:nrow(Grunfeld) originally
gr <- plm(inv ~ value + capital, data=Grunfeld, model="pooling")
# same row.names and names as before estimation [i. e. original row.names are preserved]
all.equal(row.names(gr$model), row.names(Grunfeld))         # TRUE
all.equal(row.names(model.frame(gr)), row.names(Grunfeld))  # TRUE
all.equal(names(pmodel.response(gr)), row.names(Grunfeld))  # TRUE


# make a pdata.frame with "fancy" row.names (default)
# [i.e. combination of individual index an time index]
pGrunfeld <- pdata.frame(Grunfeld, index = c("firm", "year"))
row.names(pGrunfeld) # fancy row.names
gr_fancy_rownames <- plm(inv ~ value + capital, data=pGrunfeld, model="pooling")

# original row.names of pGrunfeld (i.e. "fancy" row.names) are preserved
all.equal(row.names(gr_fancy_rownames$model), row.names(pGrunfeld))         # TRUE
all.equal(row.names(model.frame(gr_fancy_rownames)), row.names(pGrunfeld))  # TRUE
all.equal(names(pmodel.response(gr_fancy_rownames)), row.names(pGrunfeld))  # TRUE


# test as.data.frame.pdata.frame
row.names(as.data.frame(pGrunfeld))
row.names(as.data.frame(pGrunfeld, row.names = NULL))  # NULL is default, same as FALSE
row.names(as.data.frame(pGrunfeld, row.names = FALSE)) # same as NULL
row.names(as.data.frame(pGrunfeld, row.names = TRUE))  # fancy row names are added

# test data.frame - argument row.names has a different meaning here compared to as.data.frame.pdata.frame
row.names(data.frame(pGrunfeld))
row.names(data.frame(pGrunfeld, row.names = c(1:nrow(pGrunfeld))))
row.names(data.frame(pGrunfeld, row.names = TRUE))  # error due to diff. meaning
row.names(data.frame(pGrunfeld, row.names = FALSE)) # error due to diff. meaning
