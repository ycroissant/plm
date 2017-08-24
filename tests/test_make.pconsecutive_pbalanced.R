##### Testing of make.pconsecutive.* #####
#####     and of make.pbalanced.*    #####
#####
## in separate file test_is.pconsecutive.R: test of is.pconsecutive.*


library(plm)


##################### test of make.pconsecutive.indexes #####################
# (non-exported function)

# make.pconsecutive.indexes: for data.frames
data("Grunfeld", package = "plm")
drop_for_unbalanced <- c(2,42,56,78)
unbalanced_Grunfeld <- Grunfeld[-drop_for_unbalanced, ]
gindex <- plm:::make.pconsecutive.indexes(unbalanced_Grunfeld, index = c("firm", "year"))[[1]]
nrow(gindex)
all.equal(unbalanced_Grunfeld[, 1:2], gindex[-drop_for_unbalanced, ])
#compare::compare(unbalanced_Grunfeld[, 1:2], gindex[-drop_for_unbalanced, ], allowAll = TRUE)

if (!identical(unbalanced_Grunfeld[, 1:2], gindex[-drop_for_unbalanced, ])) stop("failure data.frame")
if (!isTRUE(all.equal(unbalanced_Grunfeld[, 1:2], gindex[-drop_for_unbalanced, ]))) stop("failure data.frame")


# class(unbalanced_Grunfeld[, 2])
# class(gindex[-drop_for_unbalanced, 2])
# 
# typeof(unbalanced_Grunfeld[, 2])
# typeof(gindex[-drop_for_unbalanced, 2])
# 
# attr(unbalanced_Grunfeld, "row.names")
# attr(gindex, "row.names")

# make.pconsecutive.indexes: for pdata.frames
punbalanced_Grunfeld <- pdata.frame(unbalanced_Grunfeld)
pgindex <- plm:::make.pconsecutive.indexes(punbalanced_Grunfeld, index = c("firm", "year"))[[1]]
nrow(pgindex)

if (!identical(attr(punbalanced_Grunfeld, "index")[, 1:2], pgindex[-drop_for_unbalanced, ])) stop("failure index of pdata.frame")
if (!isTRUE(all.equal(attr(punbalanced_Grunfeld, "index")[, 1:2], pgindex[-drop_for_unbalanced, ]))) stop("failure index of pdata.frame")
#compare::compare(attr(punbalanced_Grunfeld, "index")[, 1:2], pgindex[-drop_for_unbalanced, ], allowAll = TRUE)



# class(pgindex[-drop_for_unbalanced, ])
# mode(attr(unbalanced_Grunfeld[, 1:2], "row.names"))
# mode(attr(gindex[-drop_for_unbalanced, ], "row.names"))
# class(row.names(gindex[-drop_for_unbalanced, ]))



# make.pconsecutive.indexes: pseries interface
pgindex <- plm:::make.pconsecutive.indexes(punbalanced_Grunfeld$inv, index = c("firm", "year"))[[1]]
if (!identical(attr(punbalanced_Grunfeld$inv, "index")[, 1:2], pgindex[-drop_for_unbalanced, ])) stop("failure index of pdata.frame")
if (!isTRUE(all.equal(attr(punbalanced_Grunfeld$inv, "index")[, 1:2], pgindex[-drop_for_unbalanced, ]))) stop("failure index of pdata.frame")
##################### END test of make.pconsecutive.indexes #####################


##################### test of make.pconsecutive methods (various interfaces) #####################

### create some easy test data with some leading and trailing NAs ###
df_consec <- data.frame(id   = c(1, 1, 1, 2, 2, 3, 3, 3),
                        time = c(1, 2, 3, 1, 2, 1, 2, 3),
                        value = c("a", "b", "c", "d", "e", "f", "g", "h"))

df_first_t_NA <- data.frame(id   = c(1,  1, 1, 2, 2, 3, 3, 3),
                            time = c(NA, 2, 3, 1, 2, 1, 2, 3),
                            value = c("a", "b", "c", "d", "e", "f", "g", "h"))

df_first_t_NA2 <- data.frame(id   = c(1, 1, 1, 2,  2, 3, 3, 3),
                             time = c(1, 2, 3, NA, 2, 1, 2, 3),
                             value = c("a", "b", "c", "d", "e", "f", "g", "h"))

df_last_t_NA <- data.frame(id   = c(1,  1, 1,  2, 2, 3, 3, 3),
                           time = c(1,  2, NA, 1, 2, 1, 2, 3),
                           value = c("a", "b", "c", "d", "e", "f", "g", "h"))

df_first_last_t_NA <- data.frame(id  = c(1,  1, 1,  2, 2, 3, 3, 3),
                                time = c(NA, 2, NA, 1, 2, 1, 2, NA),
                                value = c("a", "b", "c", "d", "e", "f", "g", "h"))

pdf_consec          <- pdata.frame(df_consec)
pdf_first_t_NA      <- pdata.frame(df_first_t_NA)
pdf_first_t_NA2     <- pdata.frame(df_first_t_NA2)
pdf_last_t_NA       <- pdata.frame(df_last_t_NA)
pdf_first_last_t_NA <- pdata.frame(df_first_last_t_NA)


if (!identical(make.pconsecutive(pdf_consec$value), pdf_consec$value))
  stop("failure for already consecutive pseries: result is not identical to input")

if (!all(names(make.pconsecutive(pdf_first_t_NA$value)) == c("1-2", "1-3", "2-1", "2-2", "3-1", "3-2", "3-3")))
  stop("failure for leading NA")

if (!all(names(make.pconsecutive(pdf_first_t_NA2$value)) == c("1-1", "1-2", "1-3", "2-2", "3-1", "3-2", "3-3")))
  stop("failure for leading NA")

if (!all(names(make.pconsecutive(pdf_last_t_NA$value)) == c("1-1", "1-2", "2-1", "2-2", "3-1", "3-2", "3-3")))
  stop("failure for last NA")

if (!all(names(make.pconsecutive(pdf_first_last_t_NA$value)) == c("1-2", "2-1" , "2-2", "3-1", "3-2")))
  stop("failure for first/last NA pattern")


## some missing periods
data("Grunfeld", package = "plm")

Grunfeld_missing_periods_one_id <- Grunfeld[-c(2,6,7), ]
pGrunfeld_missing_periods_one_id <- pdata.frame(Grunfeld_missing_periods_one_id)
pinv_missing_periods <- pGrunfeld_missing_periods_one_id$inv

multi_periods <- c(2,6,7,22,23,56) # may not be at the first or last pos for an id, otherwise this line cannot be restored
Grunfeld_missing_periods_multi_id <- Grunfeld[-multi_periods, ]
pGrunfeld_missing_periods_multi_id <- pdata.frame(Grunfeld_missing_periods_multi_id)
pinv_missing_periods_multi_id <- pGrunfeld_missing_periods_multi_id$inv

#### test of pseries interface  ####
if (!all(is.pconsecutive(make.pconsecutive(pinv_missing_periods)))) stop("failure")
if (!nrow(attr(make.pconsecutive(pinv_missing_periods), "index")) == 200) stop("wrong index")
if (!all(class(make.pconsecutive(pinv_missing_periods)) == class(pinv_missing_periods))) stop("wrong class")

# test for fancy_rownames
names(make.pconsecutive(pinv_missing_periods)) # names should be "fancy"

pGrunfeld_missing_periods_one_id_wo_fancy_rownames <- pdata.frame(Grunfeld_missing_periods_one_id, row.names = FALSE)
pinv_missing_periods_wo_fancy_rownames <- pGrunfeld_missing_periods_one_id_wo_fancy_rownames$inv
# should not be "fancy" but sequence of numbers
if (!all(as.numeric(names(make.pconsecutive(pinv_missing_periods_wo_fancy_rownames))) == c(1:200)))
  stop("fancy rownames test failed")


### test of data.frame interface ####

# index vars automatically taken from columns 1,2, as no index arg specified
a <- make.pconsecutive(Grunfeld_missing_periods_one_id)
all.equal(a[-c(2,6,7), ], Grunfeld[-c(2,6,7), ])
identical(a[-c(2,6,7), ], Grunfeld[-c(2,6,7), ])
if (!identical(a[-c(2,6,7), ], Grunfeld[-c(2,6,7), ])) stop("data.frame interface: non identical results")

b <- make.pconsecutive(Grunfeld_missing_periods_multi_id)
all.equal(b[-multi_periods, ], Grunfeld[-multi_periods, ])
identical(b[-multi_periods, ], Grunfeld[-multi_periods, ])
if (!identical(b[-multi_periods, ], Grunfeld[-multi_periods, ])) stop("data.frame interface: non identical results")

# place index vars at other positions, specify index arg
permutate_cols <- c(3, 1, 4, 5, 2)
Grunfeld_missing_periods_multi_id_other_pos_index <- Grunfeld_missing_periods_multi_id[ , permutate_cols]

d <- make.pconsecutive(Grunfeld_missing_periods_multi_id_other_pos_index, index = c("firm", "year"))
all.equal(d[-multi_periods, ], Grunfeld_missing_periods_multi_id_other_pos_index)
identical(d[-multi_periods, ], Grunfeld_missing_periods_multi_id_other_pos_index)
if (!identical(d[-multi_periods, ], Grunfeld_missing_periods_multi_id_other_pos_index)) stop("data.frame interface: non identical results")

### test of pdata.frame interface
f <- pdata.frame(Grunfeld_missing_periods_multi_id, index = c("firm", "year"), drop.index = FALSE)
f_without_indexvars <- pdata.frame(Grunfeld_missing_periods_multi_id, index = c("firm", "year"), drop.index = TRUE)

f_consec <- make.pconsecutive(f)
f_without_indexvars_consec <- make.pconsecutive(f_without_indexvars)

# it seems like it is not possible here to check for equality of subsetted pdata.frames because
# the subsetting functions for pdata.frame alters the pdata.frame
# (this seems due to the fact that, currently, pdata.frames when created do not have
#  "pseries" in columns and carry no index attribute. Only after extracting a column, that column
#  will be of class c(“pseries”, “original_class”) and carry an index attribute.
# 
# To see this, use lapply (to avoid extraction):
# df <- data.frame(id = c(1,1,2), time = c(1,2,1), f = factor(c("a", "a", "b")), n = c(1:3))
# pdf <- pdata.frame(df)
# lapply(df, class)
# lapply(pdf, class)
# 
# lapply(df, attributes)
# lapply(pdf, attributes)



all.equal(f, f_consec[-multi_periods, ])
all.equal(f, f_consec[-multi_periods, ], check.attributes = F)
identical(f, f_consec[-multi_periods, ])
if (!identical(f, f_consec[-multi_periods, ])) stop("make.pconsecutive pdata.frame interface: non identical results")

all.equal(f_without_indexvars, f_without_indexvars_consec[-multi_periods, ])
identical(f_without_indexvars, f_without_indexvars_consec[-multi_periods, ])
if (!identical(f_without_indexvars, f_without_indexvars_consec[-multi_periods, ])) stop("pdata.frame interface: non identical results")


if (!isTRUE(all.equal(f, f_consec[-multi_periods, ], check.attributes = FALSE))) stop("pdata.frame interface: non all.equal results")
if (!isTRUE(all.equal(f_without_indexvars, f_without_indexvars_consec[-multi_periods, ], check.attributes = FALSE))) stop("pdata.frame interface: non all.equal results")



##### test for consecutive and at the same time balanced:
unbalanced_Grunfeld2 <- Grunfeld[-c(1, 41, 42, 79), ] # due to missing first time periods for some individuals, 
                                                      # simply making it consecutive is not possible, because the 
                                                      # periods cannot be infered
punbalanced_Grunfeld2 <- pdata.frame(unbalanced_Grunfeld2)
if (!nrow(make.pconsecutive(unbalanced_Grunfeld2, index = c("firm", "year"), balanced = TRUE)) == 200) stop("not balanced")
if (!pdim(make.pconsecutive(unbalanced_Grunfeld2, index = c("firm", "year"), balanced = TRUE))$balanced) stop("not balanced")

if (!nrow(make.pconsecutive(punbalanced_Grunfeld2, index = c("firm", "year"), balanced = TRUE)) == 200) stop("not balanced")
if (!pdim(make.pconsecutive(punbalanced_Grunfeld2, index = c("firm", "year"), balanced = TRUE))$balanced) stop("not balanced")

# for pseries
if (length(make.pconsecutive(punbalanced_Grunfeld2$inv, balanced = TRUE)) != 200) stop("not balanced")



######## test make.pbalanced #########
delte_2nd_period_and_3rd_for_id1 <- c(c(2, 2 + 20*c(1:9)), 3)
Grunfeld_wo_2nd_period_and_3rd_for_id1 <- Grunfeld[-delte_2nd_period_and_3rd_for_id1, ]
pGrunfeld_wo_2nd_period_and_3rd_for_id1 <- pdata.frame(Grunfeld_wo_2nd_period_and_3rd_for_id1)
nrow(Grunfeld_wo_2nd_period_and_3rd_for_id1)

# data.frame
if (!nrow(make.pbalanced(Grunfeld_wo_2nd_period_and_3rd_for_id1))        == 190) stop("failure make.pbalanced data.frame")
# pdata.frame and its index
if (!nrow(make.pbalanced(pGrunfeld_wo_2nd_period_and_3rd_for_id1))       == 190) stop("failure make.pbalanced pdata.frame")
if (!nrow(attr(make.pbalanced(pGrunfeld_wo_2nd_period_and_3rd_for_id1), "index")) == 190) stop("failure make.pbalanced pdata.frame's index")
# pseries and its index
if (!length(make.pbalanced(pGrunfeld_wo_2nd_period_and_3rd_for_id1$inv)) == 190) stop("failure make.pbalanced pseries")
if (!nrow(attr(make.pbalanced(pGrunfeld_wo_2nd_period_and_3rd_for_id1$inv), "index")) == 190) stop("failure make.pbalanced pseries' index")


## make.pbalanced with balance.type = "shared.times":
# 2 periods deleted -> 180 rows/entries left in (p)data.frame/pseries

  # data.frame
  if (!nrow(make.pbalanced(Grunfeld_wo_2nd_period_and_3rd_for_id1, balance.type = "shared.times") == 180)) stop("failure make.pbalanced, balance.type = \"shared.times\") data.frame")
  # pdata.frame
  if (!nrow(make.pbalanced(pGrunfeld_wo_2nd_period_and_3rd_for_id1, balance.type = "shared.times") == 180)) stop("failure make.pbalanced, balance.type = \"shared.times\") pdata.frame")
  if (!nrow(attr(make.pbalanced(pGrunfeld_wo_2nd_period_and_3rd_for_id1, balance.type = "shared.times"), "index")) == 180) stop("failure make.pbalanced, balance.type = \"shared.times\") pdata.frame's index")
  # pseries
  if (!length(make.pbalanced(pGrunfeld_wo_2nd_period_and_3rd_for_id1$inv, balance.type = "shared.times")) == 180) stop("failure make.pbalanced(, balance.type = \"shared.times\") pseries")
  if (!nrow(attr(make.pbalanced(pGrunfeld_wo_2nd_period_and_3rd_for_id1$inv, balance.type = "shared.times"), "index")) == 180) stop("failure make.pbalanced pseries' index")

# delete one (but different) period per id -> upper half of years (1945 to 1953) should be left
delete_1_per_id_half <- c(1, 22, 43, 64, 85, 106, 127, 148, 169, 190)
#split(Grunfeld[-delete_1_per_id_half, ]$year, Grunfeld[-delete_1_per_id_half, ]$firm) # inspect structure

if (!nrow(make.pbalanced(Grunfeld[-delete_1_per_id_half, ], balance.type = "shared.times") == 100)) stop("failure make.pbalanced, balance.type = \"shared.times\") data.frame")
if (!all(unique(make.pbalanced(Grunfeld[-delete_1_per_id_half, ], balance.type = "shared.times")$year) == c(1945:1954))) stop("wrong years")

# delete two (but different) periods per id -> none should be left -> data frame with 0 rows
delete_2_per_id_all <- c(1, 20, 22, 39, 43, 58, 64, 77, 85, 96, 106, 115, 127, 134, 148, 153, 169, 172, 190, 191)
#split(Grunfeld[-delete_2_per_id_all, ]$year, Grunfeld[-delete_2_per_id_all, ]$firm) # inspect structure
if (!nrow(make.pbalanced(Grunfeld[-delete_2_per_id_all, ], balance.type = "shared.times")) == 0) stop("failure make.pbalanced, balance.type = \"shared.times\") data.frame")



############## check that no additional individuals or times were introduced
# (because making it balanced does not introduce time periods
# which are not present for at least one individual)
# 

# pdata.frame and pseries: this is checking for new factor levels
# data.frame: check for unique values

#### pdata.frame
if (!all(levels(make.pbalanced(pGrunfeld_wo_2nd_period_and_3rd_for_id1)$year) == levels(pGrunfeld_wo_2nd_period_and_3rd_for_id1$year)))
  stop("failure pdata.frame: factor levels for time periods do not match")

# test: no new levels in index:
if (!all(levels(attr(make.pbalanced(pGrunfeld_wo_2nd_period_and_3rd_for_id1), "index")[[2]]) == levels(pGrunfeld_wo_2nd_period_and_3rd_for_id1$year)))
  stop("failure pdata.frame: factor levels for time periods in index do not match")

# for pdata.frame without index vars as columns
pGrunfeld_wo_2nd_period_and_3rd_for_id1_no_index <- pdata.frame(Grunfeld_wo_2nd_period_and_3rd_for_id1, drop.index = TRUE)
if (!all(levels(make.pbalanced(pGrunfeld_wo_2nd_period_and_3rd_for_id1_no_index)$year) == levels(pGrunfeld_wo_2nd_period_and_3rd_for_id1_no_index$year)))
  stop("failure pdata.frame: factor levels for time periods do not match")

# test: no new levels in index:
if (!all(levels(attr(make.pbalanced(pGrunfeld_wo_2nd_period_and_3rd_for_id1_no_index), "index")[[2]]) == levels(pGrunfeld_wo_2nd_period_and_3rd_for_id1_no_index$year)))
  stop("failure pdata.frame: factor levels for time periods in index do not match")

#### pseries
# (only need to test index for pseries): no new levels in index 
if (!all(levels(attr(make.pbalanced(pGrunfeld_wo_2nd_period_and_3rd_for_id1_no_index$value), "index")[[2]]) == levels(pGrunfeld_wo_2nd_period_and_3rd_for_id1_no_index$year)))
  stop("failure for pseries: factor levels for time periods in index do not match")

#### data.frame
# check that no additional values for individuals were introduced
if (!all(sort(unique(make.pbalanced(Grunfeld_wo_2nd_period_and_3rd_for_id1)$firm)) == sort(unique(Grunfeld_wo_2nd_period_and_3rd_for_id1$firm))))
  stop("failure for data.frame: unique individuals in index do not match")
# check that no additional values for time were introduced
if (!all(sort(unique(make.pbalanced(Grunfeld_wo_2nd_period_and_3rd_for_id1)$year)) == sort(unique(Grunfeld_wo_2nd_period_and_3rd_for_id1$year))))
  stop("failure for data.frame: unique time periods in index do not match")

######## END test make.pbalanced #########



### messy data with various NA patterns ###
#
## commented because needs package 'haven' and data need to be loaded from web
# library(haven)
# nlswork_r8 <- haven::read_dta("http://www.stata-press.com/data/r8/nlswork.dta")
# # remove attributes added by haven
# nlswork_r8 <- as.data.frame(lapply(nlswork_r8, function(x) {attr(x, "label") <- NULL; x}))
# pnlswork_r8 <- pdata.frame(nlswork_r8, index=c("idcode", "year"), drop.index=F)
# nlswork_r8$here_before <- TRUE
# 
# 
# length(unique(pnlswork_r8$year)) # == 15
# unique(pnlswork_r8$year)         # years missing: 74, 76, 79, 81, 84, 86 (# = 6)
#                                  # => 15 + 6 = 21
# 
# ### test of pseries interface
# 
# age_consec <- make.pconsecutive(pnlswork_r8$age)
# if (!(all(is.pconsecutive(age_consec)))) stop("failure")
# 
# 
# length(age_consec)
# length(index(age_consec)[[1]])
# length(index(age_consec)[[2]])
# 
# ### test of data.frame interface
# df_nlswork_r8_consec <- make.pconsecutive(nlswork_r8)
# 
# if (!all(is.pconsecutive(df_nlswork_r8_consec))) stop("failure")
# if (!nrow(df_nlswork_r8_consec) == 52365) stop("failure")
# 
# # make temp original data with row.names so that identical can return TRUE
# # otherwise it cannot be TRUE because new row.names were introduced and row.names
# # are a consecutive series (rownames == row numbers) in the original data
# # see how the output of all.equal diverges
# rows_there_before <- df_nlswork_r8_consec$here_before & !is.na(df_nlswork_r8_consec$here_before)
# all.equal(df_nlswork_r8_consec[rows_there_before, ], nlswork_r8)
# 
# nlswork_r8_comparison <- nlswork_r8
# attr(nlswork_r8_comparison, "row.names") <- attr(df_nlswork_r8_consec[rows_there_before, ], "row.names")
# 
# if (!identical(df_nlswork_r8_consec[rows_there_before, ],nlswork_r8_comparison)) stop("data.frame: not identical")
# 
# if (!identical(typeof(attr(nlswork_r8, "row.names")), typeof(attr(df_nlswork_r8_consec, "row.names"))))
#   stop("wrong typeof of attribute 'row.names'")
# 
# ### test of pdata.frame interface
# pdf_pnlswork_r8_consec <- make.pconsecutive(pnlswork_r8)
# 
# if (!all(is.pconsecutive(pdf_pnlswork_r8_consec))) stop("failure")
# if (!nrow(pdf_pnlswork_r8_consec) == 52365) stop("failure")
# 
# # same row.names adoption necessary as for data.frame
# pnlswork_r8_comparison  <- pnlswork_r8
# pdf_pnlswork_r8_consec_rows_there_before <- pdf_pnlswork_r8_consec[rows_there_before, ]
# attr(attr(pnlswork_r8_comparison, "index"), "row.names") <- attr(attr(pdf_pnlswork_r8_consec_rows_there_before, "index"), "row.names")
# # as the index vars are in the pdata.frame: added levels are not to be dropped; thus: adapt here to enable comparison
# pdf_pnlswork_r8_consec_rows_there_before$idcode <- droplevels(pdf_pnlswork_r8_consec_rows_there_before$idcode)
# pdf_pnlswork_r8_consec_rows_there_before$year <- droplevels(pdf_pnlswork_r8_consec_rows_there_before$year)
# 
# length(levels(pdf_pnlswork_r8_consec_rows_there_before$year))
# 
# all.equal(pdf_pnlswork_r8_consec_rows_there_before, pnlswork_r8_comparison)
# if (!identical(pdf_pnlswork_r8_consec_rows_there_before, pnlswork_r8_comparison)) stop("pdata.frame: not identical")
# 
# 
# 
# dims_consec <- pdim(pdf_pnlswork_r8_consec)
# min(dims_consec$Tint$Ti) # 1
# max(dims_consec$Tint$Ti) # 21 = 15 + 6
# dims_consec$Tint$nt # => ok! (not all the same years for each individual, because just consecutive, not balanced)
# 
# # 15 + 6 == 21
# if (!length(unique(index(pdf_pnlswork_r8_consec)[[2]])) == 21) stop("failure")
# #  years 68 to 88 need to be present (each year needs to be present)
# if (!all(levels(attr(pdf_pnlswork_r8_consec, "index")[[2]]) == factor(68:88))) stop("failure")
# 
# 
# # test argument balanced on this data set
# pdf_pnlswork_r8_consec_bal <- make.pconsecutive(pnlswork_r8, balanced = TRUE)
# dims_consec_bal <- pdim(pdf_pnlswork_r8_consec_bal)
# # need to have same numer of obs per year (because balanced)
# if (!all(dims_consec_bal$Tint$nt[1] == dims_consec_bal$Tint$nt)) stop("failure for argument balanced")
# if (!nrow(pdf_pnlswork_r8_consec_bal) == 98931) stop("failure: nrow not correct")
# if (!dims_consec_bal$balanced) stop("failure: not balanced")
# 
# ## test of only making it balanced, but not consecutive
# nlswork_r8_bal  <- make.pbalanced(nlswork_r8)  # data.frame
# pnlswork_r8_bal <- make.pbalanced(pnlswork_r8) # pdata.frame
# 
# if (!all(sort(unique(nlswork_r8$year)) == sort(unique(nlswork_r8_bal$year)))) stop("data.frame: times do not match")
# if (!all(levels(pnlswork_r8$year)      == levels(pnlswork_r8_bal$year))) stop("pdata.frame: times do not match")



########### compare results to statar ########################
# devtools::install_github("matthieugomez/statar")
# library(tidyr)
# library(dplyr)



########### compare to tidyr ##########
## commented because it requires a separate package
# 
## make panel balanced by inserting NAs
## note: this is a good bit faster than make.psconsective(, balanced = TRUE)

# nlswork_r8_no_NA <- tidyr::complete(nlswork_r8, idcode, year = as.integer(tidyr::full_seq(year, 1)))
# 
# tidyr::full_seq(c(1, 2, 4, 5, 10), 1)
# tidyr::full_seq(c(1, 2, 4, 5, 10), 2) # error: not a regular sequence
# tidyr::full_seq(c( 2, 4, 6, 10), 2)
# pnlswork_r8_no_NA <- pdata.frame(nlswork_r8_no_NA, index=c("idcode", "year"), drop.index=F)
# 
# 
# all(is.pconsecutive(pnlswork_r8_no_NA))
# 
# pdim_tidyr <- pdim(pnlswork_r8_no_NA)
# 
# min(dims$Tint$Ti)
# max(dims$Tint$Ti)
# 
# pdim(pnlswork_r8_no_NA)
# anyNA(pnlswork_r8_no_NA$year)

