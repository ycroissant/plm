## Tests equivalence of collapse to base-R version of transformation functions
## B/between, Within, Sum.

## Run tests only if package 'collapse', 'fixest', and 'lfe' are available
## (as they are 'Suggests' dependencies)
collapse.avail <- if (!requireNamespace("collapse", quietly = TRUE)) FALSE else TRUE
fixest.avail   <- if (!requireNamespace("fixest",   quietly = TRUE)) FALSE else TRUE
lfe.avail      <- if (!requireNamespace("lfe",      quietly = TRUE)) FALSE else TRUE


if(collapse.avail && fixest.avail && lfe.avail) {

# data
library("plm")
# library("collapse")

#### set up test input data ####
data("wlddev", package = "collapse")
class(wlddev)
pwlddev <- pdata.frame(wlddev, index = c("iso3c", "year"))
PCGDP <- pwlddev$PCGDP
LIFEEX <- pwlddev$LIFEEX
rm.rows <- c(1:10,50:88, 1000:1030, 10000:10111)
pwlddev_unbal <-pwlddev[-rm.rows, ]
LIFEEX_unbal <- pwlddev_unbal[ , "LIFEEX"]

l.na <- is.na(LIFEEX_unbal)
LIFEEX_unbal_wona <- LIFEEX_unbal[!l.na]
i <- index(LIFEEX_unbal)[!l.na, ]
LIFEEX_unbal_wona <- plm:::add_pseries_features(LIFEEX_unbal_wona, i)

mat <- as.matrix(wlddev[ , 8:12])
attr(mat, "index") <- index(pwlddev)

mat_unbal <- as.matrix(wlddev[-rm.rows , 8:12])
attr(mat_unbal, "index") <- index(pwlddev_unbal)

mat_noindex <- mat
attr(mat_noindex, "index") <- NULL
mat_index <- attr(mat, "index")

mat_noindex_unbal <- mat[-rm.rows , ]
attr(mat_noindex_unbal, "index") <- NULL
mat_index_unbal <- attr(mat, "index")[-rm.rows, ]
## NB: mat_index_unbal has 215 entries but 216 levels for individual dimension
##    -> by intention, for corner case check in between when no index matrix is used
# length(unique(mat_index_unbal[[1]])) 215
# length(levels(mat_index_unbal[[1]])) # 216

wlddev_unbal_wona <- na.omit(wlddev[ , 8:12])
mat_unbal_wona <- as.matrix(wlddev_unbal_wona)
attr(mat_unbal_wona, "index") <- index(pwlddev[-attr(wlddev_unbal_wona, "na.action"), ])


wlddev_bal_wona <- na.omit(wlddev[wlddev$iso3c %in% c("ARG", "BLR", "CHN", "COL") , c(2, 4, 8:12)])
pwlddev_bal_wona <- pdata.frame(wlddev_bal_wona, index = c("iso3c", "year"))
pwlddev_bal_wona <- make.pbalanced(pwlddev_bal_wona, balance.type = "shared.times")
pdim(pwlddev_bal_wona)
mat_bal_wona <- as.matrix(pwlddev_bal_wona[ , 3:7])
attr(mat_bal_wona, "index") <- index(pwlddev_bal_wona)





#### Sum - default ####

# individual
options("plm.fast" = FALSE)

S1_d_ind      <- plm:::Sum.default(as.numeric(LIFEEX), effect = index(LIFEEX)[[1L]]) # default
S1_d_ind_narm <- plm:::Sum.default(as.numeric(LIFEEX), effect = index(LIFEEX)[[1L]], na.rm = TRUE)

S1_d_ind_unbal      <- plm:::Sum.default(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[1L]]) # default
S1_d_ind_narm_unbal <- plm:::Sum.default(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[1L]], na.rm = TRUE)

options("plm.fast" = TRUE)

S2_d_ind_unbal      <- plm:::Sum.default(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[1L]]) # default
S2_d_ind_narm_unbal <- plm:::Sum.default(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[1L]], na.rm = TRUE)

S2_d_ind      <- plm:::Sum.default(as.numeric(LIFEEX), effect = index(LIFEEX)[[1L]]) # default
S2_d_ind_narm <- plm:::Sum.default(as.numeric(LIFEEX), effect = index(LIFEEX)[[1L]], na.rm = TRUE)

stopifnot(isTRUE(all.equal(S1_d_ind,            S2_d_ind,            check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(S1_d_ind_narm,       S2_d_ind_narm,       check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(S1_d_ind_unbal,      S2_d_ind_unbal,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(S1_d_ind_narm_unbal, S2_d_ind_narm_unbal, check.attributes = TRUE))) # TRUE

rm(
S1_d_ind,            S2_d_ind,
S1_d_ind_narm,       S2_d_ind_narm,
S1_d_ind_unbal,      S2_d_ind_unbal,
S1_d_ind_narm_unbal, S2_d_ind_narm_unbal)


# time
options("plm.fast" = FALSE)

S1_d_ti      <- plm:::Sum.default(as.numeric(LIFEEX), effect = index(LIFEEX)[[2L]]) # default
S1_d_ti_narm <- plm:::Sum.default(as.numeric(LIFEEX), effect = index(LIFEEX)[[2L]], na.rm = TRUE)

S1_d_ti_unbal      <- plm:::Sum.default(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[2L]]) # default
S1_d_ti_narm_unbal <- plm:::Sum.default(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[2L]], na.rm = TRUE)

options("plm.fast" = TRUE)

S2_d_ti      <- plm:::Sum.default(as.numeric(LIFEEX), effect = index(LIFEEX)[[2L]]) # default
S2_d_ti_narm <- plm:::Sum.default(as.numeric(LIFEEX), effect = index(LIFEEX)[[2L]], na.rm = TRUE)

S2_d_ti_unbal      <- plm:::Sum.default(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[2L]]) # default
S2_d_ti_narm_unbal <- plm:::Sum.default(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[2L]], na.rm = TRUE)

stopifnot(isTRUE(all.equal(S1_d_ti,            S2_d_ti,            check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(S1_d_ti_narm,       S2_d_ti_narm,       check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(S1_d_ti_unbal,      S2_d_ti_unbal,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(S1_d_ti_narm_unbal, S2_d_ti_narm_unbal, check.attributes = TRUE))) # TRUE

# be gentle with the memory
rm(
S1_d_ti,            S2_d_ti,
S1_d_ti_narm,       S2_d_ti_narm,
S1_d_ti_unbal,      S2_d_ti_unbal,
S1_d_ti_narm_unbal, S2_d_ti_narm_unbal)


#### Sum - pseries ####

# individual
options("plm.fast" = FALSE)

S1_ind      <- plm:::Sum.pseries(LIFEEX, effect = "individual") # default
S1_ind_narm <- plm:::Sum.pseries(LIFEEX, effect = "individual", na.rm = TRUE)

S1_ind_unbal      <- plm:::Sum.pseries(LIFEEX_unbal, effect = "individual") # default
S1_ind_narm_unbal <- plm:::Sum.pseries(LIFEEX_unbal, effect = "individual", na.rm = TRUE)

options("plm.fast" = TRUE)

S2_ind      <- plm:::Sum.pseries(LIFEEX, effect = "individual") # default
S2_ind_narm <- plm:::Sum.pseries(LIFEEX, effect = "individual", na.rm = TRUE)

S2_ind_unbal      <- plm:::Sum.pseries(LIFEEX_unbal, effect = "individual") # default
S2_ind_narm_unbal <- plm:::Sum.pseries(LIFEEX_unbal, effect = "individual", na.rm = TRUE)

stopifnot(isTRUE(all.equal(S1_ind,            S2_ind,            check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(S1_ind_narm,       S2_ind_narm,       check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(S1_ind_unbal,      S2_ind_unbal,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(S1_ind_narm_unbal, S2_ind_narm_unbal, check.attributes = TRUE))) # TRUE

rm(
S1_ind,            S2_ind,
S1_ind_narm,       S2_ind_narm,
S1_ind_unbal,      S2_ind_unbal,
S1_ind_narm_unbal, S2_ind_narm_unbal)


# time
options("plm.fast" = FALSE)

S1_ti      <- plm:::Sum.pseries(LIFEEX, effect = "time") # default
S1_ti_narm <- plm:::Sum.pseries(LIFEEX, effect = "time", na.rm = TRUE)

S1_ti_unbal      <- plm:::Sum.pseries(LIFEEX_unbal, effect = "time") # default
S1_ti_narm_unbal <- plm:::Sum.pseries(LIFEEX_unbal, effect = "time", na.rm = TRUE)

options("plm.fast" = TRUE)

S2_ti      <- plm:::Sum.pseries(LIFEEX, effect = "time") # default
S2_ti_narm <- plm:::Sum.pseries(LIFEEX, effect = "time", na.rm = TRUE)

S2_ti_unbal      <- plm:::Sum.pseries(LIFEEX_unbal, effect = "time") # default
S2_ti_narm_unbal <- plm:::Sum.pseries(LIFEEX_unbal, effect = "time", na.rm = TRUE)

stopifnot(isTRUE(all.equal(S1_ti,            S2_ti,            check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(S1_ti_narm,       S2_ti_narm,       check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(S1_ti_unbal,      S2_ti_unbal,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(S1_ti_narm_unbal, S2_ti_narm_unbal, check.attributes = TRUE))) # TRUE

rm(
S1_ti,            S2_ti,
S1_ti_narm,       S2_ti_narm,
S1_ti_unbal,      S2_ti_unbal,
S1_ti_narm_unbal, S2_ti_narm_unbal)


#### Sum - matrix ####

# individual
options("plm.fast" = FALSE)

S1_mat_ind      <- plm:::Sum.matrix(mat, effect = "individual") # default
S1_mat_ind_narm <- plm:::Sum.matrix(mat, effect = "individual", na.rm = TRUE)

S1_mat_no_index_ind      <- plm:::Sum.matrix(mat_noindex, effect = mat_index[[1L]]) # default
S1_mat_no_index_ind_narm <- plm:::Sum.matrix(mat_noindex, effect = mat_index[[1L]], na.rm = TRUE)

S1_mat_ind_unbal      <- plm:::Sum.matrix(mat_unbal, effect = "individual") # default
S1_mat_ind_narm_unbal <- plm:::Sum.matrix(mat_unbal, effect = "individual", na.rm = TRUE)

S1_mat_no_index_ind_unbal      <- plm:::Sum.matrix(mat_noindex_unbal, effect = mat_index_unbal[[1L]]) # default
S1_mat_no_index_ind_narm_unbal <- plm:::Sum.matrix(mat_noindex_unbal, effect = mat_index_unbal[[1L]], na.rm = TRUE)

options("plm.fast" = TRUE)

S2_mat_ind      <- plm:::Sum.matrix(mat, effect = "individual") # default
S2_mat_ind_narm <- plm:::Sum.matrix(mat, effect = "individual", na.rm = TRUE)

S2_mat_no_index_ind      <- plm:::Sum.matrix(mat_noindex, effect = mat_index[[1L]]) # default
S2_mat_no_index_ind_narm <- plm:::Sum.matrix(mat_noindex, effect = mat_index[[1L]], na.rm = TRUE)

S2_mat_ind_unbal      <- plm:::Sum.matrix(mat_unbal, effect = "individual") # default
S2_mat_ind_narm_unbal <- plm:::Sum.matrix(mat_unbal, effect = "individual", na.rm = TRUE)

S2_mat_no_index_ind_unbal      <- plm:::Sum.matrix(mat_noindex_unbal, effect = mat_index_unbal[[1L]]) # default
S2_mat_no_index_ind_narm_unbal <- plm:::Sum.matrix(mat_noindex_unbal, effect = mat_index_unbal[[1L]], na.rm = TRUE)

stopifnot(isTRUE(all.equal(S1_mat_ind,            S2_mat_ind,            check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(S1_mat_ind_narm,       S2_mat_ind_narm,       check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(S1_mat_ind_unbal,      S2_mat_ind_unbal,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(S1_mat_ind_narm_unbal, S2_mat_ind_narm_unbal, check.attributes = TRUE))) # TRUE

stopifnot(isTRUE(all.equal(S1_mat_no_index_ind,            S2_mat_no_index_ind,            check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(S1_mat_no_index_ind_narm,       S2_mat_no_index_ind_narm,       check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(S1_mat_no_index_ind_unbal,      S2_mat_no_index_ind_unbal,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(S1_mat_no_index_ind_narm_unbal, S2_mat_no_index_ind_narm_unbal, check.attributes = TRUE))) # TRUE

# time
options("plm.fast" = FALSE)

S1_mat_ti      <- plm:::Sum.matrix(mat, effect = "time") # default
S1_mat_ti_narm <- plm:::Sum.matrix(mat, effect = "time", na.rm = TRUE)

S1_mat_no_index_ti      <- plm:::Sum.matrix(mat_noindex, effect = mat_index[[2L]]) # default
S1_mat_no_index_ti_narm <- plm:::Sum.matrix(mat_noindex, effect = mat_index[[2L]], na.rm = TRUE)

S1_mat_ti_unbal      <- plm:::Sum.matrix(mat_unbal, effect = "time") # default
S1_mat_ti_narm_unbal <- plm:::Sum.matrix(mat_unbal, effect = "time", na.rm = TRUE)

S1_mat_no_index_ti_unbal      <- plm:::Sum.matrix(mat_noindex_unbal, effect = mat_index_unbal[[2L]]) # default
S1_mat_no_index_ti_narm_unbal <- plm:::Sum.matrix(mat_noindex_unbal, effect = mat_index_unbal[[2L]], na.rm = TRUE)

options("plm.fast" = TRUE)

S2_mat_ti      <- plm:::Sum.matrix(mat, effect = "time") # default
S2_mat_ti_narm <- plm:::Sum.matrix(mat, effect = "time", na.rm = TRUE)

S2_mat_no_index_ti      <- plm:::Sum.matrix(mat_noindex, effect = mat_index[[2L]]) # default
S2_mat_no_index_ti_narm <- plm:::Sum.matrix(mat_noindex, effect = mat_index[[2L]], na.rm = TRUE)

S2_mat_ti_unbal      <- plm:::Sum.matrix(mat_unbal, effect = "time") # default
S2_mat_ti_narm_unbal <- plm:::Sum.matrix(mat_unbal, effect = "time", na.rm = TRUE)

S2_mat_no_index_ti_unbal      <- plm:::Sum.matrix(mat_noindex_unbal, effect = mat_index_unbal[[2L]]) # default
S2_mat_no_index_ti_narm_unbal <- plm:::Sum.matrix(mat_noindex_unbal, effect = mat_index_unbal[[2L]], na.rm = TRUE)

stopifnot(isTRUE(all.equal(S1_mat_ti,                S2_mat_ti,               check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(S1_mat_ti_narm,           S2_mat_ti_narm,          check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(S1_mat_no_index_ti,       S2_mat_no_index_ti,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(S1_mat_no_index_ti_narm,  S2_mat_no_index_ti_narm, check.attributes = TRUE))) # TRUE

stopifnot(isTRUE(all.equal(S1_mat_ti_unbal,               S2_mat_ti_unbal,               check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(S1_mat_ti_narm_unbal,          S2_mat_ti_narm_unbal,          check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(S1_mat_no_index_ti_unbal,      S2_mat_no_index_ti_unbal,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(S1_mat_no_index_ti_narm_unbal, S2_mat_no_index_ti_narm_unbal, check.attributes = TRUE))) # TRUE

#### between/Between - default ####

# individual
options("plm.fast" = FALSE)

b1_d_ind      <- plm:::between.default(as.numeric(LIFEEX), effect = index(LIFEEX)[[1L]]) # default
b1_d_ind_narm <- plm:::between.default(as.numeric(LIFEEX), effect = index(LIFEEX)[[1L]], na.rm = TRUE)

b1_d_ind_unbal      <- plm:::between.default(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[1L]]) # default
b1_d_ind_narm_unbal <- plm:::between.default(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[1L]], na.rm = TRUE)

B1_d_ind      <- plm:::Between.default(as.numeric(LIFEEX), effect = index(LIFEEX)[[1L]]) # default
B1_d_ind_narm <- plm:::Between.default(as.numeric(LIFEEX), effect = index(LIFEEX)[[1L]], na.rm = TRUE)

B1_d_ind_unbal      <- plm:::Between.default(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[1L]]) # default
B1_d_ind_narm_unbal <- plm:::Between.default(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[1L]], na.rm = TRUE)

options("plm.fast" = TRUE)

b2_d_ind      <- plm:::between.default(as.numeric(LIFEEX), effect = index(LIFEEX)[[1L]]) # default
b2_d_ind_narm <- plm:::between.default(as.numeric(LIFEEX), effect = index(LIFEEX)[[1L]], na.rm = TRUE)

B2_d_ind      <- plm:::Between.default(as.numeric(LIFEEX), effect = index(LIFEEX)[[1L]]) # default
B2_d_ind_narm <- plm:::Between.default(as.numeric(LIFEEX), effect = index(LIFEEX)[[1L]], na.rm = TRUE)

b2_d_ind_unbal      <- plm:::between.default(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[1L]]) # default
b2_d_ind_narm_unbal <- plm:::between.default(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[1L]], na.rm = TRUE)

B2_d_ind_unbal      <- plm:::Between.default(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[1L]]) # default
B2_d_ind_narm_unbal <- plm:::Between.default(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[1L]], na.rm = TRUE)

stopifnot(isTRUE(all.equal(b1_d_ind,            b2_d_ind,            check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(b1_d_ind_narm,       b2_d_ind_narm,       check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(b1_d_ind_unbal,      b2_d_ind_unbal,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(b1_d_ind_narm_unbal, b2_d_ind_narm_unbal, check.attributes = TRUE))) # TRUE

stopifnot(isTRUE(all.equal(B1_d_ind,            B2_d_ind,            check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_d_ind_narm,       B2_d_ind_narm,       check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_d_ind_unbal,      B2_d_ind_unbal,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_d_ind_narm_unbal, B2_d_ind_narm_unbal, check.attributes = TRUE))) # TRUE

# time
options("plm.fast" = FALSE)

b1_d_ti      <- plm:::between.default(as.numeric(LIFEEX), effect = index(LIFEEX)[[2L]]) # default
b1_d_ti_narm <- plm:::between.default(as.numeric(LIFEEX), effect = index(LIFEEX)[[2L]], na.rm = TRUE)

b1_d_ti_unbal      <- plm:::between.default(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[2L]]) # default
b1_d_ti_narm_unbal <- plm:::between.default(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[2L]], na.rm = TRUE)

B1_d_ti      <- plm:::Between.default(as.numeric(LIFEEX), effect = index(LIFEEX)[[2L]]) # default
B1_d_ti_narm <- plm:::Between.default(as.numeric(LIFEEX), effect = index(LIFEEX)[[2L]], na.rm = TRUE)

B1_d_ti_unbal      <- plm:::Between.default(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[2L]]) # default
B1_d_ti_narm_unbal <- plm:::Between.default(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[2L]], na.rm = TRUE)

options("plm.fast" = TRUE)

b2_d_ti      <- plm:::between.default(as.numeric(LIFEEX), effect = index(LIFEEX)[[2L]]) # default
b2_d_ti_narm <- plm:::between.default(as.numeric(LIFEEX), effect = index(LIFEEX)[[2L]], na.rm = TRUE)

b2_d_ti_unbal      <- plm:::between.default(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[2L]]) # default
b2_d_ti_narm_unbal <- plm:::between.default(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[2L]], na.rm = TRUE)

B2_d_ti      <- plm:::Between.default(as.numeric(LIFEEX), effect = index(LIFEEX)[[2L]]) # default
B2_d_ti_narm <- plm:::Between.default(as.numeric(LIFEEX), effect = index(LIFEEX)[[2L]], na.rm = TRUE)

B2_d_ti_unbal      <- plm:::Between.default(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[2L]]) # default
B2_d_ti_narm_unbal <- plm:::Between.default(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[2L]], na.rm = TRUE)

stopifnot(isTRUE(all.equal(b1_d_ti,            b2_d_ti,            check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(b1_d_ti_narm,       b2_d_ti_narm,       check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(b1_d_ti_unbal,      b2_d_ti_unbal,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(b1_d_ti_narm_unbal, b2_d_ti_narm_unbal, check.attributes = TRUE))) # TRUE

stopifnot(isTRUE(all.equal(B1_d_ti,            B2_d_ti,            check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_d_ti_narm,       B2_d_ti_narm,       check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_d_ti_unbal,      B2_d_ti_unbal,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_d_ti_narm_unbal, B2_d_ti_narm_unbal, check.attributes = TRUE))) # TRUE

#### between/Between - pseries ####

options("plm.fast" = FALSE)

b1_ind      <- plm:::between.pseries(LIFEEX, effect = "individual") # default
b1_ind_narm <- plm:::between.pseries(LIFEEX, effect = "individual", na.rm = TRUE)

b1_ind_unbal      <- plm:::between.pseries(LIFEEX_unbal, effect = "individual") # default
b1_ind_unbal_narm <- plm:::between.pseries(LIFEEX_unbal, effect = "individual", na.rm = TRUE)

B1_ind      <- plm:::Between.pseries(LIFEEX, effect = "individual") # default
B1_ind_narm <- plm:::Between.pseries(LIFEEX, effect = "individual", na.rm = TRUE)

B1_ind_unbal      <- plm:::Between.pseries(LIFEEX_unbal, effect = "individual") # default
B1_ind_unbal_narm <- plm:::Between.pseries(LIFEEX_unbal, effect = "individual", na.rm = TRUE)

options("plm.fast" = TRUE)

b2_ind       <- plm:::between.pseries(LIFEEX, effect = "individual")
b2_ind_narm  <- plm:::between.pseries(LIFEEX, effect = "individual", na.rm = TRUE)

b2_ind_unbal       <- plm:::between.pseries(LIFEEX_unbal, effect = "individual")
b2_ind_unbal_narm  <- plm:::between.pseries(LIFEEX_unbal, effect = "individual", na.rm = TRUE)

B2_ind       <- plm:::Between.pseries(LIFEEX, effect = "individual")
B2_ind_narm  <- plm:::Between.pseries(LIFEEX, effect = "individual", na.rm = TRUE)

B2_ind_unbal       <- plm:::Between.pseries(LIFEEX_unbal, effect = "individual")
B2_ind_unbal_narm  <- plm:::Between.pseries(LIFEEX_unbal, effect = "individual", na.rm = TRUE)

stopifnot(isTRUE(all.equal(b1_ind,      b2_ind,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(b1_ind_narm, b2_ind_narm, check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_ind,      B2_ind,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_ind_narm, B2_ind_narm, check.attributes = TRUE))) # TRUE

stopifnot(isTRUE(all.equal(b1_ind_unbal,      b2_ind_unbal,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(b1_ind_unbal_narm, b2_ind_unbal_narm, check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_ind_unbal,      B2_ind_unbal,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_ind_unbal_narm, B2_ind_unbal_narm, check.attributes = TRUE))) # TRUE

# time
options("plm.fast" = FALSE)

b1_ti      <- plm:::between.pseries(LIFEEX, effect = "time") # default
b1_ti_narm <- plm:::between.pseries(LIFEEX, effect = "time", na.rm = TRUE)

b1_ti_unbal      <- plm:::between.pseries(LIFEEX_unbal, effect = "time") # default
b1_ti_unbal_narm <- plm:::between.pseries(LIFEEX_unbal, effect = "time", na.rm = TRUE)

B1_ti      <- plm:::Between.pseries(LIFEEX, effect = "time") # default
B1_ti_narm <- plm:::Between.pseries(LIFEEX, effect = "time", na.rm = TRUE)

B1_ti_unbal      <- plm:::Between.pseries(LIFEEX_unbal, effect = "time") # default
B1_ti_unbal_narm <- plm:::Between.pseries(LIFEEX_unbal, effect = "time", na.rm = TRUE)

options("plm.fast" = TRUE)

b2_ti       <- plm:::between.pseries(LIFEEX, effect = "time")
b2_ti_narm  <- plm:::between.pseries(LIFEEX, effect = "time", na.rm = TRUE)

b2_ti_unbal       <- plm:::between.pseries(LIFEEX_unbal, effect = "time")
b2_ti_unbal_narm  <- plm:::between.pseries(LIFEEX_unbal, effect = "time", na.rm = TRUE)

B2_ti       <- plm:::Between.pseries(LIFEEX, effect = "time")
B2_ti_narm  <- plm:::Between.pseries(LIFEEX, effect = "time", na.rm = TRUE)

B2_ti_unbal       <- plm:::Between.pseries(LIFEEX_unbal, effect = "time")
B2_ti_unbal_narm  <- plm:::Between.pseries(LIFEEX_unbal, effect = "time", na.rm = TRUE)

stopifnot(isTRUE(all.equal(b1_ti,      b2_ti,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(b1_ti_narm, b2_ti_narm, check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_ti,      B2_ti,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_ti_narm, B2_ti_narm, check.attributes = TRUE))) # TRUE

stopifnot(isTRUE(all.equal(b1_ti_unbal,      b2_ti_unbal,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(b1_ti_unbal_narm, b2_ti_unbal_narm, check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_ti_unbal,      B2_ti_unbal,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_ti_unbal_narm, B2_ti_unbal_narm, check.attributes = TRUE))) # TRUE

#### between/Between - matrix ####


# individual
options("plm.fast" = FALSE)

b1_mat_ind      <- plm:::between.matrix(mat, effect = "individual") # default
b1_mat_ind_narm <- plm:::between.matrix(mat, effect = "individual", na.rm = TRUE)

b1_mat_unbal_ind      <- plm:::between.matrix(mat_unbal, effect = "individual") # default
b1_mat_unbal_ind_narm <- plm:::between.matrix(mat_unbal, effect = "individual", na.rm = TRUE)

b1_mat_noindex_ind      <- plm:::between.matrix(mat_noindex, effect = mat_index[[1L]]) # default
b1_mat_noindex_ind_narm <- plm:::between.matrix(mat_noindex, effect = mat_index[[1L]], na.rm = TRUE)

b1_mat_noindex_unbal_ind      <- plm:::between.matrix(mat_noindex_unbal, effect = mat_index_unbal[[1L]]) # default
b1_mat_noindex_unbal_ind_narm <- plm:::between.matrix(mat_noindex_unbal, effect = mat_index_unbal[[1L]], na.rm = TRUE)

B1_mat_ind      <- plm:::Between.matrix(mat, effect = "individual") # default
B1_mat_ind_narm <- plm:::Between.matrix(mat, effect = "individual", na.rm = TRUE)

B1_mat_unbal_ind      <- plm:::Between.matrix(mat_unbal, effect = "individual") # default
B1_mat_unbal_ind_narm <- plm:::Between.matrix(mat_unbal, effect = "individual", na.rm = TRUE)

B1_mat_noindex_ind      <- plm:::Between.matrix(mat_noindex, effect = mat_index[[1L]]) # default
B1_mat_noindex_ind_narm <- plm:::Between.matrix(mat_noindex, effect = mat_index[[1L]], na.rm = TRUE)

B1_mat_noindex_unbal_ind      <- plm:::Between.matrix(mat_noindex_unbal, effect = mat_index_unbal[[1L]]) # default
B1_mat_noindex_unbal_ind_narm <- plm:::Between.matrix(mat_noindex_unbal, effect = mat_index_unbal[[1L]], na.rm = TRUE)

options("plm.fast" = TRUE)

b2_mat_ind       <- plm:::between.matrix(mat, effect = "individual")
b2_mat_ind_narm  <- plm:::between.matrix(mat, effect = "individual", na.rm = TRUE)

b2_mat_unbal_ind       <- plm:::between.matrix(mat_unbal, effect = "individual")
b2_mat_unbal_ind_narm  <- plm:::between.matrix(mat_unbal, effect = "individual", na.rm = TRUE)

b2_mat_noindex_ind      <- plm:::between.matrix(mat_noindex, effect = mat_index[[1L]]) # default
b2_mat_noindex_ind_narm <- plm:::between.matrix(mat_noindex, effect = mat_index[[1L]], na.rm = TRUE)

b2_mat_noindex_unbal_ind      <- plm:::between.matrix(mat_noindex_unbal, effect = mat_index_unbal[[1L]]) # default
b2_mat_noindex_unbal_ind_narm <- plm:::between.matrix(mat_noindex_unbal, effect = mat_index_unbal[[1L]], na.rm = TRUE)

B2_mat_ind       <- plm:::Between.matrix(mat, effect = "individual")
B2_mat_ind_narm  <- plm:::Between.matrix(mat, effect = "individual", na.rm = TRUE)

B2_mat_unbal_ind       <- plm:::Between.matrix(mat_unbal, effect = "individual")
B2_mat_unbal_ind_narm  <- plm:::Between.matrix(mat_unbal, effect = "individual", na.rm = TRUE)

B2_mat_noindex_ind      <- plm:::Between.matrix(mat_noindex, effect = mat_index[[1L]]) # default
B2_mat_noindex_ind_narm <- plm:::Between.matrix(mat_noindex, effect = mat_index[[1L]], na.rm = TRUE)

B2_mat_noindex_unbal_ind      <- plm:::Between.matrix(mat_noindex_unbal, effect = mat_index_unbal[[1L]]) # default
B2_mat_noindex_unbal_ind_narm <- plm:::Between.matrix(mat_noindex_unbal, effect = mat_index_unbal[[1L]], na.rm = TRUE)

stopifnot(isTRUE(all.equal(b1_mat_ind,      b2_mat_ind,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(b1_mat_ind_narm, b2_mat_ind_narm, check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_mat_ind,      B2_mat_ind,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_mat_ind_narm, B2_mat_ind_narm, check.attributes = TRUE))) # TRUE

stopifnot(isTRUE(all.equal(b1_mat_noindex_ind,      b2_mat_noindex_ind,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(b1_mat_noindex_ind_narm, b2_mat_noindex_ind_narm, check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_mat_noindex_ind,      B2_mat_noindex_ind,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_mat_noindex_ind_narm, B2_mat_noindex_ind_narm, check.attributes = TRUE))) # TRUE

stopifnot(isTRUE(all.equal(b1_mat_unbal_ind,      b2_mat_unbal_ind,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(b1_mat_unbal_ind_narm, b2_mat_unbal_ind_narm, check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_mat_unbal_ind,      B2_mat_unbal_ind,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_mat_unbal_ind_narm, B2_mat_unbal_ind_narm, check.attributes = TRUE))) # TRUE

stopifnot(isTRUE(all.equal(b1_mat_noindex_unbal_ind,      b2_mat_noindex_unbal_ind,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(b1_mat_noindex_unbal_ind_narm, b2_mat_noindex_unbal_ind_narm, check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_mat_noindex_unbal_ind,      B2_mat_noindex_unbal_ind,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_mat_noindex_unbal_ind_narm, B2_mat_noindex_unbal_ind_narm, check.attributes = TRUE))) # TRUE

# time
options("plm.fast" = FALSE)

b1_mat_ti      <- plm:::between.matrix(mat, effect = "time") # default
b1_mat_ti_narm <- plm:::between.matrix(mat, effect = "time", na.rm = TRUE)

b1_mat_noindex_ti      <- plm:::between.matrix(mat_noindex, effect = mat_index[[2L]]) # default
b1_mat_noindex_ti_narm <- plm:::between.matrix(mat_noindex, effect = mat_index[[2L]], na.rm = TRUE)

B1_mat_ti      <- plm:::Between.matrix(mat, effect = "time") # default
B1_mat_ti_narm <- plm:::Between.matrix(mat, effect = "time", na.rm = TRUE)

B1_mat_noindex_ti      <- plm:::Between.matrix(mat_noindex, effect = mat_index[[2L]]) # default
B1_mat_noindex_ti_narm <- plm:::Between.matrix(mat_noindex, effect = mat_index[[2L]], na.rm = TRUE)

options("plm.fast" = TRUE)

b2_mat_ti       <- plm:::between.matrix(mat, effect = "time")
b2_mat_ti_narm  <- plm:::between.matrix(mat, effect = "time", na.rm = TRUE)

b2_mat_noindex_ti      <- plm:::between.matrix(mat_noindex, effect = mat_index[[2L]]) # default
b2_mat_noindex_ti_narm <- plm:::between.matrix(mat_noindex, effect = mat_index[[2L]], na.rm = TRUE)

B2_mat_ti       <- plm:::Between.matrix(mat, effect = "time")
B2_mat_ti_narm  <- plm:::Between.matrix(mat, effect = "time", na.rm = TRUE)

B2_mat_noindex_ti      <- plm:::Between.matrix(mat_noindex, effect = mat_index[[2L]]) # default
B2_mat_noindex_ti_narm <- plm:::Between.matrix(mat_noindex, effect = mat_index[[2L]], na.rm = TRUE)

stopifnot(isTRUE(all.equal(b1_mat_ti,      b2_mat_ti,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(b1_mat_ti_narm, b2_mat_ti_narm, check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_mat_ti,      B2_mat_ti,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_mat_ti_narm, B2_mat_ti_narm, check.attributes = TRUE))) # TRUE

stopifnot(isTRUE(all.equal(b1_mat_noindex_ti,      b2_mat_noindex_ti,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(b1_mat_noindex_ti_narm, b2_mat_noindex_ti_narm, check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_mat_noindex_ti,      B2_mat_noindex_ti,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_mat_noindex_ti_narm, B2_mat_noindex_ti_narm, check.attributes = TRUE))) # TRUE

# time unbalanced
options("plm.fast" = FALSE)

b1_mat_unbal_ti      <- plm:::between.matrix(mat_unbal, effect = "time") # default
b1_mat_unbal_ti_narm <- plm:::between.matrix(mat_unbal, effect = "time", na.rm = TRUE)

b1_mat_noindex_unbal_ti      <- plm:::between.matrix(mat_noindex_unbal, effect = mat_index_unbal[[2L]]) # default
b1_mat_noindex_unbal_ti_narm <- plm:::between.matrix(mat_noindex_unbal, effect = mat_index_unbal[[2L]], na.rm = TRUE)

B1_mat_unbal_ti      <- plm:::Between.matrix(mat_unbal, effect = "time") # default
B1_mat_unbal_ti_narm <- plm:::Between.matrix(mat_unbal, effect = "time", na.rm = TRUE)

B1_mat_noindex_unbal_ti      <- plm:::Between.matrix(mat_noindex_unbal, effect = mat_index_unbal[[2L]]) # default
B1_mat_noindex_unbal_ti_narm <- plm:::Between.matrix(mat_noindex_unbal, effect = mat_index_unbal[[2L]], na.rm = TRUE)

options("plm.fast" = TRUE)

b2_mat_unbal_ti       <- plm:::between.matrix(mat_unbal, effect = "time")
b2_mat_unbal_ti_narm  <- plm:::between.matrix(mat_unbal, effect = "time", na.rm = TRUE)

b2_mat_noindex_unbal_ti      <- plm:::between.matrix(mat_noindex_unbal, effect = mat_index_unbal[[2L]]) # default
b2_mat_noindex_unbal_ti_narm <- plm:::between.matrix(mat_noindex_unbal, effect = mat_index_unbal[[2L]], na.rm = TRUE)

B2_mat_unbal_ti       <- plm:::Between.matrix(mat_unbal, effect = "time")
B2_mat_unbal_ti_narm  <- plm:::Between.matrix(mat_unbal, effect = "time", na.rm = TRUE)

B2_mat_noindex_unbal_ti      <- plm:::Between.matrix(mat_noindex_unbal, effect = mat_index_unbal[[2L]]) # default
B2_mat_noindex_unbal_ti_narm <- plm:::Between.matrix(mat_noindex_unbal, effect = mat_index_unbal[[2L]], na.rm = TRUE)

stopifnot(isTRUE(all.equal(b1_mat_unbal_ti,      b2_mat_unbal_ti,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(b1_mat_unbal_ti_narm, b2_mat_unbal_ti_narm, check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_mat_unbal_ti,      B2_mat_unbal_ti,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_mat_unbal_ti_narm, B2_mat_unbal_ti_narm, check.attributes = TRUE))) # TRUE

stopifnot(isTRUE(all.equal(b1_mat_noindex_unbal_ti,      b2_mat_noindex_unbal_ti,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(b1_mat_noindex_unbal_ti_narm, b2_mat_noindex_unbal_ti_narm, check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_mat_noindex_unbal_ti,      B2_mat_noindex_unbal_ti,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_mat_noindex_unbal_ti_narm, B2_mat_noindex_unbal_ti_narm, check.attributes = TRUE))) # TRUE

#### within - default ####

# individual (balanced + unbalanced)
options("plm.fast" = FALSE)

W1_d_ind      <- plm:::Within.default(as.numeric(LIFEEX), effect = index(LIFEEX)[[1L]])
W1_d_ind_narm <- plm:::Within.default(as.numeric(LIFEEX), effect = index(LIFEEX)[[1L]], na.rm = TRUE)

W1_d_ind_unbal      <- plm:::Within.default(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[1L]])
W1_d_ind_narm_unbal <- plm:::Within.default(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[1L]], na.rm = TRUE)

options("plm.fast" = TRUE)

W2_d_ind      <- plm:::Within.default(as.numeric(LIFEEX), effect = index(LIFEEX)[[1L]])
W2_d_ind_narm <- plm:::Within.default(as.numeric(LIFEEX), effect = index(LIFEEX)[[1L]], na.rm = TRUE)

W2_d_ind_unbal      <- plm:::Within.default(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[1L]])
W2_d_ind_narm_unbal <- plm:::Within.default(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[1L]], na.rm = TRUE)

stopifnot(isTRUE(all.equal(W1_d_ind,            W2_d_ind,            check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(W1_d_ind_narm,       W2_d_ind_narm,       check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(W1_d_ind_unbal,      W2_d_ind_unbal,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(W1_d_ind_narm_unbal, W2_d_ind_narm_unbal, check.attributes = TRUE))) # TRUE

# time (balanced + unbalanced)
options("plm.fast" = FALSE)

W1_d_ti      <- plm:::Within.default(as.numeric(LIFEEX), effect = index(LIFEEX)[[2L]])
W1_d_ti_narm <- plm:::Within.default(as.numeric(LIFEEX), effect = index(LIFEEX)[[2L]], na.rm = TRUE)

W1_d_ti_unbal      <- plm:::Within.default(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[2L]])
W1_d_ti_narm_unbal <- plm:::Within.default(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[2L]], na.rm = TRUE)

options("plm.fast" = TRUE)

W2_d_ti      <- plm:::Within.default(as.numeric(LIFEEX), effect = index(LIFEEX)[[2L]])
W2_d_ti_narm <- plm:::Within.default(as.numeric(LIFEEX), effect = index(LIFEEX)[[2L]], na.rm = TRUE)

W2_d_ti_unbal      <- plm:::Within.default(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[2L]])
W2_d_ti_narm_unbal <- plm:::Within.default(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[2L]], na.rm = TRUE)

stopifnot(isTRUE(all.equal(W1_d_ti,            W2_d_ti,            check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(W1_d_ti_narm,       W2_d_ti_narm,       check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(W1_d_ti_unbal,      W2_d_ti_unbal,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(W1_d_ti_narm_unbal, W2_d_ti_narm_unbal, check.attributes = TRUE))) # TRUE

# NB: Within.default does not handle twoways effects

#### within - pseries ####
options("plm.fast" = FALSE)

W1_ind      <- plm:::Within.pseries(LIFEEX, effect = "individual") # default
W1_ind_narm <- plm:::Within.pseries(LIFEEX, effect = "individual", na.rm = TRUE)

W1_ind_unbal      <- plm:::Within.pseries(LIFEEX_unbal, effect = "individual") # default
W1_ind_narm_unbal <- plm:::Within.pseries(LIFEEX_unbal, effect = "individual", na.rm = TRUE)

options("plm.fast" = TRUE)

W2_ind       <- plm:::Within.pseries(LIFEEX, effect = "individual")
W2_ind_narm  <- plm:::Within.pseries(LIFEEX, effect = "individual", na.rm = TRUE)

W2_ind_unbal       <- plm:::Within.pseries(LIFEEX_unbal, effect = "individual")
W2_ind_narm_unbal  <- plm:::Within.pseries(LIFEEX_unbal, effect = "individual", na.rm = TRUE)

stopifnot(isTRUE(all.equal(W1_ind,            W2_ind,            check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(W1_ind_narm,       W2_ind_narm,       check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(W1_ind_unbal,      W2_ind_unbal,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(W1_ind_narm_unbal, W2_ind_narm_unbal, check.attributes = TRUE))) # TRUE

# time
options("plm.fast" = FALSE)

W1_ti      <- plm:::Within.pseries(LIFEEX, effect = "time") # default
W1_ti_narm <- plm:::Within.pseries(LIFEEX, effect = "time", na.rm = TRUE)

W1_ti_unbal      <- plm:::Within.pseries(LIFEEX_unbal, effect = "time") # default
W1_ti_narm_unbal <- plm:::Within.pseries(LIFEEX_unbal, effect = "time", na.rm = TRUE)

options("plm.fast" = TRUE)

W2_ti       <- plm:::Within.pseries(LIFEEX, effect = "time")
W2_ti_narm  <- plm:::Within.pseries(LIFEEX, effect = "time", na.rm = TRUE)

W2_ti_unbal       <- plm:::Within.pseries(LIFEEX_unbal, effect = "time")
W2_ti_narm_unbal  <- plm:::Within.pseries(LIFEEX_unbal, effect = "time", na.rm = TRUE)

stopifnot(isTRUE(all.equal(W1_ti,            W2_ti,            check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(W1_ti_narm,       W2_ti_narm,       check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(W1_ti_unbal,      W2_ti_unbal,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(W1_ti_narm_unbal, W2_ti_narm_unbal, check.attributes = TRUE))) # TRUE

# twoways
# need to use non-NA data for plm's original 2-way FE unbalanced transformation (due to lm.fit being used)
  ## so these tests cannot work
  # options("plm.fast" = FALSE)
  # W1_tw      <- Within.pseries(LIFEEX, effect = "twoways") # default
  # W1_tw_narm <- Within.pseries(LIFEEX, effect = "twoways", na.rm = TRUE)
  # options("plm.fast" = TRUE)
  # W2_tw       <- Within.pseries(LIFEEX, effect = "twoways")
  # W2_tw_narm  <- Within.pseries(LIFEEX, effect = "twoways", na.rm = TRUE)
  # stopifnot(isTRUE(all.equal(W1_tw,      W2_tw,      check.attributes = TRUE))) # TRUE
  # stopifnot(isTRUE(all.equal(W1_tw_narm, W2_tw_narm, check.attributes = TRUE))) # TRUE

## but these:
options("plm.fast" = FALSE)

W1_tw_unbal_wona      <- plm:::Within.pseries(LIFEEX_unbal_wona, effect = "twoways") # default
W1_tw_narm_unbal_wona <- plm:::Within.pseries(LIFEEX_unbal_wona, effect = "twoways", na.rm = TRUE)

options("plm.fast" = TRUE)
options("plm.fast.pkg.FE.tw" = "collapse")
W2_tw_unbal_wona_collapse       <- plm:::Within.pseries(LIFEEX_unbal_wona, effect = "twoways")
W2_tw_narm_unbal_wona_collapse  <- plm:::Within.pseries(LIFEEX_unbal_wona, effect = "twoways", na.rm = TRUE)
options("plm.fast.pkg.FE.tw" = "fixest")
W2_tw_unbal_wona_fixest       <- plm:::Within.pseries(LIFEEX_unbal_wona, effect = "twoways")
W2_tw_narm_unbal_wona_fixest  <- plm:::Within.pseries(LIFEEX_unbal_wona, effect = "twoways", na.rm = TRUE)
options("plm.fast.pkg.FE.tw" = "lfe")
W2_tw_unbal_wona_lfe       <- plm:::Within.pseries(LIFEEX_unbal_wona, effect = "twoways")
W2_tw_narm_unbal_wona_lfe  <- plm:::Within.pseries(LIFEEX_unbal_wona, effect = "twoways", na.rm = TRUE)

stopifnot(isTRUE(all.equal(W1_tw_unbal_wona,      W2_tw_unbal_wona_collapse,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(W1_tw_narm_unbal_wona, W2_tw_narm_unbal_wona_collapse, check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(W1_tw_unbal_wona,      W2_tw_unbal_wona_fixest,        check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(W1_tw_narm_unbal_wona, W2_tw_narm_unbal_wona_fixest,   check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(W1_tw_unbal_wona,      W2_tw_unbal_wona_lfe,           check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(W1_tw_narm_unbal_wona, W2_tw_narm_unbal_wona_lfe,      check.attributes = TRUE))) # TRUE


#### within - matrix ####

# individual effect - balanced
options("plm.fast" = FALSE)

W1_mat_ind      <- plm:::Within.matrix(mat, effect = "individual") # default
W1_mat_ind_narm <- plm:::Within.matrix(mat, effect = "individual", na.rm = TRUE)

W1_mat_no_index_ind      <- plm:::Within.matrix(mat_noindex, effect = mat_index[[1L]]) # default
W1_mat_no_index_ind_narm <- plm:::Within.matrix(mat_noindex, effect = mat_index[[1L]], na.rm = TRUE)

options("plm.fast" = TRUE)

W2_mat_ind       <- plm:::Within.matrix(mat, effect = "individual")
W2_mat_ind_narm  <- plm:::Within.matrix(mat, effect = "individual", na.rm = TRUE)

W2_mat_no_index_ind      <- plm:::Within.matrix(mat_noindex, effect = mat_index[[1L]]) # default
W2_mat_no_index_ind_narm <- plm:::Within.matrix(mat_noindex, effect = mat_index[[1L]], na.rm = TRUE)


stopifnot(isTRUE(all.equal(W1_mat_ind,      W2_mat_ind,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(W1_mat_ind_narm, W2_mat_ind_narm, check.attributes = TRUE))) # TRUE

stopifnot(isTRUE(all.equal(W1_mat_no_index_ind,      W2_mat_no_index_ind,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(W1_mat_no_index_ind_narm, W2_mat_no_index_ind_narm, check.attributes = TRUE))) # TRUE


# individual effect - unbalanced
options("plm.fast" = FALSE)

W1_mat_unbal_ind      <- plm:::Within.matrix(mat_unbal, effect = "individual") # default
W1_mat_unbal_ind_narm <- plm:::Within.matrix(mat_unbal, effect = "individual", na.rm = TRUE)

W1_mat_no_index_ind_unbal      <- plm:::Within.matrix(mat_noindex_unbal, effect = mat_index_unbal[[1L]]) # default
W1_mat_no_index_ind_narm_unbal <- plm:::Within.matrix(mat_noindex_unbal, effect = mat_index_unbal[[1L]], na.rm = TRUE)

options("plm.fast" = TRUE)

W2_mat_unbal_ind       <- plm:::Within.matrix(mat_unbal, effect = "individual")
W2_mat_unbal_ind_narm  <- plm:::Within.matrix(mat_unbal, effect = "individual", na.rm = TRUE)

W2_mat_no_index_ind_unbal      <- plm:::Within.matrix(mat_noindex_unbal, effect = mat_index_unbal[[1L]]) # default
W2_mat_no_index_ind_narm_unbal <- plm:::Within.matrix(mat_noindex_unbal, effect = mat_index_unbal[[1L]], na.rm = TRUE)


stopifnot(isTRUE(all.equal(W1_mat_unbal_ind,      W2_mat_unbal_ind,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(W1_mat_unbal_ind_narm, W2_mat_unbal_ind_narm, check.attributes = TRUE))) # TRUE

stopifnot(isTRUE(all.equal(W1_mat_no_index_ind_unbal,      W2_mat_no_index_ind_unbal,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(W1_mat_no_index_ind_narm_unbal, W2_mat_no_index_ind_narm_unbal, check.attributes = TRUE))) # TRUE


# time effect - balanced
options("plm.fast" = FALSE)

W1_mat_ti      <- plm:::Within.matrix(mat, effect = "time") # default
W1_mat_ti_narm <- plm:::Within.matrix(mat, effect = "time", na.rm = TRUE)

W1_mat_no_index_ti      <- plm:::Within.matrix(mat_noindex, effect = mat_index[[2L]]) # default
W1_mat_no_index_ti_narm <- plm:::Within.matrix(mat_noindex, effect = mat_index[[2L]], na.rm = TRUE)

options("plm.fast" = TRUE)

W2_mat_ti       <- plm:::Within.matrix(mat, effect = "time")
W2_mat_ti_narm  <- plm:::Within.matrix(mat, effect = "time", na.rm = TRUE)

W2_mat_no_index_ti      <- plm:::Within.matrix(mat_noindex, effect = mat_index[[2L]]) # default
W2_mat_no_index_ti_narm <- plm:::Within.matrix(mat_noindex, effect = mat_index[[2L]], na.rm = TRUE)


stopifnot(isTRUE(all.equal(W1_mat_ti,      W2_mat_ti,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(W1_mat_ti_narm, W2_mat_ti_narm, check.attributes = TRUE))) # TRUE

stopifnot(isTRUE(all.equal(W1_mat_no_index_ti,      W2_mat_no_index_ti,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(W1_mat_no_index_ti_narm, W2_mat_no_index_ti_narm, check.attributes = TRUE))) # TRUE


# time effect - unbalanced
options("plm.fast" = FALSE)

W1_mat_unbal_ti      <- plm:::Within.matrix(mat_unbal, effect = "time") # default
W1_mat_unbal_ti_narm <- plm:::Within.matrix(mat_unbal, effect = "time", na.rm = TRUE)

W1_mat_no_index_ti_unbal      <- plm:::Within.matrix(mat_noindex_unbal, effect = mat_index_unbal[[2L]]) # default
W1_mat_no_index_ti_unbal_narm <- plm:::Within.matrix(mat_noindex_unbal, effect = mat_index_unbal[[2L]], na.rm = TRUE)

options("plm.fast" = TRUE)

W2_mat_unbal_ti       <- plm:::Within.matrix(mat_unbal, effect = "time")
W2_mat_unbal_ti_narm  <- plm:::Within.matrix(mat_unbal, effect = "time", na.rm = TRUE)

W2_mat_no_index_ti_unbal      <- plm:::Within.matrix(mat_noindex_unbal, effect = mat_index_unbal[[2L]]) # default
W2_mat_no_index_ti_unbal_narm <- plm:::Within.matrix(mat_noindex_unbal, effect = mat_index_unbal[[2L]], na.rm = TRUE)


stopifnot(isTRUE(all.equal(W1_mat_unbal_ti,      W2_mat_unbal_ti,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(W1_mat_unbal_ti_narm, W2_mat_unbal_ti_narm, check.attributes = TRUE))) # TRUE

stopifnot(isTRUE(all.equal(W1_mat_no_index_ti_unbal,      W2_mat_no_index_ti_unbal,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(W1_mat_no_index_ti_unbal_narm, W2_mat_no_index_ti_unbal_narm, check.attributes = TRUE))) # TRUE


# twoways - balanced
### (twoways not possible within non-index case (as Within.default does not handle more than one factor)

options("plm.fast" = FALSE)

W1_mat_tw      <- plm:::Within.matrix(mat, effect = "twoways") # default
W1_mat_tw_narm <- plm:::Within.matrix(mat, effect = "twoways", na.rm = TRUE)

options("plm.fast" = TRUE)

options("plm.fast.pkg.FE.tw" = "collapse")
W2_mat_tw_collapse       <- plm:::Within.matrix(mat, effect = "twoways")
W2_mat_tw_narm_collapse  <- plm:::Within.matrix(mat, effect = "twoways", na.rm = TRUE)

options("plm.fast.pkg.FE.tw" = "fixest")
W2_mat_tw_fixest       <- plm:::Within.matrix(mat, effect = "twoways")
W2_mat_tw_narm_fixest  <- plm:::Within.matrix(mat, effect = "twoways", na.rm = TRUE)

options("plm.fast.pkg.FE.tw" = "lfe")
W2_mat_tw_lfe       <- plm:::Within.matrix(mat, effect = "twoways")
W2_mat_tw_narm_lfe  <- plm:::Within.matrix(mat, effect = "twoways", na.rm = TRUE)


stopifnot(isTRUE(all.equal(W1_mat_tw, W2_mat_tw_collapse, check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(W1_mat_tw, W2_mat_tw_fixest,   check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(W1_mat_tw, W2_mat_tw_lfe,      check.attributes = TRUE))) # TRUE


stopifnot(isTRUE(all.equal(W1_mat_tw_narm,      W2_mat_tw_narm_collapse, check.attributes = TRUE))) # TRUE
## These two do not match as the NA-removal process is too different for the two functions:
 # stopifnot(isTRUE(all.equal(W1_mat_tw_narm,      W2_mat_tw_narm_fixest,   check.attributes = TRUE))) # TRUE
 # stopifnot(isTRUE(all.equal(W1_mat_tw_narm,      W2_mat_tw_narm_lfe,      check.attributes = TRUE))) # TRUE
# but can check fixest vs. lfe:
stopifnot(isTRUE(all.equal(W2_mat_tw_narm_fixest,      W2_mat_tw_narm_lfe,      check.attributes = TRUE))) # TRUE

## -> so use a balanced non-NA matrix instead (almost senseless but tests at least a bit of a test for na.rm = TRUE)
options("plm.fast" = FALSE)
W1_mat_bal_wona_tw_narm <- plm:::Within.matrix(mat_bal_wona, effect = "twoways", na.rm = TRUE)
options("plm.fast" = TRUE)
options("plm.fast.pkg.FE.tw" = "collapse")
W2_mat_bal_wona_tw_narm_collapse  <- plm:::Within.matrix(mat_bal_wona, effect = "twoways", na.rm = TRUE)

options("plm.fast.pkg.FE.tw" = "fixest")
W2_mat_bal_wona_tw_narm_fixest  <- plm:::Within.matrix(mat_bal_wona, effect = "twoways", na.rm = TRUE)

options("plm.fast.pkg.FE.tw" = "lfe")
W2_mat_bal_wona_tw_narm_lfe  <- plm:::Within.matrix(mat_bal_wona, effect = "twoways", na.rm = TRUE)


stopifnot(isTRUE(all.equal(W1_mat_bal_wona_tw_narm, W2_mat_bal_wona_tw_narm_collapse, check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(W1_mat_bal_wona_tw_narm, W2_mat_bal_wona_tw_narm_fixest,   check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(W1_mat_bal_wona_tw_narm, W2_mat_bal_wona_tw_narm_lfe,      check.attributes = TRUE))) # TRUE


# twoways - unbalanced
## need to use non-NA data in test for plm's original 2-way FE unbalanced transformation (due to lm.fit being used)
options("plm.fast" = FALSE)

W1_mat_unbal_tw      <- plm:::Within.matrix(mat_unbal_wona, effect = "twoways") # default
W1_mat_unbal_tw_narm <- plm:::Within.matrix(mat_unbal_wona, effect = "twoways", na.rm = TRUE)

options("plm.fast" = TRUE)
options("plm.fast.pkg.FE.tw" = "collapse")
W2_mat_unbal_tw_collapse       <- plm:::Within.matrix(mat_unbal_wona, effect = "twoways")
W2_mat_unbal_tw_narm_collapse  <- plm:::Within.matrix(mat_unbal_wona, effect = "twoways", na.rm = TRUE)

options("plm.fast.pkg.FE.tw" = "fixest")
W2_mat_unbal_tw_fixest       <- plm:::Within.matrix(mat_unbal_wona, effect = "twoways")
W2_mat_unbal_tw_narm_fixest  <- plm:::Within.matrix(mat_unbal_wona, effect = "twoways", na.rm = TRUE)

options("plm.fast.pkg.FE.tw" = "lfe")
W2_mat_unbal_tw_lfe       <- plm:::Within.matrix(mat_unbal_wona, effect = "twoways")
W2_mat_unbal_tw_narm_lfe  <- plm:::Within.matrix(mat_unbal_wona, effect = "twoways", na.rm = TRUE)


stopifnot(isTRUE(all.equal(W1_mat_unbal_tw,      W2_mat_unbal_tw_collapse,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(W1_mat_unbal_tw_narm, W2_mat_unbal_tw_narm_collapse, check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(W1_mat_unbal_tw,      W2_mat_unbal_tw_fixest,        check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(W1_mat_unbal_tw_narm, W2_mat_unbal_tw_narm_fixest,   check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(W1_mat_unbal_tw,      W2_mat_unbal_tw_lfe,           check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(W1_mat_unbal_tw_narm, W2_mat_unbal_tw_narm_lfe,      check.attributes = TRUE))) # TRUE


} ### Endif collapse.avail



#### Benchmark ####
# library("plm")
# library("collapse")
# library("microbenchmark")
# 
# rm(list = ls())
# data("wlddev", package = "collapse")
# form <- LIFEEX ~ PCGDP + GINI
# 
# # produce big data set (taken from collapse's vignette)
# wlddevsmall <- get_vars(wlddev, c("iso3c","year","OECD","PCGDP","LIFEEX","GINI","ODA"))
# wlddevsmall$iso3c <- as.character(wlddevsmall$iso3c)
# data <- replicate(100, wlddevsmall, simplify = FALSE)
# rm(wlddevsmall)
# uniquify <- function(x, i) {
#         x$iso3c <- paste0(x$iso3c, i)
#         x
# }
# data <- unlist2d(Map(uniquify, data, as.list(1:100)), idcols = FALSE)
# data <- pdata.frame(data, index = c("iso3c", "year"))
# pdim(data) # Balanced Panel: n = 21600, T = 59, N = 1274400 // but many NAs
# # data <- na.omit(data)
# # pdim(data) # Unbalanced Panel: n = 13300, T = 1-31, N = 93900
# 
# times <- 3 # no. of repetitions for benchmark
# 
# 
# bench_lag <- microbenchmark(
#      {options("plm.fast" = FALSE); lapply(as.list(data, keep.attributes = TRUE), lag)},
#      {options("plm.fast" = TRUE);  lapply(as.list(data, keep.attributes = TRUE), lag)},
#      times = times, unit = "s")
# 
# 
# bench_diff<- microbenchmark(
#      {options("plm.fast" = FALSE); lapply(as.list(data, keep.attributes = TRUE), function(x) {if(is.numeric(x)) diff(x)})},
#      {options("plm.fast" = TRUE);  lapply(as.list(data, keep.attributes = TRUE), function(x) {if(is.numeric(x)) diff(x)})},
#      times = times, unit = "s")
# 
# 
# FD <- microbenchmark(
#   {options("plm.fast" = FALSE); plm(form, data = data, model = "fd")},
#   {options("plm.fast" = TRUE);  plm(form, data = data, model = "fd")},
#   times = times, unit = "relative")
# 
# onewayFE <- microbenchmark(
#   {options("plm.fast" = FALSE); plm(form, data = data, model = "within")},
#   {options("plm.fast" = TRUE);  plm(form, data = data, model = "within")},
#   times = times, unit = "relative")
# 
# onewayRE <- microbenchmark(
#   {options("plm.fast" = FALSE); plm(form, data = data, model = "random")},
#   {options("plm.fast" = TRUE);  plm(form, data = data, model = "random")},
#   times = times, unit = "relative")
# 
# twowayRE <-  microbenchmark(
#   {options("plm.fast" = FALSE); plm(form, data = data, model = "random", effect = "twoways")},
#   {options("plm.fast" = TRUE);  plm(form, data = data, model = "random", effect = "twoways")},
#   times = times, unit = "relative")
# 
# twowayRE2 <-  microbenchmark(
#   {options("plm.fast" = FALSE);                                    plm(form, data = data, model = "random", effect = "twoways")},
#   {options("plm.fast" = TRUE, "plm.fast.pkg.FE.tw" = "collapse");  plm(form, data = data, model = "random", effect = "twoways")},
#   {options("plm.fast" = TRUE, "plm.fast.pkg.FE.tw" = "fixest");    plm(form, data = data, model = "random", effect = "twoways")},
#   {options("plm.fast" = TRUE, "plm.fast.pkg.FE.tw" = "lfe");       plm(form, data = data, model = "random", effect = "twoways")},
#   times = times, unit = "relative")
# 
# twowayFE <-  microbenchmark(
#   {options("plm.fast" = FALSE);                                    plm(form, data = data, model = "within", effect = "twoways")},
#   {options("plm.fast" = TRUE, "plm.fast.pkg.FE.tw" = "collapse");  plm(form, data = data, model = "within", effect = "twoways")},
#   {options("plm.fast" = TRUE, "plm.fast.pkg.FE.tw" = "fixest");    plm(form, data = data, model = "within", effect = "twoways")},
#   {options("plm.fast" = TRUE, "plm.fast.pkg.FE.tw" = "lfe");       plm(form, data = data, model = "within", effect = "twoways")},
#   times = times, unit = "relative")
# 
# summary(onewayFE)
# summary(onewayRE)
# summary(twowayRE)
# summary(twowayRE2)
# summary(twowayFE)
# 
# 
## 2-FE unbalanced: collapse vs. lfe
# options("plm.fast" = TRUE)
# bench_2FE_collapse <- microbenchmark(
#                           plm(form, data = data, model = "within", effect = "twoways"),
#                         times = 10)
#
# assignInNamespace("Within.pseries", Within.pseries.lfe, envir = as.environment("package:plm"))
# assignInNamespace("Within.matrix",  Within.matrix.lfe,  envir = as.environment("package:plm"))
#
# bench_2FE_lfe <- microbenchmark(
#                       plm(form, data = data, model = "within", effect = "twoways"),
#                     times = 10)
#
# print(bench_2FE_collapse, unit = "s")
# print(bench_2FE_lfe,      unit = "s")

