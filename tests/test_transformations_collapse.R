## Tests equivalence of collapse to base-R version of transformation functions
## B/between, Within, Sum.

## Run tests only if package 'collapse' is available (as it is Suggests dependency)
collapse.avail <- if (!requireNamespace("collapse", quietly = TRUE)) FALSE else TRUE

if(collapse.avail) {

# data
library("plm")
library("collapse")

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

wlddev_unbal_wona <- na.omit(wlddev[ , 8:12])
mat_unbal_wona <- as.matrix(wlddev_unbal_wona)
attr(mat_unbal_wona, "index") <- index(pwlddev[-attr(wlddev_unbal_wona, "na.action"), ])


#### Sum - default ####

Sum.default.baseR <- plm:::Sum.default.baseR
Sum.pseries.baseR <- plm:::Sum.pseries.baseR
Sum.matrix.baseR  <- plm:::Sum.matrix.baseR

Sum.default.collapse <- plm:::Sum.default.collapse
Sum.pseries.collapse <- plm:::Sum.pseries.collapse
Sum.matrix.collapse  <- plm:::Sum.matrix.collapse

# individual
S1_d_ind      <- Sum.default.baseR(as.numeric(LIFEEX), effect = index(LIFEEX)[[1L]]) # default
S1_d_ind_narm <- Sum.default.baseR(as.numeric(LIFEEX), effect = index(LIFEEX)[[1L]], na.rm = TRUE)

S2_d_ind      <- Sum.default.collapse(as.numeric(LIFEEX), effect = index(LIFEEX)[[1L]]) # default
S2_d_ind_narm <- Sum.default.collapse(as.numeric(LIFEEX), effect = index(LIFEEX)[[1L]], na.rm = TRUE)

S1_d_ind_unbal      <- Sum.default.baseR(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[1L]]) # default
S1_d_ind_narm_unbal <- Sum.default.baseR(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[1L]], na.rm = TRUE)

S2_d_ind_unbal      <- Sum.default.collapse(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[1L]]) # default
S2_d_ind_narm_unbal <- Sum.default.collapse(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[1L]], na.rm = TRUE)

stopifnot(isTRUE(all.equal(S1_d_ind,      S2_d_ind,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(S1_d_ind_narm, S2_d_ind_narm, check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(S1_d_ind_unbal,      S2_d_ind_unbal,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(S1_d_ind_narm_unbal, S2_d_ind_narm_unbal, check.attributes = TRUE))) # TRUE

# time
S1_d_ti      <- Sum.default.baseR(as.numeric(LIFEEX), effect = index(LIFEEX)[[2L]]) # default
S1_d_ti_narm <- Sum.default.baseR(as.numeric(LIFEEX), effect = index(LIFEEX)[[2L]], na.rm = TRUE)

S2_d_ti      <- Sum.default.collapse(as.numeric(LIFEEX), effect = index(LIFEEX)[[2L]]) # default
S2_d_ti_narm <- Sum.default.collapse(as.numeric(LIFEEX), effect = index(LIFEEX)[[2L]], na.rm = TRUE)

S1_d_ti_unbal      <- Sum.default.baseR(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[2L]]) # default
S1_d_ti_narm_unbal <- Sum.default.baseR(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[2L]], na.rm = TRUE)

S2_d_ti_unbal      <- Sum.default.collapse(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[2L]]) # default
S2_d_ti_narm_unbal <- Sum.default.collapse(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[2L]], na.rm = TRUE)

stopifnot(isTRUE(all.equal(S1_d_ti,      S2_d_ti,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(S1_d_ti_narm, S2_d_ti_narm, check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(S1_d_ti_unbal,      S2_d_ti_unbal,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(S1_d_ti_narm_unbal, S2_d_ti_narm_unbal, check.attributes = TRUE))) # TRUE

#### Sum - pseries ####

# individual
S1_ind      <- Sum.pseries.baseR(LIFEEX, effect = "individual") # default
S1_ind_narm <- Sum.pseries.baseR(LIFEEX, effect = "individual", na.rm = TRUE)

S2_ind      <- Sum.pseries.collapse(LIFEEX, effect = "individual") # default
S2_ind_narm <- Sum.pseries.collapse(LIFEEX, effect = "individual", na.rm = TRUE)

S1_ind_unbal      <- Sum.pseries.baseR(LIFEEX_unbal, effect = "individual") # default
S1_ind_narm_unbal <- Sum.pseries.baseR(LIFEEX_unbal, effect = "individual", na.rm = TRUE)

S2_ind_unbal      <- Sum.pseries.collapse(LIFEEX_unbal, effect = "individual") # default
S2_ind_narm_unbal <- Sum.pseries.collapse(LIFEEX_unbal, effect = "individual", na.rm = TRUE)

stopifnot(isTRUE(all.equal(S1_ind,        S2_ind,        check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(S1_ind_narm,   S2_ind_narm,   check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(S1_ind_unbal,        S2_ind_unbal,        check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(S1_ind_narm_unbal,   S2_ind_narm_unbal,   check.attributes = TRUE))) # TRUE

# time
S1_ti      <- Sum.pseries.baseR(LIFEEX, effect = "time") # default
S1_ti_narm <- Sum.pseries.baseR(LIFEEX, effect = "time", na.rm = TRUE)

S2_ti      <- Sum.pseries.collapse(LIFEEX, effect = "time") # default
S2_ti_narm <- Sum.pseries.collapse(LIFEEX, effect = "time", na.rm = TRUE)

S1_ti_unbal      <- Sum.pseries.baseR(LIFEEX_unbal, effect = "time") # default
S1_ti_narm_unbal <- Sum.pseries.baseR(LIFEEX_unbal, effect = "time", na.rm = TRUE)

S2_ti_unbal      <- Sum.pseries.collapse(LIFEEX_unbal, effect = "time") # default
S2_ti_narm_unbal <- Sum.pseries.collapse(LIFEEX_unbal, effect = "time", na.rm = TRUE)

stopifnot(isTRUE(all.equal(S1_ti,        S2_ti,        check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(S1_ti_narm,   S2_ti_narm,   check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(S1_ti_unbal,        S2_ti_unbal,        check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(S1_ti_narm_unbal,   S2_ti_narm_unbal,   check.attributes = TRUE))) # TRUE

#### Sum - matrix ####

# individual
S1_mat_ind      <- Sum.matrix.baseR(mat, effect = "individual") # default
S1_mat_ind_narm <- Sum.matrix.baseR(mat, effect = "individual", na.rm = TRUE)

S1_mat_no_index_ind      <- Sum.matrix.baseR(mat_noindex, effect = mat_index[[1L]]) # default
S1_mat_no_index_ind_narm <- Sum.matrix.baseR(mat_noindex, effect = mat_index[[1L]], na.rm = TRUE)

S2_mat_ind      <- Sum.matrix.collapse(mat, effect = "individual") # default
S2_mat_ind_narm <- Sum.matrix.collapse(mat, effect = "individual", na.rm = TRUE)

S2_mat_no_index_ind      <- Sum.matrix.collapse(mat_noindex, effect = mat_index[[1L]]) # default
S2_mat_no_index_ind_narm <- Sum.matrix.collapse(mat_noindex, effect = mat_index[[1L]], na.rm = TRUE)

S1_mat_ind_unbal      <- Sum.matrix.baseR(mat_unbal, effect = "individual") # default
S1_mat_ind_narm_unbal <- Sum.matrix.baseR(mat_unbal, effect = "individual", na.rm = TRUE)

S1_mat_no_index_ind_unbal      <- Sum.matrix.baseR(mat_noindex_unbal, effect = mat_index_unbal[[1L]]) # default
S1_mat_no_index_ind_narm_unbal <- Sum.matrix.baseR(mat_noindex_unbal, effect = mat_index_unbal[[1L]], na.rm = TRUE)

S2_mat_ind_unbal      <- Sum.matrix.collapse(mat_unbal, effect = "individual") # default
S2_mat_ind_narm_unbal <- Sum.matrix.collapse(mat_unbal, effect = "individual", na.rm = TRUE)

S2_mat_no_index_ind_unbal      <- Sum.matrix.collapse(mat_noindex_unbal, effect = mat_index_unbal[[1L]]) # default
S2_mat_no_index_ind_narm_unbal <- Sum.matrix.collapse(mat_noindex_unbal, effect = mat_index_unbal[[1L]], na.rm = TRUE)

stopifnot(isTRUE(all.equal(S1_mat_ind,        S2_mat_ind,        check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(S1_mat_ind_narm,   S2_mat_ind_narm,   check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(S1_mat_ind_unbal,        S2_mat_ind_unbal,        check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(S1_mat_ind_narm_unbal,   S2_mat_ind_narm_unbal,   check.attributes = TRUE))) # TRUE

stopifnot(isTRUE(all.equal(S1_mat_no_index_ind,        S2_mat_no_index_ind,        check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(S1_mat_no_index_ind_narm,   S2_mat_no_index_ind_narm,   check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(S1_mat_no_index_ind_unbal,        S2_mat_no_index_ind_unbal,        check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(S1_mat_no_index_ind_narm_unbal,   S2_mat_no_index_ind_narm_unbal,   check.attributes = TRUE))) # TRUE

# time
S1_mat_ti      <- Sum.matrix.baseR(mat, effect = "time") # default
S1_mat_ti_narm <- Sum.matrix.baseR(mat, effect = "time", na.rm = TRUE)

S1_mat_no_index_ti      <- Sum.matrix.baseR(mat_noindex, effect = mat_index[[2L]]) # default
S1_mat_no_index_ti_narm <- Sum.matrix.baseR(mat_noindex, effect = mat_index[[2L]], na.rm = TRUE)

S2_mat_ti      <- Sum.matrix.collapse(mat, effect = "time") # default
S2_mat_ti_narm <- Sum.matrix.collapse(mat, effect = "time", na.rm = TRUE)

S2_mat_no_index_ti      <- Sum.matrix.collapse(mat_noindex, effect = mat_index[[2L]]) # default
S2_mat_no_index_ti_narm <- Sum.matrix.collapse(mat_noindex, effect = mat_index[[2L]], na.rm = TRUE)

S1_mat_ti_unbal      <- Sum.matrix.baseR(mat_unbal, effect = "time") # default
S1_mat_ti_narm_unbal <- Sum.matrix.baseR(mat_unbal, effect = "time", na.rm = TRUE)

S1_mat_no_index_ti_unbal      <- Sum.matrix.baseR(mat_noindex_unbal, effect = mat_index_unbal[[2L]]) # default
S1_mat_no_index_ti_narm_unbal <- Sum.matrix.baseR(mat_noindex_unbal, effect = mat_index_unbal[[2L]], na.rm = TRUE)

S2_mat_ti_unbal      <- Sum.matrix.collapse(mat_unbal, effect = "time") # default
S2_mat_ti_narm_unbal <- Sum.matrix.collapse(mat_unbal, effect = "time", na.rm = TRUE)

S2_mat_no_index_ti_unbal      <- Sum.matrix.collapse(mat_noindex_unbal, effect = mat_index_unbal[[2L]]) # default
S2_mat_no_index_ti_narm_unbal <- Sum.matrix.collapse(mat_noindex_unbal, effect = mat_index_unbal[[2L]], na.rm = TRUE)


stopifnot(isTRUE(all.equal(S1_mat_ti,        S2_mat_ti,        check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(S1_mat_ti_narm,   S2_mat_ti_narm,   check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(S1_mat_no_index_ti,        S2_mat_no_index_ti,        check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(S1_mat_no_index_ti_narm,   S2_mat_no_index_ti_narm,   check.attributes = TRUE))) # TRUE

stopifnot(isTRUE(all.equal(S1_mat_ti_unbal,        S2_mat_ti_unbal,        check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(S1_mat_ti_narm_unbal,   S2_mat_ti_narm_unbal,   check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(S1_mat_no_index_ti_unbal,        S2_mat_no_index_ti_unbal,        check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(S1_mat_no_index_ti_narm_unbal,   S2_mat_no_index_ti_narm_unbal,   check.attributes = TRUE))) # TRUE


#### between/Between - default ####
between.default.baseR <- plm:::between.default.baseR
Between.default.baseR <- plm:::Between.default.baseR

between.default.collapse <- plm:::between.default.collapse
Between.default.collapse <- plm:::Between.default.collapse

# individual
b1_d_ind      <- between.default.baseR(as.numeric(LIFEEX), effect = index(LIFEEX)[[1L]]) # default
b1_d_ind_narm <- between.default.baseR(as.numeric(LIFEEX), effect = index(LIFEEX)[[1L]], na.rm = TRUE)

b2_d_ind      <- between.default.collapse(as.numeric(LIFEEX), effect = index(LIFEEX)[[1L]]) # default
b2_d_ind_narm <- between.default.collapse(as.numeric(LIFEEX), effect = index(LIFEEX)[[1L]], na.rm = TRUE)

B1_d_ind      <- Between.default.baseR(as.numeric(LIFEEX), effect = index(LIFEEX)[[1L]]) # default
B1_d_ind_narm <- Between.default.baseR(as.numeric(LIFEEX), effect = index(LIFEEX)[[1L]], na.rm = TRUE)

B2_d_ind      <- Between.default.collapse(as.numeric(LIFEEX), effect = index(LIFEEX)[[1L]]) # default
B2_d_ind_narm <- Between.default.collapse(as.numeric(LIFEEX), effect = index(LIFEEX)[[1L]], na.rm = TRUE)

b1_d_ind_unbal      <- between.default.baseR(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[1L]]) # default
b1_d_ind_narm_unbal <- between.default.baseR(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[1L]], na.rm = TRUE)

b2_d_ind_unbal      <- between.default.collapse(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[1L]]) # default
b2_d_ind_narm_unbal <- between.default.collapse(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[1L]], na.rm = TRUE)

B1_d_ind_unbal      <- Between.default.baseR(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[1L]]) # default
B1_d_ind_narm_unbal <- Between.default.baseR(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[1L]], na.rm = TRUE)

B2_d_ind_unbal      <- Between.default.collapse(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[1L]]) # default
B2_d_ind_narm_unbal <- Between.default.collapse(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[1L]], na.rm = TRUE)


stopifnot(isTRUE(all.equal(b1_d_ind,      b2_d_ind,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(b1_d_ind_narm, b2_d_ind_narm, check.attributes = TRUE))) # TRUE

stopifnot(isTRUE(all.equal(b1_d_ind_unbal,      b2_d_ind_unbal,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(b1_d_ind_narm_unbal, b2_d_ind_narm_unbal, check.attributes = TRUE))) # TRUE

stopifnot(isTRUE(all.equal(B1_d_ind,      B2_d_ind,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_d_ind_narm, B2_d_ind_narm, check.attributes = TRUE))) # TRUE

stopifnot(isTRUE(all.equal(B1_d_ind_unbal,      B2_d_ind_unbal,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_d_ind_narm_unbal, B2_d_ind_narm_unbal, check.attributes = TRUE))) # TRUE

# time
b1_d_ti      <- between.default.baseR(as.numeric(LIFEEX), effect = index(LIFEEX)[[2L]]) # default
b1_d_ti_narm <- between.default.baseR(as.numeric(LIFEEX), effect = index(LIFEEX)[[2L]], na.rm = TRUE)

b2_d_ti      <- between.default.collapse(as.numeric(LIFEEX), effect = index(LIFEEX)[[2L]]) # default
b2_d_ti_narm <- between.default.collapse(as.numeric(LIFEEX), effect = index(LIFEEX)[[2L]], na.rm = TRUE)

B1_d_ti      <- Between.default.baseR(as.numeric(LIFEEX), effect = index(LIFEEX)[[2L]]) # default
B1_d_ti_narm <- Between.default.baseR(as.numeric(LIFEEX), effect = index(LIFEEX)[[2L]], na.rm = TRUE)

B2_d_ti      <- Between.default.collapse(as.numeric(LIFEEX), effect = index(LIFEEX)[[2L]]) # default
B2_d_ti_narm <- Between.default.collapse(as.numeric(LIFEEX), effect = index(LIFEEX)[[2L]], na.rm = TRUE)

b1_d_ti_unbal      <- between.default.baseR(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[2L]]) # default
b1_d_ti_narm_unbal <- between.default.baseR(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[2L]], na.rm = TRUE)

b2_d_ti_unbal      <- between.default.collapse(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[2L]]) # default
b2_d_ti_narm_unbal <- between.default.collapse(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[2L]], na.rm = TRUE)

B1_d_ti_unbal      <- Between.default.baseR(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[2L]]) # default
B1_d_ti_narm_unbal <- Between.default.baseR(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[2L]], na.rm = TRUE)

B2_d_ti_unbal      <- Between.default.collapse(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[2L]]) # default
B2_d_ti_narm_unbal <- Between.default.collapse(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[2L]], na.rm = TRUE)


stopifnot(isTRUE(all.equal(b1_d_ti,      b2_d_ti,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(b1_d_ti_narm, b2_d_ti_narm, check.attributes = TRUE))) # TRUE

stopifnot(isTRUE(all.equal(b1_d_ti_unbal,      b2_d_ti_unbal,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(b1_d_ti_narm_unbal, b2_d_ti_narm_unbal, check.attributes = TRUE))) # TRUE

stopifnot(isTRUE(all.equal(B1_d_ti,      B2_d_ti,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_d_ti_narm, B2_d_ti_narm, check.attributes = TRUE))) # TRUE

stopifnot(isTRUE(all.equal(B1_d_ti_unbal,      B2_d_ti_unbal,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_d_ti_narm_unbal, B2_d_ti_narm_unbal, check.attributes = TRUE))) # TRUE

#### between/Between - pseries ####
between.pseries.baseR <- plm:::between.pseries.baseR
Between.pseries.baseR <- plm:::Between.pseries.baseR

between.pseries.collapse <- plm:::between.pseries.collapse
Between.pseries.collapse <- plm:::Between.pseries.collapse

b1_ind      <- between.pseries.baseR(LIFEEX, effect = "individual") # default
b1_ind_narm <- between.pseries.baseR(LIFEEX, effect = "individual", na.rm = TRUE)

B1_ind      <- Between.pseries.baseR(LIFEEX, effect = "individual") # default
B1_ind_narm <- Between.pseries.baseR(LIFEEX, effect = "individual", na.rm = TRUE)

b2_ind       <- between.pseries.collapse(LIFEEX, effect = "individual")
b2_ind_narm  <- between.pseries.collapse(LIFEEX, effect = "individual", na.rm = TRUE)

B2_ind       <- Between.pseries.collapse(LIFEEX, effect = "individual")
B2_ind_narm  <- Between.pseries.collapse(LIFEEX, effect = "individual", na.rm = TRUE)

stopifnot(isTRUE(all.equal(b1_ind,      b2_ind,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(b1_ind_narm, b2_ind_narm, check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_ind,      B2_ind,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_ind_narm, B2_ind_narm, check.attributes = TRUE))) # TRUE

b1_ind_unbal      <- between.pseries.baseR(LIFEEX_unbal, effect = "individual") # default
b1_ind_unbal_narm <- between.pseries.baseR(LIFEEX_unbal, effect = "individual", na.rm = TRUE)

B1_ind_unbal      <- Between.pseries.baseR(LIFEEX_unbal, effect = "individual") # default
B1_ind_unbal_narm <- Between.pseries.baseR(LIFEEX_unbal, effect = "individual", na.rm = TRUE)

b2_ind_unbal       <- between.pseries.collapse(LIFEEX_unbal, effect = "individual")
b2_ind_unbal_narm  <- between.pseries.collapse(LIFEEX_unbal, effect = "individual", na.rm = TRUE)

B2_ind_unbal       <- Between.pseries.collapse(LIFEEX_unbal, effect = "individual")
B2_ind_unbal_narm  <- Between.pseries.collapse(LIFEEX_unbal, effect = "individual", na.rm = TRUE)

stopifnot(isTRUE(all.equal(b1_ind_unbal,      b2_ind_unbal,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(b1_ind_unbal_narm, b2_ind_unbal_narm, check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_ind_unbal,      B2_ind_unbal,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_ind_unbal_narm, B2_ind_unbal_narm, check.attributes = TRUE))) # TRUE

# time
b1_ti      <- between.pseries.baseR(LIFEEX, effect = "time") # default
b1_ti_narm <- between.pseries.baseR(LIFEEX, effect = "time", na.rm = TRUE)

B1_ti      <- Between.pseries.baseR(LIFEEX, effect = "time") # default
B1_ti_narm <- Between.pseries.baseR(LIFEEX, effect = "time", na.rm = TRUE)

b2_ti       <- between.pseries.collapse(LIFEEX, effect = "time")
b2_ti_narm  <- between.pseries.collapse(LIFEEX, effect = "time", na.rm = TRUE)

B2_ti       <- Between.pseries.collapse(LIFEEX, effect = "time")
B2_ti_narm  <- Between.pseries.collapse(LIFEEX, effect = "time", na.rm = TRUE)

stopifnot(isTRUE(all.equal(b1_ti,      b2_ti,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(b1_ti_narm, b2_ti_narm, check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_ti,      B2_ti,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_ti_narm, B2_ti_narm, check.attributes = TRUE))) # TRUE

b1_ti_unbal      <- between.pseries.baseR(LIFEEX_unbal, effect = "time") # default
b1_ti_unbal_narm <- between.pseries.baseR(LIFEEX_unbal, effect = "time", na.rm = TRUE)

B1_ti_unbal      <- Between.pseries.baseR(LIFEEX_unbal, effect = "time") # default
B1_ti_unbal_narm <- Between.pseries.baseR(LIFEEX_unbal, effect = "time", na.rm = TRUE)

b2_ti_unbal       <- between.pseries.collapse(LIFEEX_unbal, effect = "time")
b2_ti_unbal_narm  <- between.pseries.collapse(LIFEEX_unbal, effect = "time", na.rm = TRUE)

B2_ti_unbal       <- Between.pseries.collapse(LIFEEX_unbal, effect = "time")
B2_ti_unbal_narm  <- Between.pseries.collapse(LIFEEX_unbal, effect = "time", na.rm = TRUE)

stopifnot(isTRUE(all.equal(b1_ti_unbal,      b2_ti_unbal,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(b1_ti_unbal_narm, b2_ti_unbal_narm, check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_ti_unbal,      B2_ti_unbal,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_ti_unbal_narm, B2_ti_unbal_narm, check.attributes = TRUE))) # TRUE

#### between/Between - matrix ####

between.matrix.baseR <- plm:::between.matrix.baseR
Between.matrix.baseR <- plm:::Between.matrix.baseR

between.matrix.collapse <- plm:::between.matrix.collapse
Between.matrix.collapse <- plm:::Between.matrix.collapse

# individual
b1_mat_ind      <- between.matrix.baseR(mat, effect = "individual") # default
b1_mat_ind_narm <- between.matrix.baseR(mat, effect = "individual", na.rm = TRUE)

B1_mat_ind      <- Between.matrix.baseR(mat, effect = "individual") # default
B1_mat_ind_narm <- Between.matrix.baseR(mat, effect = "individual", na.rm = TRUE)

b2_mat_ind       <- between.matrix.collapse(mat, effect = "individual")
b2_mat_ind_narm  <- between.matrix.collapse(mat, effect = "individual", na.rm = TRUE)

B2_mat_ind       <- Between.matrix.collapse(mat, effect = "individual")
B2_mat_ind_narm  <- Between.matrix.collapse(mat, effect = "individual", na.rm = TRUE)

b1_mat_noindex_ind      <- between.matrix.baseR(mat_noindex, effect = mat_index[[1L]]) # default
b1_mat_noindex_ind_narm <- between.matrix.baseR(mat_noindex, effect = mat_index[[1L]], na.rm = TRUE)

B1_mat_noindex_ind      <- Between.matrix.baseR(mat_noindex, effect = mat_index[[1L]]) # default
B1_mat_noindex_ind_narm <- Between.matrix.baseR(mat_noindex, effect = mat_index[[1L]], na.rm = TRUE)

b2_mat_noindex_ind      <- between.matrix.collapse(mat_noindex, effect = mat_index[[1L]]) # default
b2_mat_noindex_ind_narm <- between.matrix.collapse(mat_noindex, effect = mat_index[[1L]], na.rm = TRUE)

B2_mat_noindex_ind      <- Between.matrix.collapse(mat_noindex, effect = mat_index[[1L]]) # default
B2_mat_noindex_ind_narm <- Between.matrix.collapse(mat_noindex, effect = mat_index[[1L]], na.rm = TRUE)

stopifnot(isTRUE(all.equal(b1_mat_ind,      b2_mat_ind,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(b1_mat_ind_narm, b2_mat_ind_narm, check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_mat_ind,      B2_mat_ind,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_mat_ind_narm, B2_mat_ind_narm, check.attributes = TRUE))) # TRUE

stopifnot(isTRUE(all.equal(b1_mat_noindex_ind,      b2_mat_noindex_ind,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(b1_mat_noindex_ind_narm, b2_mat_noindex_ind_narm, check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_mat_noindex_ind,      B2_mat_noindex_ind,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_mat_noindex_ind_narm, B2_mat_noindex_ind_narm, check.attributes = TRUE))) # TRUE

# individual unbalanced
b1_mat_unbal_ind      <- between.matrix.baseR(mat_unbal, effect = "individual") # default
b1_mat_unbal_ind_narm <- between.matrix.baseR(mat_unbal, effect = "individual", na.rm = TRUE)

B1_mat_unbal_ind      <- Between.matrix.baseR(mat_unbal, effect = "individual") # default
B1_mat_unbal_ind_narm <- Between.matrix.baseR(mat_unbal, effect = "individual", na.rm = TRUE)

b2_mat_unbal_ind       <- between.matrix.collapse(mat_unbal, effect = "individual")
b2_mat_unbal_ind_narm  <- between.matrix.collapse(mat_unbal, effect = "individual", na.rm = TRUE)

B2_mat_unbal_ind       <- Between.matrix.collapse(mat_unbal, effect = "individual")
B2_mat_unbal_ind_narm  <- Between.matrix.collapse(mat_unbal, effect = "individual", na.rm = TRUE)

b1_mat_noindex_unbal_ind      <- between.matrix.baseR(mat_noindex_unbal, effect = mat_index_unbal[[1L]]) # default
b1_mat_noindex_unbal_ind_narm <- between.matrix.baseR(mat_noindex_unbal, effect = mat_index_unbal[[1L]], na.rm = TRUE)

B1_mat_noindex_unbal_ind      <- Between.matrix.baseR(mat_noindex_unbal, effect = mat_index_unbal[[1L]]) # default
B1_mat_noindex_unbal_ind_narm <- Between.matrix.baseR(mat_noindex_unbal, effect = mat_index_unbal[[1L]], na.rm = TRUE)

b2_mat_noindex_unbal_ind      <- between.matrix.collapse(mat_noindex_unbal, effect = mat_index_unbal[[1L]]) # default
b2_mat_noindex_unbal_ind_narm <- between.matrix.collapse(mat_noindex_unbal, effect = mat_index_unbal[[1L]], na.rm = TRUE)

B2_mat_noindex_unbal_ind      <- Between.matrix.collapse(mat_noindex_unbal, effect = mat_index_unbal[[1L]]) # default
B2_mat_noindex_unbal_ind_narm <- Between.matrix.collapse(mat_noindex_unbal, effect = mat_index_unbal[[1L]], na.rm = TRUE)

stopifnot(isTRUE(all.equal(b1_mat_unbal_ind,      b2_mat_unbal_ind,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(b1_mat_unbal_ind_narm, b2_mat_unbal_ind_narm, check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_mat_unbal_ind,      B2_mat_unbal_ind,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_mat_unbal_ind_narm, B2_mat_unbal_ind_narm, check.attributes = TRUE))) # TRUE

stopifnot(isTRUE(all.equal(b1_mat_noindex_unbal_ind,      b2_mat_noindex_unbal_ind,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(b1_mat_noindex_unbal_ind_narm, b2_mat_noindex_unbal_ind_narm, check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_mat_noindex_unbal_ind,      B2_mat_noindex_unbal_ind,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_mat_noindex_unbal_ind_narm, B2_mat_noindex_unbal_ind_narm, check.attributes = TRUE))) # TRUE

# time
b1_mat_ti      <- between.matrix.baseR(mat, effect = "time") # default
b1_mat_ti_narm <- between.matrix.baseR(mat, effect = "time", na.rm = TRUE)

B1_mat_ti      <- Between.matrix.baseR(mat, effect = "time") # default
B1_mat_ti_narm <- Between.matrix.baseR(mat, effect = "time", na.rm = TRUE)

b2_mat_ti       <- between.matrix.collapse(mat, effect = "time")
b2_mat_ti_narm  <- between.matrix.collapse(mat, effect = "time", na.rm = TRUE)

B2_mat_ti       <- Between.matrix.collapse(mat, effect = "time")
B2_mat_ti_narm  <- Between.matrix.collapse(mat, effect = "time", na.rm = TRUE)

b1_mat_noindex_ti      <- between.matrix.baseR(mat_noindex, effect = mat_index[[2L]]) # default
b1_mat_noindex_ti_narm <- between.matrix.baseR(mat_noindex, effect = mat_index[[2L]], na.rm = TRUE)

B1_mat_noindex_ti      <- Between.matrix.baseR(mat_noindex, effect = mat_index[[2L]]) # default
B1_mat_noindex_ti_narm <- Between.matrix.baseR(mat_noindex, effect = mat_index[[2L]], na.rm = TRUE)

b2_mat_noindex_ti      <- between.matrix.collapse(mat_noindex, effect = mat_index[[2L]]) # default
b2_mat_noindex_ti_narm <- between.matrix.collapse(mat_noindex, effect = mat_index[[2L]], na.rm = TRUE)

B2_mat_noindex_ti      <- Between.matrix.collapse(mat_noindex, effect = mat_index[[2L]]) # default
B2_mat_noindex_ti_narm <- Between.matrix.collapse(mat_noindex, effect = mat_index[[2L]], na.rm = TRUE)

stopifnot(isTRUE(all.equal(b1_mat_ti,      b2_mat_ti,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(b1_mat_ti_narm, b2_mat_ti_narm, check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_mat_ti,      B2_mat_ti,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_mat_ti_narm, B2_mat_ti_narm, check.attributes = TRUE))) # TRUE

stopifnot(isTRUE(all.equal(b1_mat_noindex_ti,      b2_mat_noindex_ti,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(b1_mat_noindex_ti_narm, b2_mat_noindex_ti_narm, check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_mat_noindex_ti,      B2_mat_noindex_ti,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_mat_noindex_ti_narm, B2_mat_noindex_ti_narm, check.attributes = TRUE))) # TRUE

# time unbalanced
b1_mat_unbal_ti      <- between.matrix.baseR(mat_unbal, effect = "time") # default
b1_mat_unbal_ti_narm <- between.matrix.baseR(mat_unbal, effect = "time", na.rm = TRUE)

B1_mat_unbal_ti      <- Between.matrix.baseR(mat_unbal, effect = "time") # default
B1_mat_unbal_ti_narm <- Between.matrix.baseR(mat_unbal, effect = "time", na.rm = TRUE)

b2_mat_unbal_ti       <- between.matrix.collapse(mat_unbal, effect = "time")
b2_mat_unbal_ti_narm  <- between.matrix.collapse(mat_unbal, effect = "time", na.rm = TRUE)

B2_mat_unbal_ti       <- Between.matrix.collapse(mat_unbal, effect = "time")
B2_mat_unbal_ti_narm  <- Between.matrix.collapse(mat_unbal, effect = "time", na.rm = TRUE)

b1_mat_noindex_unbal_ti      <- between.matrix.baseR(mat_noindex_unbal, effect = mat_index_unbal[[2L]]) # default
b1_mat_noindex_unbal_ti_narm <- between.matrix.baseR(mat_noindex_unbal, effect = mat_index_unbal[[2L]], na.rm = TRUE)

B1_mat_noindex_unbal_ti      <- Between.matrix.baseR(mat_noindex_unbal, effect = mat_index_unbal[[2L]]) # default
B1_mat_noindex_unbal_ti_narm <- Between.matrix.baseR(mat_noindex_unbal, effect = mat_index_unbal[[2L]], na.rm = TRUE)

b2_mat_noindex_unbal_ti      <- between.matrix.collapse(mat_noindex_unbal, effect = mat_index_unbal[[2L]]) # default
b2_mat_noindex_unbal_ti_narm <- between.matrix.collapse(mat_noindex_unbal, effect = mat_index_unbal[[2L]], na.rm = TRUE)

B2_mat_noindex_unbal_ti      <- Between.matrix.collapse(mat_noindex_unbal, effect = mat_index_unbal[[2L]]) # default
B2_mat_noindex_unbal_ti_narm <- Between.matrix.collapse(mat_noindex_unbal, effect = mat_index_unbal[[2L]], na.rm = TRUE)

stopifnot(isTRUE(all.equal(b1_mat_unbal_ti,      b2_mat_unbal_ti,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(b1_mat_unbal_ti_narm, b2_mat_unbal_ti_narm, check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_mat_unbal_ti,      B2_mat_unbal_ti,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_mat_unbal_ti_narm, B2_mat_unbal_ti_narm, check.attributes = TRUE))) # TRUE

stopifnot(isTRUE(all.equal(b1_mat_noindex_unbal_ti,      b2_mat_noindex_unbal_ti,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(b1_mat_noindex_unbal_ti_narm, b2_mat_noindex_unbal_ti_narm, check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_mat_noindex_unbal_ti,      B2_mat_noindex_unbal_ti,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(B1_mat_noindex_unbal_ti_narm, B2_mat_noindex_unbal_ti_narm, check.attributes = TRUE))) # TRUE

#### within - default ####

Within.default.baseR    <- plm:::Within.default.baseR
Within.default.collapse <- plm:::Within.default.collapse

# individual (balanced + unbalanced)
W1_d_ind      <- Within.default.baseR(as.numeric(LIFEEX), effect = index(LIFEEX)[[1L]])
W1_d_ind_narm <- Within.default.baseR(as.numeric(LIFEEX), effect = index(LIFEEX)[[1L]], na.rm = TRUE)

W2_d_ind      <- Within.default.collapse(as.numeric(LIFEEX), effect = index(LIFEEX)[[1L]])
W2_d_ind_narm <- Within.default.collapse(as.numeric(LIFEEX), effect = index(LIFEEX)[[1L]], na.rm = TRUE)

W1_d_ind_unbal      <- Within.default.baseR(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[1L]])
W1_d_ind_narm_unbal <- Within.default.baseR(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[1L]], na.rm = TRUE)

W2_d_ind_unbal      <- Within.default.collapse(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[1L]])
W2_d_ind_narm_unbal <- Within.default.collapse(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[1L]], na.rm = TRUE)


stopifnot(isTRUE(all.equal(W1_d_ind,      W2_d_ind,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(W1_d_ind_narm, W2_d_ind_narm, check.attributes = TRUE))) # TRUE

stopifnot(isTRUE(all.equal(W1_d_ind_unbal,      W2_d_ind_unbal,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(W1_d_ind_narm_unbal, W2_d_ind_narm_unbal, check.attributes = TRUE))) # TRUE

# time (balanced + unbalanced)
W1_d_ti      <- Within.default.baseR(as.numeric(LIFEEX), effect = index(LIFEEX)[[2L]])
W1_d_ti_narm <- Within.default.baseR(as.numeric(LIFEEX), effect = index(LIFEEX)[[2L]], na.rm = TRUE)

W2_d_ti      <- Within.default.collapse(as.numeric(LIFEEX), effect = index(LIFEEX)[[2L]])
W2_d_ti_narm <- Within.default.collapse(as.numeric(LIFEEX), effect = index(LIFEEX)[[2L]], na.rm = TRUE)

W1_d_ti_unbal      <- Within.default.baseR(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[2L]])
W1_d_ti_narm_unbal <- Within.default.baseR(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[2L]], na.rm = TRUE)

W2_d_ti_unbal      <- Within.default.collapse(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[2L]])
W2_d_ti_narm_unbal <- Within.default.collapse(as.numeric(LIFEEX_unbal), effect = index(LIFEEX_unbal)[[2L]], na.rm = TRUE)

stopifnot(isTRUE(all.equal(W1_d_ti,      W2_d_ti,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(W1_d_ti_narm, W2_d_ti_narm, check.attributes = TRUE))) # TRUE

stopifnot(isTRUE(all.equal(W1_d_ti_unbal,      W2_d_ti_unbal,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(W1_d_ti_narm_unbal, W2_d_ti_narm_unbal, check.attributes = TRUE))) # TRUE

# NB: Within.default does not handle twoways effects

#### within - pseries ####

Within.pseries.baseR    <- plm:::Within.pseries.baseR

Within.pseries.collapse <- plm:::Within.pseries.collapse

W1_ind      <- Within.pseries.baseR(LIFEEX, effect = "individual") # default
W1_ind_narm <- Within.pseries.baseR(LIFEEX, effect = "individual", na.rm = TRUE)

W2_ind       <- Within.pseries.collapse(LIFEEX, effect = "individual")
W2_ind_narm  <- Within.pseries.collapse(LIFEEX, effect = "individual", na.rm = TRUE)

W1_ind_unbal      <- Within.pseries.baseR(LIFEEX_unbal, effect = "individual") # default
W1_ind_narm_unbal <- Within.pseries.baseR(LIFEEX_unbal, effect = "individual", na.rm = TRUE)

W2_ind_unbal       <- Within.pseries.collapse(LIFEEX_unbal, effect = "individual")
W2_ind_narm_unbal  <- Within.pseries.collapse(LIFEEX_unbal, effect = "individual", na.rm = TRUE)

stopifnot(isTRUE(all.equal(W1_ind,      W2_ind,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(W1_ind_narm, W2_ind_narm, check.attributes = TRUE))) # TRUE

stopifnot(isTRUE(all.equal(W1_ind_unbal,      W2_ind_unbal,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(W1_ind_narm_unbal, W2_ind_narm_unbal, check.attributes = TRUE))) # TRUE

# time
W1_ti      <- Within.pseries.baseR(LIFEEX, effect = "time") # default
W1_ti_narm <- Within.pseries.baseR(LIFEEX, effect = "time", na.rm = TRUE)

W2_ti       <- Within.pseries.collapse(LIFEEX, effect = "time")
W2_ti_narm  <- Within.pseries.collapse(LIFEEX, effect = "time", na.rm = TRUE)

W1_ti_unbal      <- Within.pseries.baseR(LIFEEX_unbal, effect = "time") # default
W1_ti_narm_unbal <- Within.pseries.baseR(LIFEEX_unbal, effect = "time", na.rm = TRUE)

W2_ti_unbal       <- Within.pseries.collapse(LIFEEX_unbal, effect = "time")
W2_ti_narm_unbal  <- Within.pseries.collapse(LIFEEX_unbal, effect = "time", na.rm = TRUE)

stopifnot(isTRUE(all.equal(W1_ti,      W2_ti,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(W1_ti_narm, W2_ti_narm, check.attributes = TRUE))) # TRUE

stopifnot(isTRUE(all.equal(W1_ti_unbal,      W2_ti_unbal,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(W1_ti_narm_unbal, W2_ti_narm_unbal, check.attributes = TRUE))) # TRUE

# twoways
# need to use non-NA data for plm's original 2-way FE unbalanced transformation (due to lm.fit being used)
	## so these cannot work
	# W1_tw      <- Within.pseries.baseR(LIFEEX, effect = "twoways") # default
	# W1_tw_narm <- Within.pseries.baseR(LIFEEX, effect = "twoways", na.rm = TRUE)
	# 
	# W2_tw       <- Within.pseries.collapse(LIFEEX, effect = "twoways")
	# W2_tw_narm  <- Within.pseries.collapse(LIFEEX, effect = "twoways", na.rm = TRUE)
	# stopifnot(isTRUE(all.equal(W1_tw,      W2_tw,      check.attributes = TRUE))) # TRUE
	# stopifnot(isTRUE(all.equal(W1_tw_narm, W2_tw_narm, check.attributes = TRUE))) # TRUE

## but these:
W1_tw_unbal_wona      <- Within.pseries.baseR(LIFEEX_unbal_wona, effect = "twoways") # default
W1_tw_narm_unbal_wona <- Within.pseries.baseR(LIFEEX_unbal_wona, effect = "twoways", na.rm = TRUE)

W2_tw_unbal_wona       <- Within.pseries.collapse(LIFEEX_unbal_wona, effect = "twoways")
W2_tw_narm_unbal_wona  <- Within.pseries.collapse(LIFEEX_unbal_wona, effect = "twoways", na.rm = TRUE)

stopifnot(isTRUE(all.equal(W1_tw_unbal_wona,      W2_tw_unbal_wona,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(W1_tw_narm_unbal_wona, W2_tw_narm_unbal_wona, check.attributes = TRUE))) # TRUE

#### within - matrix ####

Within.matrix.baseR    <- plm:::Within.matrix.baseR

Within.matrix.collapse <- plm:::Within.matrix.collapse

# individual effect - balanced
W1_mat_ind      <- Within.matrix.baseR(mat, effect = "individual") # default
W1_mat_ind_narm <- Within.matrix.baseR(mat, effect = "individual", na.rm = TRUE)

W2_mat_ind       <- Within.matrix.collapse(mat, effect = "individual")
W2_mat_ind_narm  <- Within.matrix.collapse(mat, effect = "individual", na.rm = TRUE)

stopifnot(isTRUE(all.equal(W1_mat_ind,      W2_mat_ind,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(W1_mat_ind_narm, W2_mat_ind_narm, check.attributes = TRUE))) # TRUE

# individual effect - unbalanced
W1_mat_unbal_ind      <- Within.matrix.baseR(mat_unbal, effect = "individual") # default
W1_mat_unbal_ind_narm <- Within.matrix.baseR(mat_unbal, effect = "individual", na.rm = TRUE)

W2_mat_unbal_ind       <- Within.matrix.collapse(mat_unbal, effect = "individual")
W2_mat_unbal_ind_narm  <- Within.matrix.collapse(mat_unbal, effect = "individual", na.rm = TRUE)

stopifnot(isTRUE(all.equal(W1_mat_unbal_ind,      W2_mat_unbal_ind,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(W1_mat_unbal_ind_narm, W2_mat_unbal_ind_narm, check.attributes = TRUE))) # TRUE

# time effect - balanced
W1_mat_ti      <- Within.matrix.baseR(mat, effect = "time") # default
W1_mat_ti_narm <- Within.matrix.baseR(mat, effect = "time", na.rm = TRUE)

W2_mat_ti       <- Within.matrix.collapse(mat, effect = "time")
W2_mat_ti_narm  <- Within.matrix.collapse(mat, effect = "time", na.rm = TRUE)

stopifnot(isTRUE(all.equal(W1_mat_ti,      W2_mat_ti,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(W1_mat_ti_narm, W2_mat_ti_narm, check.attributes = TRUE))) # TRUE

# time effect - unbalanced
W1_mat_unbal_ti      <- Within.matrix.baseR(mat_unbal, effect = "time") # default
W1_mat_unbal_ti_narm <- Within.matrix.baseR(mat_unbal, effect = "time", na.rm = TRUE)

W2_mat_unbal_ti       <- Within.matrix.collapse(mat_unbal, effect = "time")
W2_mat_unbal_ti_narm  <- Within.matrix.collapse(mat_unbal, effect = "time", na.rm = TRUE)

stopifnot(isTRUE(all.equal(W1_mat_unbal_ti,      W2_mat_unbal_ti,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(W1_mat_unbal_ti_narm, W2_mat_unbal_ti_narm, check.attributes = TRUE))) # TRUE

# twoways - balanced
W1_mat_tw      <- Within.matrix.baseR(mat, effect = "twoways") # default
W1_mat_tw_narm <- Within.matrix.baseR(mat, effect = "twoways", na.rm = TRUE)

W2_mat_tw       <- Within.matrix.collapse(mat, effect = "twoways")
W2_mat_tw_narm  <- Within.matrix.collapse(mat, effect = "twoways", na.rm = TRUE)

stopifnot(isTRUE(all.equal(W1_mat_tw,      W2_mat_tw,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(W1_mat_tw_narm, W2_mat_tw_narm, check.attributes = TRUE))) # TRUE

# twoways - unbalanced
# need to use non-NA data for plm's original 2-way FE unbalanced transformation (due to lm.fit being used)
W1_mat_unbal_tw      <- Within.matrix.baseR(mat_unbal_wona, effect = "twoways") # default
W1_mat_unbal_tw_narm <- Within.matrix.baseR(mat_unbal_wona, effect = "twoways", na.rm = TRUE)

W2_mat_unbal_tw       <- Within.matrix.collapse(mat_unbal_wona, effect = "twoways")
W2_mat_unbal_tw_narm  <- Within.matrix.collapse(mat_unbal_wona, effect = "twoways", na.rm = TRUE)

stopifnot(isTRUE(all.equal(W1_mat_unbal_tw,      W2_mat_unbal_tw,      check.attributes = TRUE))) # TRUE
stopifnot(isTRUE(all.equal(W1_mat_unbal_tw_narm, W2_mat_unbal_tw_narm, check.attributes = TRUE))) # TRUE


} ### Endif collapse.avail



# #### Benchmark ####
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
# pkg.plm.fast(use = FALSE) # default: fast functions of 'collapse' not in use
# times <- 3 # no. of repetitions for benchmark
# bench_res_plm <- microbenchmark(
#                           plm(form, data = data, model = "within"),
#                           plm(form, data = data, model = "within", effect = "twoways"),
#                           plm(form, data = data, model = "random"),
#                           plm(form, data = data, model = "random", effect = "twoways"),
#                       times = times)
# 
# pkg.plm.fast(use = TRUE)
# bench_res_collapse <- microbenchmark(
#                           plm(form, data = data, model = "within"),
#                           plm(form, data = data, model = "within", effect = "twoways"),
#                           plm(form, data = data, model = "random"),
#                           plm(form, data = data, model = "random", effect = "twoways"),
#                         times = times)
# print(bench_res_plm,      unit = "s")
# print(bench_res_collapse, unit = "s")
# 
# 
# ## 2-FE unbalanced: collapse vs. lfe
# pkg.plm.fast(use = TRUE)
# bench_2FE_collapse <- microbenchmark(
#                           plm(form, data = data, model = "within", effect = "twoways"),
#                         times = 10)
# 
# assignInNamespace("Within.pseries", Within.pseries.collapse.lfe, envir = as.environment("package:plm"))
# assignInNamespace("Within.matrix",  Within.matrix.collapse.lfe,  envir = as.environment("package:plm"))
# 
# bench_2FE_lfe <- microbenchmark(
#                       plm(form, data = data, model = "within", effect = "twoways"),
#                     times = 10)
# 
# print(bench_2FE_collapse, unit = "s")
# print(bench_2FE_lfe,      unit = "s") # ~ 2x up to ~3x faster
