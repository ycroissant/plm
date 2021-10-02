## Run tests for B/between, Within, and Sum transformation
##   contains base-R and collapse version of transformation functions,
##   incl. the dispatching mechanism.
##
##  Equivalence of base-R and collapse versions is not tested for here,
##  but in file test_transformations_collapse.R


# data
library("plm")

#### set up test input data ####
data("Grunfeld", package = "plm")
class(Grunfeld)
Grunfeld[10, "inv"] <- NA
pGrunfeld <- pdata.frame(Grunfeld, index = c("firm", "year"))
#capital <- pGrunfeld$capital
inv     <- pGrunfeld$inv
rm.rows <- c(1:5,50:58, 154:160)
pGrunfeld_unbal <-pGrunfeld[-rm.rows, ]
inv_unbal <- pGrunfeld_unbal[ , "inv"]

l.na <- is.na(inv_unbal)
inv_unbal_wona <- inv_unbal[!l.na]
i <- index(inv_unbal)[!l.na, ]
inv_unbal_wona <- plm:::add_pseries_features(inv_unbal_wona, i)

mat <- as.matrix(pGrunfeld[ , 3:5])
attr(mat, "index") <- index(pGrunfeld)

mat_unbal <- as.matrix(Grunfeld[-rm.rows , 3:5])
attr(mat_unbal, "index") <- index(pGrunfeld_unbal)

mat_noindex <- mat
attr(mat_noindex, "index") <- NULL
mat_index <- attr(mat, "index")

mat_noindex_unbal <- mat[-rm.rows , ]
attr(mat_noindex_unbal, "index") <- NULL
mat_index_unbal <- attr(mat, "index")[-rm.rows, ]

Grunfeld_unbal_wona <- na.omit(Grunfeld[ , 3:5])
mat_unbal_wona <- as.matrix(Grunfeld_unbal_wona)
attr(mat_unbal_wona, "index") <- index(pGrunfeld[-attr(Grunfeld_unbal_wona, "na.action"), ])


#### Sum - default ####

# individual
xS1_d_ind      <- Sum(as.numeric(inv), effect = index(inv)[[1L]]) # default
xS1_d_ind_narm <- Sum(as.numeric(inv), effect = index(inv)[[1L]], na.rm = TRUE)

xS1_d_ind_unbal      <- Sum(as.numeric(inv_unbal), effect = index(inv_unbal)[[1L]]) # default
xS1_d_ind_narm_unbal <- Sum(as.numeric(inv_unbal), effect = index(inv_unbal)[[1L]], na.rm = TRUE)

# time
xS1_d_ti      <- Sum(as.numeric(inv), effect = index(inv)[[2L]]) # default
xS1_d_ti_narm <- Sum(as.numeric(inv), effect = index(inv)[[2L]], na.rm = TRUE)

xS1_d_ti_unbal      <- Sum(as.numeric(inv_unbal), effect = index(inv_unbal)[[2L]]) # default
xS1_d_ti_narm_unbal <- Sum(as.numeric(inv_unbal), effect = index(inv_unbal)[[2L]], na.rm = TRUE)

#### Sum - pseries ####

# individual
xS1_ind      <- Sum(inv, effect = "individual") # default
xS1_ind_narm <- Sum(inv, effect = "individual", na.rm = TRUE)

xS1_ind_unbal      <- Sum(inv_unbal, effect = "individual") # default
xS1_ind_narm_unbal <- Sum(inv_unbal, effect = "individual", na.rm = TRUE)

# time
xS1_ti      <- Sum(inv, effect = "time") # default
xS1_ti_narm <- Sum(inv, effect = "time", na.rm = TRUE)

xS1_ti_unbal      <- Sum(inv_unbal, effect = "time") # default
xS1_ti_narm_unbal <- Sum(inv_unbal, effect = "time", na.rm = TRUE)

#### Sum - matrix ####

# individual
xS1_mat_ind      <- Sum(mat, effect = "individual") # default
xS1_mat_ind_narm <- Sum(mat, effect = "individual", na.rm = TRUE)

xS1_mat_no_index_ind      <- Sum(mat_noindex, effect = mat_index[[1L]]) # default
xS1_mat_no_index_ind_narm <- Sum(mat_noindex, effect = mat_index[[1L]], na.rm = TRUE)

xS1_mat_ind_unbal      <- Sum(mat_unbal, effect = "individual") # default
xS1_mat_ind_narm_unbal <- Sum(mat_unbal, effect = "individual", na.rm = TRUE)

xS1_mat_no_index_ind_unbal      <- Sum(mat_noindex_unbal, effect = mat_index_unbal[[1L]]) # default
xS1_mat_no_index_ind_narm_unbal <- Sum(mat_noindex_unbal, effect = mat_index_unbal[[1L]], na.rm = TRUE)

# time
xS1_mat_ti      <- Sum(mat, effect = "time") # default
xS1_mat_ti_narm <- Sum(mat, effect = "time", na.rm = TRUE)

xS1_mat_no_index_ti      <- Sum(mat_noindex, effect = mat_index[[2L]]) # default
xS1_mat_no_index_ti_narm <- Sum(mat_noindex, effect = mat_index[[2L]], na.rm = TRUE)

xS1_mat_ti_unbal      <- Sum(mat_unbal, effect = "time") # default
xS1_mat_ti_narm_unbal <- Sum(mat_unbal, effect = "time", na.rm = TRUE)

xS1_mat_no_index_ti_unbal      <- Sum(mat_noindex_unbal, effect = mat_index_unbal[[2L]]) # default
xS1_mat_no_index_ti_narm_unbal <- Sum(mat_noindex_unbal, effect = mat_index_unbal[[2L]], na.rm = TRUE)

#### between/Between - default ####

# individual
xb1_d_ind      <- between(as.numeric(inv), effect = index(inv)[[1L]]) # default
xb1_d_ind_narm <- between(as.numeric(inv), effect = index(inv)[[1L]], na.rm = TRUE)

xB1_d_ind      <- Between(as.numeric(inv), effect = index(inv)[[1L]]) # default
xB1_d_ind_narm <- Between(as.numeric(inv), effect = index(inv)[[1L]], na.rm = TRUE)

xb1_d_ind_unbal      <- between(as.numeric(inv_unbal), effect = index(inv_unbal)[[1L]]) # default
xb1_d_ind_narm_unbal <- between(as.numeric(inv_unbal), effect = index(inv_unbal)[[1L]], na.rm = TRUE)

xB1_d_ind_unbal      <- Between(as.numeric(inv_unbal), effect = index(inv_unbal)[[1L]]) # default
xB1_d_ind_narm_unbal <- Between(as.numeric(inv_unbal), effect = index(inv_unbal)[[1L]], na.rm = TRUE)

# time
xb1_d_ti      <- between(as.numeric(inv), effect = index(inv)[[2L]]) # default
xb1_d_ti_narm <- between(as.numeric(inv), effect = index(inv)[[2L]], na.rm = TRUE)

xB1_d_ti      <- Between(as.numeric(inv), effect = index(inv)[[2L]]) # default
xB1_d_ti_narm <- Between(as.numeric(inv), effect = index(inv)[[2L]], na.rm = TRUE)

xb1_d_ti_unbal      <- between(as.numeric(inv_unbal), effect = index(inv_unbal)[[2L]]) # default
xb1_d_ti_narm_unbal <- between(as.numeric(inv_unbal), effect = index(inv_unbal)[[2L]], na.rm = TRUE)

xB1_d_ti_unbal      <- Between(as.numeric(inv_unbal), effect = index(inv_unbal)[[2L]]) # default
xB1_d_ti_narm_unbal <- Between(as.numeric(inv_unbal), effect = index(inv_unbal)[[2L]], na.rm = TRUE)

#### between/Between - pseries ####
xb1_ind      <- between(inv, effect = "individual") # default
xb1_ind_narm <- between(inv, effect = "individual", na.rm = TRUE)

xB1_ind      <- Between(inv, effect = "individual") # default
xB1_ind_narm <- Between(inv, effect = "individual", na.rm = TRUE)

xb1_ind_unbal      <- between(inv_unbal, effect = "individual") # default
xb1_ind_unbal_narm <- between(inv_unbal, effect = "individual", na.rm = TRUE)

xB1_ind_unbal      <- Between(inv_unbal, effect = "individual") # default
xB1_ind_unbal_narm <- Between(inv_unbal, effect = "individual", na.rm = TRUE)

# time
xb1_ti      <- between(inv, effect = "time") # default
xb1_ti_narm <- between(inv, effect = "time", na.rm = TRUE)

xB1_ti      <- Between(inv, effect = "time") # default
xB1_ti_narm <- Between(inv, effect = "time", na.rm = TRUE)

xb1_ti_unbal      <- between(inv_unbal, effect = "time") # default
xb1_ti_unbal_narm <- between(inv_unbal, effect = "time", na.rm = TRUE)

xB1_ti_unbal      <- Between(inv_unbal, effect = "time") # default
xB1_ti_unbal_narm <- Between(inv_unbal, effect = "time", na.rm = TRUE)

#### between/Between - matrix ####

# individual
xb1_mat_ind      <- between(mat, effect = "individual") # default
xb1_mat_ind_narm <- between(mat, effect = "individual", na.rm = TRUE)

xB1_mat_ind      <- Between(mat, effect = "individual") # default
xB1_mat_ind_narm <- Between(mat, effect = "individual", na.rm = TRUE)

xb1_mat_noindex_ind      <- between(mat_noindex, effect = mat_index[[1L]]) # default
xb1_mat_noindex_ind_narm <- between(mat_noindex, effect = mat_index[[1L]], na.rm = TRUE)

xB1_mat_noindex_ind      <- Between(mat_noindex, effect = mat_index[[1L]]) # default
xB1_mat_noindex_ind_narm <- Between(mat_noindex, effect = mat_index[[1L]], na.rm = TRUE)

# individual unbalanced
xb1_mat_unbal_ind      <- between(mat_unbal, effect = "individual") # default
xb1_mat_unbal_ind_narm <- between(mat_unbal, effect = "individual", na.rm = TRUE)

xB1_mat_unbal_ind      <- Between(mat_unbal, effect = "individual") # default
xB1_mat_unbal_ind_narm <- Between(mat_unbal, effect = "individual", na.rm = TRUE)

xb1_mat_noindex_unbal_ind      <- between(mat_noindex_unbal, effect = mat_index_unbal[[1L]]) # default
xb1_mat_noindex_unbal_ind_narm <- between(mat_noindex_unbal, effect = mat_index_unbal[[1L]], na.rm = TRUE)

xB1_mat_noindex_unbal_ind      <- Between(mat_noindex_unbal, effect = mat_index_unbal[[1L]]) # default
xB1_mat_noindex_unbal_ind_narm <- Between(mat_noindex_unbal, effect = mat_index_unbal[[1L]], na.rm = TRUE)

# time
xb1_mat_ti      <- between(mat, effect = "time") # default
xb1_mat_ti_narm <- between(mat, effect = "time", na.rm = TRUE)

xB1_mat_ti      <- Between(mat, effect = "time") # default
xB1_mat_ti_narm <- Between(mat, effect = "time", na.rm = TRUE)

xb1_mat_noindex_ti      <- between(mat_noindex, effect = mat_index[[2L]]) # default
xb1_mat_noindex_ti_narm <- between(mat_noindex, effect = mat_index[[2L]], na.rm = TRUE)

xB1_mat_noindex_ti      <- Between(mat_noindex, effect = mat_index[[2L]]) # default
xB1_mat_noindex_ti_narm <- Between(mat_noindex, effect = mat_index[[2L]], na.rm = TRUE)

# time unbalanced
xb1_mat_unbal_ti      <- between(mat_unbal, effect = "time") # default
xb1_mat_unbal_ti_narm <- between(mat_unbal, effect = "time", na.rm = TRUE)

xB1_mat_unbal_ti      <- Between(mat_unbal, effect = "time") # default
xB1_mat_unbal_ti_narm <- Between(mat_unbal, effect = "time", na.rm = TRUE)

xb1_mat_noindex_unbal_ti      <- between(mat_noindex_unbal, effect = mat_index_unbal[[2L]]) # default
xb1_mat_noindex_unbal_ti_narm <- between(mat_noindex_unbal, effect = mat_index_unbal[[2L]], na.rm = TRUE)

xB1_mat_noindex_unbal_ti      <- Between(mat_noindex_unbal, effect = mat_index_unbal[[2L]]) # default
xB1_mat_noindex_unbal_ti_narm <- Between(mat_noindex_unbal, effect = mat_index_unbal[[2L]], na.rm = TRUE)

#### within - default ####

# # individual (balanced + unbalanced)
xW1_d_ind      <- Within(as.numeric(inv), effect = index(inv)[[1L]])
xW1_d_ind_narm <- Within(as.numeric(inv), effect = index(inv)[[1L]], na.rm = TRUE)
 
xW1_d_ind_unbal      <- Within(as.numeric(inv_unbal), effect = index(inv_unbal)[[1L]])
xW1_d_ind_narm_unbal <- Within(as.numeric(inv_unbal), effect = index(inv_unbal)[[1L]], na.rm = TRUE)

# time (balanced + unbalanced)
xW1_d_ti      <- Within(as.numeric(inv), effect = index(inv)[[2L]])
xW1_d_ti_narm <- Within(as.numeric(inv), effect = index(inv)[[2L]], na.rm = TRUE)
 
xW1_d_ti_unbal      <- Within(as.numeric(inv_unbal), effect = index(inv_unbal)[[2L]])
xW1_d_ti_narm_unbal <- Within(as.numeric(inv_unbal), effect = index(inv_unbal)[[2L]], na.rm = TRUE)
 
# NB: Within.default does not handle twoways effects
 
#### within - pseries ####

xW1_ind      <- Within(inv, effect = "individual") # default
xW1_ind_narm <- Within(inv, effect = "individual", na.rm = TRUE)

xW1_ind_unbal      <- Within(inv_unbal, effect = "individual") # default
xW1_ind_narm_unbal <- Within(inv_unbal, effect = "individual", na.rm = TRUE)

# time
xW1_ti      <- Within(inv, effect = "time") # default
xW1_ti_narm <- Within(inv, effect = "time", na.rm = TRUE)

xW1_ti_unbal      <- Within(inv_unbal, effect = "time") # default
xW1_ti_narm_unbal <- Within(inv_unbal, effect = "time", na.rm = TRUE)

# twoways
# need to use non-NA data for plm's original 2-way FE unbalanced transformation (due to lm.fit being used)
	## so these cannot work
	# W1_tw      <- Within.pseries(inv, effect = "twoways") # default
	# W1_tw_narm <- Within.pseries(inv, effect = "twoways", na.rm = TRUE)

## but these:
xW1_tw_unbal_wona      <- Within(inv_unbal_wona, effect = "twoways") # default
xW1_tw_narm_unbal_wona <- Within(inv_unbal_wona, effect = "twoways", na.rm = TRUE)

#### within - matrix ####

# individual effect - balanced
xW1_mat_ind      <- Within(mat, effect = "individual") # default
xW1_mat_ind_narm <- Within(mat, effect = "individual", na.rm = TRUE)

# individual effect - unbalanced
xW1_mat_unbal_ind      <- Within(mat_unbal, effect = "individual") # default
xW1_mat_unbal_ind_narm <- Within(mat_unbal, effect = "individual", na.rm = TRUE)

# time effect - balanced
xW1_mat_ti      <- Within(mat, effect = "time") # default
xW1_mat_ti_narm <- Within(mat, effect = "time", na.rm = TRUE)

# time effect - unbalanced
xW1_mat_unbal_ti      <- Within(mat_unbal, effect = "time") # default
xW1_mat_unbal_ti_narm <- Within(mat_unbal, effect = "time", na.rm = TRUE)

# twoways - balanced
xW1_mat_tw      <- Within(mat, effect = "twoways") # default
xW1_mat_tw_narm <- Within(mat, effect = "twoways", na.rm = TRUE)

# twoways - unbalanced
# need to use non-NA data for plm's original 2-way FE unbalanced transformation (due to lm.fit being used)
xW1_mat_unbal_tw      <- Within(mat_unbal_wona, effect = "twoways") # default
xW1_mat_unbal_tw_narm <- Within(mat_unbal_wona, effect = "twoways", na.rm = TRUE)

###################### same but with fast functions ############################

## Run tests only if package 'collapse' is available (as it is Suggests dependency)
collapse.avail <- if (!requireNamespace("collapse", quietly = TRUE)) FALSE else TRUE

if(collapse.avail) {
options("plm.fast" = TRUE)


#### Sum - default ####

# individual
xS1_d_ind      <- Sum(as.numeric(inv), effect = index(inv)[[1L]]) # default
xS1_d_ind_narm <- Sum(as.numeric(inv), effect = index(inv)[[1L]], na.rm = TRUE)

xS1_d_ind_unbal      <- Sum(as.numeric(inv_unbal), effect = index(inv_unbal)[[1L]]) # default
xS1_d_ind_narm_unbal <- Sum(as.numeric(inv_unbal), effect = index(inv_unbal)[[1L]], na.rm = TRUE)

# time
xS1_d_ti      <- Sum(as.numeric(inv), effect = index(inv)[[2L]]) # default
xS1_d_ti_narm <- Sum(as.numeric(inv), effect = index(inv)[[2L]], na.rm = TRUE)

xS1_d_ti_unbal      <- Sum(as.numeric(inv_unbal), effect = index(inv_unbal)[[2L]]) # default
xS1_d_ti_narm_unbal <- Sum(as.numeric(inv_unbal), effect = index(inv_unbal)[[2L]], na.rm = TRUE)

#### Sum - pseries ####

# individual
xS1_ind      <- Sum(inv, effect = "individual") # default
xS1_ind_narm <- Sum(inv, effect = "individual", na.rm = TRUE)

xS1_ind_unbal      <- Sum(inv_unbal, effect = "individual") # default
xS1_ind_narm_unbal <- Sum(inv_unbal, effect = "individual", na.rm = TRUE)

# time
xS1_ti      <- Sum(inv, effect = "time") # default
xS1_ti_narm <- Sum(inv, effect = "time", na.rm = TRUE)

xS1_ti_unbal      <- Sum(inv_unbal, effect = "time") # default
xS1_ti_narm_unbal <- Sum(inv_unbal, effect = "time", na.rm = TRUE)

#### Sum - matrix ####

# individual
xS1_mat_ind      <- Sum(mat, effect = "individual") # default
xS1_mat_ind_narm <- Sum(mat, effect = "individual", na.rm = TRUE)

xS1_mat_no_index_ind      <- Sum(mat_noindex, effect = mat_index[[1L]]) # default
xS1_mat_no_index_ind_narm <- Sum(mat_noindex, effect = mat_index[[1L]], na.rm = TRUE)

xS1_mat_ind_unbal      <- Sum(mat_unbal, effect = "individual") # default
xS1_mat_ind_narm_unbal <- Sum(mat_unbal, effect = "individual", na.rm = TRUE)

xS1_mat_no_index_ind_unbal      <- Sum(mat_noindex_unbal, effect = mat_index_unbal[[1L]]) # default
xS1_mat_no_index_ind_narm_unbal <- Sum(mat_noindex_unbal, effect = mat_index_unbal[[1L]], na.rm = TRUE)

# time
xS1_mat_ti      <- Sum(mat, effect = "time") # default
xS1_mat_ti_narm <- Sum(mat, effect = "time", na.rm = TRUE)

xS1_mat_no_index_ti      <- Sum(mat_noindex, effect = mat_index[[2L]]) # default
xS1_mat_no_index_ti_narm <- Sum(mat_noindex, effect = mat_index[[2L]], na.rm = TRUE)

xS1_mat_ti_unbal      <- Sum(mat_unbal, effect = "time") # default
xS1_mat_ti_narm_unbal <- Sum(mat_unbal, effect = "time", na.rm = TRUE)

xS1_mat_no_index_ti_unbal      <- Sum(mat_noindex_unbal, effect = mat_index_unbal[[2L]]) # default
xS1_mat_no_index_ti_narm_unbal <- Sum(mat_noindex_unbal, effect = mat_index_unbal[[2L]], na.rm = TRUE)

#### between/Between - default ####

# individual
xb1_d_ind      <- between(as.numeric(inv), effect = index(inv)[[1L]]) # default
xb1_d_ind_narm <- between(as.numeric(inv), effect = index(inv)[[1L]], na.rm = TRUE)

xB1_d_ind      <- Between(as.numeric(inv), effect = index(inv)[[1L]]) # default
xB1_d_ind_narm <- Between(as.numeric(inv), effect = index(inv)[[1L]], na.rm = TRUE)

xb1_d_ind_unbal      <- between(as.numeric(inv_unbal), effect = index(inv_unbal)[[1L]]) # default
xb1_d_ind_narm_unbal <- between(as.numeric(inv_unbal), effect = index(inv_unbal)[[1L]], na.rm = TRUE)

xB1_d_ind_unbal      <- Between(as.numeric(inv_unbal), effect = index(inv_unbal)[[1L]]) # default
xB1_d_ind_narm_unbal <- Between(as.numeric(inv_unbal), effect = index(inv_unbal)[[1L]], na.rm = TRUE)

# time
xb1_d_ti      <- between(as.numeric(inv), effect = index(inv)[[2L]]) # default
xb1_d_ti_narm <- between(as.numeric(inv), effect = index(inv)[[2L]], na.rm = TRUE)

xB1_d_ti      <- Between(as.numeric(inv), effect = index(inv)[[2L]]) # default
xB1_d_ti_narm <- Between(as.numeric(inv), effect = index(inv)[[2L]], na.rm = TRUE)

xb1_d_ti_unbal      <- between(as.numeric(inv_unbal), effect = index(inv_unbal)[[2L]]) # default
xb1_d_ti_narm_unbal <- between(as.numeric(inv_unbal), effect = index(inv_unbal)[[2L]], na.rm = TRUE)

xB1_d_ti_unbal      <- Between(as.numeric(inv_unbal), effect = index(inv_unbal)[[2L]]) # default
xB1_d_ti_narm_unbal <- Between(as.numeric(inv_unbal), effect = index(inv_unbal)[[2L]], na.rm = TRUE)

#### between/Between - pseries ####
xb1_ind      <- between(inv, effect = "individual") # default
xb1_ind_narm <- between(inv, effect = "individual", na.rm = TRUE)

xB1_ind      <- Between(inv, effect = "individual") # default
xB1_ind_narm <- Between(inv, effect = "individual", na.rm = TRUE)

xb1_ind_unbal      <- between(inv_unbal, effect = "individual") # default
xb1_ind_unbal_narm <- between(inv_unbal, effect = "individual", na.rm = TRUE)

xB1_ind_unbal      <- Between(inv_unbal, effect = "individual") # default
xB1_ind_unbal_narm <- Between(inv_unbal, effect = "individual", na.rm = TRUE)

# time
xb1_ti      <- between(inv, effect = "time") # default
xb1_ti_narm <- between(inv, effect = "time", na.rm = TRUE)

xB1_ti      <- Between(inv, effect = "time") # default
xB1_ti_narm <- Between(inv, effect = "time", na.rm = TRUE)

xb1_ti_unbal      <- between(inv_unbal, effect = "time") # default
xb1_ti_unbal_narm <- between(inv_unbal, effect = "time", na.rm = TRUE)

xB1_ti_unbal      <- Between(inv_unbal, effect = "time") # default
xB1_ti_unbal_narm <- Between(inv_unbal, effect = "time", na.rm = TRUE)

#### between/Between - matrix ####

# individual
xb1_mat_ind      <- between(mat, effect = "individual") # default
xb1_mat_ind_narm <- between(mat, effect = "individual", na.rm = TRUE)

xB1_mat_ind      <- Between(mat, effect = "individual") # default
xB1_mat_ind_narm <- Between(mat, effect = "individual", na.rm = TRUE)

xb1_mat_noindex_ind      <- between(mat_noindex, effect = mat_index[[1L]]) # default
xb1_mat_noindex_ind_narm <- between(mat_noindex, effect = mat_index[[1L]], na.rm = TRUE)

xB1_mat_noindex_ind      <- Between(mat_noindex, effect = mat_index[[1L]]) # default
xB1_mat_noindex_ind_narm <- Between(mat_noindex, effect = mat_index[[1L]], na.rm = TRUE)

# individual unbalanced
xb1_mat_unbal_ind      <- between(mat_unbal, effect = "individual") # default
xb1_mat_unbal_ind_narm <- between(mat_unbal, effect = "individual", na.rm = TRUE)

xB1_mat_unbal_ind      <- Between(mat_unbal, effect = "individual") # default
xB1_mat_unbal_ind_narm <- Between(mat_unbal, effect = "individual", na.rm = TRUE)

xb1_mat_noindex_unbal_ind      <- between(mat_noindex_unbal, effect = mat_index_unbal[[1L]]) # default
xb1_mat_noindex_unbal_ind_narm <- between(mat_noindex_unbal, effect = mat_index_unbal[[1L]], na.rm = TRUE)

xB1_mat_noindex_unbal_ind      <- Between(mat_noindex_unbal, effect = mat_index_unbal[[1L]]) # default
xB1_mat_noindex_unbal_ind_narm <- Between(mat_noindex_unbal, effect = mat_index_unbal[[1L]], na.rm = TRUE)

# time
xb1_mat_ti      <- between(mat, effect = "time") # default
xb1_mat_ti_narm <- between(mat, effect = "time", na.rm = TRUE)

xB1_mat_ti      <- Between(mat, effect = "time") # default
xB1_mat_ti_narm <- Between(mat, effect = "time", na.rm = TRUE)

xb1_mat_noindex_ti      <- between(mat_noindex, effect = mat_index[[2L]]) # default
xb1_mat_noindex_ti_narm <- between(mat_noindex, effect = mat_index[[2L]], na.rm = TRUE)

xB1_mat_noindex_ti      <- Between(mat_noindex, effect = mat_index[[2L]]) # default
xB1_mat_noindex_ti_narm <- Between(mat_noindex, effect = mat_index[[2L]], na.rm = TRUE)

# time unbalanced
xb1_mat_unbal_ti      <- between(mat_unbal, effect = "time") # default
xb1_mat_unbal_ti_narm <- between(mat_unbal, effect = "time", na.rm = TRUE)

xB1_mat_unbal_ti      <- Between(mat_unbal, effect = "time") # default
xB1_mat_unbal_ti_narm <- Between(mat_unbal, effect = "time", na.rm = TRUE)

xb1_mat_noindex_unbal_ti      <- between(mat_noindex_unbal, effect = mat_index_unbal[[2L]]) # default
xb1_mat_noindex_unbal_ti_narm <- between(mat_noindex_unbal, effect = mat_index_unbal[[2L]], na.rm = TRUE)

xB1_mat_noindex_unbal_ti      <- Between(mat_noindex_unbal, effect = mat_index_unbal[[2L]]) # default
xB1_mat_noindex_unbal_ti_narm <- Between(mat_noindex_unbal, effect = mat_index_unbal[[2L]], na.rm = TRUE)

#### within - default ####

# # individual (balanced + unbalanced)
xW1_d_ind      <- Within(as.numeric(inv), effect = index(inv)[[1L]])
xW1_d_ind_narm <- Within(as.numeric(inv), effect = index(inv)[[1L]], na.rm = TRUE)

xW1_d_ind_unbal      <- Within(as.numeric(inv_unbal), effect = index(inv_unbal)[[1L]])
xW1_d_ind_narm_unbal <- Within(as.numeric(inv_unbal), effect = index(inv_unbal)[[1L]], na.rm = TRUE)

# time (balanced + unbalanced)
xW1_d_ti      <- Within(as.numeric(inv), effect = index(inv)[[2L]])
xW1_d_ti_narm <- Within(as.numeric(inv), effect = index(inv)[[2L]], na.rm = TRUE)

xW1_d_ti_unbal      <- Within(as.numeric(inv_unbal), effect = index(inv_unbal)[[2L]])
xW1_d_ti_narm_unbal <- Within(as.numeric(inv_unbal), effect = index(inv_unbal)[[2L]], na.rm = TRUE)

# NB: Within.default does not handle twoways effects

#### within - pseries ####

xW1_ind      <- Within(inv, effect = "individual") # default
xW1_ind_narm <- Within(inv, effect = "individual", na.rm = TRUE)

xW1_ind_unbal      <- Within(inv_unbal, effect = "individual") # default
xW1_ind_narm_unbal <- Within(inv_unbal, effect = "individual", na.rm = TRUE)

# time
xW1_ti      <- Within(inv, effect = "time") # default
xW1_ti_narm <- Within(inv, effect = "time", na.rm = TRUE)

xW1_ti_unbal      <- Within(inv_unbal, effect = "time") # default
xW1_ti_narm_unbal <- Within(inv_unbal, effect = "time", na.rm = TRUE)

# twoways
# need to use non-NA data for plm's original 2-way FE unbalanced transformation (due to lm.fit being used)
## so these cannot work
# W1_tw      <- Within.pseries(inv, effect = "twoways") # default
# W1_tw_narm <- Within.pseries(inv, effect = "twoways", na.rm = TRUE)

## but these:
xW1_tw_unbal_wona      <- Within(inv_unbal_wona, effect = "twoways") # default
xW1_tw_narm_unbal_wona <- Within(inv_unbal_wona, effect = "twoways", na.rm = TRUE)

#### within - matrix ####

# individual effect - balanced
xW1_mat_ind      <- Within(mat, effect = "individual") # default
xW1_mat_ind_narm <- Within(mat, effect = "individual", na.rm = TRUE)

# individual effect - unbalanced
xW1_mat_unbal_ind      <- Within(mat_unbal, effect = "individual") # default
xW1_mat_unbal_ind_narm <- Within(mat_unbal, effect = "individual", na.rm = TRUE)

# time effect - balanced
xW1_mat_ti      <- Within(mat, effect = "time") # default
xW1_mat_ti_narm <- Within(mat, effect = "time", na.rm = TRUE)

# time effect - unbalanced
xW1_mat_unbal_ti      <- Within(mat_unbal, effect = "time") # default
xW1_mat_unbal_ti_narm <- Within(mat_unbal, effect = "time", na.rm = TRUE)

# twoways - balanced
xW1_mat_tw      <- Within(mat, effect = "twoways") # default
xW1_mat_tw_narm <- Within(mat, effect = "twoways", na.rm = TRUE)

# twoways - unbalanced
# need to use non-NA data for plm's original 2-way FE unbalanced transformation (due to lm.fit being used)
xW1_mat_unbal_tw      <- Within(mat_unbal_wona, effect = "twoways") # default
xW1_mat_unbal_tw_narm <- Within(mat_unbal_wona, effect = "twoways", na.rm = TRUE)

}