# Test for pdiff (internal function) for the time dimension (effect = "time")

library(plm)
data("Grunfeld", package = "plm")
pGrunfeld <- pdata.frame(Grunfeld)
form <- inv ~ value + capital
fd_id <- plm(form, data = Grunfeld, model = "fd")
print(summary(fd_id))
print(vcovHC(fd_id))
print(vcovHC(fd_id, cluster = "time"))

## FD models with effect = "time" are be prevented from estimation due to
## not meaningful ordering of individuals
#
# fd_time <- plm(form, data = Grunfeld, model = "fd", effect = "time")
# summary(fd_time)
# vcovHC(fd_time)
# vcovHC(fd_time, cluster = "group")


pGrunfeld <- pdata.frame(Grunfeld)
modmat_id   <- model.matrix(pFormula(form), data = pGrunfeld, model = "fd", effect = "individual")
#YC modmat_time <- model.matrix(pFormula(form), data = pGrunfeld, model = "fd", effect = "time")

if (nrow(modmat_id)   != 190) stop(paste0("nrow not correct, should be 190, is: ", nrow(modmat_id)))
#YC if (nrow(modmat_time) != 180) stop(paste0("nrow not correct, should be 180, is: ", nrow(modmat_time)))

# "layout" of resulting matrix for effect = "time":
1362.4 - 3078.5 # pos  1 t_1 - t_1 for id 2,1 [orig pos 21 - orig pos 1]
1170.6 - 1362.4 # pos 21 t_t - t_1 for id 3,2 [orig pos 41 - orig pos 21]
417.5  - 1170.6 # pos 41 t_t - t_1 for id 4,3 [orig pos 61 - orig pos 41]

# formal test
#YC if (!isTRUE(all.equal(1362.4-3078.5, modmat_time[1,  "value"]))) stop("position for effect = \"time\" not correct")
#YC if (!isTRUE(all.equal(1170.6-1362.4, modmat_time[21, "value"]))) stop("position for effect = \"time\" not correct")
#YC if (!isTRUE(all.equal(417.5-1170.6,  modmat_time[41, "value"]))) stop("position for effect = \"time\" not correct")

head(modmat_id,   41)
#YC head(modmat_time, 41)
head(Grunfeld,    41)


# check pseries
pdiff_id   <- plm:::pdiff(pGrunfeld[ , "value"], effect = "individual")
#YC pdiff_time <- plm:::pdiff(pGrunfeld[ , "value"], effect = "time")

pos_first_id <- which(pGrunfeld$firm == 1)
pos_first_time <- which(pGrunfeld$year == 1935)

diff_id   <- base::diff(Grunfeld[pos_first_id,   "value"])
diff_time <- base::diff(Grunfeld[pos_first_time, "value"])

if (!isTRUE(all.equal(pdiff_id[pos_first_id[-length(pos_first_id)]],  diff_id, check.attributes = FALSE))) stop("pdiff on individual not correct")
#YC if (!isTRUE(all.equal(pdiff_time[pos_first_time[-length(pos_first_time)]], diff_time, check.attributes = FALSE))) stop("pdiff on time not correct")
