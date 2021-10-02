## test for order of output of between() and hence fixef() and ranef()
# -> factor level order or order of appearance in original data
## "since ever" plm had level order but this was changed to appearance order
# in plm 2.4-0 and changed back in rev. 1307 for plm 2.4-2.
# => factor level order makes more sense!

library(plm)
data("Crime", package = "plm")
delrows.a <- -c( 1,  2, 10, 12)
delrows.b <- -c(10, 12, 17, 18)

Crime.unbal.a <- Crime[delrows.a, ]
Crime.unbal.b <- Crime[delrows.b, ]

pCrime.unbal.a <- pdata.frame(Crime.unbal.a)
pCrime.unbal.b <- pdata.frame(Crime.unbal.b)

ix.a <- index(pCrime.unbal.a) # unclass for speed
ix.b <- index(pCrime.unbal.b) # unclass for speed


## between.default ##
options("plm.fast" = FALSE)
(bet.def.unbal.a <- between(Crime.unbal.a$crmrte, effect = ix.a[[1L]])) # individual effect
(bet.def.unbal.b <- between(Crime.unbal.b$crmrte, effect = ix.b[[1L]])) # individual effect

options("plm.fast" = FALSE)
(bet.def.plm.slow.unbal.a <- between(Crime.unbal.a$crmrte, effect = ix.a[[2L]])) # year 81 first from rev. 1307 (and plm < 2.4-0) (year 83 first in rev. 1305 (and plm 2.4-0/1))
(bet.def.plm.slow.unbal.b <- between(Crime.unbal.b$crmrte, effect = ix.b[[2L]])) # year 81 first

## Run tests only if package 'collapse' is available
## (as they are 'Suggests' dependencies)
collapse.avail <- if (!requireNamespace("collapse", quietly = TRUE)) FALSE else TRUE

if(collapse.avail) {
  
options("plm.fast" = TRUE) # same
(bet.def.plm.fast.unbal.a <- between(Crime.unbal.a$crmrte, effect = ix.a[[2L]])) # year 81 first from rev. 1307 (and plm < 2.4-0) (year 83 first in rev. 1305 (and plm 2.4-0/1))
(bet.def.plm.fast.unbal.b <- between(Crime.unbal.b$crmrte, effect = ix.b[[2L]])) # year 81 first

stopifnot(isTRUE(all.equal(names(bet.def.unbal.a), levels(ix.a[[1L]]))))
stopifnot(isTRUE(all.equal(names(bet.def.unbal.b), levels(ix.b[[1L]]))))

stopifnot(isTRUE(all.equal(names(bet.def.plm.slow.unbal.a), levels(ix.a[[2L]]))))
stopifnot(isTRUE(all.equal(names(bet.def.plm.slow.unbal.b), levels(ix.b[[2L]]))))

stopifnot(isTRUE(all.equal(names(bet.def.plm.fast.unbal.a), levels(ix.a[[2L]]))))
stopifnot(isTRUE(all.equal(names(bet.def.plm.fast.unbal.b), levels(ix.b[[2L]]))))


stopifnot(isTRUE(all.equal(bet.def.plm.slow.unbal.a, bet.def.plm.fast.unbal.a)))
stopifnot(isTRUE(all.equal(bet.def.plm.slow.unbal.b, bet.def.plm.fast.unbal.b)))

## between.pseries ##
options("plm.fast" = FALSE)
(bet.pser.unbal.a <- between(pCrime.unbal.a$crmrte, effect = "individual"))
(bet.pser.unbal.b <- between(pCrime.unbal.b$crmrte, effect = "individual"))

options("plm.fast" = FALSE)
(bet.pser.plm.slow.unbal.a <- between(pCrime.unbal.a$crmrte, effect = "time")) # year 81 first from rev. 1307 (and plm < 2.4-0) (year 83 first in rev. 1305 (and plm 2.4-0/1))
(bet.pser.plm.slow.unbal.b <- between(pCrime.unbal.b$crmrte, effect = "time")) # year 81 first

options("plm.fast" = TRUE) # same
(bet.pser.plm.fast.unbal.a <- between(pCrime.unbal.a$crmrte, effect = "time")) # year 81 first from rev. 1307 (and plm < 2.4-0) (year 83 first in rev. 1305 (and plm 2.4-0/1))
(bet.pser.plm.fast.unbal.b <- between(pCrime.unbal.b$crmrte, effect = "time")) # year 81 first

stopifnot(isTRUE(all.equal(names(bet.pser.unbal.a), levels(index(pCrime.unbal.a$crmrte)[[1L]]))))
stopifnot(isTRUE(all.equal(names(bet.pser.unbal.b), levels(index(pCrime.unbal.b$crmrte)[[1L]]))))

stopifnot(isTRUE(all.equal(names(bet.pser.plm.slow.unbal.a), levels(index(pCrime.unbal.a$crmrte)[[2L]]))))
stopifnot(isTRUE(all.equal(names(bet.pser.plm.slow.unbal.a), levels(index(pCrime.unbal.b$crmrte)[[2L]]))))

stopifnot(isTRUE(all.equal(names(bet.pser.plm.fast.unbal.a), levels(index(pCrime.unbal.a$crmrte)[[2L]]))))
stopifnot(isTRUE(all.equal(names(bet.pser.plm.fast.unbal.a), levels(index(pCrime.unbal.b$crmrte)[[2L]]))))


stopifnot(isTRUE(all.equal(bet.pser.plm.slow.unbal.a, bet.pser.plm.fast.unbal.a)))
stopifnot(isTRUE(all.equal(bet.pser.plm.slow.unbal.b, bet.pser.plm.fast.unbal.b)))


## between.matrix - no index case ##
mat_no_index.unbal.a <- as.matrix(Crime.unbal.a[ , 1:6])[ , 3:4]
mat_no_index.unbal.b <- as.matrix(Crime.unbal.b[ , 1:6])[ , 3:4]

options("plm.fast" = FALSE)
(bet.matnoi.unbal.a <- between(mat_no_index.unbal.a, effect = ix.a[[1L]])) # individual effect
(bet.matnoi.unbal.b <- between(mat_no_index.unbal.b, effect = ix.b[[1L]])) # individual effect

options("plm.fast" = FALSE)
(bet.matnoi.plm.slow.unbal.a <- between(mat_no_index.unbal.a, effect = ix.a[[2L]])) # year 81 first from rev. 1307 (and plm < 2.4-0) (year 83 first in rev. 1305 (and plm 2.4-0/1))
(bet.matnoi.plm.slow.unbal.b <- between(mat_no_index.unbal.b, effect = ix.b[[2L]])) # year 81 first

options("plm.fast" = TRUE)
(bet.matnoi.plm.fast.unbal.a <- between(mat_no_index.unbal.a, effect = ix.a[[2L]])) # year 81 first from rev. 1307 (and plm < 2.4-0) (year 83 first in rev. 1305 (and plm 2.4-0/1))
(bet.matnoi.plm.fast.unbal.b <- between(mat_no_index.unbal.b, effect = ix.b[[2L]])) # year 81 first

stopifnot(isTRUE(all.equal(rownames(bet.matnoi.unbal.a), levels(ix.a[[1L]]))))
stopifnot(isTRUE(all.equal(rownames(bet.matnoi.unbal.b), levels(ix.b[[1L]]))))

stopifnot(isTRUE(all.equal(rownames(bet.matnoi.plm.slow.unbal.a), levels(ix.a[[2L]]))))
stopifnot(isTRUE(all.equal(rownames(bet.matnoi.plm.slow.unbal.b), levels(ix.b[[2L]]))))

stopifnot(isTRUE(all.equal(rownames(bet.matnoi.plm.fast.unbal.a), levels(ix.a[[2L]]))))
stopifnot(isTRUE(all.equal(rownames(bet.matnoi.plm.fast.unbal.b), levels(ix.b[[2L]]))))


stopifnot(isTRUE(all.equal(bet.matnoi.plm.slow.unbal.a, bet.matnoi.plm.fast.unbal.a)))
stopifnot(isTRUE(all.equal(bet.matnoi.plm.slow.unbal.b, bet.matnoi.plm.fast.unbal.b)))


## between.matrix - index case ##
mat_index.unbal.a <- mat_no_index.unbal.a
mat_index.unbal.b <- mat_no_index.unbal.b
attr(mat_index.unbal.a, "index") <- ix.a
attr(mat_index.unbal.b, "index") <- ix.b

options("plm.fast" = FALSE)
(bet.mati.unbal.a <- between(mat_index.unbal.a, effect = "individual")) # individual effect
(bet.mati.unbal.b <- between(mat_index.unbal.b, effect = "individual")) # individual effect

options("plm.fast" = FALSE)
(bet.mati.plm.slow.unbal.a <- between(mat_index.unbal.a, effect = "time")) # year 81 first from rev. 1307 (and plm < 2.4-0) (year 83 first in rev. 1305 (and plm 2.4-0/1))
(bet.mati.plm.slow.unbal.b <- between(mat_index.unbal.b, effect = "time")) # year 81 first

options("plm.fast" = TRUE)
(bet.mati.plm.fast.unbal.a <- between(mat_index.unbal.a, effect = "time")) # year 81 first from rev. 1307 (and plm < 2.4-0) (year 83 first in rev. 1305 (and plm 2.4-0/1))
(bet.mati.plm.fast.unbal.b <- between(mat_index.unbal.b, effect = "time")) # year 81 first

stopifnot(isTRUE(all.equal(rownames(bet.mati.unbal.a), levels(ix.a[[1L]]))))
stopifnot(isTRUE(all.equal(rownames(bet.mati.unbal.b), levels(ix.b[[1L]]))))

stopifnot(isTRUE(all.equal(rownames(bet.mati.plm.slow.unbal.a), levels(ix.a[[2L]]))))
stopifnot(isTRUE(all.equal(rownames(bet.mati.plm.slow.unbal.b), levels(ix.b[[2L]]))))

stopifnot(isTRUE(all.equal(rownames(bet.mati.plm.fast.unbal.a), levels(ix.a[[2L]]))))
stopifnot(isTRUE(all.equal(rownames(bet.mati.plm.fast.unbal.b), levels(ix.b[[2L]]))))


stopifnot(isTRUE(all.equal(bet.mati.plm.slow.unbal.a, bet.mati.plm.fast.unbal.a)))
stopifnot(isTRUE(all.equal(bet.mati.plm.slow.unbal.b, bet.mati.plm.fast.unbal.b)))


### fixef ###

crime_formula_plm_FE <- lcrmrte ~ lprbarr + lpolpc + lprbconv + lprbpris + lavgsen +
  ldensity + lwcon + lwtuc + lwtrd + lwfir + lwser + lwmfg + lwfed +
  lwsta + lwloc + lpctymle

FE_id.a <- plm(crime_formula_plm_FE, data = pCrime.unbal.a, model = "within", effect = "individual")
FE_ti.a <- plm(crime_formula_plm_FE, data = pCrime.unbal.a, model = "within", effect = "time")
FE_tw.a <- plm(crime_formula_plm_FE, data = pCrime.unbal.a, model = "within", effect = "twoways")

FE_id.b <- plm(crime_formula_plm_FE, data = pCrime.unbal.b, model = "within", effect = "individual")
FE_ti.b <- plm(crime_formula_plm_FE, data = pCrime.unbal.b, model = "within", effect = "time")
FE_tw.b <- plm(crime_formula_plm_FE, data = pCrime.unbal.b, model = "within", effect = "twoways")

options("plm.fast" = FALSE)
(fx_fe_plm.slow.id.a    <- fixef(FE_id.a))
(fx_fe_plm.slow.ti.a    <- fixef(FE_ti.a)) # year 81 first from rev. 1307 (and plm < 2.4-0) (year 83 first in rev. 1305 (and plm 2.4-0/1))
(fx_fe_plm.slow.tw.id.a <- fixef(FE_tw.a, effect = "individual"))
(fx_fe_plm.slow.tw.ti.a <- fixef(FE_tw.a, effect = "time")) # year 81 first (order same as *.b for 2-ways FE)
(fx_fe_plm.slow.tw.tw.a <- fixef(FE_tw.a, effect = "twoways"))

(fx_fe_plm.slow.id.b    <- fixef(FE_id.b))
(fx_fe_plm.slow.ti.b    <- fixef(FE_ti.b)) # year 81 first from rev. 1307 (and plm < 2.4-0) (year 83 first in rev. 1305 (and plm 2.4-0/1))
(fx_fe_plm.slow.tw.id.b <- fixef(FE_tw.b, effect = "individual"))
(fx_fe_plm.slow.tw.ti.b <- fixef(FE_tw.b, effect = "time"))  # year 81 first (order same as *.a for 2-ways FE)
(fx_fe_plm.slow.tw.tw.b <- fixef(FE_tw.b, effect = "twoways"))

options("plm.fast" = TRUE) # same
(fx_fe_plm.fast.id.a    <- fixef(FE_id.a))
(fx_fe_plm.fast.ti.a    <- fixef(FE_ti.a)) # year 81 first from rev. 1307 (and plm < 2.4-0) (year 83 first in rev. 1305 (and plm 2.4-0/1))
(fx_fe_plm.fast.tw.id.a <- fixef(FE_tw.a, effect = "individual"))
(fx_fe_plm.fast.tw.ti.a <- fixef(FE_tw.a, effect = "time")) # year 81 first (order same as *.b for 2-ways FE)
(fx_fe_plm.fast.tw.tw.a <- fixef(FE_tw.a, effect = "twoways"))

(fx_fe_plm.fast.id.b    <- fixef(FE_id.b))
(fx_fe_plm.fast.ti.b    <- fixef(FE_ti.b)) # year 81 first from rev. 1307 (and plm < 2.4-0) (year 83 first in rev. 1305 (and plm 2.4-0/1))
(fx_fe_plm.fast.tw.id.b <- fixef(FE_tw.b, effect = "individual"))
(fx_fe_plm.fast.tw.ti.b <- fixef(FE_tw.b, effect = "time"))  # year 81 first (order same as *.a for 2-ways FE)
(fx_fe_plm.fast.tw.tw.b <- fixef(FE_tw.b, effect = "twoways"))


stopifnot(isTRUE(all.equal(names(fx_fe_plm.slow.id.a),    levels(ix.a[[1L]]))))
stopifnot(isTRUE(all.equal(names(fx_fe_plm.slow.ti.a),    levels(ix.a[[2L]]))))
stopifnot(isTRUE(all.equal(names(fx_fe_plm.slow.tw.id.a), levels(ix.a[[1L]]))))
stopifnot(isTRUE(all.equal(names(fx_fe_plm.slow.tw.ti.a), levels(ix.a[[2L]]))))
stopifnot(isTRUE(all.equal(names(fx_fe_plm.slow.tw.tw.a), paste(ix.a[[1L]], ix.a[[2L]], sep = "-"))))

stopifnot(isTRUE(all.equal(names(fx_fe_plm.slow.id.b),    levels(ix.b[[1L]]))))
stopifnot(isTRUE(all.equal(names(fx_fe_plm.slow.ti.b),    levels(ix.b[[2L]]))))
stopifnot(isTRUE(all.equal(names(fx_fe_plm.slow.tw.id.b), levels(ix.b[[1L]]))))
stopifnot(isTRUE(all.equal(names(fx_fe_plm.slow.tw.ti.b), levels(ix.b[[2L]]))))
stopifnot(isTRUE(all.equal(names(fx_fe_plm.slow.tw.tw.b), paste(ix.b[[1L]], ix.b[[2L]], sep = "-"))))

stopifnot(isTRUE(all.equal(names(fx_fe_plm.fast.id.a),    levels(ix.a[[1L]]))))
stopifnot(isTRUE(all.equal(names(fx_fe_plm.fast.ti.a),    levels(ix.a[[2L]]))))
stopifnot(isTRUE(all.equal(names(fx_fe_plm.fast.tw.id.a), levels(ix.a[[1L]]))))
stopifnot(isTRUE(all.equal(names(fx_fe_plm.fast.tw.ti.a), levels(ix.a[[2L]]))))
stopifnot(isTRUE(all.equal(names(fx_fe_plm.fast.tw.tw.a), paste(ix.a[[1L]], ix.a[[2L]], sep = "-"))))

stopifnot(isTRUE(all.equal(names(fx_fe_plm.fast.id.b),    levels(ix.b[[1L]]))))
stopifnot(isTRUE(all.equal(names(fx_fe_plm.fast.ti.b),    levels(ix.b[[2L]]))))
stopifnot(isTRUE(all.equal(names(fx_fe_plm.fast.tw.id.b), levels(ix.b[[1L]]))))
stopifnot(isTRUE(all.equal(names(fx_fe_plm.fast.tw.ti.b), levels(ix.b[[2L]]))))
stopifnot(isTRUE(all.equal(names(fx_fe_plm.fast.tw.tw.b), paste(ix.b[[1L]], ix.b[[2L]], sep = "-"))))



stopifnot(isTRUE(all.equal(fx_fe_plm.slow.id.a,    fx_fe_plm.fast.id.a)))
stopifnot(isTRUE(all.equal(fx_fe_plm.slow.ti.a,    fx_fe_plm.fast.ti.a)))
stopifnot(isTRUE(all.equal(fx_fe_plm.slow.tw.id.a, fx_fe_plm.fast.tw.id.a)))
stopifnot(isTRUE(all.equal(fx_fe_plm.slow.tw.ti.a, fx_fe_plm.fast.tw.ti.a)))
stopifnot(isTRUE(all.equal(fx_fe_plm.slow.tw.tw.a, fx_fe_plm.fast.tw.tw.a)))


stopifnot(isTRUE(all.equal(fx_fe_plm.slow.id.b,    fx_fe_plm.fast.id.b)))
stopifnot(isTRUE(all.equal(fx_fe_plm.slow.ti.b,    fx_fe_plm.fast.ti.b)))
stopifnot(isTRUE(all.equal(fx_fe_plm.slow.tw.id.b, fx_fe_plm.fast.tw.id.b)))
stopifnot(isTRUE(all.equal(fx_fe_plm.slow.tw.ti.b, fx_fe_plm.fast.tw.ti.b)))
stopifnot(isTRUE(all.equal(fx_fe_plm.slow.tw.tw.b, fx_fe_plm.fast.tw.tw.b)))

### ranef ###

crime_formula_plm_RE <- lpctymle ~ lmix + lprbconv

RE_id.a <- plm(crime_formula_plm_RE, data = pCrime.unbal.a, model = "random", effect = "individual")
RE_ti.a <- plm(crime_formula_plm_RE, data = pCrime.unbal.a, model = "random", effect = "time")
RE_tw.a <- plm(crime_formula_plm_RE, data = pCrime.unbal.a, model = "random", effect = "twoways")

RE_id.b <- plm(crime_formula_plm_RE, data = pCrime.unbal.b, model = "random", effect = "individual")
RE_ti.b <- plm(crime_formula_plm_RE, data = pCrime.unbal.b, model = "random", effect = "time")
RE_tw.b <- plm(crime_formula_plm_RE, data = pCrime.unbal.b, model = "random", effect = "twoways")

options("plm.fast" = FALSE)
(fx_re_plm.slow.id.a    <- ranef(RE_id.a))
(fx_re_plm.slow.ti.a    <- ranef(RE_ti.a)) # year 81 first from rev. 1307 (and plm < 2.4-0) (year 83 first in rev. 1305 (and plm 2.4-0/1))
(fx_re_plm.slow.tw.id.a <- ranef(RE_tw.a, effect = "individual"))
(fx_re_plm.slow.tw.ti.a <- ranef(RE_tw.a, effect = "time")) # year 81 first (order same as *.b for 2-ways FE)
# (fx_re_plm.slow.tw.tw.a <- ranef(RE_tw.a, effect = "twoways")) # do not have this for ranef

(fx_re_plm.slow.id.b    <- ranef(RE_id.b))
(fx_re_plm.slow.ti.b    <- ranef(RE_ti.b)) # year 81 first from rev. 1307 (and plm < 2.4-0) (year 83 first in rev. 1305 (and plm 2.4-0/1))
(fx_re_plm.slow.tw.id.b <- ranef(RE_tw.b, effect = "individual"))
(fx_re_plm.slow.tw.ti.b <- ranef(RE_tw.b, effect = "time"))  # year 81 first (order same as *.a for 2-ways FE)
# (fx_re_plm.slow.tw.tw.b <- ranef(RE_tw.b, effect = "twoways")) # do not have this for ranef

options("plm.fast" = TRUE) # same
(fx_re_plm.fast.id.a    <- ranef(RE_id.a))
(fx_re_plm.fast.ti.a    <- ranef(RE_ti.a)) # year 81 first from rev. 1307 (and plm < 2.4-0) (year 83 first in rev. 1305 (and plm 2.4-0/1))
(fx_re_plm.fast.tw.id.a <- ranef(RE_tw.a, effect = "individual"))
(fx_re_plm.fast.tw.ti.a <- ranef(RE_tw.a, effect = "time")) # year 81 first (order same as *.b for 2-ways FE)
# (fx_re_plm.fast.tw.tw.a <- ranef(RE_tw.a, effect = "twoways")) # do not have this for ranef

(fx_re_plm.fast.id.b    <- ranef(RE_id.b))
(fx_re_plm.fast.ti.b    <- ranef(RE_ti.b)) # year 81 first from rev. 1307 (and plm < 2.4-0) (year 83 first in rev. 1305 (and plm 2.4-0/1))
(fx_re_plm.fast.tw.id.b <- ranef(RE_tw.b, effect = "individual"))
(fx_re_plm.fast.tw.ti.b <- ranef(RE_tw.b, effect = "time"))  # year 81 first (order same as *.a for 2-ways FE)
# (fx_re_plm.fast.tw.tw.b <- # ranef(RE_tw.b, effect = "twoways")) # do not have this for ranef

stopifnot(isTRUE(all.equal(names(fx_re_plm.slow.id.a),    levels(ix.a[[1L]]))))
stopifnot(isTRUE(all.equal(names(fx_re_plm.slow.ti.a),    levels(ix.a[[2L]]))))
stopifnot(isTRUE(all.equal(names(fx_re_plm.slow.tw.id.a), levels(ix.a[[1L]]))))
stopifnot(isTRUE(all.equal(names(fx_re_plm.slow.tw.ti.a), levels(ix.a[[2L]]))))
# stopifnot(isTRUE(all.equal(names(fx_re_plm.slow.tw.tw.a), levels(ix.a[[2L]])))) # don't have this for ranef

stopifnot(isTRUE(all.equal(names(fx_re_plm.slow.id.b),    levels(ix.b[[1L]]))))
stopifnot(isTRUE(all.equal(names(fx_re_plm.slow.ti.b),    levels(ix.b[[2L]]))))
stopifnot(isTRUE(all.equal(names(fx_re_plm.slow.tw.id.b), levels(ix.b[[1L]]))))
stopifnot(isTRUE(all.equal(names(fx_re_plm.slow.tw.ti.b), levels(ix.b[[2L]]))))
# stopifnot(isTRUE(all.equal(names(fx_fe_plm.slow.tw.tw.b), levels(ix.b[[2L]])) # don't have this for ranef

stopifnot(isTRUE(all.equal(names(fx_re_plm.fast.id.a),    levels(ix.a[[1L]]))))
stopifnot(isTRUE(all.equal(names(fx_re_plm.fast.ti.a),    levels(ix.a[[2L]]))))
stopifnot(isTRUE(all.equal(names(fx_re_plm.fast.tw.id.a), levels(ix.a[[1L]]))))
stopifnot(isTRUE(all.equal(names(fx_re_plm.fast.tw.ti.a), levels(ix.a[[2L]]))))
# stopifnot(isTRUE(all.equal(names(fx_re_plm.fast.tw.tw.a), levels(ix.a[[2L]])))) # don't have this for ranef

stopifnot(isTRUE(all.equal(names(fx_re_plm.fast.id.b),    levels(ix.b[[1L]]))))
stopifnot(isTRUE(all.equal(names(fx_re_plm.fast.ti.b),    levels(ix.b[[2L]]))))
stopifnot(isTRUE(all.equal(names(fx_re_plm.fast.tw.id.b), levels(ix.b[[1L]]))))
stopifnot(isTRUE(all.equal(names(fx_re_plm.fast.tw.ti.b), levels(ix.b[[2L]]))))
# stopifnot(isTRUE(all.equal(names(fx_re_plm.fast.tw.tw.b), levels(ix.b[[2L]])))) # don't have this for ranef

stopifnot(isTRUE(all.equal(fx_re_plm.slow.id.a,    fx_re_plm.fast.id.a)))
stopifnot(isTRUE(all.equal(fx_re_plm.slow.ti.a,    fx_re_plm.fast.ti.a)))
stopifnot(isTRUE(all.equal(fx_re_plm.slow.tw.id.a, fx_re_plm.fast.tw.id.a)))
stopifnot(isTRUE(all.equal(fx_re_plm.slow.tw.ti.a, fx_re_plm.fast.tw.ti.a)))

stopifnot(isTRUE(all.equal(fx_re_plm.slow.id.b,    fx_re_plm.fast.id.b)))
stopifnot(isTRUE(all.equal(fx_re_plm.slow.ti.b,    fx_re_plm.fast.ti.b)))
stopifnot(isTRUE(all.equal(fx_re_plm.slow.tw.id.b, fx_re_plm.fast.tw.id.b)))
stopifnot(isTRUE(all.equal(fx_re_plm.slow.tw.ti.b, fx_re_plm.fast.tw.ti.b)))

}