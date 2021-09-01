###  test file to test plm::fixef(), inner consistency and vs. fixest::fixef() and lfe::getfe()
## (1) ordinary regression models
## (2) IV regression models

library(plm)
data("Grunfeld", package = "plm")
Grunfeld_unbalanced <- Grunfeld[1:199, ]


################## (1) ordinary models (non-IV) ##################

plm_tw   <-  plm(inv ~     value + capital              , data = Grunfeld,            effect = "twoways")
plm_tw_u <-  plm(inv ~     value + capital              , data = Grunfeld_unbalanced, effect = "twoways")

plm_ow_id   <-  plm(inv ~     value + capital              , data = Grunfeld,            effect = "individual")
plm_ow_u_id <-  plm(inv ~     value + capital              , data = Grunfeld_unbalanced, effect = "individual")
plm_ow_ti   <-  plm(inv ~     value + capital              , data = Grunfeld,            effect = "time")
plm_ow_u_ti <-  plm(inv ~     value + capital              , data = Grunfeld_unbalanced, effect = "time")


## lm_tw       <- lm(inv ~ 0 + value + capital + factor(firm) + factor(year), data = Grunfeld)
## lm_tw_int   <- lm(inv ~     value + capital + factor(firm) + factor(year), data = Grunfeld)
## lm_tw_u     <- lm(inv ~ 0 + value + capital + factor(firm) + factor(year), data = Grunfeld_unbalanced)
## lm_tw_u_int <- lm(inv ~     value + capital + factor(firm) + factor(year), data = Grunfeld_unbalanced)

#### tw unbalanced ####
## sum of effects
plm_tw_u_fixef_tw <- as.numeric(fixef(plm_tw_u, "twoways"))

# manual
pred_betas <- as.numeric(tcrossprod(coef(plm_tw_u), model.matrix(plm_tw_u, model = "pooling")[ , -1L]))
pred_y <- plm_tw_u$model[ , 1] - plm_tw_u$residuals
pred_effs_tw <- pred_y - pred_betas

## split in a individual and in a time component
plm_tw_u_fixef_id_dfirst <- c(0, as.numeric(fixef(plm_tw_u, "individual", "dfirst")))
plm_tw_u_fixef_ti_dfirst <- c(0, as.numeric(fixef(plm_tw_u, "time",       "dfirst")))

plm_tw_u_fixef_id_level <- as.numeric(fixef(plm_tw_u, "individual"))
plm_tw_u_fixef_ti_level <- as.numeric(fixef(plm_tw_u, "time"))

## check the summed up effects and splits
# effect = "twoways" (= sum) vs. manual
stopifnot(isTRUE(all.equal(plm_tw_u_fixef_tw,
													 as.numeric(pred_effs_tw),
													 check.attributes = FALSE)))
# sum = id level + time dfirst
stopifnot(isTRUE(all.equal(plm_tw_u_fixef_tw,
													 plm_tw_u_fixef_id_level[ index(plm_tw_u)[[1L]]] +
													 plm_tw_u_fixef_ti_dfirst[index(plm_tw_u)[[2L]]],
													 check.attributes = FALSE)))
# sum = id dfirst + time level
stopifnot(isTRUE(all.equal(plm_tw_u_fixef_tw,
													 plm_tw_u_fixef_id_dfirst[index(plm_tw_u)[[1L]]] +
													 plm_tw_u_fixef_ti_level[ index(plm_tw_u)[[2L]]],
													 check.attributes = FALSE)))


### # checks vs. a twoway model implemented via one-way augmented
plm_tw_u_aug_id <- plm(inv ~ value + capital + factor(year), data = Grunfeld_unbalanced, effect = "individual")
plm_tw_u_aug_ti <- plm(inv ~ value + capital + factor(firm), data = Grunfeld_unbalanced, effect = "time")

plm_tw_u_aug_id_id <- as.numeric(fixef(plm_tw_u_aug_id, "individual"))
plm_tw_u_aug_ti_ti <- as.numeric(fixef(plm_tw_u_aug_ti, "time"))

# id level
stopifnot(isTRUE(all.equal(plm_tw_u_fixef_id_level,
													 plm_tw_u_aug_id_id,
													 check.attributes = FALSE)))
# time level
stopifnot(isTRUE(all.equal(plm_tw_u_fixef_ti_level,
													 plm_tw_u_aug_ti_ti,
													 check.attributes = FALSE)))


#### oneway balanced ####
plm_ow_fixef_id_level <- as.numeric(fixef(plm_ow_id))
plm_ow_fixef_ti_level <- as.numeric(fixef(plm_ow_ti))


#### oneway unbalanced ####
plm_ow_u_fixef_id_level <- as.numeric(fixef(plm_ow_u_id))
plm_ow_u_fixef_ti_level <- as.numeric(fixef(plm_ow_u_ti))



#### tw balanced ####
## sum of effects
plm_tw_fixef_tw <- as.numeric(fixef(plm_tw, "twoways"))

# manual
bal_pred_betas <- as.numeric(tcrossprod(coef(plm_tw), model.matrix(plm_tw, model = "pooling")[ , -1L]))
bal_pred_y <- plm_tw$model[ , 1] - plm_tw$residuals
bal_pred_effs_tw <- bal_pred_y - bal_pred_betas

stopifnot(isTRUE(all.equal(as.numeric(bal_pred_effs_tw),
                           plm_tw_fixef_tw,
                           check.attributes = FALSE)))

## split in a individual and in a time component
plm_tw_fixef_id_dfirst <- c(0, as.numeric(fixef(plm_tw, "individual", "dfirst")))
plm_tw_fixef_ti_dfirst <- c(0, as.numeric(fixef(plm_tw, "time",       "dfirst")))

plm_tw_fixef_id_level <- as.numeric(fixef(plm_tw, "individual", "level"))
plm_tw_fixef_ti_level <- as.numeric(fixef(plm_tw, "time",       "level"))

## check the summed up effects and splits
stopifnot(isTRUE(all.equal(plm_tw_fixef_tw,
                           plm_tw_fixef_id_level[ index(plm_tw)[[1L]]] +
                             plm_tw_fixef_ti_dfirst[index(plm_tw)[[2L]]],
                           check.attributes = FALSE)))
stopifnot(isTRUE(all.equal(plm_tw_fixef_tw,
                           plm_tw_fixef_id_dfirst[index(plm_tw)[[1L]]] +
                             plm_tw_fixef_ti_level[ index(plm_tw)[[2L]]],
                           check.attributes = FALSE)))

### # checks vs. a twoway model implemented via one-way augmented
plm_tw_aug_id <- plm(inv ~ value + capital + factor(year), data = Grunfeld, effect = "individual")
plm_tw_aug_ti <- plm(inv ~ value + capital + factor(firm), data = Grunfeld, effect = "time")

plm_tw_aug_id_id <- as.numeric(fixef(plm_tw_aug_id, "individual"))
plm_tw_aug_ti_ti <- as.numeric(fixef(plm_tw_aug_ti, "time"))

# id level
stopifnot(isTRUE(all.equal(plm_tw_fixef_id_level,
                           plm_tw_aug_id_id,
                           check.attributes = FALSE)))
# time level
stopifnot(isTRUE(all.equal(plm_tw_fixef_ti_level,
                           plm_tw_aug_ti_ti,
                           check.attributes = FALSE)))


## checks vs. fixest::feols
# fixest.avail <- if(!requireNamespace("fixest", quietly = TRUE)) FALSE else TRUE
# if(fixest.avail) {
# 	 library(fixest) # version 0.9.0 
#   
#   # twoways balanced (one level, one dfirst)
#   feols_tw <- fixest::feols(inv ~ value + capital | firm + year, data = Grunfeld)
#   stopifnot(isTRUE(all.equal(feols_tw$sumFE,       plm_tw_fixef_tw,        check.attributes = FALSE))) # sum
#   stopifnot(isTRUE(all.equal(fixef(feols_tw)$year, plm_tw_fixef_ti_dfirst, check.attributes = FALSE))) # time
#   stopifnot(isTRUE(all.equal(fixef(feols_tw)$firm, plm_tw_fixef_id_level,  check.attributes = FALSE))) # individual
#   
#   # oneway balanced (levels)
#   feols_ow_id <- fixest::feols(inv ~ value + capital | firm, data = Grunfeld)
#   feols_ow_ti <- fixest::feols(inv ~ value + capital | year, data = Grunfeld)
#   stopifnot(isTRUE(all.equal(fixef(feols_ow_ti)$year, plm_ow_fixef_ti_level, check.attributes = FALSE))) # time
#   stopifnot(isTRUE(all.equal(fixef(feols_ow_id)$firm, plm_ow_fixef_id_level, check.attributes = FALSE))) # individual
# 
#   # twoways unbalanced (one level, one dfirst)
#   feols_tw_u <- fixest::feols(inv ~ value + capital | firm + year, data = Grunfeld_unbalanced)
#   stopifnot(isTRUE(all.equal(feols_tw_u$sumFE,       plm_tw_u_fixef_tw,        check.attributes = FALSE))) # sum
#   stopifnot(isTRUE(all.equal(fixef(feols_tw_u)$year, plm_tw_u_fixef_ti_dfirst, check.attributes = FALSE))) # time
#   stopifnot(isTRUE(all.equal(fixef(feols_tw_u)$firm, plm_tw_u_fixef_id_level,  check.attributes = FALSE))) # individual
# 
#   # oneway unbalanced (levels)
#   feols_ow_u_id <- fixest::feols(inv ~ value + capital | firm, data = Grunfeld_unbalanced)
#   feols_ow_u_ti <- fixest::feols(inv ~ value + capital | year, data = Grunfeld_unbalanced)
#   stopifnot(isTRUE(all.equal(fixef(feols_ow_u_id)$firm, plm_ow_u_fixef_id_level, check.attributes = FALSE))) # individual
#   stopifnot(isTRUE(all.equal(fixef(feols_ow_u_ti)$year, plm_ow_u_fixef_ti_level, check.attributes = FALSE))) # time
# }
# 
# # checks vs. lfe::felm
# lfe.avail <- if(!requireNamespace("lfe", quietly = TRUE)) FALSE else TRUE
# if(lfe.avail) {
# 	library(lfe) # version 2.8-7
#   
#   # twoways balanced (one level, one dfirst)
#   # (lfe::felm's default reference is vice verse compared to fixest::feols)
#   felm_tw   <- lfe::felm(inv ~ value + capital | firm + year, data = Grunfeld)
#   felm_tw_fixef_id <- lfe::getfe(felm_tw)[lfe::getfe(felm_tw)[["fe"]] == "firm", 1]
#   felm_tw_fixef_ti <- lfe::getfe(felm_tw)[lfe::getfe(felm_tw)[["fe"]] == "year", 1]
#   
#   stopifnot(isTRUE(all.equal(felm_tw_fixef_id, plm_tw_fixef_id_dfirst, check.attributes = FALSE))) # individual
#   stopifnot(isTRUE(all.equal(felm_tw_fixef_ti, plm_tw_fixef_ti_level,  check.attributes = FALSE))) # time
#   
#   # oneway balanced (levels)
#   felm_ow_id   <- lfe::felm(inv ~ value + capital | firm, data = Grunfeld)
#   felm_ow_ti   <- lfe::felm(inv ~ value + capital | year, data = Grunfeld)
#   felm_ow_id_fixef_id <- lfe::getfe(felm_ow_id)[lfe::getfe(felm_ow_id)[["fe"]] == "firm", 1]
#   felm_ow_ti_fixef_ti <- lfe::getfe(felm_ow_ti)[lfe::getfe(felm_ow_ti)[["fe"]] == "year", 1]
#   
#   stopifnot(isTRUE(all.equal(felm_ow_id_fixef_id, plm_ow_fixef_id_level, check.attributes = FALSE))) # individual
#   stopifnot(isTRUE(all.equal(felm_ow_ti_fixef_ti, plm_ow_fixef_ti_level, check.attributes = FALSE))) # time
#   
#   # twoways unbalanced (one level, one dfirst)
#   # (lfe::felm's default reference is vice verse compared to fixest::feols)
#   felm_tw_u <- lfe::felm(inv ~ value + capital | firm + year, data = Grunfeld_unbalanced)
#   felm_tw_u_fixef_id <- lfe::getfe(felm_tw_u)[lfe::getfe(felm_tw_u)[["fe"]] == "firm", 1]
#   felm_tw_u_fixef_ti <- lfe::getfe(felm_tw_u)[lfe::getfe(felm_tw_u)[["fe"]] == "year", 1]
# 
#   stopifnot(isTRUE(all.equal(felm_tw_u_fixef_id, plm_tw_u_fixef_id_dfirst, check.attributes = FALSE))) # individual
#   stopifnot(isTRUE(all.equal(felm_tw_u_fixef_ti, plm_tw_u_fixef_ti_level,  check.attributes = FALSE))) # time
# 
#   # oneway unbalanced (levels)
#   felm_ow_u_id <- lfe::felm(inv ~ value + capital | firm, data = Grunfeld_unbalanced)
#   felm_ow_u_ti <- lfe::felm(inv ~ value + capital | year, data = Grunfeld_unbalanced)
#   felm_ow_u_id_fixef_id <- lfe::getfe(felm_ow_u_id)[lfe::getfe(felm_ow_u_id)[["fe"]] == "firm", 1]
#   felm_ow_u_ti_fixef_ti <- lfe::getfe(felm_ow_u_ti)[lfe::getfe(felm_ow_u_ti)[["fe"]] == "year", 1]
#   stopifnot(isTRUE(all.equal(felm_ow_u_id_fixef_id, plm_ow_u_fixef_id_level, check.attributes = FALSE))) # individual
#   stopifnot(isTRUE(all.equal(felm_ow_u_ti_fixef_ti, plm_ow_u_fixef_ti_level, check.attributes = FALSE))) # time
# }



################## (2) Instrumental Variable (IV) Models ##################
# 
## IV balanced twoways
data("Crime", package = "plm")

# in the unbalanced twoway case, getfe() sets as reference a level depending on the unbalancedness structure
# -> somewhat arbitrary -> see also https://github.com/sgaure/lfe/issues/52 
# for this example, the unbalancedness structure leads to the first time period being the reference
delrows <- -c(10,12,17,18)
# delrows <- -c(1,2,10,12)
# delrows <- -c(9)

crime_formula_plm <- lcrmrte ~ lprbarr + lpolpc + lprbconv + lprbpris + lavgsen +
                      ldensity + lwcon + lwtuc + lwtrd + lwfir + lwser + lwmfg + lwfed +
                      lwsta + lwloc + lpctymle | . - lprbarr - lpolpc + ltaxpc + lmix

FE2SLS_id <- plm(crime_formula_plm, data = Crime, model = "within", effect = "individual")
FE2SLS_ti <- plm(crime_formula_plm, data = Crime, model = "within", effect = "time")
fixef(FE2SLS_id, effect = "individual")
fixef(FE2SLS_ti, effect = "time")

FE2SLS_tw <- plm(crime_formula_plm, data = Crime, model = "within", effect = "twoways")
fixef(FE2SLS_tw, effect = "individual")
fixef(FE2SLS_tw, effect = "time")
fixef(FE2SLS_tw, effect = "twoways")

## IV unbalanced twoways
FE2SLS_id_unbal <- plm(crime_formula_plm, data = Crime[delrows, ], model = "within", effect = "individual")
FE2SLS_ti_unbal <- plm(crime_formula_plm, data = Crime[delrows, ], model = "within", effect = "time")
fixef(FE2SLS_id_unbal, effect = "individual")
fixef(FE2SLS_ti_unbal, effect = "time")

FE2SLS_tw_unbal <- plm(crime_formula_plm, data = Crime[delrows, ], model = "within", effect = "twoways")
fixef(FE2SLS_tw_unbal, effect = "individual")
fixef(FE2SLS_tw_unbal, effect = "time")
fixef(FE2SLS_tw_unbal, effect = "twoways")


## check vs. fixest::feols
# if(fixest.avail) {
# 	library(fixest) # version 0.9.0 does not compute fixef() for IV models correctly,
#                   # fixed in 0.10.0, see bug report:
#                   # https://github.com/lrberge/fixest/issues/190
#                   # fix commit 2021-08-31: https://github.com/lrberge/fixest/commit/9cdd106b4fe87c0bfc5cbde1102ac1952e246ab0
#   
#   crime_formula_fixest_id <- lcrmrte ~ lprbconv + lprbpris + lavgsen + 
#                                   ldensity + lwcon + lwtuc + lwtrd + lwfir +
#                                   lwser + lwmfg + lwfed + lwsta + lwloc + lpctymle | 
#                                   county |
#                                   lprbarr + lpolpc ~ ltaxpc + lmix
#   
#   crime_formula_fixest_ti <- lcrmrte ~ lprbconv + lprbpris + lavgsen + 
#                                   ldensity + lwcon + lwtuc + lwtrd + lwfir +
#                                   lwser + lwmfg + lwfed + lwsta + lwloc + lpctymle | 
#                                   year |
#                                   lprbarr + lpolpc ~ ltaxpc + lmix
#   
#   crime_formula_fixest_tw <- lcrmrte ~ lprbconv + lprbpris + lavgsen + 
#     ldensity + lwcon + lwtuc + lwtrd + lwfir +
#     lwser + lwmfg + lwfed + lwsta + lwloc + lpctymle | 
#     county + year |
#     lprbarr + lpolpc ~ ltaxpc + lmix
#   
#   
#   FE2SLS_id.fixest <- fixest::feols(crime_formula_fixest_id, data = Crime)
#   FE2SLS_ti.fixest <- fixest::feols(crime_formula_fixest_ti, data = Crime)
#   
#   FE2SLS_id_unbal.fixest <- fixest::feols(crime_formula_fixest_id, data = Crime[delrows, ])
#   FE2SLS_ti_unbal.fixest <- fixest::feols(crime_formula_fixest_ti, data = Crime[delrows, ])
#   
#   FE2SLS_tw.fixest <- fixest::feols(crime_formula_fixest_tw, data = Crime)
#   FE2SLS_tw_unbal.fixest <- fixest::feols(crime_formula_fixest_tw, data = Crime[delrows, ])
#   
#   # First, check if model estimations are the same
#   stopifnot(isTRUE(all.equal(FE2SLS_id$coefficients, FE2SLS_id.fixest$coefficients, check.attributes = FALSE)))
#   stopifnot(isTRUE(all.equal(FE2SLS_ti$coefficients, FE2SLS_ti.fixest$coefficients, check.attributes = FALSE)))
#   
#   stopifnot(isTRUE(all.equal(FE2SLS_id_unbal$coefficients, FE2SLS_id_unbal.fixest$coefficients, check.attributes = FALSE)))
#   stopifnot(isTRUE(all.equal(FE2SLS_ti_unbal$coefficients, FE2SLS_ti_unbal.fixest$coefficients, check.attributes = FALSE)))
# 
#   stopifnot(isTRUE(all.equal(FE2SLS_tw$coefficients,       FE2SLS_tw.fixest$coefficients, check.attributes = FALSE)))
#   stopifnot(isTRUE(all.equal(FE2SLS_tw_unbal$coefficients, FE2SLS_tw_unbal.fixest$coefficients, check.attributes = FALSE)))
#   
#   ## check fixef
#   ## hard checks disabled for now as fixest 0.10.0 is not yet on CRAN (0.9.0 is on CRAN and has a bug in fixef for IV cases)
#   # stopifnot(isTRUE(all.equal(as.numeric(fixef(FE2SLS_id)), fixef(FE2SLS_id.fixest)[["county"]], check.attributes = FALSE))) # no match
#   # stopifnot(isTRUE(all.equal(as.numeric(fixef(FE2SLS_ti)), fixef(FE2SLS_ti.fixest)[["year"]],   check.attributes = FALSE))) # no match
#   # 
#   # stopifnot(isTRUE(all.equal(as.numeric(fixef(FE2SLS_id_unbal)), fixef(FE2SLS_id_unbal.fixest)[["county"]], check.attributes = FALSE))) # no match
#   # stopifnot(isTRUE(all.equal(as.numeric(fixef(FE2SLS_ti_unbal)), fixef(FE2SLS_ti_unbal.fixest)[["year"]],   check.attributes = FALSE))) # no match
#   # 
#   # stopifnot(isTRUE(all.equal(as.numeric(fixef(FE2SLS_tw)),                         fixef(FE2SLS_tw.fixest)[["county"]], check.attributes = FALSE))) # no match
#   # stopifnot(isTRUE(all.equal(as.numeric(c(0, fixef(FE2SLS_tw, "time", "dfirst"))), fixef(FE2SLS_tw.fixest)[["year"]],   check.attributes = FALSE))) # no match
#   # stopifnot(isTRUE(all.equal(as.numeric(fixef(FE2SLS_tw, "twoways")),              FE2SLS_tw.fixest$sumFE,              check.attributes = FALSE))) # no match
#   # 
#   # stopifnot(isTRUE(all.equal(as.numeric(fixef(FE2SLS_tw_unbal)),                         fixef(FE2SLS_tw_unbal.fixest)[["county"]], check.attributes = FALSE))) # no match
#   # stopifnot(isTRUE(all.equal(as.numeric(c(0, fixef(FE2SLS_tw_unbal, "time", "dfirst"))), fixef(FE2SLS_tw_unbal.fixest)[["year"]],   check.attributes = FALSE))) # no match
#   # stopifnot(isTRUE(all.equal(as.numeric(fixef(FE2SLS_tw_unbal, "twoways")),              FE2SLS_tw_unbal.fixest$sumFE,              check.attributes = FALSE))) # no match
# 
#   fixef(FE2SLS_id.fixest)
#   fixef(FE2SLS_id_unbal.fixest)
#   
#   fixef(FE2SLS_ti.fixest)
#   fixef(FE2SLS_ti_unbal.fixest)
#    
#   fixef(FE2SLS_tw.fixest)[["county"]]
#   fixef(FE2SLS_tw.fixest)[["year"]]
#   fixef(FE2SLS_tw_unbal.fixest)[["county"]]
#   fixef(FE2SLS_tw_unbal.fixest)[["year"]]
# }
# 
# if(lfe.avail) {
#   library(lfe) # version 2.8-7
#    
#    # check vs. lfe::felm/getfe 
#    formula_lfe_id <- lcrmrte ~ lprbconv + lprbpris + lavgsen + 
#                        ldensity + lwcon + lwtuc + lwtrd + lwfir +
#                        lwser + lwmfg + lwfed + lwsta + lwloc + lpctymle | 
#                        county |
#                        (lprbarr|lpolpc ~ ltaxpc + lmix)
#    
#    formula_lfe_ti <- lcrmrte ~ lprbconv + lprbpris + lavgsen + 
#                        ldensity + lwcon + lwtuc + lwtrd + lwfir +
#                        lwser + lwmfg + lwfed + lwsta + lwloc + lpctymle | 
#                        year |
#                        (lprbarr|lpolpc ~ ltaxpc + lmix)
#    
#    formula_lfe_tw <- lcrmrte ~ lprbconv + lprbpris + lavgsen + 
#                       ldensity + lwcon + lwtuc + lwtrd + lwfir +
#                       lwser + lwmfg + lwfed + lwsta + lwloc + lpctymle | 
#                       county + year |
#                       (lprbarr|lpolpc ~ ltaxpc + lmix)
# 
#    
#    FE2SLS_id.felm       <- lfe::felm(formula_lfe_id, data = Crime)
#    FE2SLS_ti.felm       <- lfe::felm(formula_lfe_ti, data = Crime)
#    FE2SLS_id_unbal.felm <- lfe::felm(formula_lfe_id, data = Crime[delrows, ])
#    FE2SLS_ti_unbal.felm <- lfe::felm(formula_lfe_ti, data = Crime[delrows, ])
#    FE2SLS_tw.felm       <- lfe::felm(formula_lfe_tw, data = Crime)
#    FE2SLS_tw_unbal.felm <- lfe::felm(formula_lfe_tw, data = Crime[delrows, ])
#    
#    # same order of coef as other estimations
#    FE2SLS_id.felm.coef <- as.numeric(FE2SLS_id.felm$coefficients)
#    names(FE2SLS_id.felm.coef) <- rownames(FE2SLS_id.felm$coefficients)
#    FE2SLS_id.felm.coef <- FE2SLS_id.felm.coef[c(15, 16, 1:14)]
#    
#    FE2SLS_ti.felm.coef <- as.numeric(FE2SLS_ti.felm$coefficients)
#    names(FE2SLS_ti.felm.coef) <- rownames(FE2SLS_ti.felm$coefficients)
#    FE2SLS_ti.felm.coef <- FE2SLS_ti.felm.coef[c(15, 16, 1:14)]
#    
#    FE2SLS_id_unbal.felm.coef <- as.numeric(FE2SLS_id_unbal.felm$coefficients)
#    names(FE2SLS_id_unbal.felm.coef) <- rownames(FE2SLS_id_unbal.felm$coefficients)
#    FE2SLS_id_unbal.felm.coef <- FE2SLS_id_unbal.felm.coef[c(15, 16, 1:14)]
#    
#    FE2SLS_ti_unbal.felm.coef <- as.numeric(FE2SLS_ti_unbal.felm$coefficients)
#    names(FE2SLS_ti_unbal.felm.coef) <- rownames(FE2SLS_ti_unbal.felm$coefficients)
#    FE2SLS_ti_unbal.felm.coef <- FE2SLS_ti_unbal.felm.coef[c(15, 16, 1:14)]
#    
#    FE2SLS_tw.felm.coef <- as.numeric(FE2SLS_tw.felm$coefficients)
#    names(FE2SLS_tw.felm.coef) <- rownames(FE2SLS_tw.felm$coefficients)
#    FE2SLS_tw.felm.coef <- FE2SLS_tw.felm.coef[c(15, 16, 1:14)]
#    
#    FE2SLS_tw_unbal.felm.coef <- as.numeric(FE2SLS_tw_unbal.felm$coefficients)
#    names(FE2SLS_tw_unbal.felm.coef) <- rownames(FE2SLS_tw_unbal.felm$coefficients)
#    FE2SLS_tw_unbal.felm.coef <- FE2SLS_tw_unbal.felm.coef[c(15, 16, 1:14)]
#    
#    # First, check if model estimations are the same
#    stopifnot(isTRUE(all.equal(FE2SLS_id$coefficients,       FE2SLS_id.felm.coef,       check.attributes = FALSE)))
#    stopifnot(isTRUE(all.equal(FE2SLS_ti$coefficients,       FE2SLS_ti.felm.coef,       check.attributes = FALSE)))
#    stopifnot(isTRUE(all.equal(FE2SLS_id_unbal$coefficients, FE2SLS_id_unbal.felm.coef, check.attributes = FALSE)))
#    stopifnot(isTRUE(all.equal(FE2SLS_ti_unbal$coefficients, FE2SLS_ti_unbal.felm.coef, check.attributes = FALSE)))
#    stopifnot(isTRUE(all.equal(FE2SLS_tw$coefficients,       FE2SLS_tw.felm.coef,       check.attributes = FALSE)))
#    stopifnot(isTRUE(all.equal(FE2SLS_tw_unbal$coefficients, FE2SLS_tw_unbal.felm.coef, check.attributes = FALSE)))
# 
#    FE2SLS_id.felm_fixef       <- lfe::getfe(FE2SLS_id.felm)[lfe::getfe(FE2SLS_id.felm)[["fe"]] == "county", 1]
#    FE2SLS_id_unbal.felm_fixef <- lfe::getfe(FE2SLS_id_unbal.felm)[lfe::getfe(FE2SLS_id_unbal.felm)[["fe"]] == "county", 1]
#    
#    FE2SLS_ti.felm_fixef       <- lfe::getfe(FE2SLS_ti.felm)[lfe::getfe(FE2SLS_ti.felm)[["fe"]] == "year", 1]
#    FE2SLS_ti_unbal.felm_fixef <- lfe::getfe(FE2SLS_ti_unbal.felm)[lfe::getfe(FE2SLS_ti_unbal.felm)[["fe"]] == "year", 1]
#    
#    FE2SLS_tw.id.felm_fixef       <- lfe::getfe(FE2SLS_tw.felm)[lfe::getfe(FE2SLS_tw.felm)[["fe"]] == "county", 1]
#    FE2SLS_tw.id_unbal.felm_fixef <- lfe::getfe(FE2SLS_tw_unbal.felm)[lfe::getfe(FE2SLS_tw_unbal.felm)[["fe"]] == "county", 1]
#    FE2SLS_tw.ti.felm_fixef       <- lfe::getfe(FE2SLS_tw.felm)[lfe::getfe(FE2SLS_tw.felm)[["fe"]] == "year", 1]
#    FE2SLS_tw.ti_unbal.felm_fixef <- lfe::getfe(FE2SLS_tw_unbal.felm)[lfe::getfe(FE2SLS_tw_unbal.felm)[["fe"]] == "year", 1]
#    
#    stopifnot(isTRUE(all.equal(as.numeric(fixef(FE2SLS_id)),       FE2SLS_id.felm_fixef,   check.attributes = FALSE)))
#    stopifnot(isTRUE(all.equal(as.numeric(fixef(FE2SLS_ti)),       FE2SLS_ti.felm_fixef,   check.attributes = FALSE)))
#    stopifnot(isTRUE(all.equal(as.numeric(fixef(FE2SLS_id_unbal)), FE2SLS_id_unbal.felm_fixef, check.attributes = FALSE)))
# 
#    stopifnot(isTRUE(all.equal(as.numeric(fixef(FE2SLS_ti_unbal)), FE2SLS_ti_unbal.felm_fixef, check.attributes = FALSE)))
#    stopifnot(isTRUE(all.equal(as.numeric(fixef(FE2SLS_tw)),                               FE2SLS_tw.id.felm_fixef,   check.attributes = FALSE)))
#    stopifnot(isTRUE(all.equal(as.numeric(c(0, fixef(FE2SLS_tw, "time", "dfirst"))),       FE2SLS_tw.ti.felm_fixef,   check.attributes = FALSE, tolerance = 10^(-6))))
# 
#    # in the unbalanced twoway case, getfe() sets as reference a level depending on the unbalancedness structure
#    # -> somewhat arbitrary -> see also https://github.com/sgaure/lfe/issues/52 
#    # for this example, the unbalancedness structure leads to the first time period being the reference
#    stopifnot(isTRUE(all.equal(as.numeric(fixef(FE2SLS_tw_unbal)),                         FE2SLS_tw.id_unbal.felm_fixef, check.attributes = FALSE)))
#    stopifnot(isTRUE(all.equal(as.numeric(c(0, fixef(FE2SLS_tw_unbal, "time", "dfirst"))), FE2SLS_tw.ti_unbal.felm_fixef, check.attributes = FALSE, tolerance = 10^(-6))))
# }
