library(plm)
data("EmplUK", package = "plm")

########################## one-step ######################

# Arellano/Bond 1991, Table 4, column (a1) (has robust SEs)
ab.a1 <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
              + lag(log(capital), 0:2) + lag(log(output), 0:2) | lag(log(emp), 2:99),
              data = EmplUK, effect = "twoways", model = "onestep")

mtest(ab.a1, 1, vcov = NULL)   # -3.9394  (p = 0.0001) xtabond manual, example 1
mtest(ab.a1, 2, vcov = NULL)   # -0.54239 (p = 0.5876) xtabond manual, example 1
mtest(ab.a1, 1, vcov = vcovHC) # -3.600 (p = 0.000) (reference DPD/Ox abest3.out)         / -3.5996  (p = 0.0003) xtabond manual, example 2
mtest(ab.a1, 2, vcov = vcovHC) # -0.516 (p = 0.606) (reference A/B and DPD/Ox abest3.out) / -0.51603 (p = 0.6058) xtabond manual, example 2

# Windmeijer (2005), table 2, onestep with corrected std. err
# A/B (1991) model from col. b BUT as one-step model
ab.b.onestep <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
                + log(capital) + lag(log(output), 0:1) | lag(log(emp), 2:99),
                data = EmplUK, effect = "twoways", model = "onestep")
# NB: coef match, non-robust std. errors do not match abest1.out

mtest(ab.b.onestep, 1, vcov = NULL)   # -3.409  (p = 0.001) (reference in DPD/Ox abest1.out)
mtest(ab.b.onestep, 2, vcov = NULL)   # -0.3695 (p = 0.712) (reference in DPD/Ox abest1.out)
mtest(ab.b.onestep, 1, vcov = vcovHC) # -2.493  (reference in Windmeijer) / -2.493  (p = 0.013) DPD/Ox abest1.out
mtest(ab.b.onestep, 2, vcov = vcovHC) # -0.359  (reference in Windmeijer) / -0.3594 (p = 0.719) DPD/Ox abest1.out



########################## two-steps ######################

# Arellano/Bond 1991, Table 4, column (a2) (non-robust SEs)
ab.a2 <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
              + lag(log(capital), 0:2) + lag(log(output), 0:2) | lag(log(emp), 2:99),
              data = EmplUK, effect = "twoways", model = "twosteps")

mtest(ab.a2, 1, vcov = NULL)   # -3.000 (p = 0.003) (reference DPD/Ox in abest3.out)
mtest(ab.a2, 2, vcov = NULL)   # -0.434 (reference A/B) / DPD/Ox has -0.4158 (p = 0.678) in abest3.out, see comment in footnote 7, p. 32
mtest(ab.a2, 1, vcov = vcovHC) # -2.1255  (p = 0.0335) xtabond manual, example 4
mtest(ab.a2, 2, vcov = vcovHC) # -0.35166 (p = 0.7251) xtabond manual, example 4

## Arellano and Bond (1991), table 4 col. b / Windmeijer (2005), table 2
ab.b <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
             + log(capital) + lag(log(output), 0:1) | lag(log(emp), 2:99),
             data = EmplUK, effect = "twoways", model = "twosteps")

mtest(ab.b, 1, vcov = NULL)   # -2.826 (reference about A/B's m1 in Windmeijer) / DPD/Ox has -2.428  (p = 0.015) in abest1.out
mtest(ab.b, 2, vcov = NULL)   # -0.327 (reference in A/B and in Windmeijer)     / DPD/Ox has -0.3325 (p = 0.739) in abest1.out, explanation in footnote 7 on p. 32
mtest(ab.b, 1, vcov = vcovHC) # -1.999 (reference in Windmeijer)
mtest(ab.b, 2, vcov = vcovHC) # -0.316 (reference in Windmeijer)
