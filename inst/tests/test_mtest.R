### test results with plm 2.6-4 (none of the literature's results were reproduced)
library(plm)
data("EmplUK", package = "plm")

########################## one-step ######################

# Arellano/Bond 1991, Table 4, column (a1) (has robust SEs)
ab.a1 <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
              + lag(log(capital), 0:2) + lag(log(output), 0:2) | lag(log(emp), 2:99),
              data = EmplUK, effect = "twoways", model = "onestep")

mtest(ab.a1, 1, vcov = NULL)
mtest(ab.a1, 2, vcov = NULL)
mtest(ab.a1, 1, vcov = vcovHC)
mtest(ab.a1, 2, vcov = vcovHC) # -0.516 (reference)

# Windmeijer (2025), table 2, onestep with corrected std. err
wind.s1 <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
                + log(capital) + lag(log(output), 0:1) | lag(log(emp), 2:99),
                data = EmplUK, effect = "twoways", model = "onestep")
mtest(wind.s1, 1, vcov = NULL)
mtest(wind.s1, 2, vcov = NULL)
mtest(wind.s1, 1, vcov = vcovHC) # -2.493 (reference)
mtest(wind.s1, 2, vcov = vcovHC) # -0.359 (reference)

########################## two-steps ######################

# Arellano/Bond 1991, Table 4, column (a2) (non-robust SEs)
ab.a2 <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
              + lag(log(capital), 0:2) + lag(log(output), 0:2) | lag(log(emp), 2:99),
              data = EmplUK, effect = "twoways", model = "twosteps")

mtest(ab.a2, 1, vcov = NULL)
mtest(ab.a2, 2, vcov = NULL) # -0.434 (reference)
mtest(ab.a2, 1, vcov = vcovHC)
mtest(ab.a2, 2, vcov = vcovHC)

## Arellano and Bond (1991), table 4 col. b / Windmeijer (2025), table 2
ab.b <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
             + log(capital) + lag(log(output), 0:1) | lag(log(emp), 2:99),
             data = EmplUK, effect = "twoways", model = "twosteps")

mtest(ab.b, 1, vcov = NULL)   # -2.826 (reference about A/B's m1 in Windmeijer)
mtest(ab.b, 2, vcov = NULL)   # -0.327 (reference in A/B)
mtest(ab.b, 1, vcov = vcovHC) # -1.999 (reference in Windmeijer)
mtest(ab.b, 2, vcov = vcovHC) # -0.316 (reference in Windmeijer)

