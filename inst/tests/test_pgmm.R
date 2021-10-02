library("plm")
data("EmplUK", package = "plm")
## Arellano and Bond (1991), table 4 col. b
z1 <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
           + log(capital) + lag(log(output), 0:1) | lag(log(emp), 2:99),
            data = EmplUK, effect = "twoways", model = "twosteps")
summary(z1, robust = TRUE) # default


z1col <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
           + log(capital) + lag(log(output), 0:1) | lag(log(emp), 2:99),
           data = EmplUK, effect = "twoways", model = "twosteps", collapse = TRUE)
summary(z1col, robust = TRUE) # default

z1ind <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
           + log(capital) + lag(log(output), 0:1) | lag(log(emp), 2:99),
            data = EmplUK, effect = "individual", model = "twosteps")
summary(z1ind, robust = TRUE) # default

z1indcol <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
            + log(capital) + lag(log(output), 0:1) | lag(log(emp), 2:99),
            data = EmplUK, effect = "individual", model = "twosteps")
summary(z1indcol, robust = TRUE) # default


## Blundell and Bond (1998) table 4 (cf DPD for OX p.12 col.4)
## not quite...
z2 <- pgmm(log(emp) ~ lag(log(emp), 1)+ lag(log(wage), 0:1) +
           lag(log(capital), 0:1) | lag(log(emp), 2:99) +
           lag(log(wage), 3:99) + lag(log(capital), 2:99),
           data = EmplUK, effect = "twoways", model = "onestep", 
           transformation = "ld")
summary(z2, robust = TRUE)

z2b <- pgmm(log(emp) ~ lag(log(emp), 1)+ lag(log(wage), 0:1) +
           lag(log(capital), 0:1) | lag(log(emp), 2:99) +
           lag(log(wage), 3:99) + lag(log(capital), 2:99),
           data = EmplUK, effect = "individual", model = "onestep", 
           transformation = "ld")
summary(z2b, robust = TRUE)


### further run tests with various argument values
summary(z1, robust = FALSE)
summary(z1col, robust = FALSE)
summary(z1ind, robust = FALSE)
summary(z1indcol, robust = FALSE)

summary(z2, robust = FALSE)
summary(z2b, robust = FALSE)

z3 <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
           + log(capital) + lag(log(output), 0:1) | lag(log(emp), 2:99),
           data = EmplUK, effect = "twoways", model = "twosteps", transformation = "ld")
summary(z3)

z3col <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
              + log(capital) + lag(log(output), 0:1) | lag(log(emp), 2:99),
              data = EmplUK, effect = "twoways", model = "twosteps", collapse = TRUE, transformation = "ld")
summary(z3col)


z3ind <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
              + log(capital) + lag(log(output), 0:1) | lag(log(emp), 2:99),
              data = EmplUK, effect = "individual", model = "twosteps", transformation = "ld")
summary(z3ind)

z3indcol <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
                 + log(capital) + lag(log(output), 0:1) | lag(log(emp), 2:99),
                 data = EmplUK, effect = "individual", model = "twosteps", transformation = "ld")
summary(z3indcol)




