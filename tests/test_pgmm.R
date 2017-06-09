library("plm")
data("EmplUK", package = "plm")
## Arellano and Bond (1991), table 4b 
z1 <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
           + log(capital) + lag(log(output), 0:1) | lag(log(emp), 2:99)+lag(log(capital), 3:99),
            data = EmplUK, effect = "twoways", model = "twosteps")
summary(z1)
z1b <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
           + log(capital) + lag(log(output), 0:1) | lag(log(emp), 2:99)+lag(log(capital), 3:99),
            data = EmplUK, effect = "individual", model = "twosteps")
summary(z1b)


## Blundell and Bond (1998) table 4 (cf DPD for OX p.12 col.4)
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
