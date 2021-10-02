#### Testfile for pFtest()

# SAS 9.4 Output for F Test [fixed one-way estimates, individual effect]
#
# F Test for No Fixed Effects
#
# Num DF    Den DF    F statistic   Pr > F
#      9       188          49.18  < .0001

library(plm)
data("Grunfeld", package="plm")
gp <- plm(inv ~ value + capital, data = Grunfeld, model = "pooling")

gi <- plm(inv ~ value + capital, data = Grunfeld,
          effect = "individual", model = "within")

gt <- plm(inv ~ value + capital, data = Grunfeld,
          effect = "time", model = "within")

gd <- plm(inv ~ value + capital, data = Grunfeld,
          effect = "twoways", model = "within")


pFtest(gi, gp) # test for individual effects matches SAS's Output
pFtest(gt, gp)
pFtest(gd, gp)


print(pFtest(inv ~ value + capital, data = Grunfeld, effect = "individual"))
print(pFtest(inv ~ value + capital, data = Grunfeld, effect = "time"))
print(pFtest(inv ~ value + capital, data = Grunfeld, effect = "twoways"))

# test for wrong order of arguments, this is supposed to give a meaningful error message
# pFtest(gi, gd)