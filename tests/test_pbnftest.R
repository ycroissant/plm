library(plm)
data("Grunfeld", package = "plm")


# Baltagi/Wu (1999), p. 822, Table 1: test data construction
a <- Grunfeld[["year"]][c(9, 10)]
b <- Grunfeld[["year"]][c(17, 18)]
c <- Grunfeld[["year"]][c(3, 4, 5)]
d <- Grunfeld[["year"]][c(7, 8, 9)]
e <- Grunfeld[["year"]][c(13, 14, 15)]
f <- Grunfeld[["year"]][c(3, 4, 5, 6)]
g <- Grunfeld[["year"]][c(12, 13, 14, 15)]
h <- Grunfeld[["year"]][c(2, 4, 5, 14)]
i <- Grunfeld[["year"]][c(8, 9, 16, 17, 19)]
j <- Grunfeld[["year"]][c(2, 3, 15, 16, 17, 19)]
k <- Grunfeld[["year"]][c(2, 3, 15, 18, 19, 20)]
l <- Grunfeld[["year"]][c(2, 3, 5, 7, 15, 20)]
m <- Grunfeld[["year"]][c(3, 5, 8, 9, 16, 17, 19)]
n <- Grunfeld[["year"]][c(2, 4, 5, 14, 15, 16, 19)]
o <- Grunfeld[["year"]][c(2, 3, 4, 8, 9, 16, 17, 19)]
p <- Grunfeld[["year"]][c(2, 3, 5, 7, 15, 18, 19, 20)]
q <- Grunfeld[["year"]][c(2, 4, 5, 8, 14, 15, 16, 19)]

models_fe <- lapply(letters[1:17], function(let) plm(inv ~ value + capital, data = Grunfeld[!Grunfeld[["year"]] %in% get(let), ], model = "within"))

results_modbnf <- lapply(models_fe, pbnftest)
names(results_modbnf) <- letters[1:17]
print(unlist(lapply(results_modbnf, function(i) i$statistic)))

results_lbi <- lapply(models_fe, function(mod) pbnftest(mod, test = "lbi"))
names(results_lbi) <- letters[1:17]
print(unlist(lapply(results_lbi, function(i) i$statistic)))

# formula interface
pbnftest(inv ~ value + capital, data = Grunfeld[!Grunfeld$year %in% c(1943, 1944), ], model = "within")
pbnftest(inv ~ value + capital, data = Grunfeld[!Grunfeld$year %in% c(1943, 1944), ], test = "lbi", model = "within")

# x <- plm(inv ~ value + capital, data = Grunfeld[!Grunfeld$year %in% c(1943, 1944), ], model = "within")
# x <- plm(inv ~ value + capital, data = Grunfeld[!Grunfeld$year %in% c(1951, 1952), ], model = "within")
# x <- plm(inv ~ value + capital, data = Grunfeld, model = "within")
# x <- plm(inv ~ value + capital, data = Grunfeld, model = "pooling")
# x <- plm(inv ~ value + capital, data = Grunfeld[!Grunfeld$year %in% c(1943, 1944), ], model = "pooling")



