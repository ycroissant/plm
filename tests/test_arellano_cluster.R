library(plm)

data("Grunfeld", package = "plm")
d <- Grunfeld[-c(1, 2, 40), ]
d <- d[order(d$firm, d$year), ]
for (model in c("pooling", "fd")) {
    fit <- plm(inv ~ value, data = d, model = model)
    if (model == "fd") {
        # Compare each firm with its preceding observed year.
        end <- which(diff(d$firm) == 0L) + 1L
        reference <- d[end, ]
        reference$inv <- d$inv[end] - d$inv[end - 1L]
        reference$value <- d$value[end] - d$value[end - 1L]
    } else {
        reference <- d
    }
    ols <- lm(inv ~ value, data = reference)
    stopifnot(isTRUE(all.equal(coef(fit), coef(ols), tolerance = 1e-10)))
    for (cluster in c("group", "time")) {
        g <- if (cluster == "time") reference$year else reference$firm
        expected <- sandwich::vcovCL(ols, cluster = g, type = "HC0", cadjust = FALSE)
        actual <- vcovHC(fit, method = "arellano", type = "HC0", cluster = cluster)
        stopifnot(identical(attr(actual, "cluster"), cluster))
        attr(actual, "cluster") <- NULL
        stopifnot(isTRUE(all.equal(actual, expected, tolerance = 1e-10)))
    }
}
