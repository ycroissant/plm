library("ggplot2")
plotplm <- function(x, N = 10, seed = 1, lgth = 0.1){
    mydata <- model.frame(x)
    onames <- names(mydata)
    names(mydata) <- c("y", "x")
    LGTH <- (max(mydata$x) - min(mydata$x)) ^ 2 + 
        (max(mydata$y) - min(mydata$y)) ^ 2
    lgth <- lgth * sqrt(LGTH) / 2
    seed <- set.seed(seed)
    theids <- sample(unique(index(mydata)[[1]]), N)
    small <- subset(mydata, index(mydata)[[1]] %in% theids)
    small <- cbind(small, id = index(small)[[1]])
    ymean <- with(small, tapply(y, id, mean)[as.character(theids)])
    xmean <- with(small, tapply(x, id, mean)[as.character(theids)])
    within <- update(x, model = "within")
    alpha <- mean(mydata[[1]]) - coef(within) * mean(mydata[[2]])
    beta <- as.numeric(coef(within))
    random <- update(within, model = "random")
    between <- update(within, model = "between")
    ols <- update(within, model = "pooling")
    FE <- fixef(within)[as.character(theids)]
    DATA <- data.frame(id = names(FE), FE = as.numeric(FE), slope = beta,
                       xmean = xmean, ymean = ymean, 
                       xmin = xmean - lgth / sqrt(1 + beta ^ 2),
                       xmax = xmean + lgth / sqrt(1 + beta ^ 2),
                       ymin = ymean - lgth * beta / sqrt(1 + beta ^ 2),
                       ymax = ymean + lgth * beta / sqrt(1 + beta ^ 2))
    MODELS <- data.frame(models = c("ols", "random", "within", "between"),
                         intercept = c(coef(ols)[1], coef(random)[1], alpha, coef(between)[1]),
                         slope = c(coef(ols)[2], coef(random)[2], coef(within), coef(between)[2]))
    ggplot(data = small, aes(x = x, y = y, color = id)) +
        geom_point(size = 0.4) + 
        geom_segment(aes(x = xmin, xend = xmax, y = ymin, yend = ymax, color = id), data = DATA) + 
        geom_abline(aes(intercept = intercept, slope = slope, lty = models), data = MODELS) + 
        geom_point(aes(x = xmean, y = ymean, color = id),
                   size = 1, shape = 13, data = DATA, show.legend = FALSE) + 
        xlab(onames[2]) + ylab(onames[1]) +
        theme(legend.text = element_text(size = 6), 
              legend.title= element_text(size = 8),
              axis.title = element_text(size = 8)) +
        guides(shape = FALSE)
}

plot.plm <- function(x, dx = 0.2, N = NULL, seed = 1,
                     within = TRUE, pooling = TRUE,
                     between = FALSE, random = FALSE, ...){
    dx <- 0.2;N <- 10;seed <- 2;within <- TRUE;pooling <- TRUE;between <- FALSE;random <- FALSE
    opar <- par(bg = "#DCA6FF", mai = rep(0.5, 4))
    set.seed(seed)# 8 est bien pour beertax
    subs <- ! is.null(N)
    x <- update(x, model = "within")
    mco <- update(x, model = "pooling")
    if (random) re <- update(x, model = "random")
    if (between) be <- update(x, model = "between")
    pdim <- pdim(x)
    n <- pdim$nT$n
    if (! subs) N <- n
    ids <- unique(index(x, "id"))
    if (subs) ids <- ids[sample(1:length(ids), N, replace = FALSE)]
    sel <- index(x, "id") %in% ids
    T <- pdim$nT$T
    cols <- rainbow(N)
    pts <- sample(1:25, N, replace = TRUE)
    thex <- as.numeric(model.matrix(x, model = "pooling")[sel, 2])
    they <- as.numeric(pmodel.response(x, model = "pooling")[sel])
    plot(thex, they, col = rep(cols, each = T), xaxs = "i", yaxs = "i",
         pch = rep(pts, each = T), ann = FALSE, las = 1,
         xlim = c(-8, 0), ylim =c(-8, -5), type = "n", axes = FALSE)
    polygon(c(-8, 0, 0, -8), c(-8, -8, -5, -5), col = '#FFA6C9')
    points(thex, they, col = rep(cols, each = T),
         pch = rep(pts, each = T), ann = FALSE, las = 1)
    idsel <- as.numeric(index(x, "id")[sel])
    meanx <- tapply(thex, idsel, mean)
    meany <- tapply(they, idsel, mean)
    points(meanx, meany, pch = 19, col = cols, cex = 1.5)
    if (within){
        beta <- coef(x)
        alphas <- meany - meanx * beta
        dx <- dx * (max(thex) - min(thex))
        for (i in 1:N){
            xmin <- meanx[i] - dx
            xmax <- meanx[i] + dx
            ymin <- alphas[i] + beta * xmin
            ymax <- alphas[i] + beta * xmax
            lines(c(xmin, xmax), c(ymin, ymax), col = cols[i])
        }
    }
    if(random) abline(coef(re)[1], coef(re)[2], lty = "dotted")
    if(pooling) abline(coef(mco), lty = "dashed")
    if(between) abline(coef(be), lty = "dotdash")
    # where to put the legends, depends on the sign of the OLS slope
    modploted <- c(random, pooling, between, within)
    if (sum(modploted)){
        poslegend <- ifelse(beta > 0, "topleft", "topright")
        ltylegend <- c("dotted", "dashed", "dotdash", "solid")[modploted]
        leglegend <- c("random", "pooling", "between", "within")[modploted]
#        legend(poslegend, lty = ltylegend, legend = leglegend)
    }
}


plm(imports ~ gnp, ForeignTrade) %>% plot(N = 10, seed = 2)
png("../logo.png");plm(imports ~ gnp, ForeignTrade) %>% plot(N = 10, seed = 2);dev.off()
