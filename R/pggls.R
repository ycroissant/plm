pggls <- function(formula, data, subset, na.action,
                  effect = c("individual", "time"),
                  model = c("within", "random", "pooling", "fd"),
                  index = NULL, ...)
{
  # check and match the arguments
  effect <- match.arg(effect)
  model.name <- match.arg(model)
  if(model.name=="random") {
        warning("'random' argument to pggls() has been renamed as 'pooling'",
                call.=FALSE)
        model.name <- "pooling"
  }
  data.name <- paste(deparse(substitute(data)))
  cl <- match.call()
  plm.model <- match.call(expand.dots=FALSE)
  m <- match(c("formula","data","subset","na.action","effect","model","index"),names(plm.model),0)
  plm.model <- plm.model[c(1,m)]
  plm.model[[1]] <- as.name("plm")
  plm.model$model <- model.name
  plm.model <- eval(plm.model,parent.frame())

    index <- attr(model.frame(plm.model), "index")
    pdim <- pdim(plm.model)
    balanced <- pdim$balanced

    time.names <- pdim$panel.names$time.names
    id.names <- pdim$panel.names$id.names
    coef.names <- names(coef(plm.model))
    K <- length(coef.names)

    if(model.name=="fd") {
        ## eliminate first year in indices
        nt <- pdim$Tint$nt[-1]
        Ti <- pdim$Tint$Ti - 1
        T <- pdim$nT$T - 1
        n <- pdim$nT$n
        N <- pdim$nT$N - pdim$Tint$nt[1]
        time.names <- pdim$panel.names$time.names[-1]
         tind <- as.numeric(index[,2])
         sel <- (tind-c(-1,tind[-length(tind)]))==1
         index <- index[sel,]
         id <- index[[1]]
         time <- factor(index[[2]], levels=attr(index[,2], "levels")[-1])
    } else {
        nt <- pdim$Tint$nt
        Ti <- pdim$Tint$Ti
        T <- pdim$nT$T
        n <- pdim$nT$n
        N <- pdim$nT$N

        id <- index[[1]]
        time <- index[[2]]
    }

    if (effect == "time") {
        cond <- time
        other <- id
        ncond <- T
        nother <- n
        cond.names <- time.names
        other.names <- id.names
        groupsdim <- nt
    }
    else {
        cond <- id
        other <- time
        ncond <- n
        nother <- T
        cond.names <- id.names
        other.names <- time.names
        groupsdim <- Ti
    }
    myord <- order(cond, other)
    X <- model.matrix(plm.model)[myord, ]
    y <- pmodel.response(plm.model)[myord]

    resid <- resid(lm.fit(X,y))
 
    cond <- cond[myord]
    other <- other[myord]
    drop1 <- FALSE
    if (drop1 && model.name %in% c("within","fd")) {
        ## drop first time period (see Wooldridge 10.5, eq. 10.61)
        ## this is needed according to Wooldridge, p.277 but is
        ## not totally robust to unbalancedness, dummies etc.
        ## 
        ## the function turns out to work irrespective of dropping
        ## one time period or not!! absolutely the same results...
        ## this is thx to solve.bdsmatrix() using a generalized
        ## inverse, which in this case where rank=T-1 is equivalent
        ## to discarding one year (N columns)
        ## The 'if' parameterization is just for debugging.

        numeric.t <- as.numeric(other)
        t1 <- which(numeric.t != min(numeric.t))
        X0 <- X
        y0 <- y
        X <- X[t1, ]
        y <- y[t1]
        resid <- resid(lm.fit(X,y))
        #resid[t1]
        cond <- cond[t1]
        other <- other[t1]
        nother <- nother - 1
        other.names <- other.names[-1]
    }
    tres <- array(NA, dim = c(nother, nother, ncond), dimnames = list(other.names,
        other.names, cond.names))
    lcnd <- levels(cond)
    if (pdim$balanced) {
        for (i in 1:ncond) {
            ut <- resid[cond == lcnd[i]]
            tres[, , i] <- ut %o% ut
        }
        subOmega <- apply(tres, 1:2, mean)
        omega <- bdsmatrix(rep(nother, ncond), rep(subOmega,
            ncond))
    } else {
        lti <- list()
        for (i in 1:ncond) {
            cond.i <- cond == lcnd[i]
            ut <- resid[cond.i]
            names(ut) <- lti[[i]] <- other[cond.i]
            out <- ut %o% ut
            tres[names(ut), names(ut), i] <- out
        }
        subOmega <- apply(tres, 1:2, mean, na.rm = TRUE)
        list.cov.blocks <- list()
        for (i in 1:ncond) {
            list.cov.blocks[[i]] <- subOmega[lti[[i]], lti[[i]]]
        }
        omega <- bdsmatrix(groupsdim, unlist(list.cov.blocks))
    }
    A <- crossprod(X, solve(omega, X))
    B <- crossprod(X, solve(omega, y))
    vcov <- solve(A)
    coef <- as.vector(solve(A, B))
    if (drop1 && model == "within") {
        X <- X0
        y <- y0
    }
    residuals <- y - as.vector(crossprod(t(X), coef))
    df.residual <- nrow(X) - ncol(X)
    fitted.values <- y - residuals
    names(coef) <- rownames(vcov) <- colnames(vcov) <- coef.names
    pmodel <- attr(plm.model, "pmodel")
    pmodel$model.name <- model
    fullGLS <- list(coefficients = coef, residuals = residuals,
        fitted.values = fitted.values, vcov = vcov, df.residual = df.residual,
        model = model.frame(plm.model), sigma = subOmega, call = cl,
        formula = plm.model$formula )
    fullGLS <- structure(fullGLS, pdim = pdim, pmodel = pmodel)
    class(fullGLS) <- c("pggls", "panelmodel")
    fullGLS
  

}


summary.pggls <- function(object,...){
  pmodel <- attr(object,"pmodel")
  std.err <- sqrt(diag(object$vcov))
  b <- object$coefficients
  z <- b/std.err
#  p <- 2*(1-pnorm(abs(z)))
  p <- 2*pnorm(abs(z),lower.tail=FALSE)
  CoefTable <- cbind(b,std.err,z,p)
  colnames(CoefTable) <- c("Estimate","Std. Error","z-value","Pr(>|z|)")
  object$CoefTable <- CoefTable
  y <- object$model[[1]]
  object$tss <- tss(y)
  object$ssr <- sum(residuals(object)^2)
  object$rsqr <- 1-object$ssr/object$tss
  class(object) <- c("summary.pggls")
  return(object)
}

print.summary.pggls <- function(x,digits=max(3, getOption("digits") - 2), width = getOption("width"),...){
  pmodel <- attr(x,"pmodel")
  pdim <- attr(x,"pdim")
  effect <- pmodel$effect
  formula <- pmodel$formula
  model.name <- pmodel$model.name
  cat(paste(effect.pggls.list[effect]," ",sep=""))
  cat(paste(model.pggls.list[model.name],"\n",sep=""))
  cat("\nCall:\n")
  print(x$call)
  cat("\n")
  print(pdim)
  cat("\nResiduals\n")
  print(summary(unlist(residuals(x))))
  cat("\nCoefficients\n")
  printCoefmat(x$CoefTable,digits=digits)
  cat(paste("Total Sum of Squares: ",signif(x$tss,digits),"\n",sep=""))
  cat(paste("Residual Sum of Squares: ",signif(x$ssr,digits),"\n",sep=""))
  cat(paste("Multiple R-squared: ",signif(x$rsqr,digits),"\n",sep=""))
  invisible(x)
}

