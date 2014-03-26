fixef.plm <- function(object, effect = NULL,
                      type = c("level", "dfirst", "dmean"), ...){
  model.effect <- describe(object, "effect")
  if (is.null(effect)){
    effect <- ifelse(model.effect == "time", "time", "individual")
  }
  else{
    if (!effect %in% c("individual", "time")) stop("wrong effect argument")
    if (model.effect != "twoways" && model.effect != effect) stop("wrong effect argument")
  }
    
  type <- match.arg(type)
  mf <- model.frame(object)
  if (!is.null(object$call)){
    if (describe(object, "model") != "within")
      stop("fixef is relevant only for within models")
  }
  formula <- formula(object)
  data <- model.frame(object)
  Xb <- model.matrix(formula, data, rhs = 1, model = "between", effect = effect)
  yb <- pmodel.response(formula, data, model = "between", effect = effect)
  # the between model may contain time independent variables, the
  # within model don't. So select the relevant elements using nw
  # (names of the within variables)
  nw <- names(coef(object))
  fixef <- yb - as.vector(crossprod(t(Xb[,nw,drop=FALSE]),coef(object)))
#  bet <- plm.between(formula, data, effect = effect)
  bet <- plm.fit(formula, data, model = "between", effect = effect)
  bet$args <- list(model = "between", effect = effect)
  sigma2 <- deviance(bet)/df.residual(bet)
  vcov <- vcov(object)[nw,nw]
  nother <- switch(effect,
                   "individual" = pdim(object)$Tint$Ti,
                   "time" = pdim(object)$Tint$nt)
  s2 <- deviance(object)/df.residual(object)
  if (type != "dfirst"){
    sefixef <- sqrt(s2/nother+apply(Xb[, nw, drop = FALSE],1,function(x) t(x)%*%vcov%*%x))
  }
  else{
    Xb <- t(t(Xb[-1,])-Xb[1,])
    sefixef <- sqrt(s2*(1/nother[-1]+1/nother[1])+
                    apply(Xb[, nw, drop = FALSE],1,function(x) t(x)%*%vcov%*%x))
  }
  fixef <- switch(type,
                  "level" = fixef,
                  "dfirst" = fixef[2:length(fixef)]-fixef[1],
                  "dmean" = fixef - mean(fixef)
                  )
  structure(fixef, se = sefixef, class = "fixef", type = type)
}


print.fixef <- function(x,digits= max(3, getOption("digits") - 2),
                        width=getOption("width"), ...){
  attr(x,"se") <- attr(x,"type") <- attr(x,"class") <- NULL
  print.default(x)
}

summary.fixef <- function(object, ...){
  se <- attr(object,"se")
  zvalue <- (object)/se
  res <- cbind(object,se,zvalue,(1-pnorm(abs(zvalue)))*2)
  colnames(res) <- c("Estimate","Std. Error","t-value","Pr(>|t|)")
  class(res) <- "summary.fixef"
  res
}

print.summary.fixef <- function(x,digits= max(3, getOption("digits") - 2),width=getOption("width"),...){
  printCoefmat(x,digits=digits)
}

fixef.pggls <- fixef.plm

