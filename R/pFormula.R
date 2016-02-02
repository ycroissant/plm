## pFormula:
## methods : formula, model.frame, model.matrix, pmodel.response


pFormula <- function(object) {
  stopifnot(inherits(object, "formula"))
  if (!inherits(object, "Formula")){
    object <- Formula(object)
  }
  class(object) <- c("pFormula", class(object))
  object
}

as.Formula.pFormula <- function(x, ...){
  class(x) <- class(x)[-1]
  x
}

model.frame.pFormula <- function(formula, data, ..., lhs = NULL, rhs = NULL){
  if (is.null(rhs)) rhs <- 1:(length(formula)[2])
  if (is.null(lhs)) lhs <- ifelse(length(formula)[1]>0, 1, 0)
  index <- attr(data, "index")
  mf <- model.frame(as.Formula(formula), as.data.frame(data), ..., rhs = rhs)
  index <- index[as.numeric(rownames(mf)), ]
  index <- data.frame(lapply(index, function(x) x[drop = TRUE]))
  class(index) <- c("pindex", "data.frame")
  structure(mf,
            index = index,
            class = c("pdata.frame", class(mf)))
}

model.matrix.pFormula <- function(object, data,
                                  model = c("pooling","within","Between",
                                    "between","mean","random","fd"),
                                  effect = c("individual","time","twoways"),
                                  rhs = 1,
                                  theta = NULL, ...){
  model <- match.arg(model)
  effect <- match.arg(effect)
  formula <- object
  has.intercept <- has.intercept(formula, rhs = rhs)
  
  # NB: data is not sanitized here, i.e. NAs could still be present.
  # Formula::model.matrix.Formula does not remove rows if there are
  # NAs in the corresponsing positions in the dependend variable,
  # unlike stats::model.matrix.default which drops affected rows
  # See testfiles tests/test_model.matrix_pmodel.response.R and
  # test_model.matrix_pmodel.response_NA.R
  # Achim's general advice on such things: build model.frame first
  # Estimation via plm() is not affected, because plm already builds
  # the model.frame before model.matrix.pFormula is called. Thus, only
  # direct calls to model.matrix.pFormula (and maybe pmodel.response)
  # are affected.
  # However building the model.frame here to sanitize data
  # will result in a slight (?) performance regression when calling
  # plm() because the model.frame is then build a second time.
  
  # Decision on whether to apply the balanced or unbalanced transformations
  # [if pdim(data)$balanced below] needs to be based on sanitized data (with
  # affected rows removed (also rows removed if NA in corresponsing position
  # in dependend variable)).

  
  X <- model.matrix(as.Formula(formula), rhs = rhs, data = data, ...)
  X.assi <- attr(X, "assign")
  X.contr <- attr(X, "contrasts")
  X.contr <- X.contr[ !sapply(X.contr, is.null) ]  ##drop NULL elements
    index <- attr(data, "index")
  id <- index[[1]]
  if(any(is.na(id))) {
     stop("NA in the individual index variable")
  }
  time <- index[[2]]
  pdim <- pdim(data)
  if (has.intercept && model == "within") X <- X[ , -1, drop = FALSE]
  if (effect != "twoways"){
    if (effect == "individual") cond <- id
    if (effect == "time") cond <- time
    result <- switch(model,
                     "within"  = Within(X, cond),
                     "Between" = Between(X, cond),
                     "between" = between(X, cond),
                     "pooling" = X,
                     "mean"    = matrix(apply(X, 2, mean), nrow(X), ncol(X), byrow = T),
                     "random"  = X - theta * Between(X,cond),
                     "fd"      = pdiff(X, cond, has.intercept = has.intercept)
                     )
  }
  else{
    if (pdim$balanced){
      result <- switch(model,
                       "within" = X - Between(X,id) - Between(X,time) +
                                  matrix(apply(X,2,mean),nrow(X),ncol(X),byrow=T),
                       "random" = X - theta$id * Between(X,id) - theta$time * Between(X,time) +
                                  theta$total * matrix(apply(X,2,mean),nrow(X),ncol(X),byrow=T),
                       "pooling" = X
                       )
    }
    else{
      if (model == "within"){
        Z1 <- model.matrix(~id-1)
        Z2 <- model.matrix(~time-1)
        DH <- crossprod(Z1)
        DT <- crossprod(Z2)
        A <- crossprod(Z2,Z1)
        Q <- DT-A%*%solve(DH)%*%t(A)
        Zb <- Z2-Z1%*%solve(DH)%*%t(A)
        Q <- t(Z2)%*%Zb
        PHI1 <- apply(X,2,function(x) crossprod(Z1,x))
        PHI2 <- apply(X,2,function(x) crossprod(Z2,x))
        PHIB <- ginv(Q)%*%(PHI2-A%*%solve(DH)%*%PHI1)
        result <- X-Z1%*%solve(DH)%*%PHI1-Zb%*%PHIB
      }
    }
  }
  attr(result, "assign") <- X.assi
  attr(result, "contrasts") <- X.contr
  result
}

pmodel.response <- function(object, ...) {
  UseMethod("pmodel.response")
}


pmodel.response.data.frame <- function(object,
                                       model = c("pooling","within","Between",
                                         "between","mean","random","fd"),
                                       effect = c("individual","time","twoways"),
                                       lhs = NULL,
                                       theta = NULL, ...){
  data <- object
  formula <- formula(paste("~ ", names(data)[[1]], " - 1", sep = ""))
  y <- model.matrix(pFormula(formula), data = data,
                    model = model, effect = effect,
                    lhs = lhs, theta = theta, ...)
#  dim(y) <- NULL
  namesy <- rownames(y)
  y <- as.numeric(y)
  names(y) <- namesy
  y
}

pmodel.response.pFormula <- function(object, data,
                                     model = c("pooling","within","Between",
                                       "between","mean","random","fd"),
                                     effect = c("individual","time","twoways"),
                                     lhs = NULL,
                                     theta = NULL, ...){
  formula <- pFormula(object) # was: formula <- object
  if (is.null(lhs))
    if (length(formula)[1] == 0) stop("no response") else lhs <- 1
  formula <- formula(paste("~ ", deparse(attr(formula, "lhs")[[lhs]]), " - 1", sep = ""))
  y <- model.matrix(pFormula(formula), data = data,
                    model = model, effect = effect,
                    lhs = lhs, theta = theta, ...)
                           #  dim(y) <- NULL
  namesy <- rownames(y)
  y <- as.numeric(y)
  names(y) <- namesy
  y
}


model.matrix.plm <- function(object, ...){
    dots <- list(...)
    model <- ifelse(is.null(dots$model), describe(object, "model"), dots$model)
    effect <- ifelse(is.null(dots$effect), describe(object, "effect"), dots$effect)
    rhs <- ifelse(is.null(dots$rhs), 1, dots$rhs)
    formula <- formula(object)
    data <- model.frame(object)
    if (model != "random"){
        model.matrix(formula, data, model = model, effect = effect, rhs = rhs)
    }
    else{
        theta <- ercomp(object)$theta
        model.matrix(formula, data, model = model, effect = effect, theta = theta, rhs = rhs)
    }
}

pmodel.response.plm <- function(object, ...){
  dots <- list(...)
  model <- ifelse(is.null(dots$model), describe(object, "model"), dots$model)
  effect <- ifelse(is.null(dots$effect), describe(object, "effect"), dots$effect)
  formula <- formula(object)
  data <- model.frame(object)
  if (model != "random"){
    pmodel.response(formula, data, model = model, effect = effect)
  }
  else{
    theta <- ercomp(object)$theta
    pmodel.response(formula, data, model = model, effect = effect, theta = theta)
  }
}
