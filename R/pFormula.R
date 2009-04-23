## pFormula:
## methods : formula, model.frame, model.matrix, pmodel.response


pFormula <- function(object, extra = NULL) {
  stopifnot(inherits(object, "formula"))
  if(!inherits(object, "Formula")) class(object) <- c("pFormula","Formula", "formula")
  if (!is.null(extra)) attr(object,"extra") <- extra
  object
}

print.pFormula <- function(x, ...){
  attr(x, "extra") <- NULL
  class(x) <- "formula"
  print(x, ...)
}

formula.pFormula <- function(x,
                            part = c("first", "second", "both"),
                            response = NULL, include.extra = FALSE, ...)
{
  xs <- structure(x, class = "formula")
  extra <- attr(x, "extra")
  part <- match.arg(part)
  has_response <- attr(terms(xs), "response") == 1L
  # terms.Formula calls formula.Formula, so we use a copy of class "formula"
  if (has_response){
    y <- x[[2]]
    rhs <- x[[3]]
    if (is.null(response)) response <- TRUE
  }
  else{
    if (is.null(response)) response <- FALSE
    if (response) stop("one sided formula")
    y <- NULL
    rhs <- x[[2]]
  }
  if (length(x) == 1){
    firstpart <- rhs
    secondpart <- NULL
    if (part %in% c("second", "both")) stop("one part formula")
  }
  if (length(x) == 2) {
    firstpart <- rhs[[2]]
    secondpart <- rhs[[3]]
    bothparts <- rhs
    bothparts[[1]] <- as.name("+")
  }

  switch(part,
         "first" = {
           if (include.extra){
             for (i in extra){
               firstpart <- do.call("~",list(firstpart,as.name(i)))
               firstpart[[1]] <- as.name("+")
             }
           }
           if(response) g <- do.call("~", list(y, firstpart))
           else g <- do.call("~", list(firstpart))
         },
         "second" = {
           if (include.extra){
             for (i in extra){
               secondpart <- do.call("~",list(secondpart,as.name(i)))
               secondpart[[1]] <- as.name("+")
             }
           }
           if(response) g <- do.call("~", list(y, secondpart))
           else g <- do.call("~", list(secondpart))
         },
         "both" = {
           if (include.extra){
             for (i in extra){
               bothparts <- do.call("~",list(bothparts,as.name(i)))
               bothparts[[1]] <- as.name("+")
             }
           }
           if(response) g <- do.call("~", list(y, bothparts))
           else g <- do.call("~", list(bothparts))
         }
         )
  g
}

model.frame.pFormula <- function(formula, ..., part = NULL, response = NULL,
                                 include.extra = FALSE){
  if (is.null(response)) response <- attr(terms(formula), "response") == 1L
  if (is.null(part)){
    if (length(formula) == 1) part = "first"
    if (length(formula) == 2) part = "both"
  }
  form <- formula(formula, part = part, response = response, include.extra = include.extra)
  result <- model.frame(form, ...)
  if (include.extra){
    # so that terms don't include the index and that the good formula
    # can be extracted
    form <- formula(formula, part = part, response = response, include.extra = FALSE)
    result2 <- model.frame(form, ...)
    attr(result, "terms") <- attr(result2, "terms")
  }
  
  if (include.extra){
    extra <- attr(formula,"extra")
    names(extra) <- paste("(",names(extra),")",sep="")
    names(result)[match(extra,names(result))] <- names(extra)
  }
  result
}

model.matrix.pFormula <- function(object, data,
                                  model = c("pooling","within","Between",
                                    "between","mean","random","fd"),
                                  effect = c("individual","time","twoways"),
                                  part = c("first","second"),
                                  theta = NULL, ...){
  model <- match.arg(model)
  effect <- match.arg(effect)
  part <- match.arg(part)
  formula <- object
  has.intercept <- has.intercept(formula, part = part)
  formula <- formula(formula, part = part)
  X <- model.matrix(formula, data)
  id <- data[["(id)"]]
  time <- data[["(time)"]]
  pdim <- pdim(data)
  if (has.intercept && model == "within") X <- X[,-1,drop=FALSE]
  if (effect != "twoways"){
    if (effect == "individual") cond <- id
    if (effect == "time") cond <- time
    result <- switch(model,
                     "within"  = within(X,cond),
                     "Between" = Between(X,cond),
                     "between" = between(X,cond),
                     "pooling" = X,
                     "mean"    = matrix(apply(X,2,mean),nrow(X),ncol(X),byrow=T),
                     "random"  = X - theta * Between(X,cond),
                     "fd"      = pdiff(X, cond, has.intercept = has.intercept)
                     )
  }
  else{
    if (pdim$balanced){
      result <- switch(model,
                       "within" = X - Between(X,id) - Between(X,time) +
                       matrix(apply(X,2,mean),nrow(X),ncol(X),byrow=T),
                       "pooling" = X,
                       "random" = X - theta$id * Between(X,id) - theta$time * Between(X,time) +
                       theta$total * matrix(apply(X,2,mean),nrow(X),ncol(X),byrow=T)
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
      if (model == "pooling") result <- X
    }
  }
  result
}


pmodel.response <- function(object, ...){
  UseMethod("pmodel.response")
}

pmodel.response.data.frame <- function(object,
                                       model = c("pooling","within","Between",
                                         "between","mean","random","fd"),
                                       effect = c("individual","time","twoways"),
                                       theta = NULL, ...){
  data <- object
  model <- match.arg(model)
  effect <- match.arg(effect)
  pdim <- pdim(data)
  y <- unclass(data[[1]])
  id <- data[["(id)"]]
  time <- data[["(time)"]]

  if (effect != "twoways"){
    if (effect == "individual") cond <- id
    if (effect == "time") cond <- time
    result <- switch(model,
                     "within"  = within(y,cond),
                     "Between" = Between(y,cond),
                     "between" = between(y,cond),
                     "pooling" = y,
                     "mean"    = mean(y),
                     "random"  = y - theta * Between(y,cond),
                     "fd"      = pdiff(y,cond)
                     )
  }
  else{
    if (pdim$balanced){
      result <- switch(model,
                       "within" = y - Between(y,id) - Between(y,time) + mean(y),
                       "random" = y - theta$id * Between(y,id) -
                       theta$time * Between(y,time) + theta$total * mean(y)
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
        PHI1 <- crossprod(Z1,y)
        PHI2 <- crossprod(Z2,y)
        PHIB <- ginv(Q)%*%(PHI2-A%*%solve(DH)%*%PHI1)
        result <- as.vector(y-Z1%*%solve(DH)%*%PHI1-Zb%*%PHIB)
      }
    }
  }
  result
}




model.matrix.plm <- function(object, ...){
  dots <- list(...)
  model <- ifelse(is.null(dots$model), describe(object, "model"), dots$model)
  effect <- ifelse(is.null(dots$effect), describe(object, "effect"), dots$effect)
  formula <- formula(object)
  data <- model.frame(object)
  if (model != "random"){
    model.matrix(formula, data, model = model, effect = effect)
  }
  else{
    theta <- ercomp(object)$theta
    model.matrix(formula, data, model = model, effect = effect, theta = theta)
  }

}

  


pmodel.response.plm <- function(object, model = c("pooling","within","Between",
                                          "between","mean","random","fd"),
                                effect = c("individual","time","twoways"),
                                theta = NULL, ...){
  model <- match.arg(model)
  effect <- match.arg(effect)
  if (model == "random") theta <- object$ercomp$theta
  pmodel.response(model.frame(object), model = model, effect = effect, theta = theta)
}
  

  
  
