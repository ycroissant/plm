### convert data to plm format
### Author:
### Amendments by Ott Toomet

plm.data_depr_orig <- function(x, indexes = NULL){
  ## this is the original old full plm.data() function kept as reference for testing purposes (non-exported)
  if (is.null(indexes)){
    id <- NULL
    time <- NULL
  }
  if (length(indexes) == 1){
    id <- indexes
    time <- NULL
  }
  if (length(indexes) == 2){
    id <- indexes[1]
    time <- indexes[2]
  }
  if (is.null(id) && is.null(time)){
    id.name <- names(x)[1]
    time.name <- names(x)[2]
  }
  else{
    id.name <- id
    time.name <- time
  }
  data.name <- paste(deparse(substitute(x)))
  # coerce character vectors to factors
  x.char <- names(x)[sapply(x,is.character)]
  for (i in x.char){
    x[[i]] <- factor(x[[i]])
  }

  # replace Inf by NA
  for (i in names(x)) x[[i]][!is.finite(x[[i]])] <- NA

  # check and remove complete NA series
  na.check <- sapply(x,function(x) sum(!is.na(x))==0)
  na.serie <- names(x)[na.check]
  if (length(na.serie)>0){
    if (length(na.serie)==1){
      cat(paste("series",na.serie,"is NA and has been removed\n"))
    }
    else{
      cat(paste("series",paste(na.serie,collapse=", "),"are NA and have been removed\n"))
    }
  }
  x <- x[,!na.check]
  ## Which columns are constants?
  cst.check <- sapply(x, function(x) all(x[!is.na(x)] == (x[!is.na(x)])[1]))
                           # any NA-component equal to the first non-NA component
  cst.serie <- names(x)[cst.check]
  if (length(cst.serie)>0){
    if (length(cst.serie)==1){
      cat(paste("series",cst.serie,"is constant and has been removed\n"))
    }
    else{
      cat(paste("series",paste(cst.serie,collapse=", "),"are constants and have been removed\n"))
    }
  }
#  x <- x[,!cst.check]
  if(is.numeric(id.name)){
    if(!is.null(time.name)){warning("The time argument will be ignored\n")}
    N <- nrow(x)
    if( (N%%id.name)!=0){
      stop("unbalanced panel, the id variable should be indicated\n")
    }
    else{
      T <- N%/%id.name
      n <- N%/%T
      time <- rep(1:T,n)
      id <- rep(seq(1:n),rep(T,n))
      id.name <- "id"
      time.name <- "time"
      x[[id.name]] <- id <- as.factor(id)
      x[[time.name]] <- time <- as.factor(time)
    }
  }
  else{
    if (!id.name %in% names(x))
      stop(paste("variable '",id.name,"' does not exist ",sep="")
           )
    if (is.factor(x[[id.name]])){
      id <- x[[id.name]] <- x[[id.name]][drop=TRUE]
    }
    else{
      id <- x[[id.name]] <- as.factor(x[[id.name]])
    }

    if (is.null(time.name)){
      Ti <- table(id)
      n <- length(Ti)
      time <- c()
      for (i in 1:n){
        time <- c(time,1:Ti[i])
      }
      time.name <- "time"
      time <- x[[time.name]] <- time <- as.factor(time)
    }
    else{
    if (!time.name %in% names(x))
      stop(paste("variable '",time.name,"' does not exist",sep="")
           )
      if (is.factor(x[[time.name]])){
        time <- x[[time.name]] <- x[[time.name]][drop=TRUE]
      }
      else{
        time <- x[[time.name]] <- as.factor(x[[time.name]])
      }
    }
  }
  x <- x[order(id,time),]
  indexes <- list(id=id.name,time=time.name)
  class(indexes) <- "indexes"
  var.names <- names(x)
  for (i in names(x)){
    if(is.factor(x[[i]])){
      if (length(unique(x[[i]])) < length(levels(x[[i]]))){
        x[[i]] <- x[[i]][,drop=TRUE]
      }
    }
  }
  posindexes <- match(c(id.name,time.name),names(x))
  x <- data.frame(x[posindexes],x[-posindexes])
  attr(x,"class") <- c("plm.dim","data.frame")
  x
}


data2plm.data <- function(data, indexes = NULL){
  data <- plm.data(data, indexes)
  id.name <- names(data)[1L]
  time.name <- names(data)[2L]
  list(data = data, id.name = id.name, time.name = time.name)
}

indexes <- function(x){
  if (!inherits(x, "pdata.frame")){
    stop("indexes function only for pdata.frame\n")
  }
  attr(x,"index")
}

print.indexes <- function(x, ...){
  cat(paste("Index: (individual=",x$id,") and (time=",x$time,")\n",sep=""))
}

sumsq <- function(x, ...){
    xb <- mean(x, na.rm = TRUE)
    sum((na.omit(x)-xb)^2)
}



# suml(x) is replaced by Reduce("+", x)
## suml <- function(x){
##   n <- length(x)
##   if (!is.null(dim(x[[1]]))){
##     d <- dim(x[[1]])
##     s <- matrix(0,d[1],d[2])
##     for (i in 1:n){
##       s <- s+x[[i]]
##     }
##   }
##   else{
##     s <- rep(0,length(x[[n]]))
##     for (i in 1:n){
##       s <- s+x[[i]]
##     }
##   }
##   s
## }



oppl <- function(x,y,func){
  n <- length(x)
  z <- list()
  if (!is.list(y)){
    for (i in 1:n){
      t <- paste("\"",func,"\"","(x[[i]],y)",sep="")
      z[[i]] <- eval(parse(text=t))
    }
  }
  else{
    for (i in 1:n){
      t <- paste("\"",func,"\"","(x[[i]],y[[i]])",sep="")
      z[[i]] <- eval(parse(text=t))
    }
  }
  z
}

rbindl <- function(x){
  n <- length(x)
  d <- dim(x[[1]])
  s <- c()
  for (i in 1:n){
    s <- rbind(s,x[[i]])
  }
}


print.form <- function(x, length.line){
  x <- deparse(x,width.cutoff=length.line)
  n <- length(x)
  cat(paste(x[1],"\n",sep=""))
  if (n>1){
    for (i in 2:n){
      cat(paste(x[i],"\n",sep=""))
    }
  }
}


## pdiff is (only) used in model.matrix.pFormula to calculate the model.matrix for FD models,
## works for effect = "individual" and "time", see model.matrix on how to call pdiff.
## Result is in order (id, time) for both effects
## Performs row-wise shifting
opdiff <- function(x, cond, effect = c("individual", "time"), has.intercept = FALSE){
    effect <- match.arg(effect)
    cond <- as.numeric(cond)
    n <- if(is.matrix(x)) nrow(x) else length(x)
  
    # code below is written for effect="individual". If effect="time" is
    # requested, order x so that the code works and later restore original order of x
    if (effect == "time") { order_cond <- order(cond)
        if (!is.matrix(x)) { x <- x[order_cond]} 
        else {x <- x[order_cond, ] }
        cond <- cond[order_cond]
    }
  
    cond <- c(NA, cond[2:n] - cond[1:(n-1)]) # this assumes a certain ordering
    cond[cond != 0] <- NA
    
    if (!is.matrix(x)){
        result <- c(NA, x[2:n] - x[1:(n-1)])
        result[is.na(cond)] <- NA
        # for effect = "time": restore original order of x:
        if (effect == "time") result <- result[match(seq_len(n), order_cond)]
        result <- na.omit(result)
    }
    else{
        result <- rbind(NA, x[2:n, , drop=FALSE] - x[1:(n-1), , drop = FALSE])
        result[is.na(cond), ] <- NA
        # for effect = "time": restore original order of x:
        if (effect == "time") result <- result[match(seq_len(n), order_cond), ]
        result <- na.omit(result)
        result <- result[ , apply(result, 2, var) > sqrt(.Machine$double.eps), drop = FALSE]
        if (has.intercept){
            result <- cbind(1, result)
            colnames(result)[1] <- "(intercept)"
        }
    }
    attr(result, "na.action") <- NULL
    result
}

## expand.formula <- function(x){
##   oclass <- class(x)
##   if (! any(class(x) == "Formula")) stop("not a Formula object")
##   if (length(x)[2] != 2) stop("not a two part formula")
##   xs <- structure(x, class = "formula")
##   has.response <- attr(terms(xs),"response") == 1
##   if (has.response){
##     y <- x[[2]]
##     rhs <- x[[3]]
##   }
##   else{
##     y <- NULL
##     rhs <- x[[2]]
##   }
##   firstpart <- rhs[[2]]
##   secondpart <- rhs[[3]]
##   if (has.response){
##     one <- do.call("~", list(y,firstpart))
##     two <- do.call("~", list(y,secondpart))
##   }
##   else{
##     one <- do.call("~", list(firstpart))
##     two <- do.call("~", list(secondpart))
##   }
##   two <- update(one, two)
##   one <- paste(deparse(one), collapse = "")
##   two <- paste(deparse(two[[3]]), collapse = "")
##   result <- as.formula(paste(one, "|", two, collapse = ""));
##   result <- as.Formula(result)
##   #YC  class(result) <- c("pFormula", class(result))
##   structure(result, class = oclass)
## }


##### Old pFormula stuff - removed 2023-02-28 ####
# @rdname plm-deprecated
# @export
pFormula <- function(object) {
  .Deprecated(msg = paste0("class 'pFormula' is deprecated, simply use class",
                           "'Formula'. 'pFormula' will be removed very soon!"),
              old = "pFormula", new = "Formula")
  stopifnot(inherits(object, "formula"))
  if (!inherits(object, "Formula")){
    object <- Formula(object)
  }
  class(object) <- unique(c("pFormula", class(object)))
  object
}

# @rdname plm-deprecated
# @export
as.Formula.pFormula <- function(x, ...){
  class(x) <- setdiff(class(x), "pFormula")
  x
}

# @rdname plm-deprecated
# @export
model.frame.pFormula <- function(formula, data, ..., lhs = NULL, rhs = NULL){
  if (is.null(rhs)) rhs <- 1:(length(formula)[2L])
  if (is.null(lhs)) lhs <- if(length(formula)[1L] > 0L) 1 else 0
  index <- attr(data, "index")
  mf <- model.frame(as.Formula(formula), as.data.frame(data), ..., rhs = rhs)
  index <- index[as.numeric(rownames(mf)), ]
  index <- droplevels(index)
  class(index) <- c("pindex", "data.frame")
  structure(mf,
            index = index,
            class = c("pdata.frame", class(mf)))
}


# @rdname plm-deprecated
# @export
model.matrix.pFormula <- function(object, data,
                                  model = c("pooling", "within", "Between", "Sum",
                                            "between", "mean", "random", "fd"),
                                  effect = c("individual", "time", "twoways", "nested"),
                                  rhs = 1,
                                  theta = NULL,
                                  cstcovar.rm = NULL,
                                  ...){
  model <- match.arg(model)
  effect <- match.arg(effect)
  formula <- object  
  has.intercept <- has.intercept(formula, rhs = rhs)
  # relevant defaults for cstcovar.rm
  if (is.null(cstcovar.rm)) cstcovar.rm <- ifelse(model == "within", "intercept", "none")
  balanced <- is.pbalanced(data)
  # check if inputted data is a model.frame, if not convert it to
  # model.frame (important for NA handling of the original data when
  # model.matrix.pFormula is called directly) As there is no own
  # class for a model.frame, check if the 'terms' attribute is
  # present (this mimics what lm does to detect a model.frame)    
  if (is.null(attr(data, "terms")))
    data <- model.frame.pFormula(pFormula(formula), data)  
  # this goes to Formula::model.matrix.Formula:
  X <- model.matrix(as.Formula(formula), rhs = rhs, data = data, ...)
  # check for infinite or NA values and exit if there are some
  if(any(! is.finite(X))) stop(paste("model matrix or response contains non-finite",
                                     "values (NA/NaN/Inf/-Inf)"))
  X.assi <- attr(X, "assign")
  X.contr <- attr(X, "contrasts")
  X.contr <- X.contr[ ! sapply(X.contr, is.null) ]
  index <- index(data)
  checkNA.index(index) # check for NAs in model.frame's index and error if any
  attr(X, "index") <- index
  if (effect == "twoways" && model %in% c("between", "fd"))
    stop("twoways effect only relevant for within, random and pooling models")
  if (model == "within")  X <- Within(X, effect)
  if (model == "Sum")     X <- Sum(X, effect)
  if (model == "Between") X <- Between(X, effect)
  if (model == "between") X <- between(X, effect)
  if (model == "mean")    X <- Mean(X)
  if (model == "fd")      X <- pdiff(X, effect = "individual",
                                     has.intercept = has.intercept)
  if (model == "random"){
    if (is.null(theta)) stop("a theta argument should be provided")
    if (effect %in% c("time", "individual")) X <- X - theta * Between(X, effect)
    if (effect == "nested") X <- X - theta$id * Between(X, "individual") -
        theta$gp * Between(X, "group")
    if (effect == "twoways" && balanced)
      X <- X - theta$id * Between(X, "individual") -
        theta$time * Between(X, "time") + theta$total * Mean(X)
  }
  
  if (cstcovar.rm == "intercept"){
    posintercept <- match("(Intercept)", colnames(X))
    if (! is.na(posintercept)) X <- X[ , - posintercept, drop = FALSE]
  }
  if (cstcovar.rm %in% c("covariates", "all")){
    cols <- apply(X, 2, is.constant)
    cstcol <- names(cols)[cols]
    posintercept <- match("(Intercept)", cstcol)
    cstintercept <- if(is.na(posintercept)) FALSE else TRUE
    zeroint <- if(cstintercept &&
                  max(X[, posintercept]) < sqrt(.Machine$double.eps))
      TRUE else FALSE
    if (length(cstcol) > 0L){
      if ((cstcovar.rm == "covariates" || !zeroint) && cstintercept) cstcol <- cstcol[- posintercept]
      if (length(cstcol) > 0L){
        X <- X[, - match(cstcol, colnames(X)), drop = FALSE]
        attr(X, "constant") <- cstcol
      }
    }
  }
  structure(X, assign = X.assi, contrasts = X.contr, index = index)
}

## alias.pFormula <- function(object, data,
##                            model = c("pooling", "within", "Between", "between",
##                                      "mean", "random", "fd"),
##                            effect = c("individual", "time", "twoways"),
##                            ...) {
##   dots <- list(...)
##   if (!is.null(dots$inst.method)) stop("alias.plm/alias.pFormula: IV not supported")
##   model <- match.arg(model)
##   effect <- match.arg(effect)
##   formula <- object

##   # check if object is already pFormula, try to convert if not    
##   if (!inherits(formula, "pFormula")) formula <- pFormula(formula)

##   # check if data is already a model frame, convert to if not
##   if (is.null(attr(data, "terms"))) {
##     data <- model.frame.pFormula(pFormula(formula), data)
##   }

##   plmobj <- plm(formula, data = data, model = model, effect = effect, ...)
## #  print(summary(plmobj))
##   return(alias(plmobj, ...))
## }
