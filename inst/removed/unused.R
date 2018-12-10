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
  if (is.null(id) & is.null(time)){
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
      stop(paste("variable ",id.name," does not exist",sep="")
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
      stop(paste("variable ",time.name," does not exist",sep="")
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
  id.name <- names(data)[1]
  time.name <- names(data)[2]
  list(data = data, id.name = id.name, time.name = time.name)
}

# NB: Are functions indexes and print.indexes still needed?
# NB: print.indexes does not treat the new group index var
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


## TODO: is print.form actually used? - delete?
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
    n <- ifelse(is.matrix(x), nrow(x), length(x))
  
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

