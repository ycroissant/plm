###################################################
### chunk number 1: pdata.frame
###################################################
pdata.frame <- function(x, index = NULL, drop.index = FALSE, row.names = TRUE){
  if (inherits(x, "pdata.frame")) stop("already a pdata.frame")

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
      cat(paste("serie",na.serie," is NA and has been removed\n"))
    }
    else{
      cat(paste("series ",paste(na.serie,collapse=",")," are NA and have been removed\n"))
    }
  }
  x <- x[, !na.check]
  
  # check and remove cst series
  cst.check <- sapply(x,function(x) var(as.numeric(x))==0)
  cst.serie <- names(x)[cst.check]

  if (length(cst.serie)>0){
    if (length(cst.serie)==1){
      cat(paste("serie",cst.serie," is constant and has been removed\n"))
    }
    else{
      cat(paste("series ",paste(na.serie,collapse=",")," are constants and have been removed\n"))
    }
  }
  x <- x[,!cst.check]
  
  # if index is NULL, both id and time are NULL
  if (is.null(index)){
    id <- NULL
    time <- NULL
  }
  # if the length of index is 1, id = index and time is NULL
  if (length(index)==1){
    id <- index
    time <- NULL
  }
  # if the length of index is 2, the first element is id, the second is time
  if (length(index)==2){
    id <- index[1]
    time <- index[2]
  }
  # if both id and time are NULL, the names of the index are the first
  # two names of x
  if (is.null(id) & is.null(time)){
    id.name <- names(x)[1]
    time.name <- names(x)[2]
  }
  else{
    id.name <- id
    time.name <- time
  }
  # index is numeric
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
      id <- x[[id.name]] <- x[[id.name]][drop=T]
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
        time <- x[[time.name]] <- x[[time.name]][drop=T]
      }
      else{
        time <- x[[time.name]] <- as.factor(x[[time.name]])
      }
    }
  }
  x <- x[order(id,time),]
  var.names <- names(x)
  for (i in names(x)){
    if(is.factor(x[[i]])){
      if (length(unique(x[[i]])) < length(levels(x[[i]]))){
        x[[i]] <- x[[i]][,drop=TRUE]
      }
    }
  }
  posindex <- match(c(id.name,time.name),names(x))
  index <- x[, posindex]
  if (drop.index) x <- x[, - posindex]
  if (row.names){
    attr(x, "row.names") <- paste(index[[1]],index[[2]],sep="-") 
  }
  attr(x, "index") <- index
  class(x) <- c("pdata.frame", "data.frame")
  x
}

"[.pdata.frame" <- function(x, i, j, drop = TRUE){
  index <- "[.data.frame"(attr(x, "index"), i,)
  #remove empty levels if any
  index <- data.frame(lapply(index, function(x) x[drop = TRUE]))
  data <- "[.data.frame"(x, i, j, drop = drop)
  structure(data,
            index = index,
            class = c("pdata.frame", "data.frame"))
}

print.pdata.frame <- function(x, ...){
  attr(x, "index") <- NULL
  class(x) <- "data.frame"
  print(x, ...)
}





###################################################
### chunk number 3: extracting
###################################################
"[[.pdata.frame" <- function(x, y){
  index <- attr(x, "index")
  attr(x, "index") <- NULL
  class(x) <- "data.frame"
  structure(x[[y]],
            class = c("pserie", class(x[[y]])),
            index = index,
            names = row.names(x))
}  

"$.pdata.frame" <- function(x,y){
  "[["(x, paste(as.name(y)))
}

print.pserie <- function(x, ...){
  attr(x, "index") <- NULL
  attr(x, "class") <- attr(x, "class")[-1]
  if (length(attr(x, "class")) == 1
      && class(x) %in% c("character", "logical", "numeric"))
    attr(x, "class") <- NULL
  print(x, ...)
}


###################################################
### chunk number 5: as.matrix
###################################################
as.matrix.pserie <- function(x, idbyrow = TRUE, ...){
  index <- attr(x, "index")
  id <- index[[1]]
  time <- index[[2]]
  time.names <- levels(as.factor(time))
  x <- split(data.frame(x,time), id)
  x <- lapply(x, function(x){
    rownames(x) <- x[,2]
    x[,-2,drop=F]
  })
  x <- lapply(x, function(x){
    x <- x[time.names,,drop=F]
    rownames(x) <- time.names
    x
  }
  )
  id.names <- names(x)
  x <- as.matrix(as.data.frame((x)))
  colnames(x) <- id.names
  if (idbyrow) x <- t(x)
  x
}



###################################################
### chunk number 7: lag and diff
###################################################
lag.pserie <- function(x, k = 1, ...){
  index <- attr(x, "index")
  id <- index[[1]]
  time <- index[[2]]
  isNAtime <- c(rep(1,k), diff(as.numeric(time), lag = k)) != k
  isNAid <- c(rep(1,k), diff(as.numeric(id), lag = k)) != 0
  isNA <- as.logical(isNAtime + isNAid)
  result <- c(rep(NA, k), x[1:(length(x)-k)])
  result[isNA] <- NA
  structure(result,
            class = class(x),
            index = index)
}

diff.pserie <- function(x, lag = 1, ...){
  lagx <- lag(x, k = lag)
  x-lagx
}


###################################################
### chunk number 8: Tapply
###################################################
Tapply <- function(x, ...){
  UseMethod("Tapply")
}

Tapply.default <- function(x, effect, func, ...){
  na.x <- is.na(x)
  uniqval <- tapply(x, effect, func, ...)
  nms <- attr(uniqval, "dimnames")[[1]]
  attr(uniqval, "dimnames") <- attr(uniqval, "dim") <- NULL
  names(uniqval) <- nms
  result <- uniqval[as.character(effect)]
  result[na.x] <- NA
  result
}

Tapply.pserie <- function(x, effect = c("individual", "time"), func, ...){
  effect <- match.arg(effect)
  index <- attr(x, "index")
  effect <- switch(effect,
                 "individual"= index[[1]],
                 "time"= index[[2]]
                 )
  Tapply.default(x, effect, func, ...)
}

Tapply.matrix <- function(x, effect, func, ...){
  na.x <- is.na(x)
  uniqval <- apply(x, 2, function(z) tapply(z, effect, "mean"))
  uniqval <- apply(x, 2, function(z) tapply(z, effect, func))
  uniqval <- apply(x, 2, tapply, effect, func)
  result <- uniqval[as.character(effect), , drop = F]
  result[na.x] <- NA
  result
}


###################################################
### chunk number 10: within and between
###################################################
Between <- function(x,...){
  UseMethod("Between")
}

Between.default <- function(x, effect, ...){
  if (!is.numeric(x)) stop("The Between function only applies to numeric vectors")
  Tapply(x, effect, mean, ...)
}

Between.pserie <- function(x, effect = c("individual", "time"), ...){
  effect <- match.arg(effect)
  Tapply(x, effect = effect, mean, ...)
}

between <- function(x,...){
  UseMethod("between")
}

between.default <- function(x, effect, ...){
  if (!is.numeric(x)) stop("The between function only applies to numeric vectors")
  tapply(x, effect, mean, ...)
}

between.pserie <- function(x, effect = c("individual", "time"), ...){
  effect <- match.arg(effect)
  index <- attr(x, "index")
  if (effect == "individual") effect <- index[[1]] else effect <- index[[2]]
  x <- between.default(x, effect = effect, ...)
  nms <- attr(x, "dimnames")[[1]]
  attr(x, "dimnames") <- attr(x, "dim") <- NULL
  names(x) <- nms
  x
}

between.matrix <- function(x, effect, ...){
  apply(x, 2, tapply, effect, mean, ...)
}

within <- function(x,...){
  UseMethod("within")
}

within.default <- function(x, effect, ...){
  if (!is.numeric(x)) stop("the within function only applies to numeric vectors")
  x - Between(x, effect, ...)
}

within.pserie <- function(x, effect = c("individual", "time"), ...){
  effect <- match.arg(effect)
  within.default(x, effect, ...)
}

within.matrix <- function(x, effect, ...){
  result <- within.default(x, effect, ...)
  othervar <- apply(result, 2, function(x) sum(abs(x), na.rm = TRUE)) > 1E-12
  result <- result[, othervar, drop = FALSE]
  attr(result, "constant") <- colnames(x)[!othervar]
  result
}



###################################################
### chunk number 12: methods for pserie
###################################################
sumsq <- function(x, ...){
  xb <- mean(x, na.rm = TRUE)
  sum((na.omit(x)-xb)^2)
}


summary.pserie <- function(object, ...){
  id <- attr(object, "index")[[1]]
  time <- attr(object, "index")[[2]]
  xm <- mean(object, na.rm = TRUE)
  Bid <-  Between(object, na.rm = TRUE)
  Btime <-  Between(object, effect = "time", na.rm = TRUE)
  structure( c(total = sumsq(object), between_id= sumsq(Bid), between_time = sumsq(Btime)), 
            class = c("summary.pserie", "numeric")
            )
}

plot.summary.pserie <- function(x, ...){
  x <- as.numeric(x)
  share <- x[-1]/x[1]
  names(share) <- c("id", "time")
  barplot(share, ...)
}

print.summary.pserie <- function(x, ...){
  digits <- getOption("digits")
  x <- as.numeric(x)
  share <- x[-1]/x[1]
  names(share) <- c("id", "time")
  cat(paste("total sum of squares :", signif(x[1], digits = digits),"\n"))
  print.default(share, ...)
}


###################################################
### chunk number 14: as.data.frame
###################################################
as.data.frame.pdata.frame <- function(x, row.names = NULL, optional = FALSE, ...){
  index <- attr(x, "index")
  x <- lapply(x,
              function(z){
                attr(z, "index") <- index
                class(z) <- c("pserie", class(z))
                z
              }
              )
  data.frame(x)
}


pdiff <- function(x, cond, has.intercept = FALSE){
  cond <- as.numeric(cond)
  n <- ifelse(is.matrix(x),nrow(x),length(x))
  cond <- c(NA,cond[2:n]-cond[1:(n-1)])
  cond[cond != 0] <- NA
  if (!is.matrix(x)){
    result <- c(NA,x[2:n]-x[1:(n-1)])
    result[is.na(cond)] <- NA
    result <- na.omit(result)
  }
  else{
    result <- rbind(NA,x[2:n,,drop=FALSE]-x[1:(n-1),,drop=FALSE])
    result[is.na(cond),] <- NA
    result <- na.omit(result)
    result <- result[,apply(result,2, var) > 1E-12,drop = FALSE]
    if (has.intercept){
      result <- cbind(1,result)
      colnames(result)[1] <- "(intercept)"
    }

  }
  attr(result,"na.action") <- NULL
  result
}
