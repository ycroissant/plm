papply <- function(x, ...){
  UseMethod("papply")
}

papply.default <- function(x, func, cond,...){
  na.x <- is.na(x)
  cm <- tapply(x,cond,func)
  Cm <- cm[as.character(cond)]
  Cm[na.x] <- NA
  attr(Cm,"cm") <- cm
  Cm
}

papply.pserie <- function(x, func, effect = "individual",...){
  na.x <- is.na(x)
  data.name <- attr(x,"data")
  sc <- sapply(sys.calls(),function(x) as.character(x[[1]]))
  leframe <- which(!is.na(match( sc, c("pgmm","pgls","pgmm","pvcm"))))
  leframe <- ifelse(length(leframe) == 1, leframe, 1)
  
  data <- get(data.name, sys.frame(which = leframe))
  indexes <- attr(data,"indexes")
  cond <- switch(effect,
                 "individual"= data[[indexes$id]],
                 "time"= data[[indexes$time]],
                 stop("effect must be individual or time")
                 )
  cm <- tapply(x, cond, func)
  Cm <- cm[as.character(cond)]
  Cm[na.x] <- NA
  attr(Cm,"cm") <- cm
  class(Cm) <- "pserie"
  attr(Cm,"data") <- data.name
  Cm
}

papply.matrix <- function(x, func, cond,...){
  na.x <- is.na(x)
  cm <- apply(x,2,tapply,cond,func)
  Cm <- cm[as.character(cond),,drop=F]
  Cm[na.x] <- NA
  attr(Cm,"cm") <- cm
  Cm
}

Between <- function(x,...){
  UseMethod("Between")
}

Between.default <- function(x, cond, ...){
  if (is.numeric(x)){
    res <- papply(x,mymean,cond)
  }
  else{
    stop("The Between function only applies to numeric vectors\n")
  }
  attr(res,"cm") <- NULL
  res
}

Between.pserie <- function(x, effect = "individual", ...){
  if (is.numeric(x)){
    res <- papply(x, mymean, effect)
  }
  else{
    stop("The Between function only applies to numeric vectors\n")
  }
  attr(res,"cm") <- NULL
  res
}


Between.matrix <- function(x,cond, ...){
  if (is.numeric(x)){
    res <- papply(x,mymean,cond)
  }
  else{
    stop("The Between function only applies to numeric vectors\n")
  }
  attr(res,"cm") <- NULL
  res
}

between <- function(x,...){
  UseMethod("between")
}

between.default <- function(x,cond, ...){
  if (is.numeric(x)){
    res <- tapply(x,cond,mymean)
  }
  else{
    stop("The between function only applies to numeric vectors\n")
  }
  res
}

between.pserie <- function(x,effect="individual", ...){
  data.name <- attr(x,"data")
  classx <- class(x)
  sc <- sapply(sys.calls(),function(x) as.character(x[[1]]))
  leframe <- which(!is.na(match(sc,c("pgmm","pgls","pgmm","pvcm"))))
  leframe <- ifelse(length(leframe)==1,leframe,1)

  data <- get(data.name,sys.frame(which=leframe))
  indexes <- attr(data,"indexes")
  cond <- switch(effect,
                 "individual"= data[[indexes$id]],
                 "time"= data[[indexes$time]],
                 stop("effect must be individual or time")
               )
  if (is.numeric(x)){
    res <- tapply(x,cond,mymean)
  }
  else{
    stop("The between function only applies to numeric vectors\n")
  }
  res
}

between.matrix <- function(x,cond, ...){
  if (is.numeric(x)){
    res <- apply(x,2,tapply,cond,mymean)
  }
  else{
    stop("The between function only applies to numeric vectors\n")
  }
  res
}


within <- function(x,...){
  UseMethod("within")
}

within.default <- function(x,cond, ...){
  res <- x-papply(x,mymean,cond)
  attr(res,"cm") <- NULL
  res
}

within.pserie <- function(x,effect="individual", ...){
  if (!is.numeric(x)) stop("the within function only applies to numric vectors")
  res <- switch(effect,
                "individual"= x-Between(x,effect),
                "time"=  x-Between(x,effect),
                "twoways"= x-Between(x,"individual")-Between(x,"time")+mean(x),
                stop("effect must be individual, time or twoways")
                )
  res
}

within.matrix <- function(x,cond, ...){
  if (is.numeric(x)){
    res <- x-papply(x,mymean,cond)
    timevar <- apply(res,2,myvar) > 1E-12
    res <- res[,timevar,drop = FALSE]
    attr(res, "timeconst") <- colnames(x)[!timevar]
  }
  else{
    stop("The within function only applies to numeric vectors\n")
  }
  attr(res,"cm") <- NULL
  res
}

diff.pserie <- function(x,lag=0,...){
  if (!is.numeric(x)) stop("diff meaningfull only for numeric vectors\n")
  xlagt <- lag(x,k=lag)
  xlagtm1 <- lag(x,k=lag+1)
  xdiff <- xlagt-xlagtm1
  xdiff
}

lag.pserie <- function(x,k=1,...){
  
  N <- length(x)
  data.name <- attr(x,"data")
  classx <- class(x)
  if (is.factor(x)){
    levelsx <- levels(x)
    nlevelsx <- length(levelsx)
  }
  sc <- sapply(sys.calls(),function(x) as.character(x[[1]]))
  leframe <- which(!is.na(match(sc,c("pgmm","pggls","plm","pvcm"))))
  if (length(leframe>1)) leframe <- leframe[length(leframe)]
  leframe <- ifelse(length(leframe)==1,leframe,1)

  # il faut evaluer  dans le frame en position 1,
  # celui qui correspond à la fonction pgmm
  # print(ls(sys.frame(which=1)))
  data <- get(data.name,sys.frame(which=leframe))
  id.name <- attr(data,"indexes")$id
  time.name <- attr(data,"indexes")$time
  id <- data[[id.name]]
  time <- data[[time.name]]
  id <- as.numeric(id)
  res <- c(rep(NA,k),x[1:(N-k)])
  lagid <- id-c(rep(NA,k),id[1:(N-k)])
  res[lagid!=0]=NA
  if (is.factor(x)){
    res <- factor(res,levels=1:nlevelsx,labels=levelsx)
  }
  attr(res,"data") <- data.name
  class(res) <- classx
  res
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
    result <- result[,apply(result,2,myvar) > 1E-12,drop = FALSE]
    if (has.intercept){
      result <- cbind(1,result)
      colnames(result)[1] <- "(intercept)"
    }

  }
  attr(result,"na.action") <- NULL
  result
}


myvar <- function(x){
  if(any(is.na(x))) x <- x[!is.na(x)]
  n <- length(x)
  z <- switch(as.character(n),
              "0"=NA,
              "1"=0,
              var(x))
  z
}

mymean <- function(x){
  if(any(is.na(x))) x <- x[!is.na(x)]
  n <- length(x)
  z <- switch(as.character(n),
              "0"=NA,
              mean(x))
  z
}

mysum <- function(x){
  if(any(is.na(x))) x <- x[!is.na(x)]
  n <- length(x)
  z <- switch(as.character(n),
              "0"=NA,
              sum(x))
  z
}
