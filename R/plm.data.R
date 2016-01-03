
### convert data to plm format
### Author:
### Amendments by Ott Toomet

plm.data <- function(x, indexes = NULL){
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
      cat(paste("series",paste(na.serie,collapse=", "),"are constants and have been removed\n"))
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


data2plm.data <- function(data,indexes=NULL){
  data <- plm.data(data,indexes)
  id.name <- names(data)[1]
  time.name <- names(data)[2]
  list(data=data,id.name=id.name,time.name=time.name)
}
