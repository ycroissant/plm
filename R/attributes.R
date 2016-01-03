myvar <- function(x){
  if(any(is.na(x))) x <- x[!is.na(x)]
  n <- length(x)
  z <- switch(as.character(n),
              "0" = NA,
              "1" = 0,
              var(x))
  z
}


pvar <- function(x, ...){
  UseMethod("pvar")
}

pvar.default <- function(x, id, time, ...){
  name.var <- names(x)
  time.variation <- rep(TRUE, length(x))
  id.variation <- rep(TRUE, length(x))
  K <- length(x)
  lid <- split(x, id)
  ltime <- split(x, time)
  if (is.list(x)){
    if (K == 1){
      time.variation <- sum(sapply(lid,function(x) sapply(x,myvar)==0))!=length(lid)
      id.variation <- sum(sapply(ltime,function(x) sapply(x,myvar)==0))!=length(ltime)
    }
    else{
      time.variation <- apply(sapply(lid,function(x) sapply(x,myvar)==0),1,sum)!=length(lid)
      id.variation <- apply(sapply(ltime,function(x) sapply(x,myvar)==0),1,sum)!=length(ltime)
      names(id.variation) <- names(time.variation) <- name.var
    }
    dim.var <- list(id.variation=id.variation,time.variation=time.variation)
    class(dim.var) <- "pvar"
  }
  else{
    time.variation <- sum(sapply(lid,function(x) myvar(x)==0))!=length(lid)
    id.variation <- sum(sapply(ltime,function(x) myvar(x)==0))!=length(ltime)
    dim.var <- c(time.variation,id.variation)
  }
  dim.var
}

pvar.matrix <- function(x, id, time, ...){
  x <- as.data.frame(x)
  pvar.default(x,id,time)
}

pvar.data.frame <- function(x, index = NULL, ...){
  x <- pdata.frame(x, index, ...)
  pvar(x)
}

pvar.pdata.frame <- function(x, ...){
  index <- attr(x, "index")
  id <- index[[1]]
  time <- index[[2]]
  pvar.default(x, id, time)
}

print.pvar <- function(x, ...){
  varnames <- names(x$time.variation)
  if(any(!x$time.variation)){
    var <- varnames[x$time.variation==FALSE]
#    if (!is.null(y)) var <- var[-which(var==y$id)]
    if (length(var)!=0) cat(paste("no time variation   : ",paste(var,collapse=" "),"\n"))
  }
  if(any(!x$id.variation)){
    var <- varnames[x$id.variation==FALSE]
#    if (!is.null(y)) var <- var[-which(var==y$time)]
    if(length(var)!=0) cat(paste("no individual variation : ",paste(var,collapse=" "),"\n"))
  }
}

pdim <- function(x, ...){
  UseMethod("pdim")
}

pdim.default <- function(x, y, ...){
  if (length(x) != length(y)) stop("The length of the two vectors differs\n")
  x <- x[drop = TRUE]
  y <- y[drop = TRUE]
  z <- table(x,y)
  Ti <- apply(z,1,sum)
  nt <- apply(z,2,sum)
  n <- nrow(z)
  T <- ncol(z)
  N <- length(x)
  nT <- list(n = n, T = T, N = N)
  id.names <- rownames(z)
  time.names <- colnames(z)
  panel.names <- list(id.names = id.names, time.names = time.names)
  if (any(as.vector(z)==0)){
    balanced <- FALSE
  }
  else balanced <- TRUE
  if (any(as.vector(z) > 1)) stop("duplicate couples (time-id)\n")
  Tint <- list(Ti = Ti, nt = nt)
  z <- list(nT = nT, Tint = Tint, balanced = balanced, panel.names = panel.names)
  class(z) <- "pdim"
  z
}  

pdim.data.frame <- function(x, index = NULL, ...){
  x <- pdata.frame(x, index)
  index <- attr(x, "index")
  id <- index[[1]]
  time <- index[[2]]
  pdim(id,time)
}

pdim.pdata.frame <- function(x,...){
  index <- attr(x, "index")
  pdim(index[[1]],index[[2]])
}

pdim.panelmodel <- function(x, ...){
  x <- model.frame(x)
  pdim(x)
}

pdim.pgmm <- function(x, ...){
  attr(x, "pdim")
}

print.pdim <- function(x, ...){
  if (x$balanced){
    cat("Balanced Panel: ")
    cat(paste("n=",x$nT$n,", ",sep=""))
    cat(paste("T=",x$nT$T,", ",sep=""))
    cat(paste("N=",x$nT$N,"\n",sep=""))
  }
  else{
    cat("Unbalanced Panel: ")
    cat(paste("n=",x$nT$n,", ",sep=""))
    cat(paste("T=",min(x$Tint$Ti),"-",max(x$Tint$Ti),", ",sep=""))
    cat(paste("N=",x$nT$N,"\n",sep=""))
  }
}

# NB: Are functions indexes and print.indexes still needed?
indexes <- function(x){
  if (!inherits(x, "pdata.frame")){
    stop("indexes function only for pdata.frame\n")
  }
  attr(x,"index")
}

print.indexes <- function(x, ...){
  cat(paste("Index: (individual=",x$id,") and (time=",x$time,")\n",sep=""))
}

has.intercept.panelmodel <- function(object, ...){
  object <- attr(model.frame(object),"formula")
  has.intercept(object)
}
