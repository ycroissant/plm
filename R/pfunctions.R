###################################################
### chunk number 1: pdata.frame
###################################################
pdata.frame <- function(x, index = NULL, drop.index = FALSE, row.names = TRUE){
  if (inherits(x, "pdata.frame")) stop("already a pdata.frame")

  # coerce character vectors to factors
  x.char <- names(x)[sapply(x, is.character)]
  for (i in x.char){
    x[[i]] <- factor(x[[i]])
  }
  
  # replace Inf by NA
  for (i in names(x)) x[[i]][!is.finite(x[[i]])] <- NA
  # check and remove complete NA series
  na.check <- sapply(x,function(x) sum(!is.na(x))==0)
  na.serie <- names(x)[na.check]
  if (length(na.serie) > 0){
    if (length(na.serie) == 1)
      cat(paste("series ", na.serie, " is NA and has been removed\n", sep = ""))
    else
      cat(paste("series ", paste(na.serie, collapse = ","), " are NA and have been removed\n", sep = ""))
  }
  x <- x[, ! na.check]
  
  # check and remove cst series
  cst.check <- sapply(x, function(x) var(as.numeric(x), na.rm = TRUE)==0)
  # following line : bug fixed thank's to Marciej Szelfer 
  cst.check <- cst.check | is.na(cst.check)
  cst.serie <- names(x)[cst.check]
  if (length(cst.serie) > 0){
    if (length(cst.serie) == 1){
      cat(paste("series ", cst.serie, " is constant and has been removed\n", sep = ""))
    }
    else{
      cat(paste("series ", paste(cst.serie, collapse = ","), " are constants and have been removed\n", sep = ""))
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
      # trier par individu dans le cas ou id est un facteur
#      x <- x[order(id), ]
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
  class(index) <- c("pindex", "data.frame")
  attr(x, "index") <- index
  class(x) <- c("pdata.frame", "data.frame")
  x
}

"[.pdata.frame" <- function(x, i, j, drop = TRUE){
  old.pdata.frame <- !inherits(x, "data.frame")
  if (!old.pdata.frame){
    # this part for backward compatibility (required by meboot)
    index <- "[.data.frame"(attr(x, "index"), i, )
    #remove empty levels if any
    index <- data.frame(lapply(index, function(x) x[drop = TRUE]))
  }
  mydata <- `[.data.frame`(x, i, j, drop = drop)
  if (is.null(dim(mydata))){
    structure(mydata,
              index = index,
              class = c("pseries", class(mydata))
              )
  }
  else{
    structure(mydata,
              index = index,
              class = c("pdata.frame", "data.frame"))
  }
}

print.pdata.frame <- function(x, ...){
  attr(x, "index") <- NULL
  class(x) <- "data.frame"
  print(x, ...)
}

"$<-.pdata.frame" <- function(x, name, value){
  if (class(value)[1] == "pseries"){
    if (length(class(value)) == 1) value <- unclass(value)
    else class(value) <- class(value)[-1]
    attr(value, "index") <- NULL
  }
  "$<-.data.frame"(x, name, value)
}


"$.pdata.frame" <- function(x,y){
  "[["(x, paste(as.name(y)))
}


###################################################
### chunk number 3: extracting
###################################################
"[[.pdata.frame" <- function(x, y){
  index <- attr(x, "index")
  attr(x, "index") <- NULL
  class(x) <- "data.frame"
  result <- x[[y]]
  if (!is.null(result)){
    result <- structure(result,
                        class = c("pseries", class(x[[y]])),
                        index = index,
                        names = row.names(x)
                        )
  }
  result
}  

"$.pdata.frame" <- function(x,y){
  "[["(x, paste(as.name(y)))
}

print.pseries <- function(x, ...){
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
as.matrix.pseries <- function(x, idbyrow = TRUE, ...){
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
lag.pseries <- function(x, k = 1, ...){
  nx <- names(x)
  index <- attr(x, "index")
  id <- index[[1]]
  time <- index[[2]]
  isNAtime <- c(rep(1,k), diff(as.numeric(time), lag = k)) != k
  isNAid <- c(rep(1,k), diff(as.numeric(id), lag = k)) != 0
  isNA <- as.logical(isNAtime + isNAid)
  if (is.factor(x)) levs <- levels(x)
  result <- c(rep(NA, k), x[1:(length(x)-k)])
  result[isNA] <- NA
  if (is.factor(x)) result <- factor(result, labels = levs)
  structure(result,
            names = nx,
            class = class(x),
            index = index)
}

diff.pseries <- function(x, lag = 1, ...){
  if (!is.numeric(x)) stop("diff is only relevant for numeric series")
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

Tapply.pseries <- function(x, effect = c("individual", "time"), func, ...){
  effect <- match.arg(effect)
  index <- attr(x, "index")
  effect <- switch(effect,
                 "individual"= index[[1]],
                 "time"= index[[2]]
                 )
  z <- Tapply.default(x, effect, func, ...)
  attr(z, "index") <- index
  class(z) <- c("pseries", class(z))
  z
}

Tapply.matrix <- function(x, effect, func, ...){
   ## Note: this function is not robust wrt NA in effect
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

Between.pseries <- function(x, effect = c("individual", "time"), ...){
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

between.pseries <- function(x, effect = c("individual", "time"), ...){
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

Within <- function(x,...){
  UseMethod("Within")
}

Within.default <- function(x, effect, ...){
  if (!is.numeric(x)) stop("the within function only applies to numeric vectors")
  x - Between(x, effect, ...)
}

Within.pseries <- function(x, effect = c("individual", "time"), ...){
  effect <- match.arg(effect)
  Within.default(x, effect, ...)
}

Within.matrix <- function(x, effect, rm.null = TRUE,...){
  result <- Within.default(x, effect, ...)
  othervar <- apply(result, 2, function(x) sum(abs(x), na.rm = TRUE)) > 1E-12
  if (rm.null){
    result <- result[, othervar, drop = FALSE]
  }
  else{
    result <- result[, drop = FALSE]
  }
  attr(result, "constant") <- colnames(x)[!othervar]
  result
}



###################################################
### chunk number 12: methods for pseries
###################################################
sumsq <- function(x, ...){
  xb <- mean(x, na.rm = TRUE)
  sum((na.omit(x)-xb)^2)
}

summary.pseries <- function(object, ...){
  id <- attr(object, "index")[[1]]
  time <- attr(object, "index")[[2]]
  xm <- mean(object, na.rm = TRUE)
  Bid <-  Between(object, na.rm = TRUE)
  Btime <-  Between(object, effect = "time", na.rm = TRUE)
  structure( c(total = sumsq(object), between_id= sumsq(Bid), between_time = sumsq(Btime)), 
            class = c("summary.pseries", "numeric")
            )
}

plot.summary.pseries <- function(x, ...){
  x <- as.numeric(x)
  share <- x[-1]/x[1]
  names(share) <- c("id", "time")
  barplot(share, ...)
}

print.summary.pseries <- function(x, ...){
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
                class(z) <- c("pseries", class(z))
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


lag.pseries <- function(x, k = 1, ...){
  nx <- names(x)
  index <- attr(x, "index")
  id <- index[[1]]
  time <- index[[2]]
  
  alag <- function(x, ak){
    if (ak != 0){
      isNAtime <- c(rep(1,ak), diff(as.numeric(time), lag = ak)) != ak
      isNAid <- c(rep(1,ak), diff(as.numeric(id), lag = ak)) != 0
      isNA <- as.logical(isNAtime + isNAid)
      if (is.factor(x)) levs <- levels(x)
      result <- c(rep(NA, ak), x[1:(length(x)-ak)])
      result[isNA] <- NA
      if (is.factor(x)) result <- factor(result, labels = levs)
      structure(result,
                names = nx,
                class = class(x),
                index = index)
    }
    else x
  }
  if(length(k) > 1){
    rval <- sapply(k, function(i) alag(x, i))
    colnames(rval) <- k
  }
  else{
    rval <- alag(x, k)
  }
  return(rval)
}
  

### Index methods


index.pindex <- function(x, which = NULL, ...){
  if (is.null(which)) which <- names(x)
  if (! (length(which) %in% c(1, 2))) stop("which should be of length 1 or 2")
  if (is.numeric(which)){
    if (! all(which %in% c(1, 2))) stop("if integers, which should contain 1 and/or 2")
    which <- names(x)[which]
  }
  if (length(which) == 2){
    if (which[1] == "id") which[1] = names(x)[1]
    if (which[2] == "time") which[2] = names(x)[2]
    for (i in 1:2){
      if (! (which[i] %in% names(x))) stop(paste("variable", which[i], "does not exist"))
    }
    result <- x[, which]
  }
  else{
    if (which == "id") which = names(x)[1]
    if (which == "time") which = names(x)[2]
    if (! (which %in% names(x))) stop(paste("variable", which, "does not exist"))
    result <- x[, which]
  }
  result
}
      
index.pdata.frame <- function(x, which = NULL, ...){
  anindex <- attr(x, "index")
  index(x = anindex, which = which)
}

index.pseries <- function(x, which = NULL, ...){
  anindex <- attr(x, "index")
  index(x = anindex, which = which)
}
  
index.panelmodel <- function(x, which = NULL, ...){
  anindex <- attr(x$model, "index")
  index(x = anindex, which = which)
}



pseries2pdata <- function(x) {
  ## transforms a pseries in a dataframe with the indices as regular columns
  indices <- attr(x, "index")
  vx <- as.numeric(x)
  px <- cbind(indices, vx)
  dimnames(px)[[2]] <- c("ind","tind",deparse(substitute(x)))
  return(pdata.frame(px, index=c("ind","tind")))
}

pmerge <- function(x, y, ...) {
  ## transf. if pseries
  if("pseries" %in% class(x)) x <- pseries2pdata(x)
  if("pseries" %in% class(y)) y <- pseries2pdata(y)

  z <- merge(data.frame(x), data.frame(y), by.x=dimnames(x)[[2]][1:2],
             by.y=dimnames(y)[[2]][1:2], ...)

  return(z)
}


## plots a panel series by time index
##
## can supply any panel function, e.g. a loess smoother
## > mypanel<-function(x,...) {
## + panel.xyplot(x,...)
## + panel.loess(x, col="red", ...)}
## >
## > plot(pres(mod), panel=mypanel)

plot.pseries <- function(x, plot=c("lattice", "superposed"),
                         scale=FALSE, transparency=TRUE,
                         col="blue", lwd=1, ...) {

    if(scale) {scalefun <- function(x) scale(x)
               } else {
                   scalefun <- function(x) return(x)}

    nx <- as.numeric(x)
    ind <- attr(x, "index")[[1]]
    tind <- attr(x, "index")[[2]] # possibly as.numeric():
                                  # activates autom. tick
                                  # but loses time labels

    xdata <- data.frame(nx=nx, ind=ind, tind=tind)

    switch(match.arg(plot),
           lattice={

               ##require(lattice) # make a ggplot2 version
               xyplot(nx~tind|ind, data=xdata, type="l", col=col, ...)

           }, superposed={

                   ylim <- c(min(tapply(scalefun(nx), ind, min, na.rm=TRUE)),
                             max(tapply(scalefun(nx), ind, max, na.rm=TRUE)))
                   unind <- unique(ind)
                   nx1 <- nx[ind==unind[1]]
                   tind1 <- as.numeric(tind[ind==unind[1]])

                   ## plot empty plot to provide frame
                   plot(NA, xlim=c(min(as.numeric(tind)),
                            max(as.numeric(tind))),
                        ylim=ylim, xlab="", ylab="", xaxt="n", ...)
                        #x=tind1, y=scalefun(nx1), ylim=ylim, ...)

                   axis(1, at=as.numeric(unique(tind)),
                        labels=unique(tind))

                   ## determine lwd and transparency level as a function
                   ## of n
                   if(transparency) {
                       alpha <- 5/length(unind)
                       col <- heat.colors(1, alpha=alpha)
                       lwd <- length(unind)/10
                   }

                   ## plot lines (notice: tind. are factors, so they
                   ## retain the correct labels which would be lost if
                   ## using as.numeric
                   for(i in 1:length(unind)) {
                       nxi <- nx[ind==unind[i]]
                       tindi <- tind[ind==unind[i]]
                       lines(x=tindi, y=scalefun(nxi),
                             col=col, lwd=lwd, ...)
                       }

               })

}
