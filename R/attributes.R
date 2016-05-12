myvar <- function(x){
  if(any(is.na(x))) x <- x[!is.na(x)]
  n <- length(x)
  
  z <- switch(as.character(n),
              "0" = NA,
              "1" = 0,
              ifelse(!is.factor(x), var(x), !all(duplicated(x)[-1L]))) # (var on factors is deprecated as of R 3.2.3)
  z
}

pvar <- function(x, ...){
  UseMethod("pvar")
}

pvar.default <- function(x, id, time, ...){
  name.var <- names(x)
  time.variation <- rep(TRUE, length(x))
  id.variation <- rep(TRUE, length(x))
  time.variation_anyNA <- rep(FALSE, length(x))
  id.variation_anyNA <- rep(FALSE, length(x))
  K <- length(x)
  lid <- split(x, id)
  ltime <- split(x, time)
  if (is.list(x)){
    if (K == 1){
      # time variation
      temp_time.var          <- sapply(lid,function(x) sapply(x,myvar))
      temp_time.var_sumNoVar <- sum(temp_time.var==0, na.rm=T) # number of non-varying id-time comb. (without all NA groups)
      temp_time.var_sumNA    <- sum(is.na(temp_time.var))      # number of all-NA groups
      temp_time.varResult    <- temp_time.var_sumNoVar + temp_time.var_sumNA
      time.variation         <- temp_time.varResult!=length(lid) # no variation if (no. non-varying + no. all-NA) == number of groups 
      time.variation_anyNA   <- temp_time.var_sumNA > 0          # indicates if at least one id-time comb is all NA
      
      # id variation
      temp_id.var          <- sapply(ltime,function(x) sapply(x,myvar))
      temp_id.var_sumNoVar <- sum(temp_id.var==0, na.rm=T)
      temp_id.var_sumNA    <- sum(is.na(temp_id.var))
      temp_id.varResult    <- temp_id.var_sumNoVar + temp_id.var_sumNA
      id.variation         <- temp_id.varResult!=length(ltime)
      id.variation_anyNA   <- temp_id.var_sumNA > 0
    }
    else{
     # time variation
      temp_time.var          <- sapply(lid,function(x) sapply(x,myvar))
      temp_time.var_sumNoVar <- apply(temp_time.var==0 , 1, sum, na.rm=T)
      temp_time.var_sumNA    <- apply(is.na(temp_time.var), 1, sum)
      temp_time.varResult    <- temp_time.var_sumNoVar + temp_time.var_sumNA
      time.variation         <- temp_time.varResult!=length(lid)
      time.variation_anyNA   <- temp_time.var_sumNA > 0

     # id variation
      temp_id.var          <- sapply(ltime,function(x) sapply(x,myvar))
      temp_id.var_sumNoVar <- apply(temp_id.var==0 , 1, sum, na.rm=T)
      temp_id.var_sumNA    <- apply(is.na(temp_id.var), 1, sum)
      temp_id.varResult    <- temp_id.var_sumNoVar + temp_id.var_sumNA
      id.variation         <- temp_id.varResult!=length(ltime)
      id.variation_anyNA   <- temp_id.var_sumNA > 0
    }
  }
  else{ # not a list (not a data.frame, pdata.frame) - try our best for that unknown data structure
      # time variation
      temp_time.var          <- sapply(lid,function(x) sapply(x,myvar))
      temp_time.var_sumNoVar <- sum(temp_time.var==0, na.rm=T)
      temp_time.var_sumNA    <- sum(is.na(temp_time.var))
      temp_time.varResult    <- temp_time.var_sumNoVar + temp_time.var_sumNA
      time.variation         <- temp_time.varResult!=length(lid)
      time.variation_anyNA   <- temp_time.var_sumNA > 0
      
      # id variation
      temp_id.var          <- sapply(ltime,function(x) sapply(x,myvar))
      temp_id.var_sumNoVar <- sum(temp_id.var==0, na.rm=T)
      temp_id.var_sumNA    <- sum(is.na(temp_id.var))
      temp_id.varResult    <- temp_id.var_sumNoVar + temp_id.var_sumNA
      id.variation         <- temp_id.varResult!=length(ltime)
      id.variation_anyNA   <- temp_id.var_sumNA > 0
  }

  # make 'pvar' object
  names(id.variation) <- names(time.variation) <- names(id.variation_anyNA) <- names(time.variation_anyNA) <- name.var
  dim.var <- list(id.variation         = id.variation,
                  time.variation       = time.variation,
                  id.variation_anyNA   = id.variation_anyNA,
                  time.variation_anyNA = time.variation_anyNA)
  class(dim.var) <- "pvar"
  return(dim.var)
}

pvar.matrix <- function(x, index = NULL, ...){
  x <- pdata.frame(as.data.frame(x), index, ...)
  pvar(x)
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
    if (length(var)!=0) cat(paste("no time variation:      ", paste(var,collapse=" "),"\n"))
  }
  if(any(!x$id.variation)){
    var <- varnames[x$id.variation==FALSE]
#    if (!is.null(y)) var <- var[-which(var==y$time)]
    if(length(var)!=0) cat(paste("no individual variation:", paste(var,collapse=" "),"\n"))
  }
  
  # any individual-time combinations all NA?
  if(any(x$time.variation_anyNA)){
    var_anyNA <- varnames[x$time.variation_anyNA]
    if (length(var_anyNA)!=0) cat(paste("all NA in time dimension for at least one individual: ", paste(var_anyNA,collapse=" "),"\n"))
  }
    if(any(x$id.variation_anyNA)){
    var_anyNA <- varnames[x$id.variation_anyNA]
    if (length(var_anyNA)!=0) cat(paste("all NA in ind. dimension for at least one time period:", paste(var_anyNA,collapse=" "),"\n"))
  }
}

#### pdim ####

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

# pos.index:
# not exported, helper function
# determines positions of the index variables in a pdata.frame
# returns named numeric of length 2 with column numbers of the index variables (1: individual index, 2: time index) and 
# names are the names of the index variables
#
# returns c(NA, NA) if the index variables are not a column in the pdata.frame (e.g. for pdata.frames created with drop.index = TRUE).
# Cannot detect index variables if their columns names were changed after creation of the pdata.frame
pos.index <- function(x, ...) {
  index <- attr(x, "index")
  index_names <- names(index)
  index_pos <- match(index_names, names(x))
  names(index_pos) <- index_names
  return(index_pos)
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

