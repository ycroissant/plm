myvar <- function(x){
  x.na <- is.na(x)
  if(anyNA(x.na)) x <- x[!x.na]
  n <- length(x)
  
  z <- switch(as.character(n),
              "0" = NA,
              "1" = 0,
              ifelse(!(is.factor(x) || is.character(x)), var(x), !all(duplicated(x)[-1L]))) # [var() on factors is deprecated as of R 3.2.3]
  z
}

pvar <- function(x, ...){
  UseMethod("pvar")
}

pvar.default <- function(x, id, time, ...){
  name.var <- names(x)
  len <- length(x)
  time.variation <- rep(TRUE, len)
  id.variation   <- rep(TRUE, len)
  time.variation_anyNA <- rep(FALSE, len)
  id.variation_anyNA   <- rep(FALSE, len)
  lid <- split(x, id)
  ltime <- split(x, time)
  if (is.list(x)){
    if (len == 1){
      # time variation
      temp_time.var          <- sapply(lid, function(x) sapply(x, myvar))
      temp_time.var_sumNoVar <- sum(temp_time.var == 0, na.rm = TRUE) # number of non-varying id-time comb. (without all NA groups)
      temp_time.var_sumNA    <- sum(is.na(temp_time.var))             # number of all-NA groups
      temp_time.varResult    <- temp_time.var_sumNoVar + temp_time.var_sumNA
      time.variation         <- temp_time.varResult != length(lid) # no variation if (no. non-varying + no. all-NA) == number of groups 
      time.variation_anyNA   <- temp_time.var_sumNA > 0            # indicates if at least one id-time comb is all NA
      
      # id variation
      temp_id.var          <- sapply(ltime, function(x) sapply(x, myvar))
      temp_id.var_sumNoVar <- sum(temp_id.var == 0, na.rm = TRUE)
      temp_id.var_sumNA    <- sum(is.na(temp_id.var))
      temp_id.varResult    <- temp_id.var_sumNoVar + temp_id.var_sumNA
      id.variation         <- temp_id.varResult != length(ltime)
      id.variation_anyNA   <- temp_id.var_sumNA > 0
    }
    else{
     # time variation
      temp_time.var          <- sapply(lid, function(x) sapply(x, myvar))
      temp_time.var_sumNoVar <- apply(temp_time.var == 0, 1, sum, na.rm = TRUE)
      temp_time.var_sumNA    <- apply(is.na(temp_time.var), 1, sum)
      temp_time.varResult    <- temp_time.var_sumNoVar + temp_time.var_sumNA
      time.variation         <- temp_time.varResult != length(lid)
      time.variation_anyNA   <- temp_time.var_sumNA > 0

     # id variation
      temp_id.var          <- sapply(ltime, function(x) sapply(x, myvar))
      temp_id.var_sumNoVar <- apply(temp_id.var == 0, 1, sum, na.rm = TRUE)
      temp_id.var_sumNA    <- apply(is.na(temp_id.var), 1, sum)
      temp_id.varResult    <- temp_id.var_sumNoVar + temp_id.var_sumNA
      id.variation         <- temp_id.varResult != length(ltime)
      id.variation_anyNA   <- temp_id.var_sumNA > 0
    }
  }
  else{ # not a list (not a data.frame, pdata.frame) - try our best for that unknown data structure
      # time variation
      temp_time.var          <- sapply(lid, function(x) sapply(x, myvar))
      temp_time.var_sumNoVar <- sum(temp_time.var == 0, na.rm = TRUE)
      temp_time.var_sumNA    <- sum(is.na(temp_time.var))
      temp_time.varResult    <- temp_time.var_sumNoVar + temp_time.var_sumNA
      time.variation         <- temp_time.varResult != length(lid)
      time.variation_anyNA   <- temp_time.var_sumNA > 0
      
      # id variation
      temp_id.var          <- sapply(ltime, function(x) sapply(x, myvar))
      temp_id.var_sumNoVar <- sum(temp_id.var == 0, na.rm = TRUE)
      temp_id.var_sumNA    <- sum(is.na(temp_id.var))
      temp_id.varResult    <- temp_id.var_sumNoVar + temp_id.var_sumNA
      id.variation         <- temp_id.varResult != length(ltime)
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

pvar.pseries <- function(x, ...){
  # use drop.index = TRUE so that the index columns' 
  # variations are not evaluated:
  pdfx <- pseries2pdataframe(x, drop.index = TRUE)
  pvar.pdata.frame(pdfx)
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
# Note: some parts of this code are copied verbatim to is.pbalanced() 
pdim <- function(x, ...){
  UseMethod("pdim")
}

pdim.default <- function(x, y, ...){
  if (length(x) != length(y)) stop("The length of the two vectors differs\n")
  x <- x[drop = TRUE] # drop unused factor levels so that table() 
  y <- y[drop = TRUE] # gives only needed combinations
  z <- table(x,y)
  Ti <- rowSums(z) # faster than: apply(z, 1, sum)
  nt <- colSums(z) #              apply(z, 2, sum)
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
  if (any(as.vector(z) > 1)) stop("duplicate couples (id-time)\n")
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

pdim.pseries <- function(x,...) {
  index <- attr(x, "index")
  pdim(index[[1]], index[[2]])
}

pdim.panelmodel <- function(x, ...){
  x <- model.frame(x)
  pdim(x)
}

pdim.pgmm <- function(x, ...){
## pgmm is also class panelmodel, but take advantage of the pdim attribute in it
  attr(x, "pdim")
}

print.pdim <- function(x, ...){
  if (x$balanced){
    cat("Balanced Panel: ")
    cat(paste("n = ",x$nT$n,", ",sep=""))
    cat(paste("T = ",x$nT$T,", ",sep=""))
    cat(paste("N = ",x$nT$N,"\n",sep=""))
  }
  else{
    cat("Unbalanced Panel: ")
    cat(paste("n = ",x$nT$n,", ",sep=""))
    cat(paste("T = ",min(x$Tint$Ti),"-",max(x$Tint$Ti),", ",sep=""))
    cat(paste("N = ",x$nT$N,"\n",sep=""))
  }
}


