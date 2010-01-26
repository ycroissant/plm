dynformula <- function(formula, lag.form = NULL, diff.form = NULL, log.form = NULL){

  # for backward compatibility, accept a list argument and coerce it
  # to a vector
  if (!is.null(diff.form) && !is.list(diff.form)) diff.form <- as.list(diff.form)
  if (!is.null(log.form) && !is.list(log.form)) log.form <- as.list(log.form)

  
  # exo / endog are the names of the variable
  # has.int has.resp  TRUE if the formula has an intercept and a response
  # K is the number of exogenous variables
  exo <- attr(terms(formula),"term.labels")
  has.int <- attr(terms(formula),"intercept") == 1
  if(length(formula)==3){
    endog <- deparse(formula[[2]])
    has.resp <- TRUE
  }
  else{
    endog <- NULL
    has.resp <- FALSE
  }
  K <- length(exo)

  # use the create.list function to create the lists with the relevant
  # default values
  lag.form <- create.list(lag.form, K, has.int, has.resp, endog, exo, 0)
  diff.form <- unlist(create.list(diff.form, K, has.int, has.resp, endog, exo, FALSE))
  log.form <- unlist(create.list(log.form, K, has.int, has.resp, endog, exo, FALSE))

  structure(formula, class = c("dynformula","formula"), lag = lag.form,
            diff = diff.form, log = log.form, var = c(endog,exo))
}


formula.dynformula <- function(x, ...){
  log.form <- attr(x, "log")
  lag.form <- attr(x, "lag")
  diff.form <- attr(x, "diff")
  has.resp <- length(x) == 3
  exo <- attr(x, "var")
  if (has.resp){
    endog <- exo[1]
    exo <- exo[-1]
  }
  has.int <- attr(terms(x), "intercept") == 1
  chexo <- c()
  if (has.resp){
    if (log.form[1]) endog <- paste("log(",endog,")",sep="")
    if (diff.form[1]) endog <- paste("diff(",endog,")",sep="")
    if (length(lag.form[[1]]) == 1 && lag.form[[1]]!=0) lag.form[[1]] <- c(1,lag.form[[1]])
    if (!(length(lag.form[[1]]) == 1 && lag.form[[1]]==0))
      chexo <- c(chexo,write.lags(endog,lag.form[[1]],diff.form[1]))
  }
  for (i in exo){
    lag.formi <- lag.form[[i]]
    diff.formi <- diff.form[i]
    if (log.form[[i]]) i <- paste("log(",i,")",sep="")
    chexo <- c(chexo,write.lags(i,lag.formi,diff.formi))
  }
  chexo <- paste(chexo,collapse="+")
  if (has.resp){
    formod <- as.formula(paste(endog,"~",chexo,sep=""))
  }
  else{
    formod <- as.formula(paste("~",chexo,sep=""))
  }
  if (!has.int) formod <- update(formod,.~.-1)
  formod
}

create.list <- function(alist, K, has.int, has.resp, endog, exo, default){
  # if alist is NULL, create a list of 0
  if (is.null(alist)) alist <- rep(list(default), K+has.resp)
  # if alist is note a list, coerce it
  if (!is.list(alist)) alist <- list(alist)

  if (!is.null(names(alist))){
  # case where (at least) some elements are named
    nam <- names(alist) # vector of names of elements
    oalist <- alist  # copy of the alist provided
    notnullname <- nam[nam != ""]
    if (any (nam == "")){
    # case where one element is unnamed, and therefore is the default
      unnamed <- which(nam == "")
      if (length(unnamed) > 1) stop("Only one unnamed element is adminited\n")
      default <- alist[[unnamed]]
    }
    else{
    # case where there are no unnamed elements, the default is 0
      default <- default
    }
    alist <- rep(list(default), K+has.resp)
    names(alist) <- c(endog, exo)
    alist[notnullname] <- oalist[notnullname]
  }
  else{
  # case where there are no names, in this case the relevant length is
  # whether 1 or K+1
    if (length(alist) == 1) alist <- rep(alist, c(K+has.resp))
    else if (!length(alist) %in% c(K+has.resp)) stop("irrelevant length for alist\n")
  }
  names(alist) <- c(endog,exo)
  alist
}

write.lags <- function(name,lags,diff){
  lags <- switch(length(lags),
                 "1"=c(0,lags),
                 "2"=sort(lags),
                 stop("lags should be of length 1 or 2\n")
                 )
  lag.string <- ifelse(diff,"diff","lag")
  chlag <- c()
  if (lags[2]!=0){
    lags <- lags[1]:lags[2]
    for (i in lags){
      if (i==0){
        if (diff) chlag <- c(chlag,paste("diff(",name,")")) else chlag <- c(chlag,name)
      }
      else{
        ichar <- paste(i)
        chlag <- c(chlag,paste(lag.string,"(",name,",",i,")",sep=""))
      }
    }
    ret <- paste(chlag,collapse="+")
  }
  else{
    if (diff) chlag <- paste("diff(",name,")") else chlag <- name
    ret <- chlag
  }
  ret
}   


print.dynformula <- function(x,...){
  print(formula(x), ...)
}
