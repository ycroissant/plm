pgmm <- function(formula, data, effect = c("individual", "twoways", "none"),
                 model = c("onestep", "twosteps"),
                 instruments = NULL, gmm.inst, lag.gmm,
                 transformation = c("d", "ld"), fsm = NULL, ...){
  
  effect <- match.arg(effect)
  model.name <- match.arg(model)
  transformation <- match.arg(transformation)
  
  cl <- match.call()
  
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "na.action", "index"),names(mf),0)
  mf <- mf[c(1,m)]
  mf$drop.unused.levels <- TRUE
  mf[[1]] <- as.name("plm")
  mf$model <- NA

  mf$formula <- formula(formula)
  mf$na.action <- "na.pass"

  if(is.null(fsm)){
    fsm <- switch(transformation,
                  "d"="G",
                  "ld"="full"
                  )
  }
  
  # for now, let suppose that gmm.inst and gmm.lag are not lists (why
  # should they anyway ?)
  var.gmm <- attr(terms(gmm.inst), "term.labels")
  var.tot <- attr(formula, "var")
  J <- length(var.gmm)

  # if instruments is null, use variables as their own instruments
  if (is.null(instruments)){
    var.inst <- var.tot[! var.tot %in% var.gmm]

    if (length(var.inst) > 0){
      lag.inst <- attr(formula,"lag")[var.inst]
      log.inst <- attr(formula,"log")[var.inst]
      diff.inst <- attr(formula,"diff")[var.inst]
      form.inst <- as.formula(paste("~", paste(var.inst,collapse="+"), sep=""))
      instruments <- formula(dynformula(form.inst, lag.inst, diff.inst, log.inst))
    }
  }
  
  if (!is.list(lag.gmm)) lag.gmm <- rep(list(lag.gmm),J)

  # the number of time series lost depends on the lags of the gmm
  # instruments and on the lags of the model
  max.lag.gmm <- max(sapply(lag.gmm,function(x) x[1]))
  max.lag.model <- max(sapply(attr(formula,"lag"),max)) + 1
  time.lost <- max(max.lag.model,max.lag.gmm)

  # if SYSGMM is required, the gmm.instruments are just the variables
  # in level with one lag
  if (transformation=="ld"){
    lag.gmm.level <- rep(list(c(1,1)),J)
    gmm.inst.level <- dynformula(gmm.inst, diff = T)
  }

  # we then use mf (which is a call with plm as function and NA as
  # model, ie returns a model.frame. We provide any formula we need
  # (for the model, the gmm.inst, the instruments so that the relevant
  # data.frame are created.
  mf$formula <- formula(formula) ; data.formula <- eval(mf,parent.frame())
  mf$formula <- formula(gmm.inst); data.gmm.inst <- eval(mf,parent.frame())
  if (!is.null(instruments)){
    mf$formula <- instruments ;  data.instruments <- eval(mf,parent.frame())
  }

  # we then collect some informations about the data
  pdim <- pdim(data.formula)
  time.names <- pdim$panel.names$time.names
  id.names <- pdim$panel.names$id.names
  T <- pdim$nT$T
  ti <- split(data.formula[["(time)"]], data.formula[["(id)"]])

  # we now call the extract.data to extract the response and the
  # model.matrix of the relevant formula described earlier splited by
  # individual

  # first for the model, extract the yX as a matrix splited by
  # individual, and then remove the relevant number of time series
  yX <- extract.data(data.formula)

  K <- ncol(yX[[1]])-1
  Ky <- attr(formula,"lag")[[1]]
  if (length(Ky) > 1) Ky <- Ky[2]-Ky[1]+1
  K <- K-Ky
  Kt <- T-time.lost
  if (transformation == "ld") Kt <- Kt+1
  K <- list(K=K,Ky=Ky,Kt=Kt)

  yX <- lapply(yX, function(x) if(time.lost==1) x else x[-c(1:(time.lost-1)),])

  # then do the same with the matrix of "normal" instruments (if any)
  if (is.null(instruments)) In <- NULL else{
    In <- extract.data(data.instruments)
    In <- lapply(In,function(x) if(time.lost==1) x else x[-c(1:(time.lost-1)),])
  }
  
  # now the big task, the gmm instruments (so called W) matrix. First
  # extract the matrices of instruments, splited by individual, then
  # compute the J matrix which compute the relevant lag to use, and
  # finally, use the momatrix function to compute the W matrix with
  # the relevant lags
  W <- extract.data(data.gmm.inst)
  J <- makeJ(time.names, gmm.inst, lag.gmm,time.lost)
  W <- lapply(W, momatrix, J, time.names)
  
  if (transformation=="ld"){
    # additional stuff if the system GMM is required. Same commands as
    # previously to compute the gmm instrument matrix with variable in
    # differences
    mf$formula <- formula(gmm.inst.level) ; data.gmm.inst.level <- eval(mf,parent.frame())
    Wl <- extract.data(data.gmm.inst.level)
    Jl <- makeJ(time.names[-1], gmm.inst.level, lag.gmm.level,time.lost-1)
    Wl <- lapply(Wl,
                 function(x){
                   x <- x[-1,,drop=FALSE]
                   x <- momatrix(x, Jl, time.names[-1])
                   prems <- which(time.names == rownames(x)[1])
                   x <- rbind(0,x)
                   rownames(x)[1] <- time.names[prems-1]
                   x
                 }
               )
  }

  # finally, if a twoways effect model is required, time dummies shoud
  # be added
  if (effect == "twoways"){
    if (transformation == "ld"){
      time.dummies <- cbind(1,diag(1,T)[,-(1:(time.lost))])
      dimnames(time.dummies) <- list(time.names,c("(intercept)",time.names[(time.lost+1):T]))
    }
    else{
      time.dummies <- diag(1,T)[,-(1:(time.lost))]
      dimnames(time.dummies) <- list(time.names,time.names[(time.lost+1):T])
    }
  }
  else time.dummies <- NULL

  # then, call whether pgmm.sys or pgmm.diff
  result <- switch(transformation,
                   "ld" = pgmm.sys(yX, W, Wl, In, time.dummies, fsm, cl),
                   "d"  = pgmm.diff(yX, W, In, time.dummies, fsm, cl)
                   )

  result$time.lost <- time.lost
  result$K <- K
  structure(result,class=c("pgmm","panelmodel"),pdim = pdim)
}


pgmm.diff <- function(yX, W, In, time.dummies, fsm, cl){
  model <- ifelse(is.null(cl$model), "onestep", cl$model)
  if(!is.null(time.dummies)){
    yX <- lapply(yX,function(x) cbind(x,time.dummies[rownames(x),]))
    W <- lapply(W,function(x) cbind(x,time.dummies[rownames(x),]))
  }
  yX <- lapply(yX,diff)
  if (!is.null(In)){
    In <- lapply(In,diff)
    W <- mapply(cbind,W,In,SIMPLIFY=FALSE)
  }
  Vi <- lapply(W,function(x) crossprod(t(crossprod(x,FSM(dim(x)[1],fsm))),x))
  A1 <- solve(suml(Vi))*length(W)
  WyXi <- mapply(crossprod,W,yX,SIMPLIFY=FALSE)
  Wyi <- lapply(WyXi,function(x) x[,1])
  WXi <- lapply(WyXi,function(x) x[,-1])
  Wy <- suml(Wyi)
  WX <- suml(WXi)
  var.names <- colnames(yX[[1]])
  B1 <- solve(t(WX)%*%A1%*%WX)
  rownames(B1) <- colnames(B1) <- var.names[-1]
  coefficients <- B1%*%(t(WX)%*%A1%*%Wy)
  dim(coefficients) <- NULL
  names(coefficients) <- var.names[-1]
  residuals <- lapply(yX,
                      function(x)
                      as.vector(x[,1]-crossprod(t(x[,-1]),coefficients)))
  outresid <- lapply(residuals,function(x) outer(x,x))
  A2 <- mapply(crossprod,W,outresid,SIMPLIFY=FALSE)
  A2 <- mapply("%*%",A2,W,SIMPLIFY=FALSE)
#  A2 <- solve(suml(A2))
  A2 <- ginv(suml(A2))
  B2 <- solve(t(WX)%*%A2%*%WX)
  rownames(B2) <- colnames(B2) <- var.names[-1]
  if (model=="twosteps"){
    coef1s <- coefficients
    coefficients <- B2%*%(t(WX)%*%A2%*%Wy)
    dim(coefficients) <- NULL
    names(coefficients) <- var.names[-1]
    vcov <- B2
  }
  else{
    vcov <- B1
  }
  residuals <- lapply(yX,function(x){
    nz <- rownames(x)
    z <- as.vector(x[,1]-crossprod(t(x[,-1]),coefficients))
    names(z) <- nz
    z
  }
                      )
  fitted.values <- mapply(function(x,y) x[,1]-y,yX,residuals)
  n <- apply(sapply(yX,dim),1,sum)[1]
  K <- length(attr(terms(as.formula(cl$formula)),"term.labels"))
  Kt <- dim(yX[[1]])[2]-K-1
  p <- ncol(W[[1]])
  Ky <- attr(as.formula(cl$formula),"lag")[[1]][2]
  if(is.na(Ky)) Ky <- 0
  K <- list(K=K-Ky,Ky=Ky,Kt=Kt)
  if (model=="twosteps") coefficients <- list(coef1s,coefficients)

  list(coefficients = coefficients, residuals = residuals, vcov = vcov,
       fitted.values = fitted.values,
       df.residual = df.residual, 
       model = yX, W = W, K = K, A1 = A1, A2 = A2, call = cl)
}

pgmm.sys <- function(yX,W,Wl,In,time.dummies,fsm,cl){
  model <- ifelse(is.null(cl$model), "onestep", cl$model)
  if(!is.null(time.dummies)){
    yX <- lapply(yX,function(x) cbind(x,time.dummies[rownames(x),]))
    Wl <- lapply(Wl,function(x) cbind(x,time.dummies[rownames(x),]))
  }
  else{
    yX <- lapply(yX,function(x){x <- cbind(x,1);colnames(x)[dim(x)[2]] <- "(intercept)";x})
    Wl <- lapply(Wl,function(x){x <- cbind(x,1);colnames(x)[dim(x)[2]] <- "(intercept)";x})
  }
    
  if (!is.null(In)){
    Inl <- In
    In <- lapply(In,diff)
    W <- mapply(cbind,W,In,SIMPLIFY=FALSE)
    Wl <- mapply(cbind,Wl,Inl,SIMPLIFY=FALSE)
  }
  var.names <- colnames(yX[[1]])
  yXl <- yX
  yX <- lapply(yX,diff)
  pi <- lapply(Wl,nrow)
  F <- lapply(pi,FSM,fsm)
  WS <- mapply(bdiag,W,Wl,SIMPLIFY=FALSE)
  yXS <- mapply(rbind,yX,yXl,SIMPLIFY=FALSE)
  WyXi <- mapply(crossprod,WS,yXS,SIMPLIFY=FALSE)
  Wyi <- lapply(WyXi,function(x) x[,1])
  WXi <- lapply(WyXi,function(x) x[,-1])
  Wy <- suml(Wyi)
  WX <- suml(WXi)
  Vi <- mapply(function(x,y) crossprod(t(crossprod(x,y)),x),WS,F,SIMPLIFY=FALSE)
  A1 <- solve(suml(Vi))*length(WS)
  B1 <- solve(t(WX)%*%A1%*%WX)
  coefficients <- B1%*%(t(WX)%*%A1%*%Wy)
  dim(coefficients) <- NULL
  names(coefficients) <- var.names[-1]
  residuals <- lapply(yXl,
                      function(x){
                        nx <- rownames(x)
                        z <- as.vector(x[,1]-crossprod(t(x[,-1]),coefficients))
                        names(z) <- nx
                        z
                      }
                      )
  resid <- lapply(residuals,function(x) c(diff(x),x))
  outresid <- lapply(resid,function(x) outer(x,x))
  Vi <- mapply(function(x,y) crossprod(t(crossprod(x,y)),x),WS,outresid,SIMPLIFY=FALSE)
  A2 <- solve(suml(Vi))
  B2 <- solve(t(WX)%*%A2%*%WX)
  vcov <- B1
  if (model=="twosteps"){
    coef1s <- coefficients
    coefficients <- B2%*%(t(WX)%*%A2%*%Wy)
    dim(coefficients) <- NULL
    names(coefficients) <- var.names[-1]
    vcov <- B2
    residuals <- lapply(yXl,
                        function(x){
                          nx <- rownames(x)
                          z <- as.vector(x[,1]-crossprod(t(x[,-1]),coefficients))
                          names(z) <- nx
                          z
                        }
                        )
  }
  fitted.values <- mapply(function(x,y) x[,1]-y,yXl,residuals)
  n <- apply(sapply(yX,dim),1,sum)[1]
  K <- length(attr(terms(as.formula(cl$formula)),"term.labels"))
  Kt <- dim(yX[[1]])[2]-K-1
  p <- ncol(W[[1]])
  dim(coefficients) <- NULL
  names(coefficients) <- rownames(vcov) <- colnames(vcov) <- var.names[-1]
  Ky <- attr(as.formula(cl$formula),"lag")[[1]][2]
  if(is.na(Ky)) Ky <- 0
  K <- list(K=K-Ky,Ky=Ky,Kt=Kt)
  if (model=="twosteps") coefficients <- list(coef1s,coefficients)
  list(coefficients = coefficients, residuals = residuals, vcov = vcov,
       fitted.values = fitted.values,
       df.residual = df.residual, 
       model = yXl, W = WS, K = K, A1 = A1, A2 = A2, call = cl, Wd = W, Wl = Wl)

}

extract.data <- function(data){
  attr(attr(data,"terms"),"intercept") <- 0
  trms <- attr(data,"terms")
  data <- split(data,data[["(id)"]])
  data <- lapply(data,function(x){ rownames(x) <- x[["(time)"]];return(x)})
  if (length(trms) == 3){
    data <- lapply(data,
                   function(x){
                     x <- cbind(x[,1],model.matrix(trms,x))
                     colnames(x)[1] <- deparse(trms[[2]])
                     x
                   }
                   )
  }
  else data <- lapply(data,function(x) model.matrix(trms,x))
  data
}
  

makeJ <- function(time.names,gmminst,lag.gmm,time.lost){
  T <- length(time.names)
  names.gmm <- attr(terms(gmminst),"term.label")
  J <- array(0,dim=c(T,length(names.gmm),3),
             dimnames=list(time.names,names.gmm,
               c("start","end","n")))
  first.period <- sapply(lag.gmm,max)
  last.period <- sapply(lag.gmm,min)
  names(first.period) <- names(last.period) <- names.gmm
  for (ni in names.gmm){
    for (t in 1:T){
      J[t,ni,"start"] <- max(1,t-first.period[ni])
      J[t,ni,"end"] <- max(1,min(t-last.period[ni],T))
    }
  }
  
  J[,,"n"] <- J[,,"end",drop=FALSE]-J[,,"start",drop=FALSE]+1
  if (time.lost!=0){
    J <- J[-(1:time.lost),,,drop=FALSE]
  }
  J
}

momatrix <- function(x,J,ttot){
  names.gmm <- dimnames(J)[[2]]
  z <- matrix(0, nrow = length(ttot),
              ncol = length(names.gmm),
              dimnames = list(ttot,names.gmm))
  z[rownames(x),] <- x
  t.kept <- dimnames(J)[[1]]
  t.drop <- length(ttot) - length(t.kept)
  start <- which(ttot == rownames(x)[1])
  cnames <- c()
  for (y in t.kept){
    my <- c()
    for (ng in names.gmm){
      my <- c(my,z[seq(J[y,ng,1], J[y,ng,2]), ng])
    }
    cnames <- c(cnames, names(my))
    if (y==t.kept[1]){
      maty <- matrix(my, nrow=1)
    }
    else{
      lgn <- c(rep(0, ncol(maty)), my)
      maty <- cbind(maty, matrix(0, nrow = nrow(maty), ncol = length(my)))
      maty <- rbind(maty, lgn)
    }
  }
  rownames(maty) <- t.kept
  maty <- maty[rownames(x)[(t.drop+1):dim(x)[1]],]
  maty
}


G <- function(t){
  G <- matrix(0,t,t)
  for (i in 1:(t-1)){
    G[i,i] <- 2
    G[i,i+1] <- -1
    G[i+1,i] <- -1
  }
  G[t,t] <- 2
  G
}


FD <- function(t){
  FD <- Id(t)[-1,]
  for (i in 1:(t-1)){
    FD[i,i] <- -1
  }
  FD
}

Id <- function(t){
  diag(rep(1,t))
}

FSM <- function(t,fsm){
  switch(fsm,
         "I"=Id(t),
         "G"=G(t),
         "GI"=bdiag(G(t-1),diag(1,t)),
         "full"=rbind(cbind(G(t-1),FD(t)),cbind(t(FD(t)),Id(t)))
         )
}

