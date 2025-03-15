#' Generalized Method of Moments (GMM) Estimation for Panel Data
#' 
#' Generalized method of moments estimation for static or dynamic
#' models with panel data.
#' 
#' 
#' `pgmm` estimates a model for panel data with a generalized method
#' of moments (GMM) estimator. The description of the model to
#' estimate is provided with a multi--part formula which is (or which
#' is coerced to) a `Formula` object. The first right--hand side part
#' describes the covariates. The second one, which is mandatory,
#' describes the GMM instruments. The third one, which is optional,
#' describes the 'normal' instruments. By default, all the variables
#' of the model which are not used as GMM instruments are used as
#' normal instruments with the same lag structure as the one specified
#' in the model.
#' 
#' `y~lag(y, 1:2)+lag(x1, 0:1)+lag(x2, 0:2) | lag(y, 2:99)` is similar to
#' 
#' `y~lag(y, 1:2)+lag(x1, 0:1)+lag(x2, 0:2) | lag(y, 2:99) | lag(x1,
#' 0:1)+lag(x2, 0:2)`
#'
#' and indicates that all lags from 2 of `y` are used
#' as GMM instruments.
#' 
#' `transformation` indicates how the model should be transformed for
#' the estimation. `"d"` gives the "difference GMM" model
#' \insertCite{@see @AREL:BOND:91}{plm}, `"ld"` the "system GMM" model
#' \insertCite{@see @BLUN:BOND:98}{plm}.
#' 
#' `pgmm` is an attempt to adapt GMM estimators available within the
#' DPD library for GAUSS \insertCite{@see @AREL:BOND:98}{plm} and Ox
#' \insertCite{@see @DOOR:AREL:BOND:12}{plm} and within the xtabond2
#' library for Stata \insertCite{@see @ROOD:09}{plm}.
#' 
#' @aliases pgmm
#' @param formula a symbolic description for the model to be
#'     estimated. The preferred interface is now to indicate a
#'     multi--part formula, the first two parts describing the
#'     covariates and the GMM instruments and, if any, the third part
#'     the 'normal' instruments,
#' @param object,x an object of class `"pgmm"`,
#' @param data a `data.frame` (neither factors nor character vectors
#'     will be accepted in `data.frame`),
#' @param subset see [lm()],
#' @param na.action see [lm()],
#' @param effect the effects introduced in the model, one of
#'     `"twoways"` (the default) or `"individual"`,
#' @param model one of `"onestep"` (the default) or `"twosteps"`,
#' @param collapse if `TRUE`, the GMM instruments are collapsed (default is
#'                 `FALSE`),
#' @param lost.ts the number of lost time series: if `NULL`, this is
#'     automatically computed. Otherwise, it can be defined by the
#'     user as a numeric vector of length 1 or 2. The first element is
#'     the number of lost time series in the model in difference, the
#'     second one in the model in level. If the second element is
#'     missing, it is set to the first one minus one,
#' @param transformation the kind of transformation to apply to the
#'     model: either `"d"` (the default value) for the
#'     "difference GMM" model or `"ld"` for the "system GMM" model,
#' @param fsm character of length 1 to specify type of weighting matrix
#'      for the first step /the `"onestep"` estimator: one of `"I"` (identity 
#'      matrix) or `"G"` (\eqn{=D'D} where \eqn{D} is the first--difference 
#'      operator), if `transformation="d"`, one of `"GI"` or `"full"` 
#'      if `transformation="ld"`,
#' @param index the indexes,
#' @param \dots further arguments.
#' @param robust for pgmm's summary method: if `TRUE` (default), robust inference
#'               is performed in the summary, for a two-steps model with the 
#'               small-sample correction by \insertCite{WIND:05;textual}{plm},
#' @param time.dummies for pgmm's summary method: if `TRUE`, the estimated
#'     coefficients of time dummies are present in the table of coefficients;
#'     default is `FALSE`, thus time dummies are dropped in summary's coefficient
#'     table (argument is only meaningful if there are time dummies in the model, 
#'     i.e., only for `effect = "twoways"`),
#' @param digits digits,
#' @param width the maximum length of the lines in the print output.
#' @return An object of class `c("pgmm","panelmodel")`, which has the
#'     following elements:
#' 
#' \item{coefficients}{the vector (or the list for fixed effects) of
#'                     coefficients,}
#' \item{residuals}{the list of residuals for each individual,}
#' \item{vcov}{the covariance matrix of the coefficients,}
#' \item{fitted.values}{the vector of fitted values,}
#' \item{df.residual}{degrees of freedom of the residuals,}
#' \item{model}{a list containing the variables used for the
#'              estimation for each individual,} 
#' \item{W}{a list containing the instruments for each individual (a matrix per
#'          list element) (two lists in case of system GMM,}
# TODO: not correct, W does not contain two lists for system GMM
#' \item{A1}{the weighting matrix for the one--step estimator,}
#' \item{A2}{the weighting matrix for the two--steps estimator,}
# TODO: add B1 description here
#' \item{call}{the call.}
#' 
#' It has `print`, `summary` and `print.summary` methods.
#' @author Yves Croissant
#' @export
#' @importFrom MASS ginv
#' @seealso
#' 
#' [sargan()] for the Hansen--Sargan test and [mtest()] for
#' Arellano--Bond's test of serial correlation. [vcovHC.pgmm] for the robust
#' inference.
#' @references
#'
#' \insertAllCited{}
#'
#' @keywords regression
#' @examples
#' 
#' data("EmplUK", package = "plm")
#' 
#'# Arellano/Bond 1991, Table 4, column (a1) (has robust SEs)
#'ab.a1 <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
#'              + lag(log(capital), 0:2) + lag(log(output), 0:2) | lag(log(emp), 2:99),
#'              data = EmplUK, effect = "twoways", model = "onestep")
#'summary(ab.a1, robust = TRUE)
#'
#' # Arellano/Bond 1991, Table 4, column (a2) (has non-robust SEs)
#' ab.a2 <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
#'               + lag(log(capital), 0:2) + lag(log(output), 0:2) | lag(log(emp), 2:99),
#'               data = EmplUK, effect = "twoways", model = "twosteps")
#' summary(ab.a2, robust = FALSE)
#'
#' # Arellano and Bond (1991), table 4 col. b / # Windmeijer (2005), table 2, std. errc
#' ab.b <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
#'              + log(capital) + lag(log(output), 0:1) | lag(log(emp), 2:99),
#'              data = EmplUK, effect = "twoways", model = "twosteps")
#' summary(ab.b, robust = FALSE) # Arellano/Bond
#' summary(ab.b, robust = TRUE)  # Windmeijer
#'
#' ## Blundell and Bond (1998) table 4 (cf. DPD for OX p. 12 col. 4)
#' bb.4 <- pgmm(log(emp) ~ lag(log(emp), 1)+ lag(log(wage), 0:1) +
#'            lag(log(capital), 0:1) | lag(log(emp), 2:99) +
#'            lag(log(wage), 2:99) + lag(log(capital), 2:99),
#'            data = EmplUK, effect = "twoways", model = "onestep", 
#'            transformation = "ld")
#' summary(bb.4, robust = TRUE)
#' 
#' \dontrun{
#' ## Same with the old formula or dynformula interface
#' ## Arellano and Bond (1991), table 4, col. b 
#' ab.b <- pgmm(log(emp) ~ log(wage) + log(capital) + log(output),
#'             lag.form = list(2,1,0,1), data = EmplUK, 
#'             effect = "twoways", model = "twosteps",
#'             gmm.inst = ~log(emp), lag.gmm = list(c(2,99)))
#' summary(ab.b, robust = FALSE)
#' 
#' ## Blundell and Bond (1998) table 4 (cf. DPD for OX p. 12 col. 4)
#' bb.4 <- pgmm(dynformula(log(emp) ~ log(wage) + log(capital), list(1,1,1)), 
#'             data = EmplUK, effect = "twoways", model = "onestep", 
#'             gmm.inst = ~log(emp) + log(wage) + log(capital), 
#'             lag.gmm = c(2,99), transformation = "ld")
#' summary(bb.4, robust = TRUE)
#' }
#' 
pgmm <- function(formula, data, subset, na.action,
                 effect = c("twoways", "individual"),
                 model = c("onestep", "twosteps"),
                 collapse = FALSE, # TODO: collapse does not seem to be assumed a logical in the code below but rather a character vector
                 lost.ts = NULL,
                 transformation = c("d", "ld"),
                 fsm = switch(transformation, "d" = "G", "ld" = "full"),
                 index = NULL, ...) {

  # yX : response / covariates, W : gmm instruments, 
  # Z : normal instruments, V : time dummies
  
#  cl <- match.call(expand.dots = FALSE)
  cl <- match.call(expand.dots = TRUE)
  effect <- match.arg(effect)
  model <- match.arg(model)
  transformation <- match.arg(transformation)
  if(!is.null(fsm)) {
    # some interface checks
    stopifnot(is.character(fsm) && length(fsm) == 1L)
    if(!(fsm %in% names(pgmm.fsm.list))) stop("argument 'fsm' must be one of ", oneof(pgmm.fsm.list))
    if(transformation == "d")  stopifnot(fsm == "I"  || fsm == "G")
    if(transformation == "ld") stopifnot(fsm == "GI" || fsm == "full")
  } else {
    # set fsm if null
    fsm <- switch(transformation, "d" = "G", "ld" = "full")
    warning(paste0("as argument 'fsm' was NULL, it has been set to \"", fsm, "\""))
    }

  namesV <- NULL
  
  #################################################################
  ##### 1. Backward compatibility with the old formula / dynformula
  ##### interface
  #################################################################
  
  if (inherits(formula, "dynformula") || length(Formula(formula))[2L] == 1L){
    if (!inherits(formula, "dynformula")){
      formula <- match.call(expand.dots = TRUE)
      m <- match(c("formula", "lag.form", "diff.form", "log.form"),names(formula),0)
      formula <- formula[c(1L, m)]
      formula[[1L]] <- as.name("dynformula")
      formula <- cl$formula <- eval(formula, parent.frame())
    }
    response.name <- paste(deparse(formula[[2L]]))
    main.lags <- attr(formula, "lag")
    if (length(main.lags[[1L]]) == 1L && main.lags[[1L]] > 1L)
      main.lags[[1L]] <- c(1L, main.lags[[1L]])
    main.lags[2:length(main.lags)] <- lapply(main.lags[2:length(main.lags)],
                        function(x){
                          if (length(x) == 1L && x != 0) x <- c(0, x)
                          x
                        })
    main.form <- dynterms2formula(main.lags, response.name)
    dots <- list(...)
    gmm.inst <- dots$gmm.inst
    lag.gmm <- dots$lag.gmm
    instruments <- dots$instruments
    gmm.form <- dynformula(gmm.inst, lag.form = lag.gmm)
    gmm.lags <- attr(gmm.form, "lag")
    gmm.lags <- lapply(gmm.lags, function(x) min(x):max(x))
    gmm.form <- dynterms2formula(gmm.lags)
    formula <- as.Formula(main.form, gmm.form)
  }

  #################################################################
  ##### 2. Extract the response/covariates, the gmm instruments and
  ##### the "normal" instruments, as a named list containing the lag
  ##### structure
  #################################################################
  
  x <- formula
  if (!inherits(x, "Formula")) x <- Formula(formula)
  # gmm instruments : named list with the lags, names being the variables
  gmm.form <- formula(x, rhs = 2, lhs = 0)
  gmm.lags <- dynterms(gmm.form)

  cardW <- length(gmm.lags)
  if (is.null(names(collapse))){
    if (length(collapse) == 1L){
      collapse <- as.vector(rep(collapse, cardW), mode = "list")
    }
    else{
      if (length(collapse) != cardW) stop("the 'collapse' vector has a wrong length")
    }
    names(collapse) <- names(gmm.lags)
  }
  else{
     if (any(! (names(collapse) %in% names(gmm.lags)))) stop("unknown names in the 'collapse' vector")
     else{
       bcollapse <- as.vector(rep(FALSE, cardW), mode = "list")
       names(bcollapse) <- names(gmm.lags)
       bcollapse[names(collapse)] <- collapse
       collapse <- bcollapse
     }
  }
   
  # covariates : named list with the lags, names being the variables
  main.form <- formula(x, rhs = 1, lhs = 1)
  main.lags <- dynterms(main.form)

  # Three possibilities for 'normal' instruments :
  # 1. the third part of the formula describes them
  # 2. all variables not used as gmm are normal instruments
  # 3. all variables are gmm instruments and therefore, there are no
  #    normal instruments except maybe time dummies
  
  # the third part of the formula (if any) deals with the 'normal' instruments
  if (length(x)[2L] == 3L){
    normal.instruments <- TRUE
    inst.form <- formula(x, rhs = 3, lhs = 0)
    # the . - x1 + x2 syntax is allowed, in this case update with the first part
    inst.form <- update(main.form, inst.form)
    inst.form <- formula(Formula(inst.form), lhs = 0)
    inst.lags <- dynterms(inst.form)
  }
  else{
    # the default 'normal' instruments is the subset of covariates
    # which are not used as gmm instruments
    iv <- names(main.lags)[! names(main.lags) %in% names(gmm.lags)]
    inst.lags <- main.lags[iv]
    # generate the formula for 'normal' instruments
    if (length(inst.lags) > 0L){
      normal.instruments <- TRUE
      inst.form <- dynterms2formula(inst.lags)
    }
    else{
      # the case where there are no normal instruments : set inst.form
      # and inst.lags to NULL
      normal.instruments <- FALSE
      inst.form <- NULL
      inst.lags <- NULL
    }
  }
  
  #################################################################
  ##### 3. How many time series are lost
  #################################################################

  if (!is.null(lost.ts)){
    if (!is.numeric(lost.ts)) stop("argument 'lost.ts' should be numeric")
    lost.ts <- as.numeric(lost.ts)
    if (!(length(lost.ts) %in% c(1L, 2L))) stop("argument 'lost.ts' should be of length 1 or 2")
    TL1 <- lost.ts[1L]
    TL2 <- if(length(lost.ts) == 1L) { TL1 - 1 } else lost.ts[2L]
  }
  else{
    # How many time series are lost? May be the maximum number of lags
    # of any covariates + 1 because of first - differencing or the
    # largest minimum lag for any gmm or normal instruments
    # min or max to select the number of lost time series?
    gmm.minlag  <- min(unlist(gmm.lags, use.names = FALSE))
    inst.maxlag <- if (!is.null(inst.lags)) max(unlist(inst.lags, use.names = FALSE)) else 0
    main.maxlag <- max(unlist(main.lags, use.names = FALSE))
    TL1 <- max(main.maxlag + 1, inst.maxlag + 1, gmm.minlag)
    TL2 <- max(main.maxlag,     inst.maxlag,     gmm.minlag - 1)
    # if TL2 = 0 (no lags), one observation is lost anyway because of
    # the differentiation of the lag instruments
    TL1 <- max(main.maxlag + 1, gmm.minlag)       ## TODO: TL1, TL2 calc. twice and prev. result overwritten!?!
    TL2 <- max(main.maxlag,     gmm.minlag - 1)
  }

  #################################################################
  ##### 4. Compute the model frame which contains the
  ##### response/covariates, the gmm instruments and the 'normal'
  ##### instruments without the lags
  #################################################################
  
  gmm.form <- as.formula(paste("~", paste(names(gmm.lags), collapse = "+")))
  if (!is.null(inst.form)) Form <- as.Formula(main.form, gmm.form, inst.form)
  else Form <- as.Formula(main.form, gmm.form)
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "na.action", "index"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- as.name("plm")
  mf$model <- NA
  mf$formula <- Form
  mf$na.action <- "na.pass"
  mf$subset <- NULL
  data <- eval(mf, parent.frame())
  index <- index(data)
  pdim <- pdim(data)
  N <- pdim$nT$n
  T <- pdim$nT$T
  balanced <- pdim$balanced

  # if the data is unbalanced, "balance" the data
  if (!balanced){
    un.id <- sort(unique(index(data, "id")))
    un.time <- sort(unique(index(data, "time")))
    rownames(data) <- paste(index(data, "id"), index(data, "time"), sep = ".")
    allRows <- as.character(t(outer(un.id, un.time, paste, sep = ".")))
    data <- data[allRows, ]
    rownames(data) <- allRows
    index <- data.frame(id = rep(un.id, each = length(un.time)),
                        time = rep(un.time, length(un.id)),
                        row.names = rownames(data))
    class(index) <- c("pindex", "data.frame")
    attr(data, "index") <- index
  }
  
  #################################################################
  ##### 5. Get the response/covariates matrix yX, the gmm instruments
  ##### matrix W and the normal instruments matrix inst, split by
  ##### individuals
  #################################################################

  yX <- extract.data(data, form = main.form)
  names.coef <- colnames(yX[[1L]])[-1L]
  Z <- if(normal.instruments){
          extract.data(data, form = inst.form)
        } else NULL
  W <- extract.data(data, form = gmm.form, as.matrix = FALSE)
  
  #################################################################
  ##### 6. Create the matrix of response/covariates, gmm instruments
  ##### and normal instruments for the diff model
  #################################################################
  # create the matrix of gmm instruments for every individual
  W1 <- lapply(W, function(x){
                 u <- mapply(makegmm, x, gmm.lags, TL1, collapse, SIMPLIFY = FALSE)
                 matrix(unlist(u), nrow = nrow(u[[1L]]))
               })

  # differentiate the matrix of response/covariates (and of normal
  # instruments, if any) and remove T1 - 1 time series (xd is already
  # differenced)
  yX1 <- lapply(yX, function(x){
                  xd <- diff(x)
                  xd[-seq_len(TL1 - 1), , drop = FALSE]
                })
  
  if (normal.instruments){
    Z1 <- lapply(Z, function(x){
                   xd <- diff(x)
                   xd[-seq_len(TL1 - 1), , drop = FALSE]
                 })
  }
  
  #################################################################
  ##### 7. In case of system gmm, create the matrix of
  ##### response/covariates, gmm instruments and normal instruments
  ##### for the level model and merge with the diff model
  #################################################################

  if (transformation == "ld"){
    W2 <- lapply(W, function(x){
                   u <- mapply(makeW2, x, collapse, SIMPLIFY = FALSE)
                   # the matrix of instruments in difference has T - 2
                   # rows if one time series is lost (there are no gmm
                   # instruments for t = 2 but there is a moment
                   # condition with the intercept. In this case, a row
                   # of 0 should be added. Otherwise, the number of
                   # rows is just T - TL2
                   nrow.ud <- if(TL2 == 1L) { T - 2 } else { T - TL2 }
                   u <- matrix(unlist(u), nrow = nrow.ud)
                   if (TL2 == 1) u <- rbind(0, u)
                   u
                 })
    
    # remove the relevant number of time series for data in level
    yX2 <- lapply(yX, function(x){
                    x[-seq_len(TL2), , drop = FALSE]
                  })
    
    if (normal.instruments){
      Z2 <- lapply(Z, function(x){
                     x[-seq_len(TL2), , drop = FALSE]
                     })
    }
  }

  #################################################################
  ##### 8. Add time dummies if effect = "twoways"
  #################################################################

  if (effect == "twoways"){
    namesV <- levels(index(data, which = "time"))
    if (transformation == "d"){
      V1 <- td.model.diff <- diff(diag(1, T - TL1 + 1))[ , -1, drop = FALSE]
      namesV <- namesV[-seq_len(TL1)]
    }
    else{
      td <- cbind(1, rbind(0, diag(1, T - 1)))
      # remove as many columns and rows as there are lost time series
      # in level (the difference of position between rows and columns
      # is due to the fact that the first column of td is the
      # intercept and should be kept anyway
      V2 <- td[- c(seq_len(TL2)), - c(2:(2 + TL2 - 1))]
      V1 <- diff(V2)
      namesV <- c("(Intercept)", namesV[-seq_len(TL2 + 1)])
    }
    
    for (i in seq_len(N)){
      yX1[[i]] <- cbind(yX1[[i]], V1)
      if (transformation == "d"){
        W1[[i]] <- cbind(W1[[i]], V1)
      }
      else{
        W2[[i]]  <- cbind( W2[[i]], V2)
        yX2[[i]] <- cbind(yX2[[i]], V2)
      }
    }
  }
  # A QAD fix for the bug in mtest for ld model without time.dummies
  if (effect == "individual" && transformation == "ld"){
    namesV <- levels(index(data, which = "time"))
    namesV <- c("(Intercept)", namesV[-seq_len(TL2 + 1)])
  }
  
  #################################################################
  ##### 9. In case of unbalanced data, replace NA's by 0 and overwrite
  ##### rows for missing time series with 0
  #################################################################

  for (i in seq_len(N)){
    narows <- apply(yX1[[i]], 1, function(z) anyNA(z))
    yX1[[i]][narows, ] <- 0
    W1[[i]][is.na(W1[[i]])] <- 0
    W1[[i]][narows, ] <- 0
    if (normal.instruments){
      Z1[[i]][is.na(Z1[[i]])] <- 0
      Z1[[i]][narows, ] <- 0
    }
    if (transformation == "ld"){
      narows <- apply(yX2[[i]], 1, function(z) anyNA(z))
      yX2[[i]][narows, ] <- 0
      W2[[i]][is.na(W2[[i]])] <- 0
      W2[[i]][narows, ] <- 0
      if (normal.instruments){
        Z2[[i]][is.na(Z2[[i]])] <- 0
        Z2[[i]][narows, ] <- 0
      }
    }
  }

  #################################################################
  ##### 10. a) In case of sys gmm: bdiag or rbind the diff and level
  #####        matrices; 
  #####     b) cbind normal instruments, if any
  #################################################################
  
  if (transformation == "ld"){
    for (i in seq_len(N)){
       W1[[i]] <- bdiag( W1[[i]],  W2[[i]])
      yX1[[i]] <- rbind(yX1[[i]], yX2[[i]])
      if (normal.instruments) Z1[[i]] <- bdiag(Z1[[i]], Z2[[i]])
    }
  }

  if (normal.instruments){
    for (i in seq_len(N)) W1[[i]] <- cbind(W1[[i]], Z1[[i]])
  }

  
  #################################################################
  ##### 11. Compute the estimator
  #################################################################

  W  <- W1
  yX <- yX1
  
  ## WX <- mapply(function(x, y) crossprod(x, y), W, yX, SIMPLIFY = FALSE)
  ## WX <- Reduce("+", WX)
  ## zerolines <- which(apply(WX, 1, function(z) sum(abs(z))) == 0)
  ## for (i in seq_len(N)) W[[i]] <- W[[i]][, - zerolines]

  WX <- mapply(function(x, y) crossprod(x, y), W, yX, SIMPLIFY = FALSE)
  Wy <- lapply(WX, function(x) x[ ,  1L])
  WX <- lapply(WX, function(x) x[ , -1L, drop = FALSE])
  WX <- Reduce("+", WX)
  Wy <- Reduce("+", Wy)

  # Compute the first step matrices
  H <- switch(transformation,
          "d" = FSM(T - TL1, fsm), # for fsm == "G": FSM(. "G") gives the same as previously hard coded tcrossprod(diff(diag(1, T - TL1 + 1)))
         "ld" = FSM(T - TL2, fsm))
  
  A1 <- lapply(W, function(x) crossprod(t(crossprod(x, H)), x))
  A1 <- Reduce("+", A1)
  minevA1 <- min(eigen(A1)$values)
  eps <- 1E-9
  A1 <- if(minevA1 < eps){
    warning("the first-step matrix is singular, a general inverse is used")
    ginv(A1)
  } else solve(A1)
  A1 <- A1 * length(W)

  # coefficients: Roodman (2019) formula (13, upper part for beta1) with B1 = (X'Z(Z'HZ)^(-1)Z'X)^(-1), A1 = (Z'HZ)^(-1)
  t.CP.WX.A1 <- t(crossprod(WX, A1))
  B1 <- solve(crossprod(WX, t.CP.WX.A1))
  Y1 <- crossprod(t.CP.WX.A1, Wy)
  coefficients <- as.numeric(crossprod(B1, Y1))
  
  if (effect == "twoways") names.coef <- c(names.coef, namesV)
  names(coefficients) <- names.coef

  residuals <- lapply(yX, function(x)
                      as.vector(x[ , 1L] - crossprod(t(x[ , -1L, drop = FALSE]), coefficients)))
  

  # A2 is also needed  for "onestep" model in vcovHC.pgmm, hence calc. here and 
  # always include in model object 
  A2 <- mapply(function(w, res) tcrossprod(crossprod(w, tcrossprod(res)), t(w)), W, residuals, SIMPLIFY = FALSE) # == mapply(function(w, res) t(w) %*% tcrossprod(res) %*% w, W, residuals, SIMPLIFY = FALSE)
  A2 <- Reduce("+", A2)
  minevA2 <- min(eigen(A2)$values)
  A2 <- if (minevA2 < eps) {
    warning("the second-step matrix is singular, a general inverse is used")
    ginv(A2)
  } else solve(A2)
  
  if (model == "twosteps") {
    coef1s <- coefficients
    t.CP.WX.A2 <- t(crossprod(WX, A2))
    Y2 <- crossprod(t.CP.WX.A2, Wy)
    vcov <- solve(crossprod(WX, t.CP.WX.A2)) # "B2"
    coef2s <- as.numeric(crossprod(vcov, Y2))
    names(coef2s) <- names.coef
    coefficients <- list("step1" = coef1s, "step2" = coef2s)
    
    # calc. residuals with coefs from 2nd step
    residuals <- lapply(yX, function(x){
                           nz <- rownames(x)
                           z <- as.vector(x[ , 1L] - crossprod(t(x[ , -1L, drop = FALSE]), coef2s))
                           names(z) <- nz
                           z})
  } else {
    vcov <- B1
  }
  
  rownames(vcov) <- colnames(vcov) <- names.coef

  # TODO: yX does not contain the original data (but first-diff-ed data) -> fitted.values not what you would expect
  fitted.values <- mapply(function(x, y) x[ , 1L] - y, yX, residuals)
  # fitted.values <- data[ , 1L] - unlist(residuals) # in 'data' is original data, but obs lost due to diff-ing are not dropped -> format incompatible
  
  args <- list(model          = model,
               effect         = effect,
               transformation = transformation,
           #    collapse       = collapse, # TODO: this would give a list of instruments, not the logical collapse as arg input
               namest         = namesV)
  
  result <- list(coefficients  = coefficients,
                 residuals     = residuals, # is a list (but documentation said for a long time 'vector'), mtest() and sargan() expect a list
                 vcov          = vcov,
                 fitted.values = fitted.values,
          #       df.residual   = df.residual,     # TODO: df.residual is not defined here, hence the function 'df.residual' is attached by this
                 model         = yX,
                 W             = W,
                 A1            = A1,
                 A2            = A2,
                 B1            = B1,
                 call          = cl,
                 args          = args)
  
  result <- structure(result,
                      class = c("pgmm", "panelmodel"),
                      pdim = pdim)
  result
}

dynterms <- function(x){
  trms.lab <- attr(terms(x), "term.labels")
  result <- getvar(trms.lab)
  nv <- names(result)
  dn <- names(table(nv))[table(nv) > 1]
  un <- names(table(nv))[table(nv) == 1]
  resu <- result[un]
  for (i in dn){
    v <- sort(unique(unlist(result[nv == i])))
    names(v) <- NULL
    resu[[i]] <- v
  }
  resu
}

getvar <- function(x){
  x <- as.list(x)
  result <- lapply(x, function(y){
    deb <- as.numeric(gregexpr("lag\\(", y)[[1L]])
    if (deb == -1){
      lags <- 0
      avar <- y
    }
    else{
#      inspar <- substr(y, deb + 2, nchar(y) - 1)
      inspar <- substr(y, deb + 4, nchar(y) - 1)
      coma <- as.numeric(gregexpr(",", inspar)[[1L]][1L])
      if (coma == -1){
        endvar <- nchar(inspar)
        lags <- 1
      }
      else{
        endvar <- coma - 1
        lags <- substr(inspar, coma + 1, nchar(inspar))
        lags <- eval(parse(text = lags))
      }
      avar <- substr(inspar, 1, endvar)
    }
    list(avar, lags)
  }
                   )
  nres   <- sapply(result, function(x) x[[1L]])
  result <- lapply(result, function(x) x[[2L]])
  names(result) <- nres
  result
}

dynterms2formula <- function(x, response.name = NULL){
  result <- character(0)
  for (i in seq_along(x)){
    theinst <- x[[i]]
    # if the first element is zero, write the variable without lag and
    # drop the 0 from the vector
    if (theinst[1L] == 0){
      at <- names(x)[i]
      theinst <- theinst[-1L]
    }
    else{
      at <- character(0)
    }
    # if there are still some lags, write them
    if (length(theinst) > 0L){
      if (length(theinst) > 1L){
        at <- c(at, paste("lag(", names(x)[i], ",c(",
                          paste(theinst, collapse = ","), "))", sep =""))
      }
      else{
        at <- c(at, paste("lag(", names(x)[i], ",", theinst, ")", sep =""))
      }
    }
    result <- c(result, at)
  }
  if (is.null(response.name)) as.formula(paste("~", paste(result, collapse = "+")))
  else as.formula(paste(response.name, "~", paste(result, collapse = "+")))
}

extract.data <- function(data, form, as.matrix = TRUE){
  # uses collapse's fast *split functions / 2024-12-27
  trms <- terms(form)
  has.response  <- attr(trms, 'response')  == 1
  has.intercept <- attr(trms, 'intercept') == 1
  if(has.intercept){
    # Formula is unable to update formulas with no lhs
    form <- Formula(update(formula(form), ~ . -1))
#    form <- update(form, ~. -1)
  }
  index <- attr(data, "index")
  
  X <- model.matrix(form, data)
  if (has.response){
    X <- cbind(data[[1L]], X)
    colnames(X)[1L] <- deparse(trms[[2L]])
  }
  
  if(!as.matrix) X <- as.data.frame(X)
  data <- collapse::rsplit(X, index[[1L]], simplify = FALSE)
  time <- collapse::gsplit(index[[2L]], index[[1L]])
  data <- mapply(function(x, y){
                   rownames(x) <- y
                   x
                 }, data, time, SIMPLIFY = FALSE)
  data
}

G <- function(t){
  G <- matrix(0, t, t)
  for (i in seq_len(t-1)){
    G[i,   i]   <-  2
    G[i,   i+1] <- -1
    G[i+1, i]   <- -1
  }
  G[t, t] <- 2
  G
}

FD <- function(t){
  FD <- Id(t)[-1L, ]
  for (i in seq_len(t-1)){
    FD[i, i] <- -1
  }
  FD
}

Id <- function(t){
  diag(1, t)
}

FSM <- function(t, fsm = c("I", "G", "GI", "full")){
  fsm <- match.arg(fsm)
  switch(fsm,
         "I" = Id(t),
         "G" = G(t),
         "GI" = bdiag(G(t-1), Id(t)),
         "full" = rbind(cbind(G(t-1), FD(t)), cbind(t(FD(t)), Id(t)))
         )
}

makegmm <- function(x, g, TL1, collapse = FALSE){
  T <- length(x)
  rg <- range(g)
  z <- as.list((TL1 + 1):T)
  x <- lapply(z, function(y) x[max(1, y - rg[2L]):(y - rg[1L])])
  if (collapse) {
    x <- lapply(x, rev)
    m <- matrix(0, T - TL1, min(T - rg[1L], rg[2L]+1-rg[1L]))
    for (y in seq_along(x)){ m[y, seq_along(x[[y]])] <- x[[y]]}
    result <- m
   }
   else {
     lx <- vapply(x, length, FUN.VALUE = 0.0)
     n <- length(x)
     lxc <- cumsum(lx)
     before <- c(0, lxc[-n])
     after <- lxc[n] - lx - before 
     result <- t(mapply(function(x, y, z) 
                        c(rep(0, y), x, rep(0, z)), 
                        x, before, after, SIMPLIFY = TRUE))
    }
    result
}


makeW2 <-function (x, collapse = FALSE){
  if(collapse) { diff(x[-c(length(x))]) }
  else {    diag(diff(x[-c(length(x))])) }
}

#' @rdname pgmm
#' @export
coef.pgmm <- function(object,...){
  model <- describe(object, "model")
  if(model == "onestep") object$coefficients
  else                   object$coefficients[[2L]]
}

#' @rdname pgmm
#' @export
summary.pgmm <- function(object, robust = TRUE, time.dummies = FALSE, ...) {
  model <- describe(object, "model")
  effect <- describe(object, "effect")
  transformation <- describe(object, "transformation")
  vv <- if(robust) vcovHC(object) else vcov(object)
  K <- if(model == "onestep") length(object$coefficients)
       else                   length(object$coefficients[[2L]])
  object$sargan <- sargan(object, "twosteps")
  object$m1 <- mtest(object, order = 1L, vcov = if(!robust) NULL else vv)
  # mtest with order = 2 is only feasible if more than 2 observations are present
  if(NROW(object$model[[1L]]) > 2L) object$m2 <- mtest(object, order = 2L, vcov = if(!robust) NULL else vv)
  object$wald.coef <- pwaldtest(object, param = "coef", vcov = if(!robust) NULL else vv)
  if(effect == "twoways") object$wald.td <- pwaldtest(object, param = "time", vcov = if(!robust) NULL else vv)
  Kt <- length(object$args$namest)
  rowsel <- if(!time.dummies && effect == "twoways") -c((K - Kt + 1):K)
            else seq_len(K)
  std.err <- sqrt(diag(vv))
  b <- coef(object)
  z <- b / std.err
  p <- 2 * pnorm(abs(z), lower.tail = FALSE)
  coefficients <- cbind(b, std.err, z, p)
  colnames(coefficients) <- c("Estimate", "Std. Error", "z-value", "Pr(>|z|)")
  object$coefficients <- coefficients[rowsel, , drop = FALSE]
  class(object) <- "summary.pgmm"
  object
}

#' Arellano--Bond Test of Serial Correlation
#' 
#' Test of serial correlation for models estimated by GMM
#' 
#' The Arellano--Bond test is a test of correlation based on the residuals of
#' the estimation. By default, the computation is done with the standard
#' covariance matrix of the coefficients. A robust estimator of a
#' covariance matrix can be supplied with the `vcov` argument.
#' 
#' Note that `mtest` computes like DPD for Ox and xtabond do, i.e., uses for two-steps
#' models the one-step model's residuals which were used to construct the efficient
#' two-steps estimator, see \insertCite{DOOR:AREL:BOND:12}{plm}, p. 32, footnote 7;
#' As noted by \insertCite{AREL:BOND:91}{plm} (p. 282), the m statistic is rather 
#' flexible and can be defined with any consistent GMM estimator which gives leeway 
#' for implementation, but the test's asymptotic  power depends on the estimator's efficiency. 
#' \insertCite{AREL:BOND:91}{plm} (see their footnote 9) used 
#' DPD98 for Gauss (\insertCite{AREL:BOND:98}{plm}) as did 
#' \insertCite{WIND:05}{plm} (see footnote 10) for the basis of his 
#' covariance correction, both with a slightly different implementation. 
#' Hence some results for `mtest` with two-step models diverge from original papers,
#' see examples below.
#' 
#' @param object an object of class `"pgmm"`,
#' @param order integer: the order of the serial correlation,
#' @param vcov a matrix of covariance for the coefficients or a function to
#' compute it,
#' @param \dots further arguments (currently unused).
#' @return An object of class `"htest"`.
#' @export
#' @author Yves Croissant
#' @seealso [pgmm()], [vcovHC.pgmm()]
#' @references
#'
#' \insertAllCited{}
#' 
#' @keywords htest
#' @examples
#' data("EmplUK", package = "plm")
#' # Arellano/Bond 1991, Table 4, column (a1)
#' ab.a1 <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
#'               + lag(log(capital), 0:2) + lag(log(output), 0:2) | lag(log(emp), 2:99),
#'               data = EmplUK, effect = "twoways", model = "onestep")
#' mtest(ab.a1, 1L)
#' mtest(ab.a1, 2L, vcov = vcovHC)
#' 
#' # Windmeijer (2005), table 2, onestep with corrected std. err
#' ab.b.onestep <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
#'                      + log(capital) + lag(log(output), 0:1) | lag(log(emp), 2:99),
#'                      data = EmplUK, effect = "twoways", model = "onestep")
#' mtest(ab.b.onestep, 1L, vcov = vcovHC)
#' mtest(ab.b.onestep, 2L, vcov = vcovHC)
#' 
#' # Arellano/Bond 1991, Table 4, column (a2)
#' ab.a2 <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
#'              + lag(log(capital), 0:2) + lag(log(output), 0:2) | lag(log(emp), 2:99),
#'              data = EmplUK, effect = "twoways", model = "twosteps")
#' mtest(ab.a2, 1L)
#' mtest(ab.a2, 2L) # while a la Arellano/Bond (1991) -0.434

mtest <- function(object, ...) {
  UseMethod("mtest")
}

#' @rdname mtest
#' @export
mtest.pgmm <- function(object, order = 1L, vcov = NULL, ...) {
  if (!inherits(object, "pgmm")) stop("argument 'object' needs to be class 'pgmm'")
  myvcov <- vcov
  if (is.null(vcov)) vv <- vcov(object)
  else if (is.function(vcov)) vv <- myvcov(object)
  else vv <- myvcov
  model <- describe(object, "model")
  transformation <- describe(object, "transformation")
  Kt <- length(object$args$namest)

  if(order >= (obs <- NROW(object$model[[1]]))) {
    error.msg <- paste0("argument 'order' (", order, ") specifies an order ",
                        "larger or equal than the number of available ", 
                        "observations (", obs, ")")
    stop(error.msg)
  }

  switch(transformation,
         "d" = {
           resid <- object$residuals
           residl <- lapply(resid, function(x) c(rep(0, order), x[seq_len(length(x) - order)]))
               },
         "ld" = {
           resid <- lapply(object$residuals, function(x)
                             c(x[-c(Kt:(2 * Kt + 1))], rep(0, Kt)))
           residl <- lapply(object$residuals, function(x)
                              c(rep(0, order), x[seq_len(Kt - order - 1)], rep(0, Kt)))
         })
  
  X <- lapply(object$model, function(x) x[ , -1L, drop = FALSE])
  W <- object$W
  A <- if(model == "onestep") object$A1 else object$A2
  B <- object$vcov # object$vcov is "B1" for one-step and "B2" for two-steps model
  EX  <- Reduce("+", mapply(crossprod, residl, X, SIMPLIFY = FALSE))
  XZ  <- Reduce("+", mapply(crossprod, W,      X, SIMPLIFY = FALSE))
  V <- mapply(tcrossprod, resid, SIMPLIFY = FALSE)
  EVE <- Reduce("+", mapply(function(v, e)    t(e) %*% v %*% e,    V, residl, SIMPLIFY = FALSE))
  ZVE <- Reduce("+", mapply(function(w, v, e) t(w) %*% v %*% e, W, V, residl, SIMPLIFY = FALSE))

  num <- Reduce("+", mapply(crossprod, resid, residl, SIMPLIFY = FALSE))
  denom <- EVE - 2 * EX %*% B %*% t(XZ) %*% A %*% ZVE + EX %*% vv %*% t(EX)
  stat <- as.numeric(num / sqrt(denom))
  names(stat) <- "normal"
  if(!is.null(vcov)) vcov <- paste0(", vcov: ", deparse(substitute(vcov)))
  method <- paste0("Arellano-Bond autocorrelation test of degree ", order, vcov)
  pval <- 2 * pnorm(abs(stat), lower.tail = FALSE)
  mtest <- list(statistic   = stat,
                p.value     = pval,
                alternative = "autocorrelation present",
                method      = method,
                data.name   = data.name(object))
  class(mtest) <- "htest"
  mtest
}


#' @rdname pgmm
#' @export
print.summary.pgmm <- function(x, digits = max(3, getOption("digits") - 2),
                               width = getOption("width"),
                               ...) {
  model <- describe(x, "model")
  transformation <- describe(x, "transformation")
  effect <- describe(x, "effect")
  pdim <- attr(x, "pdim")
  formula <- x$call$formula
  model.text <- paste(effect.pgmm.list[effect], model.pgmm.list[model],
                      model.pgmm.transformation.list[transformation], sep = " ")
  cat(paste(model.text, "\n"))
  ## TODO: add info about collapse argument in printed output
  
  ## TODO: print information about non-robust/robust SE, see, e.g., print.summary.plm

  cat("\nCall:\n")
  print(x$call)
  cat("\n")
  print(pdim)
  ntot <- sum(unlist(x$residuals, use.names = FALSE) != 0)
  ninst <- dim(x$W[[1L]])[2L]
  cat("\nNumber of Observations Used:", ntot, sep = " ")
#  cat("\nNumber of Instruments Used:  ", ninst, "\n", sep ="") # TODO: more checks, then activate printing
  cat("\nResiduals:\n")
  print(summary(unlist(residuals(x), use.names = FALSE)))
  cat("\nCoefficients:\n")
  printCoefmat(x$coefficients, digits = digits)

  cat("\nSargan test: ", names(x$sargan$statistic),
      "(", x$sargan$parameter, ") = ", x$sargan$statistic,
      " (p-value = ", format.pval(x$sargan$p.value,digits=digits), ")\n", sep = "")
  cat("Autocorrelation test (1): ", names(x$m1$statistic),
      " = ", x$m1$statistic,
      " (p-value = ", format.pval(x$m1$p.value, digits = digits), ")\n", sep = "")
  if(!is.null(x$m2)) {
    # # mtest with order = 2 is only present in x if more than 2 observations were present
    cat("Autocorrelation test (2): ", names(x$m2$statistic),
        " = ", x$m2$statistic,
        " (p-value = ", format.pval(x$m2$p.value,digits=digits), ")\n", sep = "")
  }
    cat("Wald test for coefficients: ", names(x$wald.coef$statistic),
      "(",x$wald.coef$parameter,") = ", x$wald.coef$statistic,
      " (p-value = ", format.pval(x$wald.coef$p.value, digits = digits), ")\n", sep = "")
  
  if(effect == "twoways") {
    cat("Wald test for time dummies: ", names(x$wald.td$statistic),
        "(", x$wald.td$parameter, ") = ", x$wald.td$statistic,
        " (p-value = ", format.pval(x$wald.td$p.value, digits = digits), ")\n", sep = "")
  }
  invisible(x)
}


#' Hansen--Sargan Test of Overidentifying Restrictions
#' 
#' A test of overidentifying restrictions for models estimated by GMM.
#' 
#' The Hansen--Sargan test ("J test") calculates the quadratic form of the moment
#' restrictions that is minimized while computing the GMM estimator. It follows
#' asymptotically a chi-square distribution with number of degrees of freedom
#' equal to the difference between the number of moment conditions and the
#' number of coefficients.
#' 
#' @param object an object of class `"pgmm"`,
#' @param weights the weighting matrix to be used for the computation of the
#' test,
#' @param \dots further arguments (currently unused).
#' @return An object of class `"htest"`.
#' @export
#' @author Yves Croissant
#' @seealso [pgmm()]
#' @references
#'
#' \insertCite{HANS:82}{plm}
#'
#' \insertCite{SARG:58}{plm}
#' 
#' @keywords htest
#' @examples
#' 
#' data("EmplUK", package = "plm")
#' ar <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1) +
#'            lag(log(capital), 0:2) + lag(log(output), 0:2) | lag(log(emp), 2:99),
#'            data = EmplUK, effect = "twoways", model = "twosteps")
#' sargan(ar)
#' 
sargan <- function(object, ...) {
  UseMethod("sargan")
}

#' @rdname sargan
#' @export
sargan.pgmm <- function(object, weights = c("twosteps", "onestep"), ...) {
  if (!inherits(object, "pgmm")) stop("argument 'object' needs to be class 'pgmm'")
  weights <- match.arg(weights)
  model <- describe(object, "model")
  Ktot <- if(model == "onestep") length(object$coefficients)
          else                   length(object$coefficients[[2L]])
  z <- as.numeric(Reduce("+", mapply(crossprod, object$W, object$residuals, SIMPLIFY = FALSE)))
  p <- ncol(object$W[[1L]])
  A <- if(weights == "onestep") object$A1 else object$A2
  stat <- as.numeric(tcrossprod(z, crossprod(z, A)))
  parameter <- p - Ktot
  names(parameter) <- "df"
  names(stat) <- "chisq"
  method <- "Sargan test"
  pval <- pchisq(stat, df = parameter, lower.tail = FALSE)
  sargan <- list(statistic = stat,
                 p.value   = pval,
                 parameter = parameter,
                 method    = method,
                 alternative = "overidentifying restrictions not valid",
                 data.name = data.name(object))
  class(sargan) <- "htest"
  sargan
}
