## Structural changes made to plm's original data transformation functions
## need to be mimicked in the *.collapse versions and vice versa.

## 1) Give the base-R version of the functions defined in tool_transformations.R
##    a new name (*.baseR).
## 2) Implement wrapper switched which call the *.baseR or *.collapse versions
##    based on the option plm.fast (a logical, the preferred way to set the
##    option is via function pkg.plm.fast() but the option can be set directly
##    via R's regular option mechanism). Unset option plm.fast results in the
##    base-R versions of functions being used.

## ad 1) new name for base R functions defined in tool_transformations.R
Sum.default.baseR <- plm:::Sum.default
Sum.pseries.baseR <- plm:::Sum.pseries
Sum.matrix.baseR  <- plm:::Sum.matrix

between.default.baseR <- plm:::between.default
between.pseries.baseR <- plm:::between.pseries
between.matrix.baseR  <- plm:::between.matrix

Between.default.baseR <- plm:::Between.default
Between.pseries.baseR <- plm:::Between.pseries
Between.matrix.baseR  <- plm:::Between.matrix

Within.default.baseR <- plm:::Within.default
Within.pseries.baseR <- plm:::Within.pseries
Within.matrix.baseR  <- plm:::Within.matrix

## ad 2) implement wrapper switches

#### Sum wrapper switches ####
Sum.default <- function(x, effect, ...) {
	if(!isTRUE(getOption("plm.fast"))) {
		Sum.default.baseR(x, effect, ...) } else {
		Sum.default.collapse(x, effect, ...) }
}

Sum.pseries <- function(x, effect = c("individual", "time", "group"), ...) {
	if(!isTRUE(getOption("plm.fast"))) {
		Sum.pseries.baseR(x, effect, ...) } else {
		Sum.pseries.collapse(x, effect, ...) }
}

Sum.matrix <- function(x, effect, ...) {
	if(!isTRUE(getOption("plm.fast"))) {
		Sum.matrix.baseR(x, effect, ...) } else {
		Sum.matrix.collapse(x, effect, ...) }
}

#### Between wrapper switches ####
Between.default <- function(x, effect, ...) {
	if(!isTRUE(getOption("plm.fast"))) {
		Between.default.baseR(x, effect, ...) } else {
		Between.default.collapse(x, effect, ...) }
}
	
Between.pseries <- function(x, effect = c("individual", "time", "group"), ...) {
	if(!isTRUE(getOption("plm.fast"))) {
		Between.pseries.baseR(x, effect, ...) } else {
		Between.pseries.collapse(x, effect, ...) }
}

Between.matrix <- function(x, effect, ...) {
	if(!isTRUE(getOption("plm.fast"))) {
		Between.matrix.baseR(x, effect, ...) } else {
		Between.matrix.collapse(x, effect, ...) }
}

#### between wrapper switches ####
between.default <- function(x, effect, ...) {
  if(!isTRUE(getOption("plm.fast"))) {
     between.default.baseR(x, effect, ...) } else {
     between.default.collapse(x, effect, ...) }
}

between.pseries <- function(x, effect = c("individual", "time", "group"), ...) {
	if(!isTRUE(getOption("plm.fast"))) {
		between.pseries.baseR(x, effect, ...) } else {
		between.pseries.collapse(x, effect, ...) }
}

between.matrix <- function(x, effect, ...) {
	if(!isTRUE(getOption("plm.fast"))) {
		between.matrix.baseR(x, effect, ...) } else {
		between.matrix.collapse(x, effect, ...) }
}

#### Within wrapper switches ####
Within.default <- function(x, effect, ...) {
	if(!isTRUE(getOption("plm.fast"))) {
		Within.default.baseR(x, effect, ...) } else {
		Within.default.collapse(x, effect, ...) }
}

Within.pseries <- function(x, effect = c("individual", "time", "group", "twoways"), ...) {
	if(!isTRUE(getOption("plm.fast"))) {
		Within.pseries.baseR(x, effect, ...) } else {
		Within.pseries.collapse(x, effect, ...) }
}

Within.matrix <- function(x, effect, rm.null = TRUE, ...) {
	if(!isTRUE(getOption("plm.fast"))) {
		Within.matrix.baseR(x, effect, ...) } else {
		Within.matrix.collapse(x, effect, ...) }
}


#### Sum ####

Sum.default.collapse <- function(x, effect, ...) {
# print("Sum.default.collapse")
# browser()
	# argument 'effect' is assumed to be a factor
	if(!is.numeric(x)) stop("The Sum function only applies to numeric vectors")
	dots <- match.call(expand.dots = FALSE)$`...`
	na.rm <- if(is.null(dots[["na.rm"]])) { 
		FALSE }# default of plm::between 
	else {
		dots[["na.rm"]]
	}
	nms <- as.character(effect)
	na.x <- is.na(x)
	res <- collapse::fsum(x, g = effect, w = NULL, na.rm = na.rm)
	res <- res[effect] # need to expand data to original length
	names(res) <- nms
	res[na.x] <- NA
	return(res)
}

Sum.pseries.collapse <- function(x, effect = c("individual", "time", "group"), ...) {
# print("Sum.pseries.collapse")
# browser()
	dots <- match.call(expand.dots = FALSE)$`...`
	effect <- match.arg(effect)
	na.rm <- if(is.null(dots[["na.rm"]])) { 
		FALSE } # default of plm's functions
	else { 
		dots[["na.rm"]]
	}
	
	eff.no <- switch(effect,
									 "individual" = 1L,
									 "time"       = 2L,
									 "group"      = 3L,
									 stop("unknown value of argument 'effect'"))
	i <- index(x)
	eff.fac <- i[ , eff.no]
	nms <- as.character(eff.fac)
	na.x <- is.na(x)
	res <- collapse::fsum(x, g = eff.fac, w = NULL, na.rm = na.rm)
	res <- res[eff.fac] # need to expand data to original length
	names(res) <- nms
	res[na.x] <- NA
	res <- add_pseries_features(res, i)
	return(res)
}

Sum.matrix.collapse <- function(x, effect, ...) {
# print("Sum.matrix.collapse")
# browser()
		if(is.null(attr(x, "index"))) {
			return(Sum.default(x, effect, ...))
		} else {
			if(!is.character(effect) && length(effect) > 1)
				stop("for matrices with index attributes, the effect argument must be a character")
			if(! effect %in% c("individual", "time", "group"))
				stop("irrelevant effect for a Sum transformation")
		}
		# translate arguments
		dots <- match.call(expand.dots = FALSE)$`...`
		na.rm <- if(is.null(dots[["na.rm"]])) { 
			FALSE } # default of plm's functions
		else { 
			dots[["na.rm"]]
		}
		eff.no <- switch(effect,
										 "individual" = 1L,
										 "time"       = 2L,
										 "group"      = 3L,
										 stop("unknown value of argument 'effect'"))

		xindex <- attr(x, "index")
		eff.fac <- xindex[ , eff.no]
		na.x <- is.na(x)
		res <- collapse::fsum(x, g = eff.fac, w = NULL, na.rm = na.rm, drop = FALSE)
		res <- res[eff.fac, ] # need to expand data to original length
		rownames(res) <- as.character(eff.fac)
		res[na.x] <- NA
		return(res)
}

#### B/between ####

# Need separate implementations of Between.pseries and between.pseries due to different NA handling

Between.default.collapse <- function(x, effect, ...) {
# print("Between.default.collapse")
# browser()
	
	# argument 'effect' is assumed to be a factor
	if(!is.numeric(x)) stop("The Between function only applies to numeric vectors")
	dots <- match.call(expand.dots = FALSE)$`...`
	na.rm <- if(is.null(dots[["na.rm"]])) { 
		FALSE }# default of plm::between 
	else { 
		dots[["na.rm"]]
	}
	nms <- as.character(effect)
	res <- collapse::fbetween(x, g = effect, w = NULL, na.rm = na.rm)
	names(res) <- nms
	return(res)
}

between.default.collapse <- function(x, effect, ...) {
#	print("between.default.collapse")
# browser()
	
	# argument 'effect' is assumed to be a factor
	if(!is.numeric(x)) stop("The Between function only applies to numeric vectors")
	dots <- match.call(expand.dots = FALSE)$`...`
	na.rm <- if(is.null(dots[["na.rm"]])) { 
		FALSE }# default of plm::between 
	else {
		dots[["na.rm"]]
	}
	res <- collapse::fbetween(x, g = effect, w = NULL, na.rm = na.rm, fill = TRUE)
	keep <- !duplicated(effect)
	res <- res[keep]
	names(res) <- as.character(effect[keep])
	return(res)
}

Between.pseries.collapse <- function(x, effect = c("individual", "time", "group"), ...) {
# print("Between.pseries.collapse")
# browser()
	
	# translate arguments
	dots <- match.call(expand.dots = FALSE)$`...`
	effect <- match.arg(effect)
	na.rm <- if(is.null(dots[["na.rm"]])) { 
		FALSE }# default of plm::between 
		else { 
			dots[["na.rm"]]
			}
	
	eff.no <- switch(effect,
						"individual" = 1L,
						"time"       = 2L,
					  "group"      = 3L,
					  stop("unknown value of argument 'effect'"))
	nms <- as.character(index(x)[[eff.no]])
	na.x <- is.na(x)
	# must be fill = TRUE [to catch case when 1 obs of an individual is NA (otherwise result could contain non-intended NA)]
	res <- collapse::fbetween(x, effect = eff.no, w = NULL, na.rm = na.rm, fill = TRUE)
	names(res) <- nms
	res[na.x] <- NA
	return(res)
}

between.pseries.collapse <- function(x, effect = c("individual", "time", "group"), ...) {
# print("fbetween.pseries.collapse")
# browser()
	effect <- match.arg(effect)
	dots <- match.call(expand.dots = FALSE)$`...`
	na.rm <- if(is.null(dots[["na.rm"]])) { 
		FALSE }# default of plm::between 
	else { 
		dots[["na.rm"]]
	}
	eff.no <-	switch(effect,
					 "individual" = 1L,
					 "time"       = 2L,
					 "group"      = 3L,
					 stop("unknown value of argument 'effect'"))
	
	i <- index(x, which = eff.no)
	# use collapse-version
	# must be fill = TRUE [to catch case when 1 obs of an individual is NA
	# (otherwise result could contain non-intended NA)]
	res <- collapse::fbetween(x, effect = eff.no, w = NULL, na.rm = na.rm, fill = TRUE)
	attr(res, "index") <- NULL
	keep <- !duplicated(i)
	res <- res[keep]
	names(res) <- as.character(i[keep])
	return(res)
}



Between.matrix.collapse <- function(x, effect, ...) {
# print("Between.matrix.collapse")
# browser()
	
	if(is.null(attr(x, "index"))) {
		return(Between.default(x, effect, ...))
	} else {
		if(!is.character(effect) && length(effect) > 1L)
			stop("for matrices with index attributes, the effect argument must be a character")
		if(! effect %in% c("individual", "time", "group"))
			stop("irrelevant effect for a between transformation")  
	}	

	# translate arguments
	dots <- match.call(expand.dots = FALSE)$`...`
	na.rm <- if(is.null(dots[["na.rm"]])) { 
		FALSE }# default of plm::between 
	else { 
		dots[["na.rm"]]
	}	
	effect <- if(!is.null(effect)) {
		switch(effect,
					 "individual" = 1L,
					 "time"       = 2L,
					 "group"      = 3L,
					 stop("unknown value of argument 'effect'"))
	} else {
		1L # 1L is default of collapse::fbetween 
	}
	
	i <- attr(x, "index")
	i <- i[ , effect]
	na.x <- is.na(x)
	res <- collapse::fbetween(x, g = i, w = NULL, na.rm = na.rm, fill = TRUE)
	attr(res, "index") <- NULL
	rownames(res) <- as.character(i)
	res[na.x] <- NA
	return(res)
}

between.matrix.collapse <- function(x, effect, ...) {
# print("between.matrix.collapse")
# browser()
	
	if(is.null(attr(x, "index"))) {
		return(between.default(x, effect, ...))
	} else {
		if(!is.character(effect) && length(effect) > 1L)
			stop("for matrices with index attributes, the effect argument must be a character")
		if(! effect %in% c("individual", "time", "group"))
			stop("irrelevant effect for a between transformation")  
	}

	# translate arguments	
	dots <- match.call(expand.dots = FALSE)$`...`
	na.rm <- if(is.null(dots[["na.rm"]])) { 
		FALSE }# default of plm::between 
	else { 
		dots[["na.rm"]]
	}
	effect.no <- if(!is.null(effect)) {
		switch(effect,
					 "individual" = 1L,
					 "time"       = 2L,
					 "group"      = 3L,
					 stop("unknown value of argument 'effect'"))
	} else {
		1L # 1L is default of collapse::fbetween 
	}
	
	i <- attr(x, "index")
	i <- i[ , effect.no]
	res <- collapse::fbetween(x, g = i, w = NULL, na.rm = na.rm, fill = TRUE)
	rownames(res) <- as.character(i)
	res <- res[!duplicated(i), ]
	return(res)
}


#### Within ####
# Within - default

Within.default.collapse <- function(x, effect, ...) {
# print("fwithin.default.collapse")
# browser()
	
		# argument 'effect' is assumed to be a factor
		if(!is.numeric(x)) stop("the within function only applies to numeric vectors")
		dots <- match.call(expand.dots = FALSE)$`...`
		na.rm <- if(is.null(dots[["na.rm"]])) { 
			FALSE }# default of plm::between 
		else { 
			dots[["na.rm"]]
		}
	  res <- collapse::fwithin(x, g = effect, w = NULL, na.rm = na.rm)
		# =(plm)= res <- x - Between(x, effect, ...)
	  names(res) <- as.character(effect)
	  return(res)
}


Within.pseries.collapse <- function(x, effect = c("individual", "time", "group", "twoways"), ...) {
# print("fwithin.pseries.collapse")
# browser()
	
	effect <- match.arg(effect)
	dots <- match.call(expand.dots = FALSE)$`...`
	na.rm <- if(is.null(dots[["na.rm"]])) { 
		FALSE }# default of plm::between 
	else { 
		dots[["na.rm"]]
	}
	if(effect != "twoways") {
		eff.no <- switch(effect,
						 "individual" = 1L,
						 "time"       = 2L,
						 "group"      = 3L,
						 stop("unknown value of argument 'effect'"))
		res <- collapse::fwithin(x, effect = eff.no, w = NULL, na.rm = na.rm, mean = 0)
	} else {
		if(is.pbalanced(x)) {
			# effect = "twoways" - balanced
			res <-   collapse::fwithin( x, effect = 1L, w = NULL, na.rm = na.rm, mean = "overall.mean") -
			         collapse::fbetween(x, effect = 2L, w = NULL, na.rm = na.rm, fill = TRUE)
			      # =(plm)= res <- x - Between(x, "individual", ...) - Between(x, "time", ...) + mean(x, ...)
		} else {
			# effect = "twoways" - unbalanced
			time <- index(x)[[2L]]
			Dmu <- model.matrix(~ time - 1)
			W1 <- collapse::fwithin(x, effect = 1L, w = NULL, na.rm = na.rm, mean = 0) # pseries interface
			WDmu <- collapse::fwithin(Dmu, g = index(x)[[1L]], w = NULL, na.rm = na.rm, mean = 0) # matrix interface
			W2 <- fitted(lm.fit(WDmu, x))
			res <- W1 - W2
		}
	}
	return(res)
}


Within.matrix.collapse <- function(x, effect, rm.null = TRUE, ...) {
# print("fwithin.matrix.collapse")
# browser()
	
	dots <- match.call(expand.dots = FALSE)$`...`
	na.rm <- if(is.null(dots[["na.rm"]])) {
		FALSE }# default of plm::between
	else {
		dots[["na.rm"]]
	}

	if(is.null(attr(x, "index"))) {
		result <- Within.default(x, effect, ...)
		othervar <- colSums(abs(x)) > sqrt(.Machine$double.eps)
		if(rm.null) {
			result <- result[ , othervar, drop = FALSE]
			attr(result, "constant") <- character(0)
		}
		else attr(result, "constant") <- colnames(x)[! othervar]
		return(result)
	}
	else {
		if(effect %in% c("individual", "time", "group")) {
			eff.fac <- switch(effect,
											 "individual" = attr(x, "index")[[1L]],
											 "time"       = attr(x, "index")[[2L]],
											 "group"      = attr(x, "index")[[3L]],
											 stop("unknown value of argument 'effect'"))
			
			result <- collapse::fwithin(x, g = eff.fac, w = NULL, na.rm = na.rm, mean = 0)
			# =(plm)= result <- x - Between(x, effect)
		}
		if(effect == "twoways") {
			xindex <- attr(x, "index")
			if(is.pbalanced(xindex)) {
				# balanced twoways
				eff.ind.fac  <- attr(x, "index")[[1L]]
				eff.time.fac <- attr(x, "index")[[2L]]
				result <- collapse::fwithin( x, g = eff.ind.fac,  w = NULL, na.rm = na.rm, mean = "overall.mean") -
				          collapse::fbetween(x, g = eff.time.fac, w = NULL, na.rm = na.rm, fill = TRUE)
				# =(plm)= result <- x - Between(x, "individual", ...) - Between(x, "time", ...) +
				#                        matrix(colMeans(x, ...), nrow = nrow(x), ncol = ncol(x), byrow = TRUE)
			}
			else { # unbalanced twoways
				time <- index(xindex, "time")
				id <- index(xindex, "individual")
				Dmu <- model.matrix(~ time - 1)
				eff.fac.ind <- attr(x, "index")[[1L]]
				W1   <- collapse::fwithin(x,   g = eff.fac.ind, w = NULL, na.rm = na.rm, mean = 0)
				WDmu <- collapse::fwithin(Dmu, g = eff.fac.ind, w = NULL, na.rm = na.rm, mean = 0)
				W2 <- fitted(lm.fit(WDmu, x))
				result <- W1 - W2
			}
		}
	}
	return(result)
}

#### Here are non-active functions which use lfe::demeanlist for within trans.
#### These are on par in the one-way cases but faster than collapse in the
#### two-way case, esp. in the unbalanced two-way case
#### (collapse 1.4.2, lfe 2.8-5.1).
# Within.pseries.collapse.lfe <- function(x, effect = c("individual", "time", "group", "twoways"), ...) {
# 	# print("fwithin.pseries.collapse")
# 	# browser()
# 	
# 	effect <- match.arg(effect)
# 	dots <- match.call(expand.dots = FALSE)$`...`
# 	na.rm <- if(is.null(dots[["na.rm"]])) { 
# 		FALSE }# default of plm::between 
# 	else { 
# 		dots[["na.rm"]]
# 	}
# 	if(effect != "twoways") {
# 		eff.no <- switch(effect,
# 										 "individual" = 1L,
# 										 "time"       = 2L,
# 										 "group"      = 3L,
# 										 stop("unknown value of argument 'effect'"))
# 		res <- collapse::fwithin(x, effect = eff.no, w = NULL, na.rm = na.rm, mean = 0)
# 	} else {
# 			lfe.avail <- if (!requireNamespace("lfe", quietly = TRUE)) FALSE else TRUE
# 			if(!lfe.avail) stop("twoways fixed effect transformation requires package 'lfe'")
# 			xindex <- attr(x, "index")
# 			eff.list <- list(xindex[[1L]], xindex[[2L]])
# 			res <- unlist(lfe::demeanlist(x, fl = eff.list, na.rm = na.rm))
# 			res <- add_pseries_features(res, i)
# 		}
# 	return(res)
# }

# Within.matrix.collapse.lfe <- function(x, effect, rm.null = TRUE, ...) {
# # print("fwithin.matrix.collapse.lfe")
# # browser()
# 	
# 	dots <- match.call(expand.dots = FALSE)$`...`
# 	na.rm <- if(is.null(dots[["na.rm"]])) {
# 		FALSE }# default of plm::between
# 	else {
# 		dots[["na.rm"]]
# 	}
# 	
# 	if(is.null(attr(x, "index"))) {
# 		result <- Within.default(x, effect, ...)
# 		othervar <- colSums(abs(x)) > sqrt(.Machine$double.eps)
# 		if(rm.null) {
# 			result <- result[ , othervar, drop = FALSE]
# 			attr(result, "constant") <- character(0)
# 		}
# 		else attr(result, "constant") <- colnames(x)[! othervar]
# 		return(result)
# 	}
# 	else {
# 		if(effect %in% c("individual", "time", "group")) {
# 			eff.fac <- switch(effect,
# 												"individual" = attr(x, "index")[[1L]],
# 												"time"       = attr(x, "index")[[2L]],
# 												"group"      = attr(x, "index")[[3L]],
# 												stop("unknown value of argument 'effect'"))
# 			
# 			result <- collapse::fwithin(x, g = eff.fac, w = NULL, na.rm = na.rm, mean = 0)
# 			# =(plm)= result <- x - Between(x, effect)
# 		}
# 		if(effect == "twoways") {
# 			lfe.avail <- if (!requireNamespace("lfe", quietly = TRUE)) FALSE else TRUE
# 			if(!lfe.avail) stop("twoways fixed effect transformation requires package 'lfe'")
# 			xindex <- attr(x, "index")
# 			eff.list <- list(xindex[[1L]], xindex[[2L]])
# 			result <- lfe::demeanlist(x, fl = eff.list, na.rm = na.rm)
# 		}
# 	}
# 	return(result)
# }


#' Function to Switch On/Off Fast Data Transformations
#' 
#' A significant speed up can be gained by using fast (panel) data transformation
#' functions from package `collapse`. By default, this speed up is not enabled
#' and function `pkg.plm.fast` can be used to enable/disable the speed up.
#' 
#' The package `collapse` provides fast data transformation functions written
#' in C/C++, among them some especially suitable for panel data.
#' 
#' By default, package `plm` uses base R implementations and R-based code
#' in from package `plm`. A significant speed-up can be gained by using the
#' functions provided in package `collapse`.
#' To enable the speed-up, execute `pkg.plm.fast(use = TRUE)` once per session.
#' The package will then make use of the faster functions until the session
#' ends or the speed-up is disabled by executing `pkg.plm.fast(use = FALSE)`.
#' 
#' Having package `collapse` installed is a requirement for this function and
#' the speed up. However, this package currently not a hard dependency for
#' package `plm` but a 'Suggests' dependency.
#' 
#' Currently, these functions benefit from the speed-up (more functions are
#' under investigation):
#' \itemize{
#'   \item between,
#'   \item Between,
#'   \item Sum,
#'   \item Within.
#' }
#' 
#' @param use logical, indicating whether the fast data transformations shall
#' be turned on (`TRUE`, the default) or off again (`FALSE`).
#' @param suppressPrint logical (default is `FALSE`), indicating whether the
#' function shall print messages about (de-)activation of the fast functions.
#' @return A logical (`TRUE` if `use = TRUE` was set, `FALSE` if `use = FALSE`),
#' returned invisibly.
#' @seealso [package 'collapse' on CRAN](https://cran.r-project.org/package=collapse)
#' @export
#' @examples
#' \dontrun{
#' ### A benchmark plm without and with speed-up
#' library("plm")
#' library("collapse")
#' library("microbenchmark")
#' rm(list = ls())
#' data("wlddev", package = "collapse")
#' form <- LIFEEX ~ PCGDP + GINI
#'
#' # produce big data set (taken from collapse's vignette)
#' wlddevsmall <- get_vars(wlddev, c("iso3c","year","OECD","PCGDP","LIFEEX","GINI","ODA"))
#' wlddevsmall$iso3c <- as.character(wlddevsmall$iso3c)
#' data <- replicate(100, wlddevsmall, simplify = FALSE)
#' rm(wlddevsmall)
#' uniquify <- function(x, i) {
#' 	x$iso3c <- paste0(x$iso3c, i)
#' 	x
#' }
#' data <- unlist2d(Map(uniquify, data, as.list(1:100)), idcols = FALSE)
#' data <- pdata.frame(data, index = c("iso3c", "year"))
#' pdim(data) # Balanced Panel: n = 21600, T = 59, N = 1274400 // but many NAs
#' # data <- na.omit(data)
#' # pdim(data) # Unbalanced Panel: n = 13300, T = 1-31, N = 93900
#'
#' pkg.plm.fast(use = FALSE) # default: fast functions of 'collapse' not in use
#' times <- 3 # no. of repetitions for benchmark
#' bench_res_plm <- microbenchmark(
#'   plm(form, data = data, model = "within"),
#'   plm(form, data = data, model = "within", effect = "twoways"),
#'   plm(form, data = data, model = "random"),
#'   plm(form, data = data, model = "random", effect = "twoways"),
#'  times = times)
#'
#' pkg.plm.fast(use = TRUE)
#' bench_res_collapse <- microbenchmark(
#'   plm(form, data = data, model = "within"),
#'   plm(form, data = data, model = "within", effect = "twoways"),
#'   plm(form, data = data, model = "random"),
#'   plm(form, data = data, model = "random", effect = "twoways"),
#'  times = times)
#' print(bench_res_plm,      unit = "s")
#' print(bench_res_collapse, unit = "s")
#' }
pkg.plm.fast <- function(use = TRUE, suppressPrint = FALSE) {
  if(use == TRUE) {
    if(!requireNamespace("collapse", quietly = TRUE)) {
      stop(paste("package 'collapse' needed to use fast data transformation functions.",
								 "Please install it, e.g., with 'install.packages(\"collapse\")"),
					 call. = FALSE)
    }

    if(isTRUE(getOption("plm.fast"))) {
       if(!suppressPrint) print("fast data transformations of package 'collapse' were already enabled; enabled again...")
    } else {
      if(!suppressPrint) print("plm will now use package 'collapse' for fast data transformations")
      # set option
      options("plm.fast" = TRUE)
    }
    res <- TRUE

  } else { ## use = FALSE

    if(!isTRUE(getOption("plm.fast"))) {
      if(!suppressPrint) print("fast data transformations of package 'collapse' where already disabled; disabled again...")
    } else {
      if(!suppressPrint) print("disabled using package 'collapse' for fast data transformations")
      # set option
      options("plm.fast" = FALSE)
    }
    res <- FALSE
  }
  invisible(res)
}
