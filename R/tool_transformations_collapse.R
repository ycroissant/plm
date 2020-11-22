## NB: New *.collapse functions/methods need to be (de-)registered in
##     in function pkg.plm.fast(), see bottom of this file.

## Structural changes made to plm's original data transformation functions
## need to be mimicked in the *.collapse versions and vice versa.

# environment to store status about usage of package 'collapse':
pkg.plm.env <- new.env(parent = emptyenv())
pkg.plm.env$use.pkg.collapse <- FALSE # default

# save original functions as backups under *.plm
between.default.plm <- plm:::between.default
between.pseries.plm <- plm:::between.pseries
between.matrix.plm  <- plm:::between.matrix

Between.default.plm <- plm:::Between.default
Between.pseries.plm <- plm:::Between.pseries
Between.matrix.plm  <- plm:::Between.matrix

Within.default.plm <- plm:::Within.default
Within.pseries.plm <- plm:::Within.pseries
Within.matrix.plm  <- plm:::Within.matrix

Sum.default.plm <- plm:::Sum.default
Sum.pseries.plm <- plm:::Sum.pseries
Sum.matrix.plm  <- plm:::Sum.matrix

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
#' Having package `collapse` installed locally, is a requirement for this function.
#' However, this package currently not a hard dependency for package `plm` but
#' a 'Suggests' dependency.
#' 
#' Currently, these functions benefit from the speed-up (more functions are
#' under investigation):
#' \itemize{
#'   \item between,
#'   \item Between,
#'   \item Sum (internal function),
#'   \item Within.
#' }
#' 
#' @param use logical, indicating whether the fast data transformations shall
#' be turned on (`TRUE`, the default) or off (`FALSE`).
#' @param suppressPrint logical (default is `FALSE`), indicating whether the
#' function shall print messages about (de-)activation of the fast functions.
#' @return A logical (`TRUE` if `use = TRUE` was set, `FALSE` if `use = FALSE`),
#' returned invisibly.
#' @seealso [package 'collapse' on CRAN](https://cran.r-project.org/package=collapse)
#' @importFrom utils assignInNamespace
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
		assignInNamespace("between.default.plm", between.default.plm, envir = as.environment("package:plm"))
		assignInNamespace("between.pseries.plm", between.pseries.plm, envir = as.environment("package:plm"))
		assignInNamespace("between.matrix.plm",  between.matrix.plm,  envir = as.environment("package:plm"))
		
		assignInNamespace("Between.default.plm", Between.default.plm, envir = as.environment("package:plm"))
		assignInNamespace("Between.pseries.plm", Between.pseries.plm, envir = as.environment("package:plm"))
		assignInNamespace("Between.matrix.plm",  Between.matrix.plm,  envir = as.environment("package:plm"))
		
		assignInNamespace("Within.default.plm", Within.default.plm, envir = as.environment("package:plm"))
		assignInNamespace("Within.pseries.plm", Within.pseries.plm, envir = as.environment("package:plm"))
		assignInNamespace("Within.matrix.plm",  Within.matrix.plm,  envir = as.environment("package:plm"))
		
		assignInNamespace("Sum.default.plm", Sum.default.plm, envir = as.environment("package:plm"))
		assignInNamespace("Sum.pseries.plm", Sum.pseries.plm, envir = as.environment("package:plm"))
		assignInNamespace("Sum.matrix.plm",  Sum.matrix.plm,  envir = as.environment("package:plm"))
		
		# register collapse versions of functions/methods
		assignInNamespace("between.default", between.default.collapse, envir = as.environment("package:plm"))
		assignInNamespace("between.pseries", between.pseries.collapse, envir = as.environment("package:plm"))
		assignInNamespace("between.matrix",  between.matrix.collapse,  envir = as.environment("package:plm"))
		
		assignInNamespace("Between.default", Between.default.collapse, envir = as.environment("package:plm"))
		assignInNamespace("Between.pseries", Between.pseries.collapse, envir = as.environment("package:plm"))
		assignInNamespace("Between.matrix",  Between.matrix.collapse,  envir = as.environment("package:plm"))
		
		assignInNamespace("Within.default", Within.default.collapse, envir = as.environment("package:plm"))
		assignInNamespace("Within.pseries", Within.pseries.collapse, envir = as.environment("package:plm"))
		assignInNamespace("Within.matrix",  Within.matrix.collapse,  envir = as.environment("package:plm"))
		
		assignInNamespace("Sum.default", Sum.default.collapse, envir = as.environment("package:plm"))
		assignInNamespace("Sum.pseries", Sum.pseries.collapse, envir = as.environment("package:plm"))
		assignInNamespace("Sum.matrix",  Sum.matrix.collapse,  envir = as.environment("package:plm"))
		
		if(pkg.plm.env$use.pkg.collapse == TRUE) {
			if(!suppressPrint) print("fast data transformations of package 'collapse' were already enabled; enabled again...")
		} else {
			if(!suppressPrint) print("plm will now use package 'collapse' for fast data transformations")
			pkg.plm.env$use.pkg.collapse <- TRUE
		}
		res <- TRUE
		
	} else {
		
		# (re-)register original plm versions of functions/methods
		assignInNamespace("between.default", between.default.plm, envir = as.environment("package:plm"))
		assignInNamespace("between.pseries", between.pseries.plm, envir = as.environment("package:plm"))
		assignInNamespace("between.matrix",  between.matrix.plm,  envir = as.environment("package:plm"))
		
		assignInNamespace("Between.default", Between.default.plm, envir = as.environment("package:plm"))
		assignInNamespace("Between.pseries", Between.pseries.plm, envir = as.environment("package:plm"))
		assignInNamespace("Between.matrix",  Between.matrix.plm,  envir = as.environment("package:plm"))
		
		assignInNamespace("Within.default", Within.default.plm, envir = as.environment("package:plm"))
		assignInNamespace("Within.pseries", Within.pseries.plm, envir = as.environment("package:plm"))
		assignInNamespace("Within.matrix",  Within.matrix.plm,  envir = as.environment("package:plm"))
		
		assignInNamespace("Sum.default", Sum.default.plm, envir = as.environment("package:plm"))
		assignInNamespace("Sum.pseries", Sum.pseries.plm, envir = as.environment("package:plm"))
		assignInNamespace("Sum.matrix",  Sum.matrix.plm,  envir = as.environment("package:plm"))
		
		if(pkg.plm.env$use.pkg.collapse == FALSE) {
			if(!suppressPrint) print("fast data transformations of package 'collapse' where already disabled; disabled again...")
		} else {
			if(!suppressPrint) print("disabled using package 'collapse' for fast data transformations")
			pkg.plm.env$use.pkg.collapse <- FALSE
		}
		res <- FALSE
	}
	invisible(res)
}