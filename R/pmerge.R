
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
