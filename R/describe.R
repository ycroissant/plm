
# describe function: to extract the characteristics of the plm model
describe <- function(x, what = c('model', 'effect', 'ercomp', 'ivar')){
  cl <- x$call
  what <- match.arg(what)
  switch(what,
         model  = ifelse(!is.null(cl$model), cl$model, "within"),
         effect = ifelse(!is.null(x$effect), x$effect, "individual"),
         ercomp = ifelse(!is.null(cl$ercomp), cl$ercomp, "swar"),
         ivar   = ifelse(!is.null(cl$ivar), cl$ivar, "bvk")
         )
}
