
is_error <- function(call) {
  k <- tryCatch(call, error = function(e) e)
  if(is.null(k$message)) {
    stop(deparse(substitute(call)), " didn't throw an error.", call. = FALSE)
  }
}

#' Multiple expect
#' 
#' Memory-wise, C code can be called multiple times to check if any garbage
#' values are returned.
multiple_expect <- function(call, n = 5) {
  
  Actuals <- list()
  for (i in seq(n)) {
    Actuals[[i]] <- capture.output(dput(call))
  }
  
  # compare all:
  for (i in seq_along(Actuals)) {
    for (j in seq_along(Actuals)) {
      tryCatch({
        stopifnot(identical(Actuals[[i]], Actuals[[j]]))
      }, error = function(e) {
        stop(paste(i, j, "not identical"))
      })
    }
  }
  
}
