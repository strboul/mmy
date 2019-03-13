
is_error <- function(call) {
  k <- tryCatch(call, error = function(e) e)
  if(is.null(k$message)) {
    stop(deparse(substitute(call)), " didn't throw an error.", call. = FALSE)
  }
}
