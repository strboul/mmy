
#' Lagged differences for the integer values
#'
#' @param x an integer vector.
#'
#' @return
#' Number of integers which equals to the length of the \code{x} minus one.
#' @examples 
#' diff_int(as.integer(c(1, 5, 3)))
#' @export
diff_int <- function(x) {
  .Call(`_diff_int`, x, PACKAGE = "mmy")
}

