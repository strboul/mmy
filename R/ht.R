
#' Head and tail of a data.frame
#'
#' Glance on first and last rows of a \code{data.frame}
#' in R super quickly.
#' 
#' @param x a \code{data.frame}
#' @param n an integer indicating the number of first and last rows to be
#' printed. The default value is \code{5}.
#'
#' @details
#' The call cannot be assigned to any variable, and the result will be
#' \code{NULL (empty)} if it is done it so.
#'
#' @export
ht <- function(x, n = 5) {
	res <- .Call("_ht", PACKAGE = "mmy", x, n)
	invisible(res)
}
