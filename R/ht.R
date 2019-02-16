
#' Head and tail of a data.frame
#'
#' @param x a \code{data.frame}
#' @param n an integer indicating the number of first and last rows to be
#' printed. The default value is \code{5}.
#'
#' @details
#'
#' Why creating such function? - I want to often glance on first and last rows
#' of a \code{data.frame} in R in a quick way.
#'
#' Why using \emph{R's C API} for such a simple function? - I saw this as an
#' opportunity to start learning R internals.
#'
#' Your implementation isn't the most effective way of doing it. - I don't know
#' about this and it may be correct, but this's not a brain surgery either.
#'
#' @export
ht <- function(x, n = 5) {
	res <- .Call("_ht", PACKAGE = "mmy", x, n)
	invisible(res)
}
