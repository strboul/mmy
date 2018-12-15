
#' Sequence numbers out of object length
#'
#' TODO: Rcpp
#' @param x a vector or a data frame.
#' @export
lsenq <- function(x) {
  UseMethod("lsenq")
}

lsenq.default <- function(x) {
	if (is.numeric(x) || is.character(x)) {
		# TODO
	} else if (is.data.frame(x)) {
		# TODO
	} else {
		stop("cannot compute outlier for an object of class ", class(x))
	}
}

lsenq.data.frame <- function(x) {
  n <- nrow(x)
  seq_len(n)
}
