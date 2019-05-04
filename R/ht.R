
#' Glance first and last rows
#' 
#' @param x a \code{data.frame}
#' @param n an integer indicating the number of first and last rows to be
#' printed. The default value is \code{5}.
#'
#' @details
#' The objects containing class of \code{data.frame} will
#' call C implementation and the other implementations calls R functions.
#'
#' The call cannot be assigned to any variable, and the result will be
#' \code{NULL (empty)} if it is done it so.
#'
#' @export
ht <- function(x, ...) {
	UseMethod("ht", x)	
}

#' @rdname ht
#' @export
ht.data.frame <- function(x, n = 5) {
	ret <- .Call("_ht_df", PACKAGE = "mmy", x, n)
	invisible(ret)
}

#' @rdname ht
#' @export
ht.default <- function(x, n = 5) {
  len <- length(x)
  if (len < n*2L) {
    cat("1:", x)
  } else {
    sec.ind <- len-n+1L
    h <- as.character(x[1L:n])
    t <- as.character(x[sec.ind:len])
    space <- paste(rep(" ", nchar(sec.ind) - 1L), collapse = "")
    dots <- paste(rep(".", 6L), collapse = "")
    cat(paste0(space, "1:"), h, "\n", dots, paste0("\n", sec.ind, ":"), t, sep = " ")
  }
  invisible(NULL)
}

#' @importFrom utils capture.output
#' @rdname ht
#' @export
ht.list <- function(x) {
  cat(utils::capture.output(x), sep = "\n")
}

