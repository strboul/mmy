
#' Head and tail of an object
#'
#' Glance on first and last rows of an object
#' in R super quickly.
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
ht <- function(x, n = 5) {
	UseMethod("ht", x)	
}

ht.data.frame <- function(x, n = 5) {
	ret <- .Call("_ht_df", PACKAGE = "mmy", x, n)
	invisible(ret)
}

ht.default <- function(x, n = 5) {
  len <- length(x)
  sec.ind <- len-n+1L
  h <- as.character(x[1L:n])
  t <- as.character(x[sec.ind:len])
  if (len < n*2L) {
    cat("1:", x)
  } else {
    space <- paste(rep(" ", nchar(sec.ind) - 1L), collapse = "")
    dots <- paste(rep(".", 6L), collapse = "")
    cat(paste0(space, "1:"), h, "\n", dots, paste0("\n", sec.ind, ":"), t, sep = " ")
  }
  invisible(NULL)
}

#' @importFrom utils head tail
ht.list <- function(x, n = 5) {
	head(L)
	cat(paste(rep("=", 9L), collapse = ""), sep = "\n")
	tail(L)
	invisible(NULL)
}

