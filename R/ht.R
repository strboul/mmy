
#' Print head-tail
#'
#' @param x an \R object.
#' @param n an integer indicating the number of first and last rows to be
#' printed. The default value is \code{5}.
#'
#' @details
#' The objects containing class of \code{data.frame} will
#' call \code{C} implementation and the other implementations calls \R functions.
#'
#' The call cannot be assigned to any variable, and the result will be
#' \code{NULL (empty)} if it is done it so.
#'
#' @examples 
#' ## data.frame:
#' ht(mtcars)
#' ## vector (default):
#' ht(LETTERS, n = 10)
#' ## list:
#' ht(as.list(letters))
#' @export
ht <- function(x, ...) {
	UseMethod("ht", x)
}

#' @rdname ht
#' @export
ht.data.frame <- function(x, n = 5) {
	invisible(.Call(`_ht_df`, x, n, PACKAGE = "mmy"))
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
  cat("\n")
  invisible(NULL)
}

#' @importFrom utils capture.output
#' @rdname ht
#' @export
ht.list <- function(x, n = 5) {
  out <- utils::capture.output(x)
  i <- n
  head_lines <- local({
    line <- out[i]
    while (line != "") {
      i <- i + 1L
      line <- out[i]
    }
    out[seq(1, i)]
  })
  tail_lines <- local({
    line <- out[length(out) - i]
    while (line != "") {
      i <- i - 1L
      line <- out[length(out) - i]
    }
    out[seq(length(out) - i, length(out))]
  })
  return <- c(head_lines, "[...]", tail_lines)
  returnd <- trimws(paste(return, collapse = "\n"))
  cat(returnd, sep = "\n")
}

