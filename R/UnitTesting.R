
#' A minimal unit testing suite
#' @name UnitTesting
NULL

#' @rdname UnitTesting
test_suite <- function(section, ...) {
  if (!is.character(section) && length(section) == 1L) {
    stop("section must be a character")
  }
  cat(sprintf("Testing `%s` ...", section), "\n")
  dots <- list(...)
  invisible(local(dots))
  cat("success\n\n")
}

#' @rdname UnitTesting
is_error <- function(call) {
  k <- tryCatch(call, error = function(e) e)
  if (!"simpleError" %in% class(k)) {
    msg <- sprintf("`%s` %s", deparse(substitute(call)), "did not throw any error.")
    stop(msg, call. = FALSE)
  }
}

#' @rdname UnitTesting
is_identical <- function(actual, expected) {
  res <- identical(actual, expected)
  if (!res) {
    msg <- sprintf("`%s` and `%s` are not identical", 
                   deparse(substitute(actual)), 
                   deparse(substitute(expected))
                   )
    stop(msg, call. = FALSE)
  }
}

#' @rdname UnitTesting
is_equal <- function(actual, expected) {
  res <- all.equal(actual, expected)
  if (!isTRUE(res)) {
    msg <- sprintf("`%s` and `%s` are not equal\n%s", 
                   deparse(substitute(actual)), 
                   deparse(substitute(expected)),
                   res)
    stop(msg, call. = FALSE)
  }
}

