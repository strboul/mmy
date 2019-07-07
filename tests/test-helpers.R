
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

#' Multiple expect
#' 
#' Do it for the calls including native code.
#' 
#' @param x.call a valid R call.
#' @param n number of times.
#' @param use.gctorture \code{\link{gctorture}} calls \code{\link{gc}} after every allocation.
#' 
#' @details
#' Although it's safer to check code with a memory management profiler like Valgrind,
#' the C code can be called multiple times to check if any garbage values are
#' returned due to buffer overflow.
#' 
#' \code{\link{gctorture}} makes R hundred times slower. Consider choosing sane
#' values for \code{n} when \code{use.gctorture} is \code{TRUE}.
#' 
#' @examples \dontrun{
#' ## this silently passes!
#' multiple_expect(cat('hi'), n = 5)
#' ## Below will give error when there's no seed set.
#' multiple_expect(rnorm(5), n = 5, use.gctorture = FALSE)
#' }
#' 
#' @importFrom utils capture.output
#' @noRd
multiple_expect <- function(x.call, n = 5, use.gctorture = TRUE) {
  
  stopifnot(is.numeric(n))
  
  if (use.gctorture) {
    gctorture(TRUE)
    on.exit(gctorture(FALSE))
  }
  
  dx.call <- deparse(substitute(x.call))
  
  Actuals <- list()
  for (i in seq(n)) {
    Actuals[[i]] <- utils::capture.output(eval(parse(text = dx.call)))
  }
  
  # compare all:
  for (i in seq_along(Actuals)) {
    for (j in seq_along(Actuals)) {
      tryCatch({
        stopifnot(identical(Actuals[[i]], Actuals[[j]]))
      }, error = function(e) {
        stop(paste(
          "multiple calls are not identical",
          paste(Actuals[[i]], Actuals[[j]], sep = "\n"),
          sep = "\n"
        ),
        call. = FALSE)
      })
    }
  }
  
  ## if everything goes well, return TRUE invisibly:
  invisible(TRUE)
}

# Is user Travis-CI?
IS.TRAVIS <- ifelse(Sys.getenv("USER") == "travis", TRUE, FALSE)

