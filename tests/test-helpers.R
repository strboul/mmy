
is_error <- function(call) {
  k <- tryCatch(call, error = function(e) e)
  if(is.null(k$message)) {
    stop(deparse(substitute(call)), " didn't throw an error.", call. = FALSE)
  }
}

#' Multiple expect
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
    Actuals[[i]] <- capture.output(eval(parse(text = dx.call)))
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

