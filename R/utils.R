
#' Timestamp comforting to ISO-8601 format
#'
#' @param tz time zone.
#' @param offset Add UTC offset hours at the end. Default is \code{TRUE}.
#'
#' @export
tms <- function(tz = "", offset = TRUE) {
  utc <- ifelse(offset, "%z", "")

  format.str <- paste("%FT%T", utc, sep = "")
  format(Sys.time(), format.str, tz = tz)
}

#' Cat bold and yellow text to console
#'
#' Yellow is a nice distinct color and sometimes I need it especially when I am in
#' the middle of a development.
#'
#' @param ... a valid \R object.
#' @export
catby <- function(...) {
	fmt <- tryCatch(paste0("\033[1m\033[33m", ..., "\033[39m\033[22m "),
					 error = function(e) stop(e[["message"]], call. = FALSE))
  cat(fmt, "\n")
}

ANSI_STYLES <-
  list(
    reset = list(c0 = "\033[0m"),
    color = list(
      red = "\033[31m",
      green = "\033[32m",
      yellow = "\033[33m",
      blue = "\033[34m",
      white = "\033[37m"
    ),
    style = list(bold = "\033[1m")
  )

#' Cat with ANSI styles
#'
#' @param color color name.
#' @param style style name.
#' @param ... arguments passed by \code{\link{cat}}.
#'
#' @rawRd
#' \section{Available options for the parameters}{
#' \subsection{for the \sQuote{color} parameter}{
#' \Sexpr[stage=install,results=rd]{
#' mmy::list_en_end(names(mmy:::ANSI_STYLES[["color"]]), add.quotes = TRUE)
#' }
#' }
#' \subsection{for the \sQuote{style} parameter}{
#' \Sexpr[stage=install,results=rd]{
#' mmy::list_en_end(names(mmy:::ANSI_STYLES[["style"]]), add.quotes = TRUE)
#' }
#' }
#' }
#' @export
catcl <- function(..., color = NULL, style = NULL) {
	if (is.null(color) && is.null(style)) {
				return(cat(...))
	}
	s <- ANSI_STYLES
	txt <- paste(paste0(s[["color"]][[color]], s[["color"]][[color]]),
	             ...,
	             s[["reset"]][["c0"]], sep = "")
	cat(txt)
}


#' A primitive way to see if Makevars has debug flags
#'
#' @description
#' The location of Makevars file is platform-specific. Use
#' \code{tools::makevars_user()} to locate it in your system.
#'
#' @details
#' The most common \emph{Makevars} flags for \R:
#' \itemize{
#' \item \code{CFLAGS}
#' \item \code{CXXFLAGS}
#' \item \code{CXX11FLAGS}
#' \item \code{FFLAGS}
#' \item \code{FCFLAGS}
#' }
#' For debugging purposes, it's recommended to have the following flags:
#' \code{CC=clang \n CFLAGS=-g -O0}
#'
#' @importFrom tools makevars_user
#' @export
warn_debug_makevars_flags <- function() {
  file <- tools::makevars_user()
  contents <- readLines(file)
  contents.clean <- gsub("#[^\n]*", "", contents) # stripping comments
  if (!unlist(gregexpr("=", contents.clean)) > 0) {
    return (invisible(NULL))
  }
  flags <- strsplit(contents.clean, "=")
  val <- sapply(seq_along(flags), function(i) flags[[i]][[2L]])
  collap_val <- paste(val, collapse = " ")
  chr.debug.flag <- "-O0"
  if (grepl(chr.debug.flag, paste(val, collapse = "")))
    warning(
      paste(
        "Seems you are using some debug build flags in the Makevars file.",
        "It's not the best e.g. for benchmarking as compiler optimization is omitted.",
        paste0(
          "Try again after removing debug flags ",
          "`", chr.debug.flag, "`",
          " from ",
          "`", file, "`"
        ),
        sep = "\n"
      ),
    call. = FALSE)
}

#' Checks objects multiple times to see whether the produced effects are resilent
#'
#' The output is called multiple times, and each time the output should be the same.
#' Especially do it for the calls running native code, best for detecting buffer
#' overflows, memory segfaults, and so on.
#'
#' @param x.call a valid \R call.
#' @param n number of times.
#' @param use.gctorture \code{\link{gctorture}} calls \code{\link{gc}} after every
#'   allocation.
#'
#' @details
#' Although it's safer to check code with a memory management profiler like
#' \emph{Valgrind}, the \code{C} code can be called multiple times to check if any
#' garbage values are returned due to buffer overflow.
#'
#' \code{\link{gctorture}} makes \R hundred times slower. Consider choosing sane
#' values for \code{n} when \code{use.gctorture} is \code{TRUE}.
#'
#' @examples \dontrun{
#' ## this silently passes!
#' multiple_expect(cat('hi'), n = 5)
#' ## Below will give error when no seed is set.
#' multiple_expect(rnorm(5), n = 5, use.gctorture = FALSE)
#' }
#' @importFrom utils capture.output
#' @export
multiple_expect <- function(x.call, n = 5, use.gctorture = FALSE) {

  stopifnot(is.numeric(n))

  if (use.gctorture) {
    gctorture(TRUE)
    on.exit(gctorture(FALSE), add = TRUE)
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


#' Is the \R session running on a CI (continious integration) platform?
#'
#' @export
is_on_ci_platform <- function() {
  if (Sys.getenv("USER") == "travis" ||
      ifelse(Sys.getenv("APPVEYOR") == "", FALSE, TRUE))
    TRUE
  else
    FALSE
}

