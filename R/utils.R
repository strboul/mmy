
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
  cat(paste0("\033[1m\033[33m", ..., "\033[39m\033[22m"), "\n")
}

#' A primitive way to see if Makevars has debug flags
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
