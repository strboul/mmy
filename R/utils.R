
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

#' A primitive way to see if Makevars has debug flags
#' 
#' Most common Makevars flags:
#' CFLAGS, CXXFLAGS, CXX11FLAGS, FFLAGS, FCFLAGS
#' 
#' @noRd
.warn_debug_makevars_flags <- function() {
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
        "Seems like a debug build.",
        "Not the best for benchmarking as compiler optimization is omitted.",
        "Try again after removing debug flags",
        chr.debug.flag,
        "from",
        file
      ),
    call. = FALSE)
}
