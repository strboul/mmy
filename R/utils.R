
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
