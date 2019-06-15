
#' Truncate longer texts
#'
#' @param x a vector.
#' @param level threshold for truncation.
#' @param sep separator between the three-dots and truncated text.
#' 
#' @examples \dontrun{
#' text_trunc(c("apple", "this is a long text with more letters in it"))
#' text_trunc("The quick brown fox jumps over the lazy dog", level = 20, sep = "%")
#' }
#' @export
text_trunc <- function(x, level = 15, sep = "") {
  as.character(sapply(x, function(t) {
    if (nchar(t) <= level) {
      t
    } else {
      text <- trimws(strtrim(t, level))
      paste(text, "...", sep = sep)
    }
  }))
}

