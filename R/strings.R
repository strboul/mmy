
#' Turn a string compatible for machines
#' 
#' Non-compatible file names may create some nuisance in operating systems. This call
#' makes the string name ASCII compatible. Sadly no UTF support (yet). During the
#' convertion, non-ASCII names will be omitted.
#'
#' @param x a \code{character} vector.
#' 
#' @details 
#' Takes the input string and do the following steps: 
#' \itemize{
#' \item lower case
#' \item remove all punctuation (except hyphen)
#' \item add hyphen between characters splited into separate pieces
#' }
#' @examples \dontrun{
#' machine_readable_name(rownames(mtcars))
#' }
#' @export
machine_readable_name <- function(x) {
  if (!is.character(x)) {
    stop("input not a character object")
  }
  vapply(x, function(text) {
    lower <- tolower(text)
    punct <- gsub("[^[:alnum:][:space:]']", "", lower)
    split <- strsplit(punct, split = " ")
    hyphen <- paste(split[[1L]], collapse = "-")
    hyphen
  }, character(1), USE.NAMES = FALSE)
}

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

