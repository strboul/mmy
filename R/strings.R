
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
  
  if (!is.character(x)) stop("input not a character object")
  
  ## excluded punctuation:
  excl.punc <- c("-", "_")
  excl.punc.pcre <- paste(paste0("=\\", excl.punc), collapse = "")
  alnum.pcre <- "[:alnum:]"
  space.pcre <- "[:space:]"
  pattern <- sprintf(
    "[^%s%s%s]",
    alnum.pcre,
    excl.punc.pcre,
    space.pcre
  )
  
  ret <- vapply(x, function(text) {
    lower <- tolower(text)
    punct <- gsub(pattern, "", lower, perl = TRUE)
    split <- strsplit(punct, split = " ")
    hyphen <- paste(split[[1L]], collapse = "-")
    hyphen
  }, character(1), USE.NAMES = FALSE)
  
  ret
}

#' Truncate longer texts
#'
#' @param x a vector.
#' @param value threshold for truncation.
#' @param symbol the suffix symbol for the truncation. Default: \code{...}
#' @param sep separator between the three-dots and truncated text.
#' 
#' @examples \dontrun{
#' text_trunc(c("apple", "this is a long text with more letters in it"))
#' text_trunc("The quick brown fox jumps over the lazy dog", 20, sep = " ")
#' }
#' @export
text_trunc <- function(x, value = 15, symbol = "...", sep = "") {
  as.character(sapply(x, function(text) {
    if (nchar(text) <= value) {
      text
    } else {
      trimmed <- trimws(strtrim(text, value))
      paste(trimmed, symbol, sep = sep)
    }
  }))
}

