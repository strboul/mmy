
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

