
#' Turn a string compatible for system directories
#' 
#' Non-compatible file names may create some nuisance in operating systems. This call
#' makes the string name ASCII compatible. Sadly no UTF support (yet). During the
#' convertion, non-ASCII names will be omitted.
#'
#' @param text \code{character}.
#' 
#' @details 
#' Takes the input string and do the following steps: 
#' \itemize{
#' \item lower case
#' \item remove all punctuation (except hyphen)
#' \item add hyphen between characters splited into separate pieces
#' }
#' @export
turn_system_name_comp <- function(text) {
  if (!is.character(text)) {
    stop("is not a character object: ", text)
  }
  lower <- tolower(text)
  punct <- gsub("[^[:alnum:][:space:]']", "", lower)
  split <- strsplit(punct, split = " ")
  hyphen <- paste(split[[1]], collapse = "-")
  hyphen
}
