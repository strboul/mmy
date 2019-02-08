
#' Turn a string compatible for directories
#'
#' @param text \code{character}.
#' @details Takes the input string and do the following steps: turn it into
#'   lower case, remove all punctuation, add hyphen between characters splited
#'   into separate pieces.
#' @export
comp_text <- function(text) {
  if (!is.character(text)) {
    stop("is not a character object: ", text)
  }
  lower <- tolower(text)
  punct <- gsub("[^[:alnum:][:space:]']", "", lower)
  split <- strsplit(punct, split = " ")
  hyphen <- paste(split[[1]], collapse = "-")
  hyphen
}
