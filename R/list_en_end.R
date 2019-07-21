
#' List character vector by comma except the last one ending with 'and' phrase
#' 
#' @param x a vector.
#' @param add.quotes wrap each element with single quotes.
#'
#' @examples 
#' players <- LETTERS[1:10]
#' list_en_end(players)
#' 
#' list_en_end(round(rnorm(5), 3), add.quotes = TRUE)
#' @export
list_en_end <- function(x, add.quotes = FALSE) {
  len <- length(x)
  if (!len > 1) {
    if (add.quotes) {
      return(paste0("'", x, "'"))
    } else {
      return(x)
    }
  }
  sub <- x[seq(len - 1)]
  last <- x[len]
  if (add.quotes) {
    lst <- paste0("'", sub, "'")
    last <- paste0("'", last, "'")
  } else {
    lst <- sub
  }
  with.commas <- paste(lst, collapse = ", ")
  paste(with.commas, "and", last)
}

