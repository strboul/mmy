
#' Ask question in the command line
#' 
#' @param title the question.
#' @param answers the vector for answers to the question. You may define as many as possible.
#' @details 
#' Press "Enter" to quit ask, which will return \code{NULL}.
#' @examples \dontrun{
#' ask_question("Do you like me?")
#' ask_question("How much do you like this?", seq(5))
#' }
#' @export
ask_question <- function(title, answers = c("y", "N")) {
  if (!is.character(title)) title <- as.character(title)
  if (is.atomic(answers)) {
    if (!length(answers) > 0) {
      stop("'answers' length must be greater than zero")
    }
  } else {
    stop("'answers' must be a atomic vector")
  }
  answers.display <- paste0("[", paste(answers, collapse = "/"), "]")
  not.answered <- TRUE
  while (not.answered) {
    cat(title, "\n")
    cat(answers.display, "\n")
    answer <- readLines(con = stdin(), n = 1L)
    if (answer %in% answers) {
      not.answered <- FALSE
    } else if (answer == "") { ## equals to entering
      not.answered <- FALSE
    } else {
      cat(paste0("not a valid response: ", answer), "\n\n")
      next
    }
  }
  if (!answer == "") {
    answer
  } else {
    invisible(NULL)
  }
}
