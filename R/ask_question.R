
#' Ask question in the command line
#'
#' @param title for the question.
#' @param answers the vector for answers for the question. You may include
#' vector elements (answers) as many as possible.
#' @param enterIsValidQuit exits the prompt when an empty answer is provided,
#' e.g. with \emph{Enter}, with a return value of \code{NULL}. Default value for
#' this argument is \code{TRUE}.
#' @return The selected answer as a character vector. If no answer returned and
#' if \code{enterIsValidQuit} is set to \code{TRUE}, a \code{NULL} is returned.
#' @examples \dontrun{
#' ask_question("Do you like me?")
#' ask_question("How much do you like this?", seq(5))
#' }
#' @export
ask_question <- function(title, answers = c("y", "N"),
                         enterIsValidQuit = TRUE) {
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
    answer <- scan("stdin", character(), nlines = 1, quiet = TRUE)
    if (identical(length(answer), 0L) || answer == "") {
      if (enterIsValidQuit) {
        not.answered <- FALSE
        answer <- NULL
      } else {
        cat(paste("not a valid response:", answer), "\n\n")
        next
      }
    } else if (answer %in% answers) {
      not.answered <- FALSE
    } else {
      cat(paste("not a valid response:", answer), "\n\n")
      next
    }
  }
  answer
}


