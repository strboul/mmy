
#' Ask question in the command line
#'
#' @param title the question.
#' @param answers the vector for answers to the question. You may define as many
#' as possible.
#' @return The selected answer as a character vector. If no answer returned,
#' e.g. pressed on Enter immediately after the prompt, a \code{NULL} is returned
#' instead.
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
		answer <- scan("stdin", character(), nlines = 1, quiet = TRUE)
		if (identical(length(answer), 0L) || answer == "") {
			not.answered <- FALSE
			answer <- NULL
		} else if (answer %in% answers) {
			not.answered <- FALSE
		} else {
			cat(paste0("not a valid response: ", answer), "\n\n")
			next
		}
	}
	answer
}


