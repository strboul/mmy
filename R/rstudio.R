
#' Paste Output RStudio Addin
#'
#' Why this addin? :: Helps e.g. when writing unit tests.
#' Why wrap it in R6? :: It keeps around tidy!
#'
#' TODO Paste output as comment (a new endpoint)
#' TODO Reformat/reindent the pasted code
#'
#' @importFrom rstudioapi modifyRange insertText getActiveDocumentContext documentSave
#' @importFrom utils capture.output
pasteOutputAddin <- function() {
  pasteOutput$new()
}

pasteOutput <-
  R6::R6Class(

    classname = NULL,

    public = list(

      initialize = function() {

        selected <- private$get_selection_context()
        if (!is.null(selected[["details"]])) {
          output <- private$capture_selected_output(selected[["details"]][["text"]])
          if (!is.null(output)) {
            private$paste_editor(selected, output)
            private$save_current_file(selected)
          }
        }
      }

    ),
    private = list(
      msg.prefix = "pasteOutput:",

      save_current_file = function(context) {

        if (!context[["path"]] == "") {
          rstudioapi::documentSave(context[["id"]])
        }

      },

      capture_selected_output = function(expr) {

        tryCatch({
          utils::capture.output(dput(eval(parse(text = expr))))
        }, error = function(cond) {
          message(paste(
            private$msg.prefix,
            sprintf("\` %s \` cannot be evaluated!", expr),
            "\n",
            conditionMessage(cond)
          ))
          invisible(NULL)
        })

      },

      paste_editor = function(actual, modified) {

        rstudioapi::modifyRange(
          id = actual[["details"]][["id"]],
          location = actual[["details"]][["range"]],
          text = ""
        )

        rstudioapi::insertText(
          id = actual[["details"]][["id"]],
          location = actual[["details"]][["range"]],
          text = paste(modified, collapse = "")
        )

      },

      get_selection_context = function() {

        context <- rstudioapi::getActiveDocumentContext()
        selection <- context[["selection"]]

        lapply(selection, function(s) {

          start <- s[["range"]][["start"]]
          end <- s[["range"]][["end"]]
          if (!identical(s[["text"]], "") && !identical(start, end)) {
            c(range = list(s[["range"]]), text = s[["text"]])
          } else {
            message(paste(private$msg.prefix, "select code in the editor..."))
            invisible(NULL)
          }

        }) -> details

        c(id = context[["id"]], path = context[["path"]], details = details)
      }

    ),
    class = FALSE,
    lock_class = TRUE
  )

# QUICK TEST CONVERT THIS -->
# head(mtcars)
# other text ...
