
# Shiny helper methods ------------------------------

#' Shiny "printf" style tracing for debugging purposes
#'
#' Put it inside the \code{shiny::observe} in case of reactive values are run.
#' Recommended to add the following snippet as \strong{shinyprinf}:
#' \code{observe(mmy::shinyprintf(${1:var}, text = NULL))}.
#'
#' @param x variable.
#' @param text character. (\emph{optional}) description added to the variable
#' name. if empty, the default value is \code{NULL} therefore only the variable
#' name will be used.
#' @param con character. pipe connection. default is \code{stdout}.
#' @importFrom shiny is.reactive
#' @references \url{https://shiny.rstudio.com/articles/debugging.html}
#' @export
shinyprintf <- function(x, text = NULL, con = c("stdout", "stderr")) {
  stopifnot(is.character(text) || is.null(text))
  x.name <- deparse(substitute(x))
  connection <- switch (match.arg(con),
                        "stdout" = stdout(),
                        "stderr" = stderr())
  input <- if (shiny::is.reactive(x)) {
    x()
  } else {
    x
  }
  description <- if (is.null(text)) {
    x.name
  } else {
    paste(text, x.name)
  }
  tms <- sprintf("[%s]", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  output <- sprintf("%s %s: %s", tms, description, input)
  cat(file = connection, output, sep = "\n")
}
