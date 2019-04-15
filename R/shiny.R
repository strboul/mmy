
#' Shiny "printf" style tracing for debugging purposes
#'
#' Put it inside the \code{shiny::observe} in case of reactive values are run.
#' Timestamp displaying date and time is prepended to log messages.
#' Other than debugging purposes, it can also be used as a logger when
#' re-directed to a file. However, the performance is not tested.
#'
#' Recommended to add the following snippet as \strong{shinyprinf}:
#' \code{observe(mmy::shinyprintf(${1:var}, text = NULL))}.
#'
#' @param x variable (static or reactive). See Details.
#' @param text character. (\emph{optional}) description added to the variable name.
#'   if empty, the default value is \code{NULL} therefore only the variable name will
#'   be used.
#' @param con character. Pipe connection. The options are \code{stdout},
#'   \code{stderr} or an absolute or a relative path. Default is \code{stdout}.
#'
#' @details
#' See Shiny's Reactivity to understand what a "reactive variable" is.
#' 
#' @examples \dontrun{
#' x <- 1L
#' shinyprintf(x, "test")
#' ## Persistent logging to a file:
#' shinyprintf("test", con = file.path(getwd(), "file.log"))
#' }
#' @references \url{https://shiny.rstudio.com/articles/debugging.html}
#' @export
shinyprintf <- function(x, text = NULL, con = "stdout") {
  stopifnot(is.character(text) || is.null(text))
  x.name <- deparse(substitute(x))
  connection <- switch (con,
                        "stdout" = stdout(),
                        "stderr" = stderr(),
                        NULL)
  if (is.null(connection))
    stop("faulty connection. Path doesn't work at the moment.")
  input <- if (inherits(x, "reactive")) x() else x # does same as shiny::is.reactive
  description <- if (is.null(text)) x.name else paste("##", text, "##", x.name)
  tms <- sprintf("[%s]", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  output <- sprintf("%s %s: %s", tms, description, input)
  cat(file = connection, output, sep = "\n")
}


