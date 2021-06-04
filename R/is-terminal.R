#' Check if session is in an "interactive" terminal
#'
#' @references
#' \url{https://github.com/r-lib/prettycode/blob/7275e2a9f972c8837e070cccaf5ef514643cf607/R/utils.R#L4#L11}
#' @export
is_terminal <- function() {
  interactive() &&
    isatty(stdin()) &&
    Sys.getenv("RSTUDIO") != 1 &&
    Sys.getenv("R_GUI_APP_VERSION") == "" &&
    .Platform$GUI != "Rgui" &&
    !identical(getOption("STERM"), "iESS") &&
    Sys.getenv("EMACS") != "t"
}
