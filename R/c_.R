#' Override base R allowing to put commas at the end (trailing comma)
#'
#' @param ... objects to be passed onto [base::c].
#' @references
#' \url{https://gist.githubusercontent.com/flodel/5283216/raw/a781f7f320bd11e41a2c81affe5bec6f226dbf5e/ok.comma.R}
#'
#' @examples
#' c_("trailing", "commas", "are", "cool", )
#' @export
c_ <- function(...) {
  arg_objects <- as.list(match.call())[-1L]
  len <- length(arg_objects)
  if (len > 1L) {
    last_obj <- arg_objects[[len]]
    if (missing(last_obj)) {
      arg_objects <- arg_objects[-len]
    }
  }
  do.call(base::c, arg_objects)
}
