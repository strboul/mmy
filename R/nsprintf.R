
#' Named sprintf
#' 
#' @param fmt format string including placeholders between shapes.
#' @param phs placeholder shapes.
#' @param ... the expressions used in placeholders.
#' 
#' @examples
#' nsprintf(
#'   "My name is {{name}}. I am {{number}} years old.",
#'   number = "25",
#'   name = "Apple"
#' )
#' @seealso
#' \href{https://cran.r-project.org/package=glue}{glue} and 
#' \href{https://cran.r-project.org/package=whisker}{whisker} packages.
#' @references 
#' Inspired from here:
#' \url{https://stackoverflow.com/a/55423080}
#' @export
nsprintf <- function(fmt, ..., phs = c("\\{\\{", "\\}\\}")) {
  stopifnot(is.character(fmt))
  stopifnot(is.atomic(phs) && is.character(phs))
  if (!identical(length(phs), 2L)) {
    stop("the length of phs must be two")
  }
  PH <- .extract_placeholders(fmt, phs)
  PH_uniq <- unique(PH)
  PH_len <- length(PH)
  PH_uniq_len <- length(PH_uniq)
  args <- list(...)
  args_name <- names(args)
  args_name_len <- length(args_name)
  if (is.null(args_name)) {
    if (length(args) > 0) {
      stop("expressions must be named")
    }
  }
  if (!identical(PH_uniq_len, args_name_len)) {
    stop("the number of placeholders and expressions not equal")
  }
  if (PH_uniq_len == 0) {
    if (is.null(args_name)) {
      return(fmt)
    } else {
      stop("no curly braces found for the args provided")
    }
  }
  for(i in seq_along(args_name)) {
    if(!length(args_name[i]) > 0L) {
      next
    }
    numbered.arg <- paste("%", i, "$s", sep = "")
    placeholder <- paste("{{", args_name[i], "}}", sep = "")
    fmt <- gsub(placeholder, numbered.arg, fmt, fixed = TRUE)
  }
  do.call(sprintf, append(args, fmt, 0))
}

.extract_placeholders <- function(fmt, phs) {
  pattern <- paste0(phs[1L], "(.*?)", phs[2L])
  match <- regmatches(fmt, gregexpr(pattern, fmt))
  unlist(match)
}
