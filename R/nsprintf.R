
#' Named sprintf
#' 
#' @param fmt format string including placeholders between curly braces.
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
nsprintf <- function(fmt, ...) {
  stopifnot(is.character(fmt))
  phs <- c("\\{\\{", "\\}\\}")
  num_ph <- .number_placeholders(fmt, phs)
  if (!.has_valid_placeholders(num_ph)) {
    stop("the number of curly braces don't match")
  }
  args <- list(...)
  args_name <- names(args)
  args_name_n <- length(args_name)
  if (!identical(num_ph, args_name_n)) {
    stop("the number of placeholders and expressions not equal")
  }
  if (num_ph == 0) {
    if (is.null(args_name)) {
      return(sprintf(fmt, ...))
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

.has_valid_placeholders <- function(num_placeholders) {
  if (num_placeholders %% 2L == 0L) TRUE else FALSE
}

.number_placeholders <- function(text, placeholders) {
  lst <- sapply(placeholders, function(ph) regmatches(text, gregexpr(ph, text)))
  len <- length(unlist(lst)) / 2L
  stopifnot(all.equal(len %% 2L, 0L))
  len
}

