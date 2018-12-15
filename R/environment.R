#' Get environment variables inside a call
#'
#' Returns all variables in a call environment. Made it possible with
#' \code{sys.frames()}, which accesses to the environments in the call stack.
#'
#' @param which integer. the frame number in the call stack.
#' @param exclude character. which arguments should be excluded? It is possible
#' to exclude some variables by name.
#' @export
get_environment <- function(which = 1L, exclude = NA_character_) {
  stopifnot(is.numeric(which))
  stopifnot(is.character(exclude))
  e <- sys.frames()[[which]]
  args <- as.list(e, all.names = TRUE)
  args[names(args)[!names(args) %in% exclude]]
}

