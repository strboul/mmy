
#' Search a keyword in a list names - fast?
#'
#' Useful when working with large lists. Especially for the content return as
#' JSON type converted into R list type.
#'
#' @param list list object.
#' @param query keyword to look in the list names.
#' @export
search_list_names <- function(list, query) {
  ret <- .Call("_sln", PACKAGE = "mmy", list, query)
  invisible(ret)
}

