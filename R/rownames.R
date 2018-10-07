
#' Dealing with rownames
#'
#' @param x `data.frame`.
#' @details
#' + `un_row_names` removes the row names of a data.frame.
#' + `stnd_row_names` standardizes row names. In other words, remove them and
#' attach as a new column in the `new data.frame`.
#' @examples \dontrun{
#' un_row_names(mtcars)
#' }
#' @name row_names
NULL

#' @rdname row_names
#' @export
un_row_names <- function(x) {
  stopifnot(is.data.frame(x))
  rownames(x) <- NULL
  x
}

#' @rdname row_names
#' @export
stnd_row_names <- function(x) {
  stopifnot(is.data.frame(x))
  x[["rowname"]] <- row.names(x)
  x <- x[c("rowname", setdiff(names(x), "rowname"))]
  un_row_names(x)
}
