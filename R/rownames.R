
#' Dealing with rownames
#'
#' @param x a \code{data.frame}.
#' @details
#' \enumerate{
#' \item \code{un_row_names} removes the row names of a data.frame.
#' \item \code{std_row_names} standardizes row names. In other words, it removes them
#' and attaches as a new column into a new \code{data.frame}.
#' }
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
std_row_names <- function(x) {
  stopifnot(is.data.frame(x))
  x[["rowname"]] <- row.names(x)
  x <- x[c("rowname", setdiff(names(x), "rowname"))]
  un_row_names(x)
}
