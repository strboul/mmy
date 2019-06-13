
#' Dealing with rownames
#' 
#' @details
#' \enumerate{
#' \item \code{un_row_names} removes the row names of a data.frame.
#' \item \code{std_row_names} standardizes row names. In other words, it removes them
#' and attaches as a new column into a new \code{data.frame}.
#' }
#' @examples
#' un_rownames(mtcars)
#' std_rownames(mtcars)
#' @name rownames
NULL

#' @param x data.frame.
#' @rdname rownames
#' @export
un_rownames <- function(x) {
  stopifnot(is.data.frame(x))
  rownames(x) <- NULL
  x
}

#' @param x data.frame.
#' @param rowname the column name for the rownames. Default value is \code{.rowname}.
#' @rdname rownames
#' @export
std_rownames <- function(x, rowname = NULL) {
  stopifnot(is.data.frame(x))
  rowname.column.name <- if (is.null(rowname)) ".rowname" else as.character(rowname)
  x[[rowname.column.name]] <- row.names(x)
  x <- x[c(rowname.column.name, setdiff(names(x), rowname.column.name))]
  un_rownames(x)
}
