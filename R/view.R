#' View objects in the browser
#'
#' @param x a generic object.
#' @export
view <- function(x, ...) {
  UseMethod("view", x)
}

#' View a data frame in the browser with reactable
#'
#' Note: Pagination is must for performance for the large tables.
#' @param x a.data.frame or a matrix.
#' @references
#' \url{https://glin.github.io/reactable/articles/examples.html}
#' @export
view.data.frame <- function(x, ...) {
  stopifnot(all(c("reactable", "htmltools") %in% installed.packages()))
  x <- un_rownames(x)
  x_len <- nrow(x)
  call_str <- paste(deparse(as.list(match.call())[[2L]]), collapse = "")
  call_str_trunc <- text_trunc(call_str, 20L)
  time_str <- format(Sys.time(), "%H:%m")
  site_title <- paste0(call_str_trunc, " | ", time_str)
  call_reactable <- function() {
    reactable::reactable(
      x,
      searchable = TRUE,
      filterable = TRUE,
      showSortable = TRUE,
      bordered = TRUE,
      striped = TRUE,
      highlight = TRUE,
      compact = TRUE,
      height = 900,
      pagination = TRUE,
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(10, if (x_len <= 100) x_len else c(100, x_len)),
      defaultPageSize = pmin(100, x_len)
    )
  }
  htmltools::browsable(
    htmltools::tagList(
      htmltools::tags$head(
        htmltools::tags$title(site_title)
      ),
      call_reactable()
    )
  )
}

#' @export
view.matrix <- function(x, ...) {
  x <- as.data.frame(x)
  view.data.frame(x, ...)
}

#' View a list in the browser
#'
#' @references
#' \url{https://cran.r-project.org/package=listviewer}
#' @export
view.list <- function(x, ...) {
  stopifnot("listviewer" %in% installed.packages())
  listviewer::jsonedit(x)
}


#' @export
view.default <- function(x, ...) {
  view.list(x, ...)
}
