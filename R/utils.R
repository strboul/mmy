
#' Timestamp comforting to ISO-8601
#'
#' @param tz time zone.
#' @export
tms <- function(tz = "") {
  format(Sys.time(), "%FT%T%z", tz = tz)
}

#' Display head and tail of an object together
#' 
#' TODO on progress....
#' 
#' @param x an object.
#' @param n a single integer. number of rows to be displayed per data i.e.
#'   \code{n = 2} means four rows in total.
#' @param ... arguments to be passed to or from other methods.
#' @examples \dontrun{
#' headtail(iris)
#' }
#' @export
headtail <- function(x, n = 3L, ...) {
  if (!is.data.frame(x)) stop("works only on data.frame", call. = FALSE)
  stopifnot(is.integer(n) || is.double(n))
  
  NROW <- nrow(x)
  NCOL <- ncol(x)
  first <- seq(1L, n)
  last <- seq((NROW - 1L), NCOL)
  
  pattern <- function(col) {
    nchr.max <- nchar(col)
    type <- switch (typeof(col),
                    "integer" = "i",
                    "double" = "d",
                    "character" = "s"
    )
    paste0("%-", nchr.max, type, " ")
  }
  
  # it's enough to look at first row for col data types because of R's recycle
  # rule:
  pats <- vapply(x[1L, ], function(i) {
    pattern(i)
  }, character(1))
  names(pats) <- NULL
  
  available <- c(first, last)
  for (r in seq_along(available) + 1L) {
    # as nrow will always be even as n is multiplied by two:
    if (identical(r-1, ceiling((length(available) + 1L) / 2L))) {
      cat("---")
    } else {
      row <- available[r-1L]
      for (col in seq(NCOL)) {
        cat(sprintf(pats[i], x[row, col]))
      } 
    }
    
    cat("\n")
    
  }
  
}

# Handling factors:
# for (i in dt) {
#   if (is.factor(i)) {
#     print(levels(i))
#   } else {
#     print(i)
#   }
# }


