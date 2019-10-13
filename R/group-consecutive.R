
#' Group sequences
#'
#' @description 
#' Could be either increasing or decreasing sequences.
#' The order in the vector matters. There's no ordering done during the computation.
#' 
#' The missing values aren't handled gracefully. Be careful if the input data has 
#' any missing values.
#'
#' @param x a numeric vector with a length of one.
#' @param .early_exit exits function early if there're no consecutive numbers. 
#' This argument is mainly for benchmarking purposes. Default value is \code{TRUE}.
#' @return
#' A list with a number of groups. The vectors in the list elements are the vector
#' \strong{indices}, not values.
#' @examples
#' x <- c(16, 17, 18, 19, 100, 99, 98)
#' group_consecutive(x)
#' ## missing one in the middle:
#' y <- c(seq(1, 3), seq(5, 8))
#' group_consecutive(y)
#' @export
group_consecutive <- function(x, .early_exit = TRUE) {
  stopifnot(is.logical(.early_exit))
  .Call(`_grp_cns`, x, early_exit = .early_exit, PACKAGE = "mmy")
}

