#' Feature scaling: Min-max normalization
#'
#' @param x a numeric vector.
#'
#' @details
#' The formula is as follows:
#' \deqn{
#' X_{scale} = \frac{X_i - X_{min}}{X_{max}-X_{min}}
#' }
#' @examples
#' ## Before rescaling:
#' plot(mtcars$mpg, mtcars$disp)
#' ## After rescaling:
#' plot(rescale_minmax(mtcars$mpg), rescale_minmax(mtcars$disp))
#'
#' @export
rescale_minmax <- function(x) {
  if (is.integer(x)) x <- as.numeric(x)
  .Call(`_RescaleMinMax`, x, PACKAGE = "mmy")
}

