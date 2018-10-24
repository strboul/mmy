
#' Display head and tail of an object together
#'
#' @param x an object.
#' @param n a single integer. number of rows to be displayed per data i.e.
#'   \code{n = 2} means four rows in total.
#' @param visual. logical. default \code{FALSE}. should tables be separated by a
#'   line rule?
#' @importFrom utils head
#' @inheritDotParams head
#' @references \code{\link[utils]{head}} function documentation.
#' @examples \dontrun{
#' headtail(mtcars, n = 2L)
#'
#' ##                mpg cyl disp  hp drat    wt  qsec vs am gear carb
#' ## Mazda RX4     21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
#' ## Mazda RX4 Wag 21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
#' ## Maserati Bora 15.0   8  301 335 3.54 3.570 14.60  0  1    5    8
#' ## Volvo 142E    21.4   4  121 109 4.11 2.780 18.60  1  1    4    2
#' }
#' @export
headtail <- function(x, n = 2L, ...) {
  rbind(
    utils::head(x, n = n, ...),
    utils::tail(x, n = n, ...)
  )
}
