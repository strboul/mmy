
#' Group sequences
#' 
#' Could be either increasing or decreasing sequences.
#' 
#' @param vector a numeric vector with a length of one.
#' @examples 
#' x <- c(16, 17, 18, 19, 100, 99, 98)
#' grps <- group_sequence(x)
#' ## get actual values from indices:
#' sapply(grps, function(g) x[g])
#' ## create a data.frame from groups:
#' data.frame(
#' group = seq_along(grps), 
#' indices = I(grps), 
#' elements = I(sapply(grps, function(g) x[g])),
#' stringsAsFactors = FALSE
#' )
#' @export
group_sequence <- function(vector) {
  stopifnot(is.numeric(vector) && !length(vector) == 1L)
  dif <- diff(vector)
  dif.values <- c(1L, -1L)
  slices <- which(!dif %in% dif.values)
  ## exit function early to omit overhead for the cases that the vector doesn't 
  ## have any sequenced slice(s):
  if (!any(dif %in% dif.values)) {
    return(sapply(seq_along(vector), list))
  }
  slices <- c(slices, length(vector))
  out <- vector("list", length(slices))
  start <- 1L
  for (i in seq_along(out)) {
    slice <- slices[i]
    indices <- seq(start, slice)
    out[[i]] <- indices
    start <- slice + 1L
  }
  out
}

