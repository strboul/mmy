
#' One-dimensional plot
#'
#' @description
#' This type of graph provides a batteries-included solution to plot one
#' dimensional graphs, which is also production ready.
#'
#' @param x a numeric vector.
#' @param highlight highlight
#' @param title title
#' @param subtitle subtitle
#' @param display.max.min display.max.min
#' @param labels labels
#' @param labels.at labels.at
#' @param labels.round round the displayed min and max values to 2 after
#'   decimals.
#' @param ablines.at ablines.at
#' @param linecolor linecolor
#' @param points.color points.color
#' @param alpha.points alpha.points
#' @param highlight.color highlight.color
#' @param abline.color abline.color
#' @param points.pch.type points.pch.type
#' @param box box
#' @param grid add gridlines.
#'
#' @examples 
#' ## simple vector:
#' x <- c(-0.1, 0.34, 0.55, 0.95)
#' plot_1d(x)
#' ## with highlighting:
#' unif <- runif(1e2)
#' cond <- unif > 0.3 & unif < 0.5
#' plot_1d(
#'   x = unif,
#'   highlight = cond,
#'   alpha.points = TRUE,
#'   labels.at = mean(unif[cond]),
#'   title = "Uniform distribution",
#'   subtitle = "Subset values and the subset mean represented in red",
#'   highlight.color = "orange",
#'   box = FALSE,
#'   grid = TRUE
#' )
#' ## multiple highlights:
#' norm <- rnorm(1e2)
#' cond1 <- norm < -0.8 & norm > -1.2
#' cond2 <- norm > 0.7 & norm < 1.0
#' cond1.mean <- mean(norm[cond1])
#' cond2.mean <- mean(norm[cond2])
#' plot_1d(
#'   x = norm,
#'   highlight = list(cond1, cond2),
#'   alpha.points = TRUE,
#'   labels.at = list(cond1.mean, cond2.mean),
#'   ablines.at = list(cond1.mean, cond2.mean),
#'   highlight.color = list("steelblue", "red"),
#'   title = "Normal distribution",
#'   subtitle = "Peak mean values in groups",
#'   box = FALSE,
#'   grid = TRUE
#' )
#' @details
#' One-dimensional graphs are useful in the cases:
#' \itemize{
#' \item plotting regression residuals,
#' \item plotting clustered data (e.g. k-means)
#' \item various coefficients etc.
#' }
#' @export
plot_1d <- function(x,
                    highlight = NULL,
                    title = NULL,
                    subtitle = NULL,
                    display.max.min = TRUE,
                    labels = TRUE,
                    labels.at = range(x),
                    labels.round = TRUE,
                    ablines.at = NULL,
                    linecolor = "lightgray",
                    points.color = "darkgrey",
                    alpha.points = FALSE,
                    highlight.color = "red",
                    abline.color = highlight.color,
                    points.pch.type = 21,
                    box = TRUE,
                    grid = FALSE)
{
  stopifnot(is.numeric(x))
  if (is.list(labels.at)) {
    labels.at <- do.call(c, labels.at)
  }
  plot.specs.at <- if (labels.round) round(labels.at, 2L) else labels.at
  len.prox <- rep(0L, length(x))
  plot(
    x,
    len.prox,
    type = "l",
    col = linecolor,
    axes = FALSE,
    xlab = "",
    ylab = "",
    xaxt = "n"
  )
  mtext(title, side = 3, line = 1, cex = 1.2)
  mtext(subtitle, side = 3, cex = 0.8)
  axis.labels <- if (display.max.min) TRUE else FALSE
  axis(
    side = 1,
    at = plot.specs.at,
    tick = FALSE,
    labels = axis.labels,
    line = -4
  )
  points <- list(
    x = x,
    len.prox = len.prox,
    points.pch.type = points.pch.type,
    points.color = points.color,
    col = gray(.0, .1)
  )
  if (!alpha.points) {
    points(
      x = points$x,
      y = points$len.prox,
      pch = points$points.pch.type,
      bg = points$points.color
    )
  } else {
    points(
      x = points$x,
      y = points$len.prox,
      pch = 16, # make pch "filled circles" if alpha applied --looks better
      bg = points$points.color,
      col = points$col
    )
  }
  if (!is.null(highlight)) {
    if (is.list(highlight)) {
      for (h in seq_along(highlight)) {
        hg <- highlight[[h]]
        x.hg <- x[hg]
        hg.color <- highlight.color[[h]]
        points(
          x = x.hg,
          y = rep(0L, length(x.hg)),
          pch = points.pch.type,
          bg = hg.color
        )
      }
    } else if (is.logical(highlight)) {
      points(
        x = x[highlight],
        y = rep(0L, length(x[highlight])),
        pch = points.pch.type,
        bg = highlight.color
      )
    } else {
      stop("provided non-logical or non-list object to the highlight argument")
    }
  }
  if (!is.null(ablines.at)) {
    if (is.list(ablines.at)) {
      for (a in seq_along(ablines.at)) {
        ab <- ablines.at[[a]]
        x.ab <- x[ab]
        ab.color <- abline.color[[a]]
        abline(v = ab, lty = 2, col = ab.color)
      }
    } else if (is.logical(ablines.at)) {
      abline(v = ablines.at, lty = 2, col = abline.color)
    } else {
      stop("provided non-logical or non-list object to the ablines.at argument")
    }
  }
  if (box) box()
  if (grid) grid()
}

# TODO next feature can be some facetted line-by-line dots

