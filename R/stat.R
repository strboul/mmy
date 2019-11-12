
#' Calculating sample means from a population
#'
#' @description
#' According to the central limit theorem, the mean of the samples taken from a
#' population would be normally distributed regardless of the distribution of
#' the population.
#'
#' @return
#' This function takes \code{n} number of samples with a size of \code{size} and
#' returns a vector with a length of \code{n}.
#'
#' @param x an atomic numeric vector.
#' @param n number of samples to be taken.
#' @param size sample size to choose.
#' @param replace should sampling done with replacement? default value is FALSE.
#' @param na.rm remove NAs when calculating arithmetic mean. default value is TRUE.
#'
#' @examples
#' data <- runif(1e3)
#' sm <- calculate_sample_means(data, 1e2, 40) # now look at the hist(sm)
#' @export
calculate_sample_means <- function(x, n, size, replace = FALSE, na.rm = TRUE) {
  vapply(seq(n), function(.) {
    s <- sample(x, size, replace = replace)
    m <- mean(s, na.rm = na.rm)
    m
  }, double(1))
}

#' Standard error of the mean
#'
#' @description
#' In the cases that we do not know about the population parameter of variation
#' (which is usually the case), we can take a number of samples from the population
#' and calculate each of these sample means that will always deviate from normal.
#' 
#' @details
#' The standard error of the mean formula is as follows.
#'
#' If the population standard deviation is known:
#' 
#' \deqn{SE_{\overline{x}} = \frac{\sigma}{\sqrt{n}}}
#' 
#' where
#' \eqn{\sigma}: standard deviation of the population
#' \eqn{n}: number of observations of the sample
#'
#' If the population standard deviation is not known, the standard error of the mean
#' is \emph{approximately} equal to the \dQuote{sample standard deviation} divided by
#' the number of observations.
#' 
#' @param x a numeric vector.
#' @export
std_error <- function(x) {
  sd(x) / sqrt(length(x))
}

