## Example file to test.
## Run `Rscript inst/test-minmax.R`
library(minmax)
library(tinytest)
## A reference function to verify and to check the corner
## cases of the `minmax` function.
basesolution <- function(x) {
    min <- min(x); max <- max(x);
    max_min_diff <- max(x) - min(x)
    vapply(x, function(i) (i - min) / max_min_diff, numeric(1))
}
PrintAndTest <- function(x) {
    cat("basesolution\n"); print(head(basesolution(x)))
    cat("rescale_minmax\n"); print(head(rescale_minmax(x)))
    print(tinytest::expect_equal(basesolution(x), rescale_minmax(x)))
}
# sample <- c(11, 25, 33, 100, 65, 66)
# PrintAndTest(sample)

# ### Testing ####
# cat("***** floating point numbers *****\n")
# x1 <- rnorm(100)
# expect_equal(basesolution(x1), rescale_minmax(x1))

# cat("***** integer input *****\n")
# x2 <- as.integer(c(3, 4, 5))
# expect_equal(basesolution(x2), rescale_minmax(x2))

cat("***** with incomparables *****\n")
x3_1 <- c(NaN, 43, NA, 88)
x3_2 <- c(NA, 43, 88)
x3_3 <- c(NaN, 43, 88)
x3_4 <- c(Inf, 43, 88, -Inf)

## FIXME!
PrintAndTest(x3_1)
PrintAndTest(x3_2)
PrintAndTest(x3_3)
PrintAndTest(x3_4)

# TODO big vectors doesn't work
# cat("***** big vectors *****\n")
# x5 <- rnorm(1e7)
# head(basesolution(x5)); head(rescale_minmax(x5))
# expect_equal(basesolution(x5), rescale_minmax(x5))

#### Benchmarking ####
# bench <- rnorm(1e5)
# microbenchmark::microbenchmark(rescale_minmax(bench), basesolution(bench), check = "equal")
# big_bench <- rnorm(1e7)
# microbenchmark::microbenchmark(rescale_minmax(big_bench), basesolution(big_bench), check = "equal")
