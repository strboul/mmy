
source("test-helpers.R")

## the base solution is adapted from:
## https://stackoverflow.com/a/5222365/
basesolution <- function(x) cumsum(c(1L, diff(x) != 1L))

test_suite("group_consecutive", {
  x <- c(11L, 12L, 13L, 65L, 66L, 100L)
  is_identical(mmy::group_consecutive(x), basesolution(x))
  y <- c(100L, 101L, 250L, 255L, 256L)
  is_identical(mmy::group_consecutive(y), basesolution(y))
  z <- c(1L, 2L, 3L, 10L, 11L, 12L, 14L)
  is_identical(mmy::group_consecutive(z), basesolution(z))
  allConsecutive <- c(1000L, 1001L, 1002L, 1003L, 1004L, 1005L)
  is_identical(mmy::group_consecutive(allConsecutive), basesolution(allConsecutive))
  missingVals <- c(1L, 2L, 3L, NA, NA, 7L, 11L, NA)
  is_identical(mmy::group_consecutive(missingVals), c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 4L))
  is_error(mmy::group_consecutive(list(1L, 2L)))
})

