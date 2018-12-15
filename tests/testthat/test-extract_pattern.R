
context("testing extract_pattern")

test_that("can test", {

  a <- "Today I ate 2 sandwiches."

  extract_pattern(a, "good")
  extract_pattern(a, "\\d")

  b <- "Today I ate 2 sandwiches and drunk 1 wine."
  # TODO: take both numerics.
  extract_pattern(b, "\\d")

})
