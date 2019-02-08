
context("test-utils")

test_that("headtail", {
  
  dt <- data.frame(
    a = c(1, 2, 3, 4, 5, 6, 7),
    b = c(11, 12, 13, 14, 15, 16, 17),
    c = c(101, 102, 103, 104, 105, 106, 107),
    d = c("a", "b", "c", "d", "x", "y", "z"),
    stringsAsFactors = FALSE
  )

  expect_output(
    headtail(dt),
    "
        a  b  c  d
    1:  1 11 101 a
    2:  2 12 102 b
  ---
    6:  6 16 106 y
    7:  7 17 107 z
    "
  )
    
})
