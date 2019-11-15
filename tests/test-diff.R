
test_suite("diff_int", {
  
  ## testing with base diff:
  x1 <- c(674L, 972L, 312L, 503L, 688L, 630L, 788L, 952L, 527L, 837L)
  x2 <- c(116L, 746L, 923L, 772L, 504L, 172L, 628L, 242L, 308L, 591L)
  
  is_identical(mmy::diff_int(x1), base::diff(x1)) 
  is_identical(mmy::diff_int(x2), base::diff(x2)) 
  
  ## fix NAs:
  # z <- c(1L, 2L, NA_integer_, 8L, 11L)
  # is_identical(
  #   mmy::diff_int(z),
  #   base::diff(z)
  # )
  
  ## testing errors:
  is_error(mmy::diff_int(c(100)))
  
})

test_suite("diff_int (memory test)", {

  x <- seq(10)
  y <- sample(10)
  #z <- c(1L, 2L, NA_integer_, 8L, 11L)
  mmy::multiple_expect(diff_int(x), use.gctorture = is_on_ci_platform())
  mmy::multiple_expect(diff_int(y), use.gctorture = is_on_ci_platform())
  #mmy::multiple_expect(diff_int(z), use.gctorture = is_on_ci_platform())
  
})

