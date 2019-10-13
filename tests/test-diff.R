
source("test-helpers.R")

test_suite("diff_int", {
  
  ## testing with base diff:
  x1 <- c(674L, 972L, 312L, 503L, 688L, 630L, 788L, 952L, 527L, 837L)
  x2 <- c(116L, 746L, 923L, 772L, 504L, 172L, 628L, 242L, 308L, 591L)
  x3 <- c(739L, 773L, 764L, 14L, 689L, 821L, 679L, 645L, 501L, 525L)
  x4 <- c(235L, 924L, 28L, 531L, 733L, 499L, 362L, 640L, 349L, 281L)
  x5 <- c(444L, 721L, 856L, 119L, 433L, 963L, 403L, 212L, 567L, 217L)
  x6 <- c(968L, 310L, 672L, 360L, 826L, 293L, 307L, 267L, 713L, 676L)
  x7 <- c(142L, 1000L, 952L, 737L, 520L, 820L, 611L, 992L, 373L, 437L)
  x8 <- c(897L, 35L, 604L, 60L, 390L, 185L, 374L, 694L, 960L, 103L)
  x9 <- c(757L, 111L, 949L, 184L, 603L, 952L, 917L, 635L, 994L, 421L)
  x10 <- c(718L, 4L, 390L, 549L, 79L, 261L, 384L, 311L, 643L, 873L)
  
  is_identical(mmy::diff_int(x1), base::diff(x1)) 
  is_identical(mmy::diff_int(x2), base::diff(x2)) 
  is_identical(mmy::diff_int(x3), base::diff(x3)) 
  is_identical(mmy::diff_int(x4), base::diff(x4)) 
  is_identical(mmy::diff_int(x5), base::diff(x5)) 
  is_identical(mmy::diff_int(x6), base::diff(x6)) 
  is_identical(mmy::diff_int(x7), base::diff(x7)) 
  is_identical(mmy::diff_int(x8), base::diff(x8)) 
  is_identical(mmy::diff_int(x9), base::diff(x9)) 
  is_identical(mmy::diff_int(x10), base::diff(x10))
  
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

