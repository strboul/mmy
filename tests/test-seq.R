
source("test-helpers.R")

test_suite("group sequence", {
  
  x <- c(5, 6, 7, 8, 9, 10, 26, 27, 28, 29, 30, 100, 101, 102)
  is_equal(
    mmy::group_sequence(x),
    list(1:6, 7:11, 12:14)
  )
  
  not_sequenced <- c(6, 8, 10)
  is_equal(
    mmy::group_sequence(not_sequenced),
    list(1L, 2L, 3L)
  )
  
  shuffled <- c(12, 1, 19, 7, 6, 20, 24, 25, 18, 23, 16, 13, 10, 
                11, 4, 15, 5, 9, 22, 17, 21, 2, 8, 14, 3)
  is_equal(
    mmy::group_sequence(shuffled),
    list(1L, 2L, 3L, 4:5, 6L, 7:8, 9L, 10L, 11L, 12L, 13:14, 15L, 
         16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 25L)
  )
  
  seq_repeating <- c(98, 99, 100, 98, 99, 100)
  is_equal(
    mmy::group_sequence(seq_repeating),
    list(1:3, 4:6)
  )
  
  seq_mirror <- c(3, 4, 5, 6, 6, 5, 4, 3)
  is_equal(
    mmy::group_sequence(seq_mirror),
    list(1:4, 5:8)
  )
  
})
