
source("test-helpers.R")

test_suite("list en end", {
  
  is_identical(list_en_end(c("a")), "a")
  is_identical(mmy::list_en_end(c("a"), add.quotes = TRUE), "'a'")
  
  is_identical(mmy::list_en_end(c("a", "b")), "a and b")
  is_identical(mmy::list_en_end(c("a", "b"), add.quotes = TRUE), "'a' and 'b'")
  
  is_identical(mmy::list_en_end(c("a", "b", "c")), "a, b and c")
  is_identical(mmy::list_en_end(c("a", "b", "c"), add.quotes = TRUE), "'a', 'b' and 'c'")
  
  numbers <- c(0.123, 1.516, -1.96, 0.056, 0.152)
  is_identical(
    mmy::list_en_end(numbers, add.quotes = TRUE),
    "'0.123', '1.516', '-1.96', '0.056' and '0.152'"
  )
  
})

