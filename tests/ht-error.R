
source("test-helpers.R")

### different input types & lengths:
is_error(ht("A"))
is_error(ht(c("a", "b", "c")))
is_error(ht(100))
is_error(ht(c(1,2,3)))
is_error(ht(list(x = seq(10))))
is_error(ht(c(iris, mtcars)))
is_error(ht(iris, "A"))
is_error(ht(iris, c("A", "B")))
