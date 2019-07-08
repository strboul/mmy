
source("test-helpers.R")

## different input types & lengths:
is_error(ht("A"))
is_error(ht(c("a", "b", "c")))
is_error(ht(100))
is_error(ht(c(1,2,3)))
is_error(ht(list(x = seq(10))))
is_error(ht(c(iris, mtcars)))
is_error(ht(iris, "A"))
is_error(ht(iris, c("A", "B")))

## Memory tests:
## I'd rather run gctorture on a different process or Travis CI platform rather than
## the current interactive process because it's a long haul.
multiple_expect(mmy::ht(iris), n = 10, use.gctorture = is_on_ci_platform())

