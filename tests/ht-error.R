
# Check ht.data.frame method specifically
ht_df <- mmy:::ht.data.frame

## different input types & lengths:
is_error(ht_df("A"))
is_error(ht_df(c("a", "b", "c")))
is_error(ht_df(100))
is_error(ht_df(c(1,2,3)))
is_error(ht_df(list(x = seq(10))))
is_error(ht_df(c(iris, mtcars)))
is_error(ht_df(iris, "A"))
is_error(ht_df(iris, c("A", "B")))

## Memory tests:
## I'd rather run gctorture on a different process or Travis CI platform rather than
## the current interactive process because it's a long haul.
multiple_expect(mmy::ht(iris), n = 10, use.gctorture = is_on_ci_platform())

