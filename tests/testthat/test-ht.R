
context("test-ht")

test_that("see head and tail of a data frame", {
  expect_output(
    ht(iris, n = 3),
    "  Sepal.Length Sepal.Width Petal.Length Petal.Width Species
      ------------- ----------- ------------ ----------- -------
    1        5.1         3.5          1.4         0.2    setosa
    2        4.9         3.0          1.4         0.2    setosa
    3        4.7         3.2          1.3         0.2    setosa
  ---
  148        6.5         3.0          5.2         2.0 virginica
  149        6.2         3.4          5.4         2.3 virginica
  150        5.9         3.0          5.1         1.8 virginica
    "
  )
})

