
context("test-ht")

skip(NULL)

# modified iris adding char column:
iris[["charcol"]] <- rep_len(letters, nrow(iris))

test_that("be sure the iris is modified", {

  expect_s3_class(iris, "data.frame")

  expect_equal(dim(iris), c(150L, 6L))

  expect_is(iris[["Sepal.Length"]], "numeric")
  expect_type(iris[["Sepal.Length"]], "double")

  expect_is(iris[["Sepal.Width"]], "numeric")
  expect_type(iris[["Sepal.Width"]], "double")

  expect_is(iris[["Petal.Length"]], "numeric")
  expect_type(iris[["Petal.Length"]], "double")

  expect_is(iris[["Petal.Width"]], "numeric")
  expect_type(iris[["Petal.Width"]], "double")

  expect_is(iris[["Species"]], "factor")
  expect_type(iris[["Species"]], "integer")

  expect_is(iris[["charcol"]], "character")
  expect_type(iris[["charcol"]], "character")

})

test_that("see head and tail of a data frame", {

  # regular output:
  expect_output(
    ht(iris, n = 3)
		,
    "  Sepal.Length Sepal.Width Petal.Length Petal.Width Species charcol
      ------------- ----------- ------------ ----------- ------- -------
    1        5.1         3.5          1.4         0.2    setosa     a
    2        4.9         3.0          1.4         0.2    setosa     b
    3        4.7         3.2          1.3         0.2    setosa     c
  ---
  148        6.5         3.0          5.2         2.0 virginica     r
  149        6.2         3.4          5.4         2.3 virginica     s
  150        5.9         3.0          5.1         1.8 virginica     t
    "
  )

  # TODO
  # if n is set a little less, equal, and more than the nrow of df:
  ht(iris, n = (nrow(iris) - 1L))

  ht(iris, n = nrow(iris))

  ht(iris, n = (nrow(iris) + 1L))

	# different input types & lengths:
	expect_error(ht("A"))
	expect_error(ht(c("a", "b", "c")))
	expect_error(ht(100))
	expect_error(ht(c(1,2,3)))
	expect_error(ht(list(x = seq(10))))

	expect_error(ht(c(iris, mtcars)))

	expect_error(ht(iris, "A"))
	expect_error(ht(iris, c("A", "B")))

})

