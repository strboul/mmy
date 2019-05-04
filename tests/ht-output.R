
## copy iris data.frame:
DT <- iris
DT$Species <- as.factor(DT$Species)
## create character column:
DT$charcol <- rep_len(LETTERS, nrow(DT))
## create long factor column:
DT$factrcol <- sample(c("short", "long", "x", "y", "z"), nrow(DT), replace=TRUE)
DT[1, "factrcol"] <- paste(letters, collapse = "")
DT$factrcol <- as.factor(DT$factrcol)
## create double column (and add some integer-doubles e.g. 1.00000):
DT$doubcol <- runif(nrow(DT))
even_indices <- seq(2, nrow(DT), 2)
DT[even_indices, "doubcol"] <- as.integer(runif(length(even_indices), max = 2, min = 0))
## create integer column:
DT$intcol <- as.integer(runif(nrow(DT), max = 100, min = 0))
## create logical column (appropriate):
DT$logicol <- rep_len(c(TRUE, FALSE), length.out = nrow(DT))
## whole NA column:
DT$nacol <- NA

## -------- Special values --------
## make all second row from bottom NA:
DT[149,] <- NA

## control if columns are in correct class, dim etc.:
stopifnot(identical(dim(DT), c(150L, 11L)))
stopifnot(
  identical(
    vapply(DT, class, FUN.VALUE = character(1)),
    c(
      Sepal.Length = "numeric",
      Sepal.Width = "numeric",
      Petal.Length = "numeric",
      Petal.Width = "numeric",
      Species = "factor",
      charcol = "character",
      factrcol = "factor",
      doubcol = "numeric",
      intcol = "integer",
      logicol = "logical",
      nacol = "logical"
    )
  )
)

# TODO test output with utils::capture.output() and identical
mmy::ht(DT, 2)
