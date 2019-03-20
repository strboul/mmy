
## copy iris data.frame:
df <- iris
df$Species <- as.factor(df$Species)
## create character column:
df$charcol <- rep_len(LETTERS, nrow(df))
## create long factor column:
df$factrcol <- sample(c("short", "long", "x", "y", "z"), nrow(df), replace=TRUE)
df[1, "factrcol"] <- paste(letters, collapse = "")
df$factrcol <- as.factor(df$factrcol)
## create double column (and add some integer-doubles e.g. 1.00000):
df$doubcol <- runif(nrow(df))
even_indices <- seq(2, nrow(df), 2)
df[even_indices, "doubcol"] <- as.integer(runif(length(even_indices), max = 2, min = 0))
## create integer column:
df$intcol <- as.integer(runif(nrow(df), max = 100, min = 0))
## create logical column (appropriate):
df$logicol <- rep_len(c(TRUE, FALSE), length.out = nrow(df))
## whole NA column:
df$nacol <- NA

## -------- Special values --------
## make all second row from bottom NA:
df[149,] <- NA

## control if columns are in correct class, dim etc.:
stopifnot(identical(dim(df), c(150L, 11L)))
stopifnot(
  identical(
    vapply(df, class, FUN.VALUE = character(1)),
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

mmy::ht(df, 2)

# utils::head(df, 2)
