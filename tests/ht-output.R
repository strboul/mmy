
df <- iris
df$Species <- as.character(df$Species)
df[1, "Species"] <- paste(letters, collapse = "")
df$charcol <- rep_len(LETTERS, nrow(df))

mmy::ht(df, 2)

utils::head(df, 2)
