
## a sample list object (countries):

file <- file.path("tests", "data", "countries.json")
txt <- paste(readLines(file), collapse = "")

## a list all elements are properly named:
ALL.NAMED.LIST <- jsonlite::fromJSON(txt)

mmy::search_list_names(ALL.NAMED.LIST, "name")

## a list which all elements are not properly named:
# TODO
