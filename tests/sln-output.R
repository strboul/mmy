
## Test output from command line
## - assume that `pwd` is the same as the location of the `DESCRIPTION` file)
## - command wrapped between parentheses as it gonna run in a subshell
## (cd tests/ && Rscript --vanilla -e "source('sln-output.R')")
##
## Test output from R
## setwd('tests');source('sln-output.R')
## 

source("test-helpers.R")

## a sample list object (countries):

# countries <- readRDS(file.path("data", "countries.RDS"))
basic <- readRDS(file.path("data", "basic.RDS"))

# mmy::search_list_names(basic, "elephant")
# mmy::search_list_names(basic, "color")

## a list which all elements are not properly named:
# TODO

# cat("==========================\n")
mmy::search_list_names(basic, "name")

