
source("test-helpers.R")

### ----------------------------------------------------------------- ###
### TEST CODE WRITTEN ONLY IN R ----
### ----------------------------------------------------------------- ###

test_suite("machine_readable_name", {
  
  k <- 1:5
  is_error(mmy::machine_readable_name(k))
  
  is_identical(
    mmy::machine_readable_name(as.character(k)),
    c("1", "2", "3", "4", "5")
  )
  
  is_equal(
    mmy::machine_readable_name(rownames(mtcars)),
    c("mazda-rx4", "mazda-rx4-wag", "datsun-710", "hornet-4-drive", 
      "hornet-sportabout", "valiant", "duster-360", "merc-240d", "merc-230", 
      "merc-280", "merc-280c", "merc-450se", "merc-450sl", "merc-450slc", 
      "cadillac-fleetwood", "lincoln-continental", "chrysler-imperial", 
      "fiat-128", "honda-civic", "toyota-corolla", "toyota-corona", 
      "dodge-challenger", "amc-javelin", "camaro-z28", "pontiac-firebird", 
      "fiat-x1-9", "porsche-914-2", "lotus-europa", "ford-pantera-l", 
      "ferrari-dino", "maserati-bora", "volvo-142e")
  )
  
  ## `machine_readable_name` doesn't change hyphen and underscore:
  is_equal(
    mmy::machine_readable_name("Sub-category files"),
    "sub-category-files"
  )
  
  is_equal(
    mmy::machine_readable_name("Sub_Category_FILES"),
    "sub_category_files"
  )
  
})

test_suite("object_types", {
  is_equal(
    mmy::object_types(1, "a"),
    structure(
      list(
        `__type__` = c("class", "typeof", "mode", "storage.mode",
                       "sexp.type"),
        `__value_1__` = c("numeric", "double", "numeric",
                          "double", "REALSXP"),
        `__value_2__` = c("character", "character",
                          "character", "character", "STRSXP")
      ),
      row.names = c(NA,-5L),
      class = "data.frame",
      substitutes = c("1",
                      "\"a\"")
    )
  )
  
  is_equal(
    mmy::object_types(1, 5L),
    structure(
      list(
        `__type__` = c("class", "typeof", "mode", "storage.mode",
                       "sexp.type"),
        `__value_1__` = c("numeric", "double", "numeric",
                          "double", "REALSXP"),
        `__value_2__` = c("integer", "integer",
                          "numeric", "integer", "INTSXP")
      ),
      row.names = c(NA,-5L),
      class = "data.frame",
      substitutes = c("1",
                      "5L")
    )
  )
  
  is_equal(
    mmy::object_types(as.name("mean")),
    structure(
      list(
        `__type__` = c("class", "typeof", "mode", "storage.mode",
                       "sexp.type"),
        `__value__` = c("name", "symbol", "name", "symbol",
                        "SYMSXP")
      ),
      row.names = c(NA,-5L),
      class = "data.frame",
      substitutes = "as.name(\"mean\")"
    )
  )
  
  is_equal(
    mmy::object_types(`(`),
    structure(
      list(
        `__type__` = c("class", "typeof", "mode", "storage.mode",
                       "sexp.type"),
        `__value__` = c("function", "builtin", "function",
                        "function", "BUILTINSXP")
      ),
      row.names = c(NA,-5L),
      class = "data.frame",
      substitutes = "("
    )
  )
  
  is_equal(
    mmy::object_types(`$`, 1L, `[[<-`),
    structure(
      list(
        `__type__` = c("class", "typeof", "mode", "storage.mode",
                       "sexp.type"),
        `__value_1__` = c("function", "special", "function",
                          "function", "SPECIALSXP"),
        `__value_2__` = c("integer", "integer",
                          "numeric", "integer", "INTSXP"),
        `__value_3__` = c("function",
                          "special", "function", "function", "SPECIALSXP")
      ),
      row.names = c(NA,-5L),
      class = "data.frame",
      substitutes = c("$", "1L", "[[<-")
    )
  )
  
})

test_suite("text_trunc", {
  
  is_equal(
    mmy::text_trunc(c("apple", "this is a long text with more letters in it")),
    c("apple", "this is a long...")
  )
  
  is_equal(
    mmy::text_trunc("The quick brown fox jumps over the lazy dog", 20, sep = " "),
    "The quick brown fox ..."
  )
  
  is_equal(
    mmy::text_trunc("The quick brown fox jumps over the lazy dog", 20, symbol = "***"),
    "The quick brown fox***"
  )
  
})

### ----------------------------------------------------------------- ###
### TLIST TEST ----
### ----------------------------------------------------------------- ###

test_suite("tlist", {
  # TODO
})

