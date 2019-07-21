
source("test-helpers.R")

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
  
  ## Detailed tests due to manipulating expressions under the hood of
  ## `object_types()`.
  
  o1 <- mmy::object_types(1, "a")
  
  is_equal(o1[o1[["__type__"]] == "class", "__value_1__"], class(1))
  is_equal(o1[o1[["__type__"]] == "typeof", "__value_1__"], typeof(1))
  is_equal(o1[o1[["__type__"]] == "mode", "__value_1__"], mode(1))
  is_equal(o1[o1[["__type__"]] == "storage.mode", "__value_1__"], storage.mode(1))
  is_equal(o1[o1[["__type__"]] == "sexp.type", "__value_1__"], mmy::sexp.type(1))
  
  is_equal(o1[o1[["__type__"]] == "class", "__value_2__"], class("a"))
  is_equal(o1[o1[["__type__"]] == "typeof", "__value_2__"], typeof("a"))
  is_equal(o1[o1[["__type__"]] == "mode", "__value_2__"], mode("a"))
  is_equal(o1[o1[["__type__"]] == "storage.mode", "__value_2__"], storage.mode("a"))
  is_equal(o1[o1[["__type__"]] == "sexp.type", "__value_2__"], mmy::sexp.type("a"))
  
  is_equal(o1,
           structure(
             list(
               `__type__` = c("class", "typeof", "mode", "storage.mode",
                              "sexp.type"),
               `__value_1__` = c("numeric", "double", "numeric",
                                 "double", "REALSXP"),
               `__value_2__` = c("character", "character",
                                 "character", "character", "STRSXP")
             ),
             row.names = c(NA, -5L),
             class = "data.frame",
             substitutes = c("1",
                             "\"a\"")
           ))
  
  o2 <- mmy::object_types(1, 5L)
  
  is_equal(o2[o2[["__type__"]] == "class", "__value_1__"], class(1))
  is_equal(o2[o2[["__type__"]] == "typeof", "__value_1__"], typeof(1))
  is_equal(o2[o2[["__type__"]] == "mode", "__value_1__"], mode(1))
  is_equal(o2[o2[["__type__"]] == "storage.mode", "__value_1__"], storage.mode(1))
  is_equal(o2[o2[["__type__"]] == "sexp.type", "__value_1__"], mmy::sexp.type(1))

  is_equal(o2[o2[["__type__"]] == "class", "__value_2__"], class(5L))
  is_equal(o2[o2[["__type__"]] == "typeof", "__value_2__"], typeof(5L))
  is_equal(o2[o2[["__type__"]] == "mode", "__value_2__"], mode(5L))
  is_equal(o2[o2[["__type__"]] == "storage.mode", "__value_2__"], storage.mode(5L))
  is_equal(o2[o2[["__type__"]] == "sexp.type", "__value_2__"], mmy::sexp.type(5L))
  
  is_equal(
    o2,
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
  
  o3 <- mmy::object_types(as.name("mean"))
  
  is_equal(o3[o3[["__type__"]] == "class", "__value__"], class(as.name("mean")))
  is_equal(o3[o3[["__type__"]] == "typeof", "__value__"], typeof(as.name("mean")))
  is_equal(o3[o3[["__type__"]] == "mode", "__value__"], mode(as.name("mean")))
  is_equal(o3[o3[["__type__"]] == "storage.mode", "__value__"], storage.mode(as.name("mean")))
  is_equal(o3[o3[["__type__"]] == "sexp.type", "__value__"], mmy::sexp.type(as.name("mean")))
  
  is_equal(
    o3,
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
  
  o4 <- mmy::object_types(`(`)
  
  is_equal(o4[o4[["__type__"]] == "class", "__value__"], class(`(`))
  is_equal(o4[o4[["__type__"]] == "typeof", "__value__"], typeof(`(`))
  is_equal(o4[o4[["__type__"]] == "mode", "__value__"], mode(`(`))
  is_equal(o4[o4[["__type__"]] == "storage.mode", "__value__"], storage.mode(`(`))
  is_equal(o4[o4[["__type__"]] == "sexp.type", "__value__"], mmy::sexp.type(`(`))
  
  is_equal(
    o4,
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
  
  opt <- mmy::object_types(`$`, 1L, `[[<-`)
  
  is_equal(opt[opt[["__type__"]] == "class", "__value_1__"], class(`$`))
  is_equal(opt[opt[["__type__"]] == "typeof", "__value_1__"], typeof(`$`))
  is_equal(opt[opt[["__type__"]] == "mode", "__value_1__"], mode(`$`))
  is_equal(opt[opt[["__type__"]] == "storage.mode", "__value_1__"], storage.mode(`$`))
  is_equal(opt[opt[["__type__"]] == "sexp.type", "__value_1__"], mmy::sexp.type(`$`))
  
  is_equal(opt[opt[["__type__"]] == "class", "__value_2__"], class(1L))
  is_equal(opt[opt[["__type__"]] == "typeof", "__value_2__"], typeof(1L))
  is_equal(opt[opt[["__type__"]] == "mode", "__value_2__"], mode(1L))
  is_equal(opt[opt[["__type__"]] == "storage.mode", "__value_2__"], storage.mode(1L))
  is_equal(opt[opt[["__type__"]] == "sexp.type", "__value_2__"], mmy::sexp.type(1L))
  
  is_equal(opt[opt[["__type__"]] == "class", "__value_3__"], class(`[[<-`))
  is_equal(opt[opt[["__type__"]] == "typeof", "__value_3__"], typeof(`[[<-`))
  is_equal(opt[opt[["__type__"]] == "mode", "__value_3__"], mode(`[[<-`))
  is_equal(opt[opt[["__type__"]] == "storage.mode", "__value_3__"], storage.mode(`[[<-`))
  is_equal(opt[opt[["__type__"]] == "sexp.type", "__value_3__"], mmy::sexp.type(`[[<-`))
  
  is_equal(
    opt,
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
  
  ofor <- mmy::object_types(quote(for (i in seq(5L)) i))
  
  is_equal(ofor[ofor[["__type__"]] == "class", "__value__"], class(quote(for (i in seq(5L)) i)))
  is_equal(ofor[ofor[["__type__"]] == "typeof", "__value__"], typeof(quote(for (i in seq(5L)) i)))
  is_equal(ofor[ofor[["__type__"]] == "mode", "__value__"], mode(quote(for (i in seq(5L)) i)))
  is_equal(ofor[ofor[["__type__"]] == "storage.mode", "__value__"], storage.mode(quote(for (i in seq(5L)) i)))
  is_equal(ofor[ofor[["__type__"]] == "sexp.type", "__value__"], mmy::sexp.type(quote(for (i in seq(5L)) i)))
  
  is_equal(
    ofor,
    structure(
      list(
        `__type__` = c("class", "typeof", "mode", "storage.mode",
                       "sexp.type"),
        `__value__` = c("for", "language", "call", "language",
                        "LANGSXP")
      ),
      row.names = c(NA,-5L),
      class = "data.frame",
      substitutes = "quote(for (i in seq(5L)) i)"
    )
  )
  
})

test_suite("check_language_object_types", {
  
  quo <- quote(x <- 2)
  ot <- mmy::check_language_object_types(quo, quo[[1]])
  
  is_equal(ot[ot[["__type__"]] == "is.list", "__value_1__"], is.list(quo))
  is_equal(ot[ot[["__type__"]] == "is.expression", "__value_1__"], is.expression(quo))
  is_equal(ot[ot[["__type__"]] == "is.name", "__value_1__"], is.name(quo))
  is_equal(ot[ot[["__type__"]] == "is.symbol", "__value_1__"], is.symbol(quo))
  is_equal(ot[ot[["__type__"]] == "is.call", "__value_1__"], is.call(quo))
  is_equal(ot[ot[["__type__"]] == "is.function", "__value_1__"], is.function(quo))
  is_equal(ot[ot[["__type__"]] == "is.primitive", "__value_1__"], is.primitive(quo))
  is_equal(ot[ot[["__type__"]] == "is.pairlist", "__value_1__"], is.pairlist(quo))
  is_equal(ot[ot[["__type__"]] == "is.language", "__value_1__"], is.language(quo))
  
  is_equal(ot[ot[["__type__"]] == "is.list", "__value_2__"], is.list(quo[[1]]))
  is_equal(ot[ot[["__type__"]] == "is.expression", "__value_2__"], is.expression(quo[[1]]))
  is_equal(ot[ot[["__type__"]] == "is.name", "__value_2__"], is.name(quo[[1]]))
  is_equal(ot[ot[["__type__"]] == "is.symbol", "__value_2__"], is.symbol(quo[[1]]))
  is_equal(ot[ot[["__type__"]] == "is.call", "__value_2__"], is.call(quo[[1]]))
  is_equal(ot[ot[["__type__"]] == "is.function", "__value_2__"], is.function(quo[[1]]))
  is_equal(ot[ot[["__type__"]] == "is.primitive", "__value_2__"], is.primitive(quo[[1]]))
  is_equal(ot[ot[["__type__"]] == "is.pairlist", "__value_2__"], is.pairlist(quo[[1]]))
  is_equal(ot[ot[["__type__"]] == "is.language", "__value_2__"], is.language(quo[[1]]))
    
  is_equal(ot,
           structure(
             list(
               `__type__` = c(
                 "is.list",
                 "is.expression",
                 "is.name",
                 "is.symbol",
                 "is.call",
                 "is.function",
                 "is.primitive",
                 "is.pairlist",
                 "is.language"
               ),
               `__value_1__` = c(FALSE, FALSE, FALSE, FALSE,
                                 TRUE, FALSE, FALSE, FALSE, TRUE),
               `__value_2__` = c(FALSE, FALSE,
                                 TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE)
             ),
             row.names = c(NA,-9L),
             class = "data.frame",
             substitutes = c("quo", "quo[[1]]")
           ))
  
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

