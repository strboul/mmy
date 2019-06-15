
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
      "fiat-x19", "porsche-9142", "lotus-europa", "ford-pantera-l", 
      "ferrari-dino", "maserati-bora", "volvo-142e")
  )
  
})

test_suite("see_object_types", {
  
  is_equal(mmy::see_object_types(1, "a"),
           structure(
             list(
               `__type__` = c("class", "typeof", "mode", "storage.mode"),
               `__value_1__` = c("numeric", "double", "numeric", "double"),
               `__value_2__` = c("character", "character", "character", "character")
             ),
             row.names = c(NA,-4L),
             class = "data.frame"
           ))
  
  is_equal(mmy::see_object_types(1, 5L),
           structure(
             list(
               `__type__` = c("class", "typeof", "mode", "storage.mode"),
               `__value_1__` = c("numeric", "double", "numeric", "double"),
               `__value_2__` = c("integer", "integer", "numeric", "integer")
             ),
             row.names = c(NA,-4L),
             class = "data.frame"
           ))
  
  is_equal(
    mmy::see_object_types(as.name("mean")),
    structure(
      list(
        `__type__` = c("class", "typeof", "mode", "storage.mode"),
        `__value__` = c("name", "symbol", "name", "symbol")
      ),
      row.names = c(NA,-4L),
      class = "data.frame"
    )
  )
  
  is_equal(mmy::see_object_types(`(`),
           structure(
             list(
               `__type__` = c("class", "typeof", "mode", "storage.mode"),
               `__value__` = c("function", "builtin", "function", "function")
             ),
             row.names = c(NA,-4L),
             class = "data.frame"
           ))
  
  is_equal(
    mmy::see_object_types(`$`, 1L, `[[<-`),
    structure(
      list(
        `__type__` = c("class", "typeof", "mode", "storage.mode"),
        `__value_1__` = c("function", "special", "function", "function"),
        `__value_2__` = c("integer", "integer", "numeric", "integer"),
        `__value_3__` = c("function", "special", "function", "function")
      ),
      row.names = c(NA,-4L),
      class = "data.frame"
    )
  )
  
})

