
source("test-helpers.R")

### ----------------------------------------------------------------- ###
### TEST CODE WRITTEN ONLY IN R ----
### ----------------------------------------------------------------- ###

test_suite("machine_readable_name", {
  
  k <- 1:5
  is_error(machine_readable_name(k))
  
  is_identical(
    machine_readable_name(as.character(k)),
    c("1", "2", "3", "4", "5")
  )
  
  is_equal(
    machine_readable_name(rownames(mtcars)),
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

