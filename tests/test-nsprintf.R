
source("test-helpers.R")

test_suite("nsprintf", {
  
  is_equal( 
    mmy::nsprintf(
      "My name is {{name}}. I am {{number}} years old.",
      number = "25",
      name = "Apple"
    ),
    "My name is Apple. I am 25 years old."
  )
  
  is_error(
    mmy::nsprintf(
      "My name is {{name}}. I am {{number}} years old.",
      name = "Apple",
      number = "25",
      class = "BB"
    )
  )
  
  is_equal(
    mmy::nsprintf("No placeholders."),
    "No placeholders."
  )
  
  is_error(
    mmy::nsprintf("A {{placeholder}} but no expressions.")
  )
  
  is_error(
    mmy::nsprintf("No placeholders but an argument throws an error.", hello = "this")
  )
  
  is_error(
    mmy::nsprintf("There are {placeholders}} but the {{double}} braces don't matching.",
                  placeholders = "A", double = "B")
  )
  
})

