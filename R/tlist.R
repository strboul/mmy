
#' An opinionated way to construct (tree-like) lists
#' 
#' @details 
#' Some specifications:
#' - There're no list names in \code{tlist}, only indices.
#' - The list is flat. The nodes staying at the same level are not wrapped in another
#' list, rather a new node has been created.
#' - 'tlist's are designed to be used in small, not long stuctures. The semantics
#' doesn't allow to go super deep down.
#' 
#' @examples
#' TODO
#' ## Instantiate a tlist to keep fruit data:
#' fruits <- tlist()
#' fruits$append("apple", level = 1)
#' fruits$main()
#' ## Append a child
#' fruits$append("seed", level = c(1, 2))
#' fruits$main()
#' ## Append siblings:
#' fruits$append(c("orange", "watermelon"), level = 1)
#' }
#' @export
tlist <- function() {
  
  ## /_____________________________________________/
  ## FIELDS
  ## - main: main `tlist` to be returned.
  ## /_____________________________________________/
  self <- list(
    env = NULL,
    main = NULL
  )
  
  ## /_____________________________________________/
  ## INSTANTIATION
  ## When the `tlist` object created, assign fields 
  ## in the global env.
  ## /_____________________________________________/
  instantize <- function() {
    self$main <<- list()
    self$env <<- environment()
    list(
      main = self$main,
      env = self$env
    )
  }
  
  if (is.null(self$env)) {
    instantize()
  } 
  
  ## /_____________________________________________/
  ## METHODS
  ## As a convention, private methods should start 
  ## with dots.
  ## - new: create a new `tlist` list.
  ## - main: return main `tlist` list.
  ## /_____________________________________________/
  getEnv <- function() {
    self$env
  }
  
  getMainList <- function() {
    structure(self$main, class = "tlist_main")
  }
  
  getMainListLength <- function() {
    length(self$main)
  }
  
  append <- function(x, ...) {
    
  }
  
  # appendSibling <- function(x, ...) {
  #   levels <- list(...)
  #   index <- length(self$main) + 1L
  #   self$main[[index]] <<- list(x)
  # }
  # 
  # appendChild <- function(x, ...) {
  #   levels <- list(...)
  #   
  #   index <- length(self$main[[level]]) + 1L
  #   self$main[[level]][[index]] <<- x
  # }
  
  # TODO .makeIndex <- function(x)
  
  ## /_____________________________________________/
  ## Exported objects.
  ## /_____________________________________________/
  exported <- structure(
    list(
      env = getEnv,
      main = getMainList,
      mainListLength = getMainListLength,
      append = append
    ), 
    class = "tlist"
  )
  
  exported
}

### ----------------------------------------------------------------- ###
### PRINT CALLS ----
### ----------------------------------------------------------------- ###

#' Eliminate partial matching of dollar sign
#' @export
`$.tlist` <- function(x, name) {
  name <- as.character(name)
  res <- base::`[[`(x, name)
  if (is.null(res)) {
    stop(sprintf("<tlist> method not found: '%s'", name), call. = FALSE)
  } else {
    res
  }
}

#' @importFrom utils capture.output
print.tlist <- function(x, ...) {
  env.name <- utils::capture.output(x$env())
  methods <- names(x)[sapply(x, typeof) == "closure"]
  methods.display <- paste(" -", paste0(methods, "()"), collapse = "\n")
  cat(
    paste(
      "<tlist> constructor",
      "------------------",
      " available methods:",
      methods.display,
      "",
      env.name,
      sep = "\n"
    ),
    "\n"
  )
}

print.tlist_main <- function(x, ...) {
  if (identical(length(x), 0L)) {
    cat("tlist()")
  } else {
    cat("<tlist>\n\n")
    print(unclass(x))
  }
}

