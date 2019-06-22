
### ----------------------------------------------------------------- ###
### TLIST ----
### ----------------------------------------------------------------- ###

#' An opinionated way to construct (tree-like) lists
#'
#' (WORK IN PROGRESS)
#'
#' \itemize{
#' \item There're no list names in \code{tlist}, only indices.
#' \item The list is flat. The nodes staying at the same level are not wrapped in
#' another list, rather a new node has been created.
#' \item 'tlist's are designed to be used in small, not long stuctures. The semantics
#' doesn't allow to go super deep down.
#' }
#'
#' @usage
#' \code{tlist()} \cr
#' \Sexpr[strip.white=FALSE,results=rd]{mmy:::tlist_example_tree()}
#'
#' @details
#' As seen in the graph above,
#' 
#' \itemize{
#' \item The indexes surrounded by parentheses appearing in each line indicate
#' \emph{levels}, e.g. \code{(1)}.
#' \item The indexes placed in the bar appearing under the letters indicate
#' \emph{nodes}, e.g. \code{[5]}.
#' }
#'
#' @examples
#' ## Instantiate a tlist to keep fruit data:
#' fruits <- tlist()
#' fruits$append("apple", 1, 1)
#' ## Append a child
#' fruits$append("seed", 1, 3)
#' ## Append siblings:
#' fruits$append(c("orange", "watermelon"), 1)
#' ## Get the main list:
#' fruits$main()
#' 
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

  append <- function(x, level, node) {

  }
  
  access <- function(level, node) {}

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
#' @noRd
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
#' @noRd
print.tlist <- function(x, ...) {
  env.name <- utils::capture.output(x$env())
  methods <- names(x)[vapply(x, typeof, character(1)) == "closure"]
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

#' @noRd
print.tlist_main <- function(x, ...) {
  if (identical(length(x), 0L)) {
    cat("tlist()")
  } else {
    cat("<tlist>\n\n")
    print(unclass(x))
  }
}

### ----------------------------------------------------------------- ###
### UTILS ----
### ----------------------------------------------------------------- ###

#' Documentation macro used in the 'tlist' documentation.
#' @noRd
tlist_example_tree <- function() {
  tree <- '
  ________________________________________________________
  (1)           A          B              C               
  _____________[1]________[2]____________[3]______________
              / | \        |        /   |   |    \        
  (2)       A   B  C       A       A   B    C     D       
  _____________[1]________[2]____________[3]______________
           / \     |    / | | \        |    |   / | \     
  (3)     A   B    A   A  B C  D       A    A  A  B  C    
  _________[1]____[2]_____[3]_________[4]__[5]___[6]______
  (...)
  '
  
  ## Replace all new lines with '\cr'
  tree <- gsub(paste0("\\", "n"), paste0("\\\\", "cr"), tree)
  
  ## Escape slashes:
  #TODO
  
  tree
}

