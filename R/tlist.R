
### ----------------------------------------------------------------- ###
### TLIST ----
### ----------------------------------------------------------------- ###

#' An opinionated way to construct (tree-like) lists
#'
#' @details
#' 
#' \itemize{
#' \item There're no list names in \code{tlist}, only indices.
#' \item The list is flat. The nodes staying at the same level are not wrapped in
#' another list, rather a new node has been created.
#' \item 'tlist's are designed to be used in small, not long stuctures. The semantics
#' doesn't allow to go super deep down.
#' }
#' 
#' ________________________________________________________\cr
#' (1)...........A..........B..............C...............\cr
#' _____________[1]________[2]____________[3]______________\cr
#' ............/.|.\........|......../...|...|...\.........\cr
#' (2)........A..B..C.......A.......A....B...C....D........\cr
#' _____________[1]________[2]____________[3]______________\cr
#' ........../.\....|..../.|.|.\........|....|.../.|.\.....\cr
#' (3)......A...B...A...A..B.C..D.......A....A..A..B..C....\cr
#' _________[1]____[2]_____[3]_________[4]__[5]___[6]______\cr
#' (...)\cr
#' 
#' As seen in the graph above,
#' 
#' \itemize{
#' \item The indexes surrounded by parentheses appearing in each line indicate
#' \emph{levels}, e.g. \code{(1)}.
#' \item The indexes placed in the bar appearing under the letters indicate
#' \emph{nodes}, e.g. \code{[5]}.
#' }
#'
#' @examples \dontrun{
#' ## Instantiate a tlist to keep fruit data:
#' fruits <- tlist()
#' fruits$append("apple", 1, 1)
#' ## Append a child
#' fruits$append("seed", 1, 3)
#' ## Append siblings:
#' fruits$append(c("orange", "watermelon"), 1)
#' ## Get the main list:
#' fruits$main()
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

  append <- function(x, level, node) {
    if (length(self$main) < level) {
      self$main[[level]] <- list()
    }
    index <- length(self$main[[node]]) + 1L
    self$main[[level]][[index]] <<- x
  }
  
  access <- function(level, node) {
    
  }
  

  ## /_____________________________________________/
  ## Exported objects.
  ## /_____________________________________________/
  exported <- structure(
    list(
      env = getEnv,
      main = getMainList,
      dim = getMainListLength,
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
#' @export
print.tlist <- function(x, ...) {
  env.name <- utils::capture.output(x$env())
  cat(
    paste(
      paste(tlist_identifier(), "constructor"),
      "------------------",
      " available methods:",
      " - main(): see the tlist",
      " - append(): add new nodes to tlist",
      "",
      env.name,
      sep = "\n"
    ),
    "\n"
  )
}

#' @export
print.tlist_main <- function(x, ...) {
  cat(tlist_identifier(), "\n")
  print(unclass(x))
}

### ----------------------------------------------------------------- ###
### UTILS ----
### ----------------------------------------------------------------- ###

#' Check if an object is a tlist
#' 
#' @param x any \R object.
#' @export
is_tlist <- function(x) {
  inherits(x, "tlist") 
}

tlist_identifier <- function(x) {
  "\033[3m<tlist>\033[23m"
}