
#' List objects in stack frame
#' 
#' Useful for debugging sessions.
#'
#' Returns all variables in a call environment. Made it possible with
#' \code{sys.frames()}, which accesses to the environments in the call stack.
#'
#' @param which integer. the frame number in the call stack.
#' @param exclude character. which arguments should be excluded? It is possible
#' to exclude some variables by name.
#' 
#' @examples \dontrun{ 
#' e <- function() {x<-"A";y<-rnorm(5);get_environment()};e()
#' ## inline functions:
#' ei <- function() {x<-"A";y<-rnorm(5); function() {get_environment(2)}}
#' ei()
#' }
#' 
#' @export
get_environment <- function(which = 1, exclude = NULL) {
  if (!is.numeric(which)) {
    stop("which has to be numeric: ", which)
  }
  if (is.null(exclude)) {
    exclude <- NA_character_
  } else {
    if (!is.character(exclude)) {
      stop("exclude not a character vector")
    }
  }
  envr <- sys.frames()[[which]]
  args <- as.list(envr, all.names = TRUE)
  args[names(args)[!names(args) %in% exclude]]
}

#' See types of R objects
#' 
#' @param x valid \R object(s).
#' 
#' @examples 
#' see_object_types(list(1, "a"))
#' see_object_types(call("mean"))
#' see_object_types(as.name("fun"))
#' @export
see_object_types <- function(x) {
  if (!is.list(x) && is.data.frame(x)) {
    stop("x must be a list.")
  }
  tbl <- do.call(rbind, lapply(x, function(xi) {
    data.frame(
      class = class(xi),
      mode = mode(xi),
      typeof = typeof(xi),
      storage.mode = storage.mode(xi),
      stringsAsFactors = FALSE
    )
  }))
  tbl
}

