
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
#' @param ... valid \R object(s).
#' 
#' @examples 
#' see_object_types(1, "a")
#' see_object_types(1, 5L)
#' see_object_types(as.name("mean"))
#' see_object_types(`(`)
#' see_object_types(`$`, 1L, `[[<-`)
#' @export
see_object_types <- function(...) {
  x <- list(...)
  tbl <- do.call(rbind, lapply(seq_along(x), function(i) {
    xi <- x[[i]]
    data.frame(
      class = class(xi),
      typeof = typeof(xi),
      mode = mode(xi),
      storage.mode = storage.mode(xi),
      stringsAsFactors = FALSE
    )
  }))
  out <- mmy::std_rownames(data.frame(t(tbl), stringsAsFactors = FALSE))
  val.names <- if (nrow(tbl) > 1L) {
    paste0("__value_", seq(nrow(tbl)), "__")
  } else {
    "__value__"
  }
  names(out) <- c("__type__", val.names)
  out
}

#' Return \code{SEXPTYPE} from C internals
#' 
#' @param x any valid \R object.
#' 
#' @seealso 
#' \code{\link{pryr}{sexp_type}} returns a "similar output" as this call does.
#' @export
sexp.type <- function(x) {
  .Call(`_sexptype`, x, PACKAGE = "mmy")
}

#' List global environment objects
#'
#' @param env which environment to look at.
#' @details Similar to what RStudio environment pane displays.
#' @importFrom utils object.size
#' @export
list_global_objects <- function(env = ".GlobalEnv") {
  envir <- eval(parse(text = env))
  do.call("rbind", lapply(ls(envir = envir, all.names = TRUE), function(i) {
    g <- get(i)
    data.frame(
      object = i,
      dim = if(is.null(dim(g))) NA else paste(dim(g), collapse = "x"),
      class = class(g),
      typeof = typeof(g),
      size.byte = as.character(utils::object.size(g)),
      stringsAsFactors = FALSE)
  })) -> objs
  objs[order(objs[["size.byte"]], decreasing = TRUE), ]
}

