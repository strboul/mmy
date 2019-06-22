
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
#' @details 
#' \itemize{
#' \item \code{class} displays \R's built-in S3 class.
#' \item \code{typeof} type of an object.
#' \item \code{mode} storage mode of an object.
#' \item \code{storage.mode} \emph{used when calling functions written in another
#' language.}
#' \item \code{sexp.type} internal \code{SEXPTYPE} reprentation, see
#' \code{\link{sexp.type}}.
#' }
#' 
#' Substituted object names are stored in the \code{attributes}. See \emph{Examples}.
#' 
#' @examples 
#' object_types(1, 5L)
#' object_types(as.name("mean"))
#' types <- object_types(`$`, 1L, `[[<-`)
#' attributes(types)
#' 
#' @references 
#' \href{https://cran.r-project.org/doc/manuals/r-release/R-lang.html}{R Language 
#' Definition manual}
#' @export
object_types <- function(...) {
  x <- list(...)
  
  tbl <- do.call(rbind, lapply(seq_along(x), function(i) {
    xi <- x[[i]]
    data.frame(
      class = class(xi),
      typeof = typeof(xi),
      mode = mode(xi),
      storage.mode = storage.mode(xi),
      sexp.type = sexp.type(xi),
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
  
  ## Add substitute attributes:
  ll <- as.list(substitute(list(...)))
  ## inds + 1 as the first element is 'list'
  inds <- seq_along(x) + 1L
  obj.names <- sapply(inds, function(n) deparse(ll[[n]]))
  
  attr(out, "substitutes") <- obj.names
  
  out
}

#' Return \code{SEXPTYPE} from C internals
#' 
#' @param x any valid \R object.
#' 
#' @seealso 
#' \code{\link{pryr}{sexp_type}} returns a "similar output" as this call does.
#' However, \code{pryr} package does a lot more than showing underlying SEXP types.
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

