
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

#' Inspect types of R objects
#' 
#' @param ... valid \R object(s).
#' @param echo should objects be printed into the console?
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
#' \code{\link{mode}} and \code{\link{storage.mode}} are mainly used for the types
#' that are compatible with \emph{S}.
#' 
#' Substituted object names are stored in the \code{attributes}. See \emph{Examples}.
#' 
#' @examples 
#' object_types(1, 5L)
#' mmy::object_types(quote(for (i in seq(5L)) i))
#' types <- object_types(`$`, 1L, `[[<-`)
#' attributes(types)
#' 
#' @examples 
#' quo <- quote(x <- 2)
#' check_language_object_types(quo, quo[[1]])
#' 
#' @references 
#' \href{https://cran.r-project.org/doc/manuals/r-release/R-lang.html}{R Language 
#' Definition manual}
#' @name ObjectChecks
NULL

#' @rdname ObjectChecks
#' @export
object_types <- function(..., echo = FALSE) {
  calls <- c("class", "typeof", "mode", "storage.mode", "sexp.type")
  res <- .construct_type_table(..., calls = calls, echo = echo)
  res
}

#' @rdname ObjectChecks
#' @export
check_language_object_types <- function(..., echo = FALSE) {
  calls <- c(
    "is.list",
    "is.expression",
    "is.name",
    "is.symbol",
    "is.call",
    "is.function",
    "is.primitive",
    "is.pairlist",
    "is.language"
  )
  .construct_type_table(..., calls = calls, echo = echo)
}

.construct_type_table <- function(..., calls, echo) {
  x <- list(...)
  tbl <- do.call(rbind, lapply(seq_along(x), function(i) {
    xi <- x[[i]]
    dfi <- do.call(cbind, lapply(seq_along(calls), function(t) {
      value <- eval(substitute(call(xname, quote(xi)), list(xname = calls[t])))
      eval(bquote(data.frame(.(value), stringsAsFactors = FALSE)))
    }))
    names(dfi) <- calls
    dfi
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
  obj.names <- vapply(inds, function(n) deparse(ll[[n]]), character(1))
  attr(out, "substitutes") <- obj.names
  if (echo) {
    ## TODO pairlist in the example below not echoed 
    ## object_types(as.list(expression(a<-function(x,y) x+y))[[1]][[3]][[2]],echo=T)
    objs <- paste(unlist(x), collapse = "\n")
    italic <- paste0("\033[3m", objs,"\033[23m")
    styled <- paste0("`", italic, "`")
    cat(paste(styled, collapse = "\n"), "\n\n")
  }
  out
}

#' Return \code{SEXPTYPE} from C internals
#' 
#' @param x any valid \R object.
#' 
#' @seealso 
#' \code{\link[pryr]{sexp_type}} returns a \dQuote{similar output} as this call does.
#' However, \code{pryr} package does more things.
#' @export
sexp.type <- function(x) {
  .Call(`_sexptype`, x, PACKAGE = "mmy")
}

#' List environment objects
#'
#' @param env which environment to look at.
#' @param show.dot display objects starting with dot.
#' @details Similar to what RStudio environment pane displays.
#' @importFrom utils object.size
#' @export
list_env_objects <- function(env = globalenv(), show.dot = FALSE) {
  stopifnot(inherits(env, "environment"))
  # TODO object_types
  do.call(rbind, lapply(ls(envir = env, all.names = show.dot), function(i) {
    g <- get(i)
    data.frame(
      object = i,
      dim = if(is.null(dim(g))) NA else paste(dim(g), collapse = "x"),
      class = class(g),
      typeof = typeof(g),
      size.byte = as.character(utils::object.size(g)),
      stringsAsFactors = FALSE)
  })) -> objs
  if (is.null(objs)) {
    return(NULL)
  }
  res <- objs[order(objs[["object"]], decreasing = TRUE), ]
  mmy::un_rownames(res)
}

