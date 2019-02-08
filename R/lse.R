
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
