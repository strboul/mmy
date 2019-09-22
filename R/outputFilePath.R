
#' Output character file path as R's platform-agnostic file.path
#'
#' @param path file path as character.
#' @param platform name of the platform. Specify if you have a path separated by
#'   different file separators which doesn't belong to the current platform where \R
#'   runs. Select one of \code{unix} or \code{win} alternatives. If the argument is
#'   \code{NULL}, which is the default value, a separator, which is specific to the
#'   current platform, will automatically be selected \emph{(from
#'   \code{.Platform$file.sep})}.
#' @examples 
#' path_nix <- "/var/log/wifi.log"
#' outputFilePath(path_nix)
#' @export
outputFilePath <- function(path, platform = NULL) {
  path_exp <- path.expand(path)
  curr_plt <- if (is.null(platform)) {
    .Platform$file.sep
  } else {
    available <- list(unix = "/", win = "\\\\")
    plt <- switch(
      platform,
      "unix" = available[["unix"]],
      "win" = available[["win"]],
      NULL
    )
    if (is.null(plt)) {
      stop("selected platform not recognized. Choose one of those: ",
           paste(paste0("\"", names(available), "\""), collapse = ", "),
           ".")
    }
    plt
  }
  splt <- unlist(strsplit(path_exp, split = curr_plt))
  splt_inc <- splt[nchar(splt) > 0]
  out <- paste0("file.path(",
                paste(
                  paste0("\"", splt_inc, "\""),
                  sep = "",
                  collapse = ", "),
                ")")
  cat(out, "\n")
  invisible(splt_inc)
}

