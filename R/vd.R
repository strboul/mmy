#' vd: VisiData table
#'
#' @param x a data.frame.
#' @examples \dontrun{
#' mmy::vd(mtcars)
#' }
#' @export
vd <- function(x) {
  cmd_details <- list(
    name    = "visidata",
    command = "vd",
    url     = "https://www.visidata.org/"
  )
  check_cmd_exists(cmd_details)
  if (!is.data.frame(x))
    stop(
      sprintf("table must be a data.frame, provided: \"%s\"", class(x)),
      call. = FALSE
    )
  x <- std_rownames(x)
  tmp_file <- tempfile("r_view_", fileext = ".csv")
  write.table(x, tmp_file, sep = ", ", quote = TRUE, row.names = FALSE)
  system2(cmd_details$command, tmp_file)
  on.exit(unlink(tmp_file), add = TRUE)
  invisible(NULL)
}

check_cmd_exists <- function(x) {
  cmd <- Sys.which(x$command)
  if (!nchar(cmd)) {
    stop(sprintf(
      "\"%s\" command for %s cannot be found. Visit: %s\n",
      x$command,
      x$name,
      x$url
    ), call. = FALSE)
  }
}
