#' Warn recommended packages
#'
#' @param packages a data.frame containing the metadata of the packages.
#' @example \dontrun{
#' packages <- data.frame(
#'   package = c("devtools", "tidyverse", "mmy"),
#'   source = c("CRAN", "CRAN", "GitHub"),
#'   link = c(NA, NA, "strboul/mmy")
#' )
#' warn_recommended_packages(packages)
#' }
#' @export
warn_recommended_packages <- function(packages) {
  stopifnot(
    is.data.frame(packages),
    c("package", "source", "link") %in% names(packages)
  )
  if (!interactive()) {
    return(invisible(NULL))
  }
  installed_packages <- utils::installed.packages()
  not_installed <- !packages$package %in% installed_packages
  if (any(not_installed)) {
    not_installed_pkgs <- packages[not_installed, ]
    generate_install_msg <- function(pkgs, call_name) {
      pkgs_len <- length(pkgs)
      if (!pkgs_len > 0L) return(NULL)
      pkgs_quoted <- paste("\"", pkgs, "\"", sep = "", collapse = ", ")
      if (pkgs_len > 1L) pkgs_quoted <- paste0("c(", pkgs_quoted, ")")
      sprintf("%s(%s)\n", call_name, pkgs_quoted)
    }
    cran_msg <- generate_install_msg(
      not_installed_pkgs[not_installed_pkgs$source == "CRAN", "package"],
      "install.packages"
    )
    gh_msg <- generate_install_msg(
      not_installed_pkgs[not_installed_pkgs$source == "GitHub", "link"],
      "remotes::install_github"
    )
    cat(paste(
      " Note: Not all the recommended packages found in the system.\n",
      "Please run the line(s) to install them:\n\n",
      cran_msg,
      gh_msg
    ), "\n")
  }
  invisible()
}
