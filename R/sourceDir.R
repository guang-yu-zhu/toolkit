#' Source All R Scripts in a Directory
#'
#' This function sources all R scripts from a specified directory. It recursively searches for files
#' with extensions `.R`, `.r`, `.S`, `.s`, `.Q`, and `.q` and sources them into the current environment.
#'
#' @param path A character string specifying the path to the directory containing the R scripts.
#' @param trace A logical value indicating whether to print the names of the files being sourced. Default is TRUE.
#' @param ... Additional arguments to be passed to the \code{\link[base]{source}} function.
#'
#' @return Invisible NULL. This function is called for its side effects.
#'
#'
#' @export
#' @md
sourceDir <- function(path, trace = TRUE, ...) {
  # List all files with extensions .R, .r, .S, .s, .Q, or .q
  for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {

    # If trace is TRUE, print the filename being sourced
    if (trace) cat(nm, ":")

    # Source the file from the directory
    source(file.path(path, nm), ...)

    # Print a newline if trace is TRUE
    if (trace) cat("\n")
  }
}
