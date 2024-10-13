#' Remove LaTeX Auxiliary Files
#'
#' This function deletes auxiliary files generated after compiling LaTeX in the current working directory.
#' Common auxiliary file extensions such as `.log`, `.aux`, `.out`, etc., are removed.
#'
#' @details
#' The function automatically removes common temporary files generated during the LaTeX compilation process,
#' including `.log`, `.aux`, `.out`, `.bbl`, `.toc`, `.bak`, and others. It does not prompt the user and will
#' clean the current directory without confirmation.
#'
#' @param folder The directory to clean. If not specified, it defaults to the current working directory (`getwd()`).
#' @param verbose Logical; if `TRUE`, prints the names of files that are deleted. Default is `FALSE`.
#'
#' @return The function returns `TRUE` if the files were successfully removed, and `FALSE` otherwise.
#'
#' @examples
#' \dontrun{
#' # Clean the current working directory
#' cleanLatex()
#'
#' # Clean a specific directory
#' cleanLatex("/path/to/latex/files", verbose = TRUE)
#' }
#' @export
cleanLatex <- function(folder = getwd(), verbose = FALSE) {
  # Define the auxiliary file extensions to remove
  rules <- c('.log', '.vrb', '.nav', '.snm', '.toc', '-tikzDictionary',
             '.synctex.gz', '.aux', '.out', '.bbl', '.bak')

  # List files in the directory matching the extensions
  files_to_remove <- list.files(path = folder, pattern = paste0('\\', rules, '$', collapse = '|'), full.names = TRUE)

  if (length(files_to_remove) > 0) {
    # Remove files
    file.remove(files_to_remove)

    # Optionally print file names if verbose is TRUE
    if (verbose) {
      cat("Deleted files:\n", paste(basename(files_to_remove), collapse = "\n"))
    }

    return(TRUE)
  } else {
    if (verbose) {
      cat("No auxiliary files found in", folder, "\n")
    }

    return(FALSE)
  }
}
