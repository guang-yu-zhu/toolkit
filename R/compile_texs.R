#' Compile LaTeX Files
#'
#' @description This function compiles a list of LaTeX (.tex) files using `tinytex::latexmk`.
#' If no file list is provided, it compiles all `.tex` files in the current working directory.
#' Note that files that fail to compile will still be included in the results data frame, marked with `success = FALSE`.
#'
#' @param file_list A character vector of LaTeX file names to be compiled. If NULL, all `.tex` files in the working directory will be compiled.
#' @param clean Logical indicating whether to clean auxiliary files after compilation (default is TRUE).
#'
#' @return A character vector of files that were unsuccessfully compiled.
#'
#' @examples
#' \dontrun{
#'   failed_files <- compile_texs()
#'   file_list = list.files(pattern = "^Sta.*tex", ignore.case = TRUE)
#'   failed_files <- compile_texs(file_list)
#' }
#'
#' @importFrom tinytex latexmk
#' @importFrom readr read_file
#' @export
compile_texs <- function(file_list = NULL, clean = TRUE) {
  # If no file list is provided, get all .tex files in the current directory
  if (is.null(file_list)) {
    file_list <- list.files(pattern = "\\.tex$", full.names = TRUE)
  }

  # Initialize a data frame to store results with the full list of filenames
  compile_results <- data.frame(file = file_list, success = NA, stringsAsFactors = FALSE)

  # Loop through the list of files
  for (filename in file_list) {
    cat('Compiling:', filename, '\n')

    # Attempt to compile the file
    tryCatch({
      tinytex::latexmk(filename, emulation = FALSE, clean = clean)
      # If successful, update the success status
      compile_results$success[compile_results$file == filename] <- TRUE
    }, error = function(e) {
      # If an error occurs, record the failure
      compile_results$success[compile_results$file == filename] <- FALSE
      cat('Error compiling:', filename, '\n', e$message, '\n')
    })
  }

  # Clean up temporary files if specified
  if (clean) {
    cleanLatex()
  }

  # Extract the list of files that failed to compile
  failed_files <- compile_results$file[is.na(compile_results$success) | !compile_results$success]

  # Return only the failed files
  return(failed_files)
}
