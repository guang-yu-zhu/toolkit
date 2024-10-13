#' Compile LaTeX Files
#'
#' @description This function compiles a list of LaTeX (.tex) files using `tinytex::latexmk`.
#' If no file list is provided, it compiles all `.tex` files in the current working directory.
#'
#' @param file_list A character vector of LaTeX file names to be compiled. If NULL, all `.tex` files in the working directory will be compiled.
#' @param clean Logical indicating whether to clean auxiliary files after compilation (default is TRUE).
#'
#' @return A data frame containing the names of the files and their compilation success status.
#'
#' @examples
#' \dontrun{
#'   compile_latex_files()
#'   compile_latex_files(c("file1.tex", "file2.tex"))
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

  # Initialize a data frame to store results
  compile_results <- data.frame(file = character(), success = logical(), stringsAsFactors = FALSE)

  # Loop through the list of files
  for (filename in file_list) {
    cat('Compiling:', filename, '\n')

    # Attempt to compile the file
    tryCatch({
      tinytex::latexmk(filename, clean = clean)
      # If successful, record the result
      compile_results <- rbind(compile_results, data.frame(file = filename, success = TRUE, stringsAsFactors = FALSE))
    }, error = function(e) {
      # If an error occurs, record the failure
      compile_results <- rbind(compile_results, data.frame(file = filename, success = FALSE, stringsAsFactors = FALSE))
      cat('Error compiling:', filename, '\n', e$message, '\n')
    })
  }
  # Clean up temporary files if specified
  if (clean) {
    cleanLatex()
  }
  # Return the compilation results
  return(compile_results)
}
