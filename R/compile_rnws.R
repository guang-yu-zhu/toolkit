#' Knit Files to PDF and Track Status
#'
#' This function takes a list of Rnw or other LaTeX source files, attempts to knit each one to a PDF using the `knit2pdf` function, and tracks the success or failure of each attempt.
#'
#' @param file_list A character vector containing the file paths to the Rnw or LaTeX source files. If NULL, the function will search for all `.Rnw` files in the current directory.
#'
#' @return A data frame with two columns:
#' \describe{
#'   \item{file}{The name of the file that was processed.}
#'   \item{success}{Logical value indicating whether the knitting process was successful (`TRUE`) or encountered an error (`FALSE`).}
#' }
#'
#' @details The function iterates through the list of files, attempts to compile each one using `knit2pdf`, and stores the result (success or failure) in a data frame. If an error occurs during the knitting process, the function will skip that file and proceed to the next one.
#'
#' @examples
#' \dontrun{
#'   file_list = list.files(pattern = "^Sta.*Rnw", ignore.case = TRUE)
#'   knit_results <- compile_rnws(file_list)
#'   print(knit_results)
#' }
#'
#' @export
compile_rnws <- function(file_list = NULL) {
  # If no file list is provided, get all .Rnw files in the current directory
  if (is.null(file_list)) {
    file_list <- list.files(pattern = "\\.Rnw$", ignore.case = TRUE, full.names = TRUE)
  }

  # Initialize result data frame
  compile_results <- data.frame(file = character(), success = logical(), stringsAsFactors = FALSE)

  # Iterate through the file list
  for (filename in file_list) {
    cat('Processing:', filename, '\n')  # Print current filename

    # Initialize a flag to track if we need to skip to the next file
    skip_to_next <- FALSE

    tryCatch({
      # Attempt to knit the file to PDF
      knit2pdf(input = filename, compiler = 'pdflatex', emulation = FALSE)
    }, error = function(e) {
      # On error, set flag to skip the file
      cat('Error encountered for:', filename, '\n', e$message, '\n')
      skip_to_next <<- TRUE  # Use the superassignment operator to modify the outer scope variable
    })

    # If there was an error, skip to the next file
    if (skip_to_next) {
      next
    }

    # Append the result to the compile_results data frame
    compile_results <- rbind(compile_results, data.frame(file = filename, success = TRUE, stringsAsFactors = FALSE))
  }

  # Return the knit results
  if(clean) cleanLatex()
  return(compile_results)
}
