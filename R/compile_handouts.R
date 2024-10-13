#' Create and Compile Handouts from a List of TeX Files
#'
#' @description This function processes a list of LaTeX files, creates corresponding handout files using a specified template, compiles them into PDFs, and removes the intermediate handout files after successful compilation.
#'
#' @param file_list A list of TeX filenames to be processed and compiled into handouts.
#' @param template_path A string specifying the path to the template LaTeX file (default is "handout_tex/handout.tex").
#'
#' @return This function does not return a value but compiles PDFs and deletes the temporary handout TeX files after compilation.
#'
#' @details The function reads a template LaTeX file and loops over a user-provided list of TeX files. For each TeX file, the function generates a corresponding handout file by replacing a placeholder in the template, compiles the file into a PDF using `tinytex::latexmk`, and then deletes the handout TeX file once the PDF is generated. If an error occurs during compilation, it skips to the next file.
#'
#' @examples
#' \dontrun{
#'   tex_files <- list.files(pattern = "^Sta.*\\.tex")
#'   compile_handouts(tex_files)
#' }
#'
#' @importFrom stringr str_replace
#' @importFrom tinytex latexmk
#' @importFrom readr read_file
#' @export
compile_handouts <- function(file_list, template_path = 'handout_tex/handout.tex') {
  # Read the LaTeX handout template
  template <- readr::read_file(template_path)

  # Loop through the list of TeX files
  for (filename in file_list) {
    skip_to_next <- FALSE
    cat('---------', filename, '\n')

    # Create a new handout filename based on the current TeX file
    handout_name <- paste0("handout_", filename)

    # Replace the 'filename' placeholder in the template and write to handout file
    tryCatch({
      template %>%
        stringr::str_replace('filename', filename) %>%
        cat() %>%
        capture.output(file = handout_name)

      # Compile the handout into a PDF
      tinytex::latexmk(handout_name, clean = TRUE, emulation = TRUE)

    }, error = function(e) {
      # If there's an error, set skip_to_next to TRUE
      cat("Error compiling", filename, "\n")
      skip_to_next <- TRUE
    })

    # Skip to the next file if an error occurred during compilation
    if (skip_to_next) {
      next
    }

    # Remove the temporary handout file
    file.remove(handout_name)
  }
}
