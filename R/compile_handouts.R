#' Create and Compile Handouts from a List of TeX Files
#'
#' @description This function processes a list of LaTeX files, creates corresponding handout 
#' files using a specified template, compiles them into PDFs, and removes intermediate 
#' files based on success status. Failed compilations always keep the TeX file for debugging.
#'
#' @param file_list A character vector of TeX filenames to be processed and compiled.
#' @param template_path Path to the template LaTeX file (default: 'handout_tex/handout.tex').
#' @param remove Logical; remove intermediate TeX files after successful PDF compilation (default: TRUE).
#' @param keep_log Logical; keep compilation log files (default: FALSE).
#'
#' @return Invisibly returns a data frame with compilation status for each file.
#' 
#' @details For each file: reads template, replaces 'filename' placeholder, compiles to PDF.
#' TeX files are kept if: compilation fails, no PDF is created, or remove=FALSE.
#' 
#' @examples
#' \dontrun{
#'   tex_files <- c("lecture1.tex", "lecture2.tex")
#'   results <- compile_handouts(tex_files)
#' }
#'
#' @importFrom stringr str_replace
#' @importFrom tinytex latexmk
#' @importFrom readr read_file write_file
#' @export
compile_handouts <- function(file_list, 
                             template_path = 'handout_tex/handout.tex',
                             remove = TRUE,
                             keep_log = FALSE) {
  
  # Input validation
  if (missing(file_list) || length(file_list) == 0) {
    stop("file_list must be provided and non-empty")
  }
  
  if (!file.exists(template_path)) {
    stop("Template file not found: ", template_path)
  }
  
  # Initialize results
  results <- data.frame(
    filename = character(),
    handout = character(),
    success = logical(),
    pdf = logical(),
    tex_kept = logical(),
    error = character(),
    stringsAsFactors = FALSE
  )
  
  template <- readr::read_file(template_path)
  
  cat("Compiling", length(file_list), "files...\n")
  
  for (filename in file_list) {
    handout <- paste0("handout_", basename(filename))
    tex_kept <- FALSE
    error_msg <- NA
    
    cat("\nProcessing:", filename, "\n")
    
    if (!file.exists(filename)) {
      warning("File not found: ", filename)
      results <- rbind(results, data.frame(
        filename = filename, handout = handout, success = FALSE,
        pdf = FALSE, tex_kept = FALSE, error = "File not found"
      ))
      next
    }
    
    tryCatch({
      # Create handout
      content <- stringr::str_replace(template, 'filename', filename)
      readr::write_file(content, handout)
      
      # Compile
      tinytex::latexmk(handout, clean = TRUE, emulation = TRUE)
      
      # Check PDF
      pdf_file <- sub("\\.tex$", ".pdf", handout)
      pdf_exists <- file.exists(pdf_file)
      
      if (pdf_exists) {
        cat("  ✓ PDF created\n")
        if (remove) {
          file.remove(handout)
          cat("  ✓ TeX removed\n")
        } else {
          tex_kept <- TRUE
          cat("  ✓ TeX kept (remove=FALSE)\n")
        }
        
        # Clean logs if not keeping
        if (!keep_log) {
          aux_pattern <- paste0("^", sub("\\.tex$", "", handout), "\\.")
          aux_files <- list.files(pattern = aux_pattern, full.names = TRUE)
          log_files <- aux_files[grepl("\\.(log|aux|out|toc)$", aux_files)]
          if (length(log_files) > 0) file.remove(log_files)
        }
      } else {
        warning("No PDF created for: ", filename)
        tex_kept <- TRUE
        error_msg <- "No PDF generated"
        cat("  ⚠ No PDF, keeping TeX\n")
      }
      
      results <- rbind(results, data.frame(
        filename = filename, handout = handout, success = TRUE,
        pdf = pdf_exists, tex_kept = tex_kept, error = error_msg
      ))
      
    }, error = function(e) {
      tex_kept <- TRUE
      error_msg <- as.character(e$message)
      cat("  ✗ Error:", error_msg, "\n")
      cat("  ✓ TeX kept for debugging\n")
      
      results <- rbind(results, data.frame(
        filename = filename, handout = handout, success = FALSE,
        pdf = FALSE, tex_kept = tex_kept, error = error_msg
      ))
    })
  }
  
  # Summary
  cat("\n", "========================", "\nSummary:\n", sep="")
  cat("Success:", sum(results$pdf), "/", nrow(results), "\n")
  cat("TeX kept:", sum(results$tex_kept), "\n")
  
  if (any(!results$success)) {
    cat("\nFailed:\n")
    failed <- results[!results$success, ]
    for (i in seq_len(nrow(failed))) {
      cat("  -", failed$filename[i], ":", failed$error[i], "\n")
    }
  }
  
  invisible(results)
}