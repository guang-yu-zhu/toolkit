#' Insert multiple images into R Markdown documents
#'
#' @param image_names Character vector of image filenames or paths. Files
#'   without directory paths will be relative to the default folder.
#' @param default_folder Optional folder path to override the global default.
#'   If NULL (default), uses the folder set by `set_default_image_folder()`.
#'
#' @return Output from `knitr::include_graphics()` containing the existing
#'   image files.
#'
#' @details This function is designed to be used within R Markdown chunks.
#'   It applies the default folder to image names that don't contain path
#'   separators (/ or \\), checks for file existence, and includes only
#'   existing files in the output. Missing files generate warnings but don't
#'   stop execution.
#'
#' @examples
#' \dontrun{
#' # After setting default folder
#' set_default_image_folder("images/")
#' insert_images(c("plot1.png", "plot2.png"))
#'
#' # With mixed paths
#' insert_images(c("plot1.png", "../other/plot2.png"))
#'
#' # Override default folder
#' insert_images(c("plot1.png"), default_folder = "alternative/path")
#' }
#'
#' @seealso \code{\link{set_default_image_folder}}
#' @export
insert_images <- function(image_names, default_folder = NULL) {

  # Use provided default_folder or get from global options
  if (is.null(default_folder)) {
    default_folder <- getOption("default_image_folder")
  }

  # Apply default folder if provided
  if (!is.null(default_folder)) {
    # Clean folder path (remove trailing slash)
    default_folder <- gsub("/$|\\\\$", "", default_folder)
    # Prepend folder to image names that don't have paths
    image_paths <- sapply(image_names, function(fig) {
      if (!grepl("/|\\\\", fig)) {
        return(file.path(default_folder, fig))
      } else {
        return(fig)
      }
    })
  } else {
    image_paths <- image_names
  }

  # Check if files exist
  missing_files <- image_paths[!file.exists(image_paths)]
  if (length(missing_files) > 0) {
    warning("The following image files were not found: ", paste(missing_files, collapse = ", "))
  }

  # Get only existing files
  existing_files <- image_paths[file.exists(image_paths)]

  if (length(existing_files) == 0) {
    stop("No image files found. Please check your file paths and default folder settings.")
  }

  # Use knitr to include graphics with chunk options
  # Note: In an R Markdown chunk, you would set these as chunk options
  # This function is designed to be used within an R Markdown chunk
  knitr::include_graphics(existing_files)
}
