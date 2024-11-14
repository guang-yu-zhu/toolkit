#' Save a Flextable as a PDF
#'
#' This function saves a flextable object as a PDF file. The height of the table
#' is calculated based on its aspect ratio if not explicitly provided.
#'
#' @param ft A flextable object to be saved as a PDF.
#' @param path A character string specifying the file path for the output PDF.
#' @param width A numeric value specifying the width of the PDF in inches (default: 7).
#' @param height A numeric value specifying the height of the PDF in inches. If NULL, the height is calculated
#'        based on the aspect ratio of the flextable.
#'
#' @return NULL. The function saves the flextable as a PDF file at the specified path.
#'
#' @examples
#' ft <- print_flextable(mtcars[1:10, 1:3], num_col = 2, rowname_label = 'car')
#' \dontrun{
#'  save_flextable(ft, "table.pdf", width = 6, height = 4)
#' }
#'
#' @importFrom flextable flextable_dim
#' @export
save_flextable <- function(ft, path, width = 7, height = NULL) {
  # Set default height if not provided
  if (is.null(height)) {
    height <- flextable::flextable_dim(ft)$aspect_ratio * width
  }

  # Validate the flextable object
  if (!inherits(ft, "flextable")) {
    stop("The 'ft' parameter must be a flextable object.")
  }

  # Save the flextable as a PDF
  pdf(path, width = width, height = height)
  plot(ft)
  dev.off()
}
