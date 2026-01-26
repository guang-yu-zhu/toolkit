#' Set a global default folder for image files
#'
#' @param folder_path Path to the default image folder. If the folder does not 
#'   exist, a warning will be issued but the setting will still be applied.
#'
#' @return No return value, called for side effects. Sets the global option 
#'   `default_image_folder` and displays a confirmation message.
#'
#' @details This function sets a global default folder that will be used by 
#'   `insert_figures()` when no specific folder is provided. The function 
#'   checks if the folder exists and issues a warning if it doesn't, but still 
#'   sets the option to allow for folder creation later.
#'
#' @examples
#' \dontrun{
#' set_default_image_folder("images/")
#' }
#'
#' @export
set_default_image_folder <- function(folder_path) {
  # Validate folder exists
  if (!dir.exists(folder_path)) {
    warning("Folder '", folder_path, "' does not exist. Setting anyway, but files may not be found.")
  }
  options(default_image_folder = folder_path)
  message("Default image folder set to: '", folder_path, "'")
}
