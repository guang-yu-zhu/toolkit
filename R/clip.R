#' Capture output and copy to clipboard
#'
#' This function captures the output of an R object and writes it to a file, then copies the contents of the file to the clipboard.
#'
#' @param obj The R object whose output is to be captured.
#' @param file The file to which the output will be written (default: "output.txt").
#'
#' @return None
#'
#' @examples
#' summary(glm(case ~ spontaneous+induced,data = infert,family = binomial()))%>%clip()
#'
#' @importFrom magrittr %>%
#' @importFrom clipr write_clip
#' @importFrom readr read_file
#' @importFrom utils capture.output
#' @export
clip <- function(obj, file = "output.txt") {
  obj %>%
    utils::capture.output(file = file)
  	readr::read_file(file) %>%
    clipr::write_clip(object_type = "character",
                      allow_non_interactive = TRUE)
}
