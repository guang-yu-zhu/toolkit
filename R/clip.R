#' Save string output to output.txt and save in clipboard
#'
#'
#' @param obj NA
#' @param file the file name for saving the output
#'
#' @return NA
#'
#' @family
#'
#' @details

#' @references
#' @keywords internal

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,kableExtra,readr,clipr)

clip<-function(obj,file = "output.txt"){
  obj%>%
    capture.output(file = "output.txt")
  read_file("output.txt")%>%
    clipr::write_clip(object_type = "character",
                      allow_non_interactive = TRUE)
}
