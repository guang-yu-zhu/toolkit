library(tidyverse)
library(readr)
library(clipr)

#' Save output to clipboard
#'
#' Save string output to output.txt and save in clipboard
#'
#' @param obj
#' @param file
#'
#' @return
#' @export
#'
clip<-function(obj,file = "output.txt"){
  obj%>%
    capture.output(file = "output.txt")
  read_file("output.txt")%>%
    clipr::write_clip(object_type = "character",
                      allow_non_interactive = TRUE)
}
