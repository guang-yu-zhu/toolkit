#' Save output to clipboard
#'
#' Save string output to output.txt and save in clipboard
#' @import dplyr clipr kableExtra readr
#' @param obj the object for save
#' @param file the file name
#'
#' @return
#' @examples
#' library(dplyr,warn.conflicts = FALSE)
#' summary(glm(case ~ spontaneous+induced,data = infert,family = binomial()))%>%clip()
#'
#' @export
#'
clip<-function(obj,file = "output.txt"){
  # library(tidyverse)
  # library(readr)
  # library(clipr)
  # if (!require("pacman")) install.packages("pacman")
  # pacman::p_load(tidyverse,kableExtra,readr,clipr)

  library(dplyr,warn.conflicts = FALSE)
  obj%>%
    capture.output(file = "output.txt")
  read_file("output.txt")%>%
    clipr::write_clip(object_type = "character",
                      allow_non_interactive = TRUE)
}
