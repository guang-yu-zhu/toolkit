#' Replace Chinese Punctuation with English Equivalents
#'
#' This function takes a file name as input and replaces specific Chinese punctuation marks
#' with their English equivalents. It also converts Traditional Chinese characters to Simplified
#' Chinese characters.
#'
#' @param filename A character string representing the file name to be processed.
#' @return A character string with the modified file name.
#' @importFrom stringr str_replace_all
#' @importFrom tmcn toTrad
#' @examples
#' updated_name <- replace_punctuation("A，B.m4a")
#'
#' @export
replace_punctuation <- function(filename) {
  newname <- filename %>%
    stringr::str_replace_all(pattern = "[\u3001\uff0c]", replacement = ',') %>% # Replace Chinese comma and enumeration comma with English comma
    stringr::str_replace_all(pattern = "\u3002", replacement = '.') %>% # Replace Chinese period
    stringr::str_replace_all(pattern = "\uff01", replacement = '!') %>% # Replace Chinese exclamation mark
    stringr::str_replace_all(pattern = "\uff1f", replacement = '?') %>% # Replace Chinese question mark
    stringr::str_replace_all(pattern = "\uff1a", replacement = '-') %>% # Replace Chinese colon
    stringr::str_replace_all(pattern = "[\u201c\u201d]", replacement = '"') %>% # Replace Chinese double quotes
    stringr::str_replace_all(pattern = "[\u2018\u2019]", replacement = "'") %>% # Replace Chinese single quotes
    stringr::str_replace_all(pattern = "\uff08", replacement = "(") %>% # Replace Chinese left parenthesis
    stringr::str_replace_all(pattern = "\uff09", replacement = ")") %>% # Replace Chinese right parenthesis
    stringr::str_replace_all(pattern = "\u300a", replacement = '<') %>% # Replace Chinese left angle bracket
    stringr::str_replace_all(pattern = "\u300b", replacement = '>') %>% # Replace Chinese right angle bracket
    stringr::str_replace_all(pattern = "\u3010", replacement = '[') %>% # Replace Chinese left square bracket
    stringr::str_replace_all(pattern = "\u3011", replacement = ']') # Replace Chinese right square bracket
  #data("SIMTRA", package = "tmcn", envir = .tmcnEnv)
  #newname <- tmcn::toTrad(newname, rev = TRUE) # Convert Traditional Chinese to Simplified Chinese
  return(newname)
}
