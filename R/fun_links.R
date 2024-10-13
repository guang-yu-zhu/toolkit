#' Generate Links to Function Documentation
#'
#' This function takes a list of function names and generates a markdown
#' formatted string with links to their documentation.
#'
#' @param function_names A character vector of function names.
#' @param package_name A string specifying the name of the package (default is "toolkit").
#' @return A single concatenated string containing markdown links to the function documentation.
#' @examples
#' function_list <- c("compile_rnws", "cleanFolder", "knit_rnws")
#' result <- fun_links(function_list, package_name = "toolkit")
#' cat(result)
#' @export
fun_links <- function(function_names, package_name = "toolkit") {
  base_url <- paste0("https://guang-yu-zhu.github.io/", package_name, "/reference/")

  # Create a markdown link for each function name
  links <- sapply(function_names, function(name) {
    paste0("[", name, "](", base_url, name, ".html)")
  })

  # Concatenate all links into a single string
  result <- paste(links, collapse = ", ")

  return(result)
}
