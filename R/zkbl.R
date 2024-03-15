#' Format and style a table for LaTeX output
#'
#' This function formats and styles a table for LaTeX output using the kableExtra package.
#'
#' @param table The data table to be formatted.
#' @param booktabs Logical indicating whether to use the booktabs formatting (default: TRUE).
#' @param escape Logical indicating whether to escape special characters in the table (default: TRUE).
#' @param ... Additional arguments to be passed to the kbl function.
#' 
#' @return A LaTeX-formatted table with specified styling.
#' 
#' @examples
#' df <- data.frame(x = 1:5, y = letters[1:5])
#' zkbl(df)
#' 
#' @importFrom kableExtra kbl kable_styling
#' @export
zkbl <- function(table, booktabs = TRUE, escape = TRUE, ...) {
  options(knitr.kable.NA = '')
  table %>%
    kableExtra::kbl(booktabs = booktabs, escape = escape,format.args = list(decimal.mark = '.', big.mark = ","),...) %>%
    kableExtra::kable_styling(c("striped", "hover"), full_width = FALSE,
                  latex_options = "HOLD_position", fixed_thead = TRUE)
}



