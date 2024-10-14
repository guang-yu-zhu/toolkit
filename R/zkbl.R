#' Format and Style a Table for LaTeX Output
#'
#' This function formats and styles a table for LaTeX output using the
#' kableExtra package.
#'
#' @param table A data frame or matrix to be formatted as a table.
#' @param booktabs Logical indicating whether to use the booktabs formatting (default: TRUE).
#' @param escape Logical indicating whether to escape special characters in the table (default: TRUE).
#' @param format.args A list of additional formatting arguments to customize number representation.
#' @param ... Additional arguments to be passed to the kbl function.
#'
#' @return A LaTeX-formatted table with the specified styling.
#'
#' @examples
#' zkbl(mtcars[1:5, ])
#'
#' @importFrom kableExtra kbl kable_styling
#' @export
zkbl <- function(table,
                 booktabs = TRUE,
                 escape = TRUE,
                 format.args = list(decimal.mark = '.', big.mark = ","),
                 ...) {
  options(knitr.kable.NA = '')  # Set NA display option for kable

  table %>%
    kableExtra::kbl(booktabs = booktabs, escape = escape, format.args = format.args, ...) %>%
    kableExtra::kable_styling(c("striped", "hover"),
                              full_width = FALSE,
                              latex_options = "HOLD_position",
                              fixed_thead = TRUE)
}
