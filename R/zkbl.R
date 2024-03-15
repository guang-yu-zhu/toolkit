#' Create a styled LaTeX table using kableExtra with additional options
#'
#' This function creates a LaTeX table using the kableExtra package with the provided options.
#'
#' @param table The data table to be formatted.
#' @param booktabs Logical indicating whether to use the booktabs formatting (default: TRUE).
#' @param escape Logical indicating whether to escape special characters in the table (default: TRUE).
#' @param ... Additional arguments to be passed to the kbl function.
#'
#' @return A LaTeX table formatted using kableExtra package.
#'
#' @examples
#' library(kableExtra)
#' df <- data.frame(x = 1:5, y = letters[1:5])
#' zkbl(df)
#'
#' @import kableExtra
#' @import magrittr
#' @export
zkbl <- function(table, booktabs = TRUE, escape = TRUE, ...) {
  options(knitr.kable.NA = '')
  table %>%
    kbl(booktabs = booktabs, escape = escape, ...) %>%
    kable_styling(c("striped", "hover"), full_width = FALSE,
                  latex_options = "HOLD_position", fixed_thead = TRUE)
}



