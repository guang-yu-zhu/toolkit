#' Print a styled LaTeX table with alternating row colors
#'
#' This function prints a LaTeX table using the kableExtra package with alternating row colors.
#'
#' @param df The data frame to be formatted as a table.
#' @param fontsize Font size for the table (default: 7).
#' @param stripe_color Color for the alternating row stripes (default: "cyan!15").
#' @param ... Additional arguments to be passed to the kbl function.
#'
#' @return A LaTeX table with alternating row colors.
#'
#'
#' @importFrom kableExtra kbl kable_styling row_spec
#' @export
print_table_stripe <- function(df, fontsize = 7, stripe_color = "cyan!15", ...) {
  kableExtra::kbl(df, booktabs = TRUE, linesep = "", ...) %>%
    kableExtra::kable_styling(
      latex_options = c("striped", "HOLD_position"),
      position = 'center', table.envir = 'table',
      font_size = fontsize, stripe_color = stripe_color, full_width = FALSE
    ) %>%
    kableExtra::row_spec(0, font_size = fontsize)
}
