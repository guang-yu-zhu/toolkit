#' Print and Format a Styled Table for HTML or LaTeX Output
#'
#' This function prints and formats a styled table in either HTML or LaTeX format using kableExtra.
#'
#' @param x The  matrix or data frame to be formatted as a table.
#' @param num_col An integer specifying the number of columns to split the output table into (default: 1).
#' @param rowname_label A character string specifying the column name for the row names; if not provided, the row names will not be printed (default: NA).
#' @param caption A character string specifying the caption for the table (default: '').
#' @param digits An integer indicating the number of digits to display (default: 2).
#' @param fontsize Font size for the table (default: 9).
#' @param big.mark A character string used as the thousands separator for numbers (default: ',').
#' @param na_str A string used to replace missing values (default: '').
#' @param booktabs Logical indicating whether to use the booktabs formatting (default: TRUE).
#' @param escape Logical indicating whether to escape special characters in the table (default: TRUE).
#' @param format.args A list of additional formatting arguments to customize number representation.
#' @param format A character string specifying the output format: either "latex" or "html".
#' @param ... Additional arguments to be passed to the kbl function.
#'
#' @return A styled and formatted table in the specified format (LaTeX or HTML).
#'
#' @examples
#' dt = mtcars[1:10, 1:3]
#' print_flextable(dt, num_col = 2, rowname_label = 'car',format='html')
#'
#' @importFrom magrittr %>%
#' @importFrom kableExtra kbl kable_styling row_spec
#' @importFrom tibble rownames_to_column
#' @export
#' @md
print_kable <- function(x, num_col = 1, rowname_label = NA, caption = NA, digits = 2, fontsize = 16,
                        big.mark = ',', na_str = '', booktabs = TRUE, escape = TRUE,
                        format.args = list(decimal.mark = '.', big.mark = ","), format, ...) {
  if (missing(format) || is.null(format)) {
    if (knitr::is_latex_output())
      format <- "latex"
    else format <- "html"
  }

  # Convert to data frame in case of tibble input
  df <- x %>% as.data.frame()
  colnames(df) <- colnames(x)

  # Add row names as a column if rowname_label is provided
  if (!is.na(rowname_label)) {
    df <- df %>% tibble::rownames_to_column(var = rowname_label)
  }

  # Split the data frame using the split_df function
  new_df <- split_df(df, num_col, return_list = TRUE)

  # Set NA display option for kable
  options(knitr.kable.NA = na_str)

  # Choose output format and render the table
  if (format == "latex") {
    res <- kbl(new_df,format =format, row.names = FALSE, caption = caption, digits = digits,
               booktabs = booktabs,
               escape = escape, format.args = format.args, ...) %>%
      kable_styling(latex_options = c("HOLD_position","striped","scale_down"),
                    full_width = FALSE,
                    position = 'center')
  } else if (format == "html") {
    res <- kbl(new_df,format =format, row.names = FALSE, caption = caption, digits = digits,
               escape = escape, format.args = format.args, ...) %>%
      kable_classic(full_width = F) %>%
      kable_styling(position = 'center',
                    font_size = fontsize,
                    full_width = FALSE) # %>%
    # kableExtra::row_spec(0, font_size = fontsize+2)
  } else {
    stop("Invalid format specified. Choose either 'latex' or 'html'.")
  }
  return(res)
}
