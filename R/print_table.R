#' Print a Styled Table for HTML or LaTeX Output
#'
#' This function prints a styled table in either HTML or LaTeX format, depending on the output document type.
#'
#' @param df The data frame to be formatted as a table.
#' @param caption A character string specifying the caption for the table (default: '').
#' @param digits An integer indicating the number of digits to display (default: 2).
#' @param print_rownames Logical value indicating whether to print row names (default: FALSE).
#' @param fontsize Font size for the table (default: 9).
#' @param big.mark A character string used as the big mark separator for numbers (default: ',').
#' @param na_str A string used to replace missing values (default: '').
#' @param ... Additional arguments to be passed to the formatting functions.
#'
#' @return A styled table in HTML or LaTeX format, depending on the document type.
#'
#' @examples
#' head(mtcars) %>% print_table()
#'
#' @importFrom magrittr %>%
#' @importFrom kableExtra kbl kable_styling row_spec
#' @importFrom flextable set_flextable_defaults flextable fontsize set_caption colformat_double autofit
#' @importFrom knitr opts_knit
#' @importFrom tibble rownames_to_column
#' @export
print_table <- function(df, caption = '', digits = 2, print_rownames = FALSE, fontsize = 9,
                        big.mark = ',', na_str = '', ...) {

  # Set default settings for flextable output
  flextable::set_flextable_defaults(
    digits = digits,
    decimal.mark = ".",
    big.mark = big.mark,
    na_str = na_str,
    post_process_html = autofit,
    post_process_pdf = autofit,
    post_process_docx = autofit
  )

  # Determine the document type (HTML, LaTeX, or Word)
  doc.type <- knitr::opts_knit$get('rmarkdown.pandoc.to')
  if (is.null(doc.type)) doc.type <- 'html'

  # Convert to data frame in case of tibble input
  df <- df %>% as.data.frame()

  # Render for HTML or Word output (flextable)
  if (doc.type %in% c("docx", "html")) {
    if (print_rownames) df <- df %>% tibble::rownames_to_column(var = 'rowname')
    flextable::flextable(df) %>%
      flextable::fontsize(size = fontsize) %>%
      flextable::set_caption(caption = caption) %>%
      flextable::colformat_double(big.mark = big.mark, digits = digits, na_str = na_str) %>%
      flextable::autofit()

    # Render for LaTeX output (kableExtra)
  } else {
    kableExtra::kbl(df, caption = caption, digits = digits, booktabs = TRUE, linesep = "", ...) %>%
      kableExtra::kable_styling(latex_options = c("HOLD_position"),
                                position = 'center',
                                table.envir = 'table',
                                font_size = fontsize,
                                full_width = FALSE) %>%
      kableExtra::row_spec(0, font_size = fontsize)
  }
}
