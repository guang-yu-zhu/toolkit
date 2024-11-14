#' Print a Styled Table for HTML or Word Output
#'
#' This function prints a styled table in HTML or Word format using flextable.
#'
#' @param df The data frame to be formatted as a table.
#' @param num_col An integer specifying the number of columns to split the output table into (default: 1).
#' @param rowname_label A character string specifying the column name for the row names; if not provided, the row names will not be printed (default: NA).
#' @param caption A character string specifying the caption for the table (default: '').
#' @param digits An integer indicating the number of digits to display (default: 2).
#' @param fontsize Font size for the table (default: 9).
#' @param big.mark A character string used as the thousands separator for numbers (default: ',').
#' @param na_str A string used to replace missing values (default: '').
#' @param ... Additional arguments to be passed to the formatting functions.
#'
#' @return A styled table in HTML or Word format.
#'
#' @examples
#' dt = mtcars[1:10, 1:3]
#' print_flextable(dt, num_col = 2, rowname_label = 'car')
#'
#' @importFrom magrittr %>%
#' @importFrom flextable set_flextable_defaults flextable fontsize colformat_double autofit
#' @importFrom tibble rownames_to_column
#' @importFrom officer fp_border
#' @export
#' @md
print_flextable <- function(df, num_col = 1, rowname_label = NA, caption = '', digits = 2, fontsize = 11,
                                  big.mark = ',', na_str = '', ...) {
  # Set default settings for flextable output

  my_theme<-function(ft){
    ft%>% flextable::colformat_double() %>%
      flextable::theme_vanilla()%>%
      flextable::align(align = "left", part = 'all')%>%
      flextable::align(align = "left", part = 'footer')%>%
      flextable::valign(valign="center",part='all')
  }
  zpost_process<-function(ft){
    ft%>%
      flextable::fix_border_issues()%>%
      flextable::autofit()
  }
  flextable::set_flextable_defaults(
    theme_fun = my_theme,
    digits = digits,
    decimal.mark = ".",
    big.mark = big.mark,
    na_str = na_str,
    post_process_html = zpost_process,
    post_process_pdf = zpost_process,
    post_process_docx = zpost_process
  )

  # Convert to data frame in case of tibble input
  df <- df %>% as.data.frame()

  # Add row names as a column if rowname_label is provided
  if (!is.na(rowname_label)) {
    df <- df %>% tibble::rownames_to_column(var = rowname_label)
  }

  # Split the data frame using the split_df function
  new_df <- split_df(df, num_col)

  # Determine the document type (HTML, LaTeX, or Word)
  doc.type <- knitr::opts_knit$get('rmarkdown.pandoc.to')
  if (is.null(doc.type)) doc.type <- 'html'

  # Render for HTML or Word output (flextable)
  res <- flextable::flextable(new_df) %>%
    flextable::fontsize(size = fontsize) %>%
    flextable::colformat_double(big.mark = big.mark, digits = digits, na_str = na_str) %>%
    flextable::autofit()

  if (num_col > 1) {
    columns_add_vline = seq(ncol(new_df) / num_col, ncol(new_df) - 1, ncol(new_df) / num_col)
    res <- res %>% flextable::vline(j = columns_add_vline,border=officer::fp_border(style='double'))
  }

  return(res)
}
