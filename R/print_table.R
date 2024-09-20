#' Print a Styled Table for HTML or LaTeX Output
#'
#' This function prints a styled table in either HTML or LaTeX format, depending on the output document type.
#'
#' @param df The data frame to be formatted as a table.
#' @param num_col An integer specifying the number of columns to split the output table into (default: 1).
#' @param rowname A character string specifying the column name for the row names; if not provided, the row names will not be printed (default: NA).
#' @param caption A character string specifying the caption for the table (default: '').
#' @param digits An integer indicating the number of digits to display (default: 2).
#' @param fontsize Font size for the table (default: 9).
#' @param big.mark A character string used as the thousands separator for numbers (default: ',').
#' @param na_str A string used to replace missing values (default: '').
#' @param ... Additional arguments to be passed to the formatting functions.
#'
#' @return A styled table in HTML or LaTeX format, depending on the document type.
#'
#' @examples
#' df <- mtcars[1:10, 1:3]
#' print_table(df, num_col = 2, rowname = 'car')
#'
#' @importFrom magrittr %>%
#' @importFrom kableExtra kbl kable_styling row_spec
#' @importFrom flextable set_flextable_defaults flextable fontsize set_caption colformat_double autofit
#' @importFrom knitr opts_knit
#' @importFrom tibble rownames_to_column
#' @export
#' @md
print_table <- function(df, num_col=1, rowname = NA, caption = '', digits = 2, fontsize = 9,
                        big.mark = ',', na_str = '', ...) {
  # num_col=3;caption = ''; digits = 2; rowname ='car'; fontsize = 9;big.mark = ','; na_str = ''
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
  if(!is.na(rowname)) df <- df %>% tibble::rownames_to_column(var = rowname)
  if(num_col>1){
    ncols = ncol(df)
    nrows <- nrow(df)
    rows_per_part <- ceiling(nrows / num_col)
    df_list <- list()
    colname_list<-list()
    for (i in 1:num_col) {
      # Define the row range for each part
      df_part <- as.data.frame(matrix(NA, nrow = rows_per_part, ncol = ncols))
      row_start <- (i - 1) * rows_per_part + 1
      row_end <- min(i * rows_per_part, nrows)
      df_part[seq_along(row_start:row_end),] = df[row_start:row_end, , drop = FALSE]
      df_list[[i]] <- df_part
      colname_list[[i]]=paste0(colnames(df),paste0(rep("\r",i-1),collapse = ""))
    }
    new_df=do.call(cbind, df_list)
    colnames(new_df) <-  do.call(c, colname_list)
  }else{
    new_df = df
  }
  # Render for HTML or Word output (flextable)
  if (doc.type %in% c("docx", "html")) {
    res<-flextable::flextable(new_df) %>%
      flextable::fontsize(size = fontsize) %>%
      #flextable::set_caption(caption = caption) %>%
      flextable::colformat_double(big.mark = big.mark, digits = digits, na_str = na_str) %>%
      flextable::autofit()
    if(num_col>1){
      res = res%>%flextable::vline(j= seq(ncols,num_col* ncols-1, ncols))
    }
  }
  # Render for LaTeX output (kableExtra)
  else {
    res<-kableExtra::kbl(new_df, caption = caption, digits = digits, booktabs = TRUE, linesep = "", ...) %>%
      kableExtra::kable_styling(latex_options = c("HOLD_position"),
                                position = 'center',
                                table.envir = 'table',
                                font_size = fontsize,
                                full_width = FALSE) %>%
      kableExtra::row_spec(0, font_size = fontsize)
  }
  return(res)
}
