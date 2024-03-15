#' Print a styled table for HTML or LaTeX output
#'
#' This function prints a styled table either in HTML or LaTeX format based on the output document type.
#'
#' @param df The data frame to be formatted as a table.
#' @param caption The caption for the table (default: '').
#' @param digits Number of digits to display (default: 2).
#' @param print_rownames Logical indicating whether to print row names (default: FALSE).
#' @param fontsize Font size for the table (default: 9).
#' @param ... Additional arguments to be passed to the formatting functions.
#'
#' @return A styled table either in HTML or LaTeX format.
#'
#' @examples
#' df <- data.frame(x = 1:5, y = letters[1:5])
#' print_table(df)
#'
#' @import magrittr
#' @import flextable
#' @import knitr
#' @import kableExtra
#' @export
print_table<-function(df,caption = '',digits=2,print_rownames=FALSE,fontsize=9,...){
  set_flextable_defaults(
    digits = 4,
    decimal.mark = ".",
    big.mark=",",
    na_str = "",
    post_process_html = autofit,
    post_process_pdf = autofit,
    post_process_docx = autofit
  )
  doc.type <- knitr::opts_knit$get('rmarkdown.pandoc.to')
  df=df%>%as.data.frame()
  if(is.null(doc.type)) doc.type<-'html'
  if (doc.type == "docx"){
    if(print_rownames) df <- df %>% rownames_to_column(var='rowname')
    flextable(df)%>%
      fontsize(size=fontsize)%>%
      set_caption(caption=caption)%>%
      colformat_double(digits = digits)%>%
      autofit()
  }
  else{
    kbl(df,caption=caption,digits=digits, booktabs = TRUE, linesep = "", ...)%>%
      kable_styling(latex_options = c("HOLD_position"),
                    position = 'center',
                    table.envir='table',
                    font_size=fontsize,
                    full_width = FALSE)%>%
      row_spec(0, font_size=fontsize)
  }
}

