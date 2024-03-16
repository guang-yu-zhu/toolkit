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
#'
#' @importFrom magrittr %>%
#' @importFrom kableExtra kbl kable_styling row_spec
#' @importFrom flextable set_flextable_defaults flextable fontsize set_caption colformat_double autofit
#' @importFrom knitr opts_knit
#' @importFrom tibble rownames_to_column
#' @export
print_table<-function(df,caption = '',digits=2,print_rownames=FALSE,fontsize=9,...){
  flextable::set_flextable_defaults(
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
    if(print_rownames) df <- df %>% tibble::rownames_to_column(var='rowname')
    flextable::flextable(df)%>%
      flextable::fontsize(size=fontsize)%>%
      flextable::set_caption(caption=caption)%>%
      flextable::colformat_double(digits = digits)%>%
      flextable::autofit()
  }
  else{
    kableExtra::kbl(df,caption=caption,digits=digits, booktabs = TRUE, linesep = "", ...)%>%
      kableExtra::kable_styling(latex_options = c("HOLD_position"),
                    position = 'center',
                    table.envir='table',
                    font_size=fontsize,
                    full_width = FALSE)%>%
      kableExtra::row_spec(0, font_size=fontsize)
  }
}

