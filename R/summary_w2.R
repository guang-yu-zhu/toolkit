#' Summarize W2 data for a given year
#'
#' This function reads W2 data from an Excel file for a given year ('2023/2323-w2/xlsx'), calculates
#' the total amount for each person and category, and generates a summary table.
#'
#' @param year The year for which W2 data is to be summarized.
#'
#' @return A summary table of W2 data for the specified year.
#' @export
#'
#' @import readxl
#' @import dplyr
#' @importFrom flextable flextable
#' @importFrom magrittr %>%
#' @importFrom flextable set_caption
#' @importFrom flextable align
#' @importFrom flextable colformat_num
#' @importFrom flextable colformat_double
#' @importFrom flextable merge_v
#' @importFrom flextable hline
#'
summary_w2 <- function(year,by_person=FALSE) {
  file_name <- paste0(year, '/', year, '-W2.xlsx')
  dat_W2 <- readxl::read_excel(file_name, sheet = 1, skip = 0)
  if(by_person){
    tab <- dat_W2 %>%
      dplyr::group_by(Person) %>%
      dplyr::mutate(Person_Total = sum(Amount)) %>%
      dplyr::group_by(Person, Category) %>%
      dplyr::mutate(Category_Total = sum(Amount)) %>%
      dplyr::mutate(Category_Percentage = Category_Total / Person_Total * 100) %>%
      dplyr::relocate(Person_Total, .after = dplyr::last_col())%>%
      dplyr::arrange(Person,Category,Item)
    tab %>%
      flextable::flextable() %>%
      flextable::merge_v(j = ~Person + Category + Category_Total + Category_Percentage + Person_Total) %>%
      flextable::align(align = "right", part = "all") %>%
      flextable::colformat_num(j = ~Amount + Category_Total + Person_Total, big.mark = ",", decimal.mark = ".", prefix = "$") %>%
      flextable::colformat_double(j = ~Category_Percentage, suffix = '%', digits = 2) %>%
      flextable::hline(part = "all") %>%
      flextable::set_caption(paste0(year, '年W2 by Person'))
  }
  else{
   tab<-dat_W2 %>%
      select(-Person)%>%
      dplyr::group_by()%>%
      dplyr::summarise(Amount=sum(Amount),.by = c(Category,Item))%>%
      ungroup()%>%
      dplyr::mutate(Total = sum(Amount)) %>%
      dplyr::group_by(Category) %>%
      dplyr::mutate(Category_Total = sum(Amount)) %>%
      dplyr::mutate(Category_Percentage = Category_Total / Total * 100) %>%
      dplyr::relocate(Total, .after = dplyr::last_col())%>%
      dplyr::arrange(Category,Item)
    tab %>%
      flextable::flextable() %>%
      flextable::merge_v(j = ~Category + Category_Total + Category_Percentage + Total) %>%
      flextable::align(align = "right", part = "all") %>%
      flextable::colformat_num(j = ~Amount + Category_Total +Total, big.mark = ",", decimal.mark = ".", prefix = "$") %>%
      flextable::colformat_double(j = ~Category_Percentage, suffix = '%', digits = 2) %>%
      flextable::hline(part = "all") %>%
      flextable::set_caption(paste0(year, '年W2'))
  }
  
  

}
