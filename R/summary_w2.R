#' Summarize W2 Data for a Given Year
#'
#' This function reads W2 data from an Excel file for a given year, calculates
#' the total amount for each person and category, and generates a summary table.
#'
#' @param year The year for which W2 data is to be summarized.
#' @param by_person Logical indicating whether to summarize the data by person (default: FALSE).
#'
#' @return A flextable object representing the summary table of W2 data for the specified year.
#'
#' @import readxl
#' @import dplyr
#' @importFrom flextable flextable set_caption align colformat_num colformat_double merge_v hline
#' @importFrom magrittr %>%
#'
#' @export
summary_w2 <- function(year, by_person = FALSE) {
  # Construct the file path
  file_name <- paste0(year, '/', year, '-W2.xlsx')

  # Read the W2 data from the Excel file
  dat_W2 <- readxl::read_excel(file_name, sheet = 1, skip = 0)

  if (by_person) {
    # Summarize by person
    tab <- dat_W2 %>%
      dplyr::group_by(Person) %>%
      dplyr::mutate(Person_Total = sum(Amount)) %>%
      dplyr::group_by(Person, Category) %>%
      dplyr::mutate(Category_Total = sum(Amount)) %>%
      dplyr::mutate(Category_Percentage = Category_Total / Person_Total * 100) %>%
      dplyr::relocate(Person_Total, .after = dplyr::last_col()) %>%
      dplyr::arrange(Person, Category, Item)

    # Format and return the flextable summary
    tab %>%
      flextable::flextable() %>%
      flextable::merge_v(j = ~Person + Category + Category_Total + Category_Percentage + Person_Total) %>%
      flextable::align(align = "right", part = "all") %>%
      flextable::colformat_num(j = ~Amount + Category_Total + Person_Total, big.mark = ",", decimal.mark = ".", prefix = "$") %>%
      flextable::colformat_double(j = ~Category_Percentage, suffix = '%', digits = 2) %>%
      flextable::hline(part = "all") %>%
      flextable::set_caption(paste0(year, '年 W2 by Person'))

  } else {
    # Summarize without grouping by person
    tab <- dat_W2 %>%
      dplyr::select(-Person) %>%
      dplyr::summarise(Amount = sum(Amount), .by = c(Category, Item)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Total = sum(Amount)) %>%
      dplyr::group_by(Category) %>%
      dplyr::mutate(Category_Total = sum(Amount)) %>%
      dplyr::mutate(Category_Percentage = Category_Total / Total * 100) %>%
      dplyr::relocate(Total, .after = dplyr::last_col()) %>%
      dplyr::arrange(Category, Item)

    # Format and return the flextable summary
    tab %>%
      flextable::flextable() %>%
      flextable::merge_v(j = ~Category + Category_Total + Category_Percentage + Total) %>%
      flextable::align(align = "right", part = "all") %>%
      flextable::colformat_num(j = ~Amount + Category_Total + Total, big.mark = ",", decimal.mark = ".", prefix = "$") %>%
      flextable::colformat_double(j = ~Category_Percentage, suffix = '%', digits = 2) %>%
      flextable::hline(part = "all") %>%
      flextable::set_caption(paste0(year, '年 W2'))
  }
}
