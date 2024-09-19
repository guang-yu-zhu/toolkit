#' Combine Two Tables with different statistics.
#'
#' @description This function combines two tables: one with raw counts and another with percentages (or other calculated values). It returns a formatted table that merges these values in a "count (percentage)" format.
#'
#' @param tab1 A data frame or matrix of the first statistics.
#' @param tab2 A data frame or matrix of the second statistics to be combined with `tab1`.
#' @param percent Logical. If `TRUE`, formats `tab2` as percentages (i.e., appends a `%` sign). If `FALSE`, combines the counts and values without the percentage sign.
#'
#' @return A data frame with the combined counts and percentages (or values).
#'
#' @details This function takes two tables, converts them to data frames, and then combines their values in a "count (percentage)" format. If the `percent` argument is `TRUE`, the values in `tab2` are treated as percentages; otherwise, they are combined as raw values.
#'
#' @importFrom dplyr %>% 
#'
#' @export
combine_table <- function(tab1, tab2, percent = TRUE) {
  
  # Convert tables to data frames if they are not already
  tab1 <- as.data.frame(unclass(tab1))
  tab2 <- as.data.frame(unclass(tab2))
  
  # Initialize a new table for combined results
  combined_table <- tab1
  
  # Combine counts and percentages/values
  for (i in seq_len(ncol(tab1))) {
    if (percent) {
      combined_table[, i] <- paste0(tab1[, i], ' (', tab2[, i], '%)')
    } else {
      combined_table[, i] <- paste0(tab1[, i], ' (', tab2[, i], ')')
    }
  }
  
  combined_table
}