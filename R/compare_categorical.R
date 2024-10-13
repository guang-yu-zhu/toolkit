#' Perform Cross-Tabulation and Fisher's Exact Test
#'
#' @description This function performs a cross-tabulation between two categorical variables and conducts Fisher's exact test, returning a formatted table with counts, proportions, and p-values.
#'
#' @param dat A data frame containing the variables.
#' @param col_var The grouping variable (categorical or factor) to classify the data into two or more groups.
#' @param row_var The categorical variable to compare against `col_var`.
#' @param varname Optional. A character string to label the `row_var` row in the output table (default is an empty string).
#' @param total Logical; if TRUE, includes an overall total in the table.
#' @param colname Optional. The name of the first column in the output table (default is 'Variables').
#'
#' @return A data frame with the cross-tabulation results, including counts, proportions, and the p-value from Fisher's exact test.
#'
#' @details This function creates a contingency table for `col_var` and `row_var` using `xtabs`, adds row margins if requested, computes proportions, and performs Fisher's exact test to assess the association between the two variables. The output is a formatted table with counts, proportions, and the test's p-value.
#'
#' @examples
#' compare_categorical(mtcars, 'cyl', 'gear', varname = 'Gear')
#'
#' @importFrom dplyr %>%
#' @importFrom stats xtabs fisher.test addmargins
#'
#' @export
#' @md
compare_categorical <- function(dat, col_var, row_var, varname = '', total = FALSE, colname = 'Variables') {
  # dat=mtcars; col_var='cyl'; row_var='gear'; varname = ''; total = FALSE; colname = 'Variables'
  # Create a contingency table
  tab1 <- dat %>%
    stats::xtabs(formula = paste0('~', row_var, '+', col_var))

  # Add row margins (total column) if 'total' is TRUE
  if (total) {
    tab1 <- tab1 %>% stats::addmargins(margin = 2)
  }

  # Calculate proportions based on column totals
  tab2 <- round(prop.table(tab1, margin = 2) * 100, 1)

  # Perform Fisher's exact test with simulated p-value
  test1 <- stats::fisher.test(tab1, simulate.p.value = TRUE)

  # Combine counts and proportions into a single table
  combined_table <- toolkit::combine_table(tab1, tab2)
  rownames(combined_table) <- paste0('     ', rownames(combined_table))
  if(total) colnames(combined_table)[ncol(combined_table)] <- 'Total'

  # Create the final output table
  final_table <- NA %>%
    rbind(combined_table) %>%
    cbind('p-value' = NA)

  # Set the row name for the variable label and p-value
  rownames(final_table)[1] <- paste0(varname, ', n (%)')
  final_table[1, ncol(final_table)] <- toolkit::zpvalue(test1$p.value)

  # Convert rownames to a column and return the formatted table
  final_table <- final_table %>% tibble::rownames_to_column(var = colname)

  return(final_table)
}
