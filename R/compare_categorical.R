#' Perform Cross-Tabulation and Fisher's Exact Test
#'
#' @description This function performs a cross-tabulation and Fisher's exact test on two categorical variables, returning a formatted table with proportions and p-values.
#'
#' @param dat A data frame containing the variables.
#' @param var1 The grouping variable (factor or categorical) used to classify the data into two groups.
#' @param var2 The categorical variable to compare against `var1`.
#' @param varname Optional. A character string to name the `var2` row in the output table (default is an empty string).
#' @param colname Optional. The name of the first column in the output table (default is 'Variables').
#'
#' @return A data frame with cross-tabulation results, including counts, proportions, and the p-value from Fisher's exact test.
#'
#' @details The function creates a contingency table for `var1` and `var2` using `xtabs`, adds row margins, calculates proportions, and performs Fisher's exact test. The output is a formatted table with the calculated proportions and p-value.
#'
#' @examples
#' compare_categorical(mtcars,'cyl','gear',varname = 'gear')
#'
#' @importFrom dplyr %>%
#' @importFrom stats xtabs fisher.test
#'
#' @export
#' @md
compare_categorical <- function(dat, var1, var2, varname = '', colname = 'Variables') {
  # Create contingency table with margins
  tab1 <- dat %>% xtabs(formula = paste0('~', var2, '+', var1)) %>%
    addmargins(margin = 2)

  # Calculate proportions
  tab2 <- round(prop.table(tab1, margin = 2) * 100, 1)

  # Perform Fisher's exact test
  test1 <- tab1 %>% fisher.test(simulate.p.value = TRUE)

  # Combine the tables
  table <- combine_table(tab1, tab2)
  rownames(table) <- paste0('     ', rownames(table))
  colnames(table)[ncol(table)]='Total'
  #colnames(table) <- c(levels(dat[[var1]]), 'Total')

  # Format the final table
  table1 <- NA %>% rbind(table) %>% cbind('p-value' = NA)
  rownames(table1)[1] <- paste0(varname, ', n (%)')
  table1[1, ncol(table1)] <- zpvalue(test1$p.value)

  # Convert rownames to column
  table1 <- table1 %>% rownames_to_column(var=colname)
  table1
}



