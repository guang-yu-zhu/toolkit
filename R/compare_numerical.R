#' Compute Mean and SD for Two Samples by Grouping Variable and Perform One-Way ANOVA
#'
#' @description This function calculates the mean and standard deviation of a numerical variable for each level of a grouping variable and performs a one-way ANOVA test. The output is a formatted table that includes group-wise means, standard deviations, and a p-value.
#'
#' @param dat A data frame containing the variables.
#' @param col_var The grouping variable (factor or categorical).
#' @param row_var The numerical variable for which mean and standard deviation are computed.
#' @param varname A character string to label the row in the output table.
#' @param total Logical; if TRUE, includes overall mean and SD.
#' @param colname A character string for naming the output table column.
#'
#' @return A data frame containing group-wise means and standard deviations of `row_var` for each level of `col_var`, along with the p-value from the one-way ANOVA test.
#'
#' @details The function computes the mean and standard deviation for `row_var` within each group defined by `col_var`, as well as overall. It performs a one-way ANOVA test to assess differences in means between groups. Results are presented in a table that includes the p-value.
#'
#' @examples
#' compare_numerical(mtcars, 'cyl', 'mpg', varname = 'Miles per Gallon')
#'
#' @importFrom dplyr %>% select summarize group_by mutate_at
#' @importFrom stats oneway.test sd formula
#'
#' @export
compare_numerical <- function(dat, col_var, row_var,varname='',total=FALSE,colname='Variables') {
  rowname = paste0(varname, ', mean (SD)')
  column = c(col_var, row_var)
  dat2 <- dat %>% select(all_of(column))

  # Mean and SD for each group defined by col_var
  res1 <- dat2 %>% dplyr::group_by(.data[[col_var]]) %>%  dplyr::summarize(
    mean = mean(.data[[row_var]], na.rm = TRUE),
    sd = sd(.data[[row_var]], na.rm = TRUE)
  ) %>%
    as.data.frame()%>%
    dplyr::mutate_at(col_var, as.character)

  # Append overall results to the group-wise results
  if(total){
    res <- rbind(res1, NA)
    # Overall mean and SD for row_var
    res2 <- dat2 %>%
      dplyr::summarize(
        mean = mean(.data[[row_var]], na.rm = TRUE),
        sd = sd(.data[[row_var]], na.rm = TRUE)
      )%>%
      as.data.frame()
    res[nrow(res), 1] = 'Total'
    res[nrow(res), 2:3] = res2
  }else{
    res <- res1
  }

  # Perform one-way ANOVA test
  test2 = stats::oneway.test(stats::formula(paste(row_var, "~", col_var)), dat2)

  # Create result table with group statistics and p-value
  table2 = as.data.frame(matrix(ncol = nrow(res) + 1, nrow = 1))
  colnames(table2) <- c(as.character(res[, 1]), 'p-value')

  table2[1, 1:nrow(res)] = c(paste0(sprintf("%.2f", res[, 2]), ' (', sprintf("%.2f", res[, 3]), ')'))
  table2[1, nrow(res) + 1] = zpvalue(test2$p.value)

  rownames(table2) = c(rowname)
  table2 <- table2 %>% tibble::rownames_to_column(colname)

  table2
}
