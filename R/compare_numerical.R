#' Compute Mean, SD for Two Samples Decided by Grouping Variable and Perform One-Way Test
#'
#' @description This function calculates the mean and standard deviation of a numerical variable for each level of a grouping variable and performs a one-way ANOVA test. The result is a formatted table that includes group-wise means, standard deviations, and a p-value.
#'
#' @param dat A data frame containing the variables.
#' @param var1 The grouping variable (factor or categorical).
#' @param var2 The numerical variable for which mean and standard deviation are computed.
#' @param varname A character string used to name the row for the output table.
#'
#' @return A data frame containing group-wise means and standard deviations of `var2` for each level of `var1`, along with the p-value from a one-way ANOVA test.
#'
#' @details The function computes the mean and standard deviation for `var2` within each group defined by `var1` and overall. It performs a one-way ANOVA test to assess differences in means between groups. The results are returned in a table that includes the p-value.
#'
#' @examples
#' compare_numerical(mtcars,'cyl','mpg',varname = 'mile per gallon')
#'
#' @importFrom dplyr %>% select summarize group_by mutate_at
#' @importFrom stats oneway.test sd
#'
#' @export
compare_numerical <- function(dat, var1, var2, total=FALSE,varname,colname='Variables') {
  rowname = paste0(varname, ', mean (SD)')
  column = c(var1, var2)
  dat2 <- dat %>% select(all_of(column))

  # Mean and SD for each group defined by var1
  res1 <- dat2 %>% group_by(.data[[var1]]) %>% summarize(
    mean = mean(.data[[var2]], na.rm = TRUE),
    sd = sd(.data[[var2]], na.rm = TRUE)
  ) %>% as.data.frame() %>% mutate_at(var1, as.character)

  # Append overall results to the group-wise results
  if(total){
    res <- rbind(res1, NA)
    # Overall mean and SD for var2
    res2 <- dat2 %>% summarize(
      mean = mean(.data[[var2]], na.rm = TRUE),
      sd = sd(.data[[var2]], na.rm = TRUE)
    ) %>% as.data.frame()
    res[nrow(res), 1] = 'Total'
    res[nrow(res), 2:3] = res2
  }else{
    res <- res1
  }

  # Perform one-way ANOVA test
  test2 = oneway.test(formula(paste(var2, "~", var1)), dat2)

  # Create result table with group statistics and p-value
  table2 = as.data.frame(matrix(ncol = nrow(res) + 1, nrow = 1))
  colnames(table2) <- c(as.character(res[, 1]), 'p-value')

  table2[1, 1:nrow(res)] = c(paste0(sprintf("%.2f", res[, 2]), ' (', sprintf("%.2f", res[, 3]), ')'))
  table2[1, nrow(res) + 1] = zpvalue(test2$p.value)

  rownames(table2) = c(rowname)
  table2 <- table2 %>% rownames_to_column(colname)

  table2
}
