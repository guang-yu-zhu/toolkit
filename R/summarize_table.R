#' Summarize Data Frame by Column Type
#'
#' This function provides a summary of each column in a data frame, generating different statistics based on the type of data (numeric, factor, character, logical, or other).
#' It returns a tibble with the variable names and associated summary statistics, categorized by their data type.
#'
#' @param df A data frame containing the variables to be summarized.
#'
#' @return A tibble with summary statistics for each variable in the input data frame. The summary includes:
#' \item{Type}{The data type of the variable (Numeric, Factor, Character, Logical, Other).}
#' \item{Mean}{The mean of the numeric variables (if applicable).}
#' \item{Median}{The median of the numeric variables (if applicable).}
#' \item{SD}{The standard deviation of the numeric variables (if applicable).}
#' \item{Min}{The minimum value of the numeric variables (if applicable).}
#' \item{Max}{The maximum value of the numeric variables (if applicable).}
#' \item{Levels}{The number of levels for factor variables (if applicable).}
#' \item{Unique}{The number of unique values for character or factor variables.}
#' \item{Most_Common}{The most frequent value for character or factor variables.}
#' \item{Frequency}{The frequency of the most common value for character or factor variables.}
#' \item{True_Count}{The number of TRUE values for logical variables.}
#' \item{False_Count}{The number of FALSE values for logical variables.}
#' \item{NA_Count}{The number of missing values for each variable.}
#'
#' @examples
#' summarize_table(mtcars)
#'
#' @importFrom dplyr tibble bind_rows
#' @importFrom stats median sd
#' @export
summarize_table <- function(df) {
  # Create a list to store summaries
  summary_list <- list()

  # Loop through each column
  for (col_name in names(df)) {
    # Get the column data
    col_data <- df[[col_name]]

    # Summarize based on type
    if (is.numeric(col_data)) {
      summary_list[[col_name]] <- dplyr::tibble(
        Type = "Numeric",
        Mean = mean(col_data, na.rm = TRUE),
        Median = stats::median(col_data, na.rm = TRUE),
        SD = stats::sd(col_data, na.rm = TRUE),
        Min = min(col_data, na.rm = TRUE),
        Max = max(col_data, na.rm = TRUE),
        NA_Count = sum(is.na(col_data))
      )
    } else if (is.factor(col_data) || is.character(col_data)) {
      summary_list[[col_name]] <- tibble(
        Type = ifelse(is.factor(col_data), "Factor", "Character"),
        Levels = ifelse(is.factor(col_data), nlevels(col_data), NA),
        Unique = length(unique(col_data)),
        Most_Common = names(sort(table(col_data), decreasing = TRUE))[1],
        Frequency = sort(table(col_data), decreasing = TRUE)[1],
        NA_Count = sum(is.na(col_data))
      )
    } else if (is.logical(col_data)) {
      summary_list[[col_name]] <- dplyr::tibble(
        Type = "Logical",
        True_Count = sum(col_data, na.rm = TRUE),
        False_Count = sum(!col_data, na.rm = TRUE),
        NA_Count = sum(is.na(col_data))
      )
    } else {
      summary_list[[col_name]] <- dplyr::tibble(
        Type = "Other",
        NA_Count = sum(is.na(col_data))
      )
    }
  }

  # Combine all summaries into a single data frame
  summary_df <- dplyr::bind_rows(summary_list, .id = "Variable")

  return(summary_df)
}
