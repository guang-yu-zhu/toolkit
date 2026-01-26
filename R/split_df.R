#' Split Data Frame into Multiple Parts
#'
#' This function takes a data frame and splits it into multiple parts based on the specified number of columns.
#' It can also include row names as a column if a valid row name label is provided.
#'
#' @param df A data frame to be split.
#' @param num_part An integer specifying the number of columns to split the data frame into.
#' @param return_list A logical indicating whether to return a list of data frames (default: FALSE).
#'
#' @return A new data frame that is split into the specified number of columns or a list of data frames if return_list is TRUE.
#' @examples
#' split_df(mtcars[1:10, 1:3], num_part = 2, return_list = FALSE)
#' @importFrom magrittr %>%
#' @importFrom tibble rownames_to_column
#' @export
#' @md
split_df <- function(df, num_part, return_list = FALSE) {
  ncols <- ncol(df)
  nrows <- nrow(df)

  if (num_part <= 0) {
    stop("num_part must be a positive integer.")
  }

  if (num_part == 1) {
    return(df)  # No splitting needed
  }

  rows_per_part <- ceiling(nrows / num_part)
  df_list <- vector("list", num_part)  # Preallocate list for efficiency
  colname_list <- vector("list", num_part)
  for (i in seq_len(num_part)) {
    row_start <- (i - 1) * rows_per_part + 1
    row_end <- min(i * rows_per_part, nrows)

    # Create a part and ensure it has the correct dimensions
    df_part <- df[row_start:row_end, , drop = FALSE]
    colname_list[[i]] <- paste0(colnames(df), paste0(rep("\r", i - 1), collapse = ""))

    # If part is smaller than expected, fill with NA
    if (nrow(df_part) < rows_per_part) {
      empt_df=as.data.frame(matrix(NA, nrow = rows_per_part - nrow(df_part), ncol = ncols))
      colnames(empt_df)<-colnames(df_part)
      df_part <- rbind(df_part, empt_df)
    }

    df_list[[i]] <- df_part
  }



  if (return_list) {
    return(df_list)  # Return the list of data frames
  } else {
    new_df <- do.call(cbind, df_list)  # Combine parts into a single data frame
    colnames(new_df) <- do.call(c, colname_list)
    return(new_df)
  }
}
