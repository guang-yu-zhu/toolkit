#' Summarize and Plot Columns of a Data Frame
#'
#' This function generates summary plots for each column in a data frame.
#' For numeric columns, it produces histograms, and for factor or character columns,
#' it creates bar plots of the frequencies.
#'
#' @param df A data frame to summarize and plot.
#' @param ncol An integer specifying the number of columns to arrange the plots in (default: 3).
#' This parameter is useful when displaying plots in a grid.
#' @param fill A string specifying the fill color for the bars in histograms or bar plots (default: "firebrick").
#' @param width Numeric value specifying the width of the plot (default: 10).
#' @param height Numeric value specifying the height of the plot (default: 4).
#' @param ... Additional arguments passed to `patchwork::wrap_plots()` for customizing the layout.
#'
#' @return A combined patchwork of `ggplot2` objects arranged in a grid, where each plot corresponds
#' to one of the columns of the data frame.
#'
#' @details
#' This function checks the data type of each column in the provided data frame.
#' - For numeric columns, it creates histograms showing the distribution of the values.
#' - For categorical columns (factors or characters), it converts any character columns to factors
#'   and generates bar plots showing the counts of each level.
#'
#' The function returns a combined set of plots arranged in a grid using `patchwork::wrap_plots`.
#' You can control the number of columns in the grid using the `ncol` argument.
#'
#' @examples
#' # Example with the mtcars dataset
#' summarize_plots(mtcars)
#'
#' @importFrom dplyr group_by summarize across all_of
#' @importFrom ggplot2 ggplot aes_string geom_histogram geom_bar xlab ylab theme_bw theme element_text
#' @importFrom patchwork wrap_plots
#' @export
#' @md
summarize_plots <- function(df, ncol = 3, fill = "firebrick", width = 10, height = 4, ...) {
  plots <- list()

  for (col_name in names(df)) {
    col_data <- df[[col_name]]
    plot <- NULL

    if (is.numeric(col_data)) {
      # Plot for numeric variable
      plot <- ggplot2::ggplot(df, ggplot2::aes_string(x = col_name)) +
        ggplot2::geom_histogram(binwidth = diff(range(col_data, na.rm = TRUE)) / 30, fill = fill, color = "black") +
        ggplot2::xlab(col_name) +
        ggplot2::ylab("") +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.title.y = ggplot2::element_blank())

    } else if (is.factor(col_data) || is.character(col_data)) {
      # Convert character to factor if needed
      if (is.character(col_data)) {
        df[[col_name]] <- as.factor(df[[col_name]])
      }

      # Summary statistics
      summary_stats <- df %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(col_name))) %>%
        dplyr::summarize(Count = n(), .groups = 'drop')

      # Plot for factor/character variable
      plot <- ggplot2::ggplot(summary_stats, ggplot2::aes_string(x = col_name, y = "Count")) +
        ggplot2::geom_bar(stat = "identity", fill = fill, color = "black") +
        ggplot2::xlab(col_name) +
        ggplot2::ylab("") +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                       axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
    }

    if (!is.null(plot)) {
      plots[[col_name]] <- plot
    }
  }

  # Combine the plots into a grid layout
  patchwork::wrap_plots(plots, ncol = ncol, widths = width, heights = height, ...)
}
