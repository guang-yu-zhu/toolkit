#' Plot Regression Coefficients
#'
#' @description This function creates a bar plot to visualize regression coefficients, with optional ordering, intercept removal, and coefficient selection.
#'
#' @param coefficients A named vector or data frame of regression coefficients.
#' @param remove_intercept Logical. If TRUE, the intercept (first row) will be removed from the plot (default: FALSE).
#' @param order_coef Logical. If TRUE, the coefficients will be sorted by absolute value before plotting (default: TRUE).
#' @param select A logical vector to specify which coefficients are selected. If provided, the plot will highlight selected coefficients (default: NULL).
#' @param title A character string specifying the title of the plot (default: "Regression Coefficients").
#'
#' @return A ggplot object displaying the regression coefficients as a bar plot.
#'
#' @details This function takes regression coefficients and creates a horizontal bar plot. It allows for sorting by the absolute value of coefficients, removing the intercept, and highlighting selected coefficients if a logical vector is provided. The plot is useful for interpreting regression models.
#'
#' @examples
#' # Example usage:
#' fit<-lm(mpg~.,data=mtcars)
#' coef(fit)%>%plot_coef(remove_intercept = TRUE)
#'
#' @importFrom ggplot2 ggplot aes geom_bar coord_flip labs theme_minimal
#' @importFrom dplyr mutate arrange slice rename
#' @importFrom tidyr drop_na
#' @importFrom tibble as_tibble rownames_to_column
#' @export
#' @md
plot_coef <- function(coefficients, remove_intercept = FALSE, order_coef = TRUE, select = NULL, title = "Regression Coefficients") {

  # Convert coefficients to a data frame
  coef_df <- coefficients %>%
    as.matrix() %>%
    as.data.frame() %>%
    dplyr::rename(Coefficient = 1) %>%
    tibble::rownames_to_column("Variable")

  # Optionally remove the intercept
  if (remove_intercept) {
    coef_df <- coef_df %>%
      slice(-1)  # Remove the first row (intercept)
  }

  # Handle selection of coefficients if 'select' is provided
  if (!is.null(select)) {
    coef_df <- coef_df %>%
      dplyr::mutate(select = select) %>%
      dplyr::mutate(select = factor(select, levels = c(TRUE, FALSE), labels = c("Selected", "Not Selected"))) %>%
      tidyr::drop_na()  # Remove rows with NA
  }

  # Optionally order by the absolute value of the coefficients
  if (order_coef) {
    coef_df <- coef_df %>%
      dplyr::arrange(abs(Coefficient)) %>%
      dplyr::mutate(Variable = factor(Variable, levels = Variable))  # Reorder factors
  }

  # Create the bar plot

  g <- ggplot2::ggplot(coef_df, aes(x = Variable, y = Coefficient, fill = select)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::coord_flip() +
    ggplot2::labs(title = title,
         x = "Variables",
         y = "Coefficient") +
    ggplot2::theme_bw()

  return(g)
}
