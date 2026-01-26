#' Plot Regression Coefficients
#'
#' @description Creates a bar plot of regression coefficients with optional confidence intervals, ordering, intercept removal, and coefficient selection.
#'
#' @param fit A fitted model object (e.g., from `lm` or `glm`).
#' @param remove_intercept Logical. If TRUE, the intercept is removed from the plot (default: TRUE).
#' @param order_coef Logical. If TRUE, coefficients are sorted by absolute value (default: TRUE).
#' @param select A logical vector indicating which coefficients to highlight (default: NULL).
#' @param include_ci Logical. If TRUE, confidence intervals are displayed (default: TRUE).
#' @param title Character. The plot title (default: "Regression Coefficients").
#'
#' @return A ggplot object visualizing the regression coefficients.
#'
#' @details This function plots regression coefficients as a horizontal bar chart. It supports sorting by absolute value, removing the intercept, highlighting selected coefficients, and displaying confidence intervals. It is useful for interpreting regression models.
#'
#' @examples
#' fit <- lm(mpg ~ ., data = mtcars)
#' plot_coef(fit, remove_intercept = TRUE)
#'
#' @importFrom ggplot2 ggplot aes geom_bar geom_errorbar coord_flip labs theme_bw
#' @importFrom dplyr mutate arrange filter rename bind_cols
#' @importFrom tidyr drop_na
#' @importFrom tibble rownames_to_column
#' @export
#' @md
plot_coef <- function(fit, remove_intercept = TRUE, order_coef = TRUE,
                      select = NULL, include_ci = TRUE, title = '') {

  # Convert coefficients to a data frame
  coef_df <- fit %>% coef()%>%
    as.data.frame() %>%
    dplyr::rename(Coefficient = 1) %>%
    tibble::rownames_to_column("Variable")

  # If confidence intervals are included, rename columns accordingly
  if (include_ci) {
    CI_df <- fit %>%
      confint.default() %>%
      as.data.frame() %>%
      dplyr::rename(CI_lower = 1, CI_upper = 2)
    coef_df <- coef_df%>%
      dplyr::bind_cols(CI_df)
  }

  # Optionally remove the intercept
  if (remove_intercept) {
    coef_df <- coef_df %>%
      dplyr::filter(Variable != "(Intercept)")
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

  # Create the base plot
  g <- ggplot2::ggplot(coef_df, ggplot2::aes(x = Variable, y = Coefficient, fill = select)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::geom_hline(yintercept = 0, linewidth=1, color = "black") +
    ggplot2::labs(title = title, x = "Variables", y = "Coefficient") +
    ggplot2::coord_flip() +
    ggplot2::theme_bw()

  # Add confidence intervals if requested
  if (include_ci) {
    g <- g +
      ggplot2::geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.5)
  }

  return(g)
}
