#' Set custom theme for ggplot2 plots
#'
#' This function sets a custom theme for ggplot2 plots with a light background color.
#'
#' @details
#' The function sets various graphical parameters for ggplot2 plots to customize their appearance.
#' 
#' @return None
#' 
#' @examples
#' set_ggplot_theme()
#' 
#' @importFrom ggplot2 theme_bw theme element_rect theme_set
#' @export
set_ggplot_theme <- function() {
  bg <- '#FAFAFA'
  my_light_theme <- ggplot2::theme_bw(base_size = 14) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = bg),
      panel.background = ggplot2::element_rect(fill = bg),
      legend.background = ggplot2::element_rect(fill = bg),
      legend.key = ggplot2::element_rect(fill = bg)
    )
  ggplot2::theme_set(my_light_theme)
}
