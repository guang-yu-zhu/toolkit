#' Set custom theme for ggplot2 plots
#'
#' This function sets a custom theme for ggplot2 plots with a light background color.
#'
#' @details
#' The function loads the ggplot2 library and sets a custom theme with a light background color.
#'
#' @return None
#'
#' @examples
#' set_ggplot_theme()
#'
#' @import ggplot2
#' @export
set_ggplot_theme<-function(){
  library(ggplot2)
  bg='#FAFAFA'
  my_light_theme <- theme_bw(base_size = 14)+
    theme(plot.background = element_rect(fill = bg),
          panel.background= element_rect(fill = bg),
          legend.background= element_rect(fill = bg),
          legend.key = element_rect(fill = bg))
  theme_set(my_light_theme)
}
