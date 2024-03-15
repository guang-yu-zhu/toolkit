#' Draw a normal distribution plot using ggplot2.
#'
#' This function creates a ggplot2 plot representing the probability density function (PDF)
#' of a normal distribution with specified mean and standard deviation.
#'
#' @param mu Mean of the normal distribution.
#' @param sd Standard deviation of the normal distribution.
#'
#' @return A ggplot2 object representing the normal distribution plot.
#'
#' @examples
#' # Example usage:
#' # draw_normal(mu = 0, sd = 1)
#'
#' @import ggplot2
#'
#' @export
draw_normal <- function(mu = 0, sd = 1,color_density="blue") {
  lower <- mu - 3 * sd
  upper <- mu + 3 * sd

  # Create ggplot with normal distribution plot
  p <- ggplot(NULL, aes(c(lower, upper))) +
    geom_line(stat = "function", fun = dnorm, args = list(mean = mu, sd = sd), color = color_density) +
    labs(title = paste0('N(',mu,', ',sd,')',' PDF'),
         x = "",
         y = "Probability Density") +
    theme_minimal()

  return(p)
}
