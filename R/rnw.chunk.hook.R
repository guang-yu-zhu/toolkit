#' Custom Knitr Chunk Hook for LaTeX Output
#'
#' Modifies the `knitr` chunk output in LaTeX documents, setting font size specifically
#' within the `kframe` environment rather than at the `knitrout` level. This ensures that
#' only the code portions are affected by the font size setting, while the text output remains independently controlled.
#'
#' @param x A character string containing the chunk content.
#' @param options A list of chunk options from `knitr`, specifying attributes such as font size,
#'   background color, and output format.
#'
#' @return A character string with the LaTeX-formatted output, adjusted for font size
#'   in code-only sections and customized colors.
#'
#' @details
#' - The function begins the `kframe` environment with the specified font size.
#' - If `output_asis()` is `TRUE`, the output is directly inserted without additional formatting.
#' - Font size for the code is controlled within `kframe`, while output text font size
#'   remains unaffected, allowing separate control over text.
#' - If `options$split` is `TRUE`, the output is saved as a separate `.tex` file, allowing for
#'   later inclusion with `\\input`.
#'
#' @examples
#' # Typical usage within a knitr document:
#' # knitr::opts_chunk$set(hook = rnw.chunk.hook)
#' # Set `options$size` to specify the code font size in LaTeX.
#' @keywords internal
#' @importFrom knitr fig_path
rnw.chunk.hook <- function(x, options) {
  # Check if the output should be treated "as is"
  ai <- output_asis(x, options)

  # Set color definition and foreground color, except in TikZ devices
  col <- if (!ai) {
    paste0(color_def(options$background), if (!is_tikz_dev(options)) '\\color{fgcolor}')
  }

  # Define font size, adjusting if it's not 'normalsize'
  size <- if (options$size == 'normalsize') '' else sprintf('\\%s', options$size)

  # Wrap content in kframe with color and size options
  k1 <- paste0(col, '\n\\begin{kframe}', size, '\n')
  k2 <- '\\end{kframe}'

  # Remove empty environments and wrap content in kframe
  x <- .rm.empty.envir(paste0(k1, x, k2))

  # Wrap in knitrout environment if output is not "as is"
  if (!ai) {
    n1 <- if (grepl('^\\s*\n', x)) '' else '\n'
    n2 <- if (grepl('\n\\s*$', x)) '' else '\n'
    x <- sprintf('\\begin{knitrout}%s%s%s\\end{knitrout}', n1, x, n2)
  }

  # Handle file splitting option for external LaTeX input
  if (options$split) {
    name <- knitr::fig_path('.tex', options, NULL)
    if (!file.exists(dirname(name))) dir.create(dirname(name))
    write_utf8(x, name)
    sprintf('\\input{%s}', name)
  } else x
}
