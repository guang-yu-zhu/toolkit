# This function add an option to kintr such that the output is put into multicol enviroment
# For example: columns = 2

#' Format Code Output with Custom Font Size in LaTeX
#'
#' This function wraps code in a LaTeX `Verbatim` environment for display in LaTeX documents.
#' The font size of the code output can be customized by setting the `outputsize` parameter
#' in the chunk options.
#'
#' @param x A character string containing the code to be formatted.
#' @param options A list of chunk options, including `outputsize`, which sets the font size
#' in the Verbatim environment.
#'
#' @return A character vector containing the LaTeX-formatted Verbatim environment with the specified font size.
#'
#' @details
#' - You can set the font size for the Verbatim environment by specifying `outputsize` in the chunk options.
#'   For example, `outputsize = 'small'` will use `\small` for the font size.
#' - Valid font size values in LaTeX include options like `tiny`, `scriptsize`, `footnotesize`, `small`,
#'   `normalsize`, `large`, `Large`, `LARGE`, `huge`, etc.
#'
#' @examples
#' # Setting output font size to small in a chunk:
#' # rnw.verb.hook("my code", list(outputsize = "small"))
#' @keywords internal
rnw.verb.hook <- function(x, options) {
  headpart <- paste0('\\begin{Verbatim}[fontsize=\\', options$outputsize, ']')
  one_string(c(headpart, sub('\n$', '', x), '\\end{Verbatim}', ''))
}
