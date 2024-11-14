#' Custom Output Formatting for Knitr in LaTeX
#'
#' This function customizes how the output of a `knitr` chunk is formatted in LaTeX, supporting multi-column layouts
#' and font size adjustments based on chunk options. It handles both "asis" outputs (displayed directly) and
#' verbatim outputs, making it ideal for LaTeX documents with custom styling needs.
#'
#' @param x A character string representing the content of the chunk to be output.
#' @param options A list of chunk options from `knitr`, specifying how the output should be displayed, including
#' attributes like `results`, `size`, and `columns`.
#'
#' @return A LaTeX-formatted string, with optional multi-column formatting and font size adjustments.
#'
#' @details
#' - If `options$results` is set to "hide", the function immediately returns an empty string.
#' - Font size for the output can be specified with `options$size`.
#' - If `output_asis(x, options)` is `TRUE`, the function directly outputs the content, optionally wrapped in a
#'   multi-column environment if `options$columns > 1`.
#' - If `output_asis` is `FALSE`, the function formats the output with `rnw.verb.hook()` for verbatim display,
#'   and wraps in multi-columns if specified.
#'
#' @examples
#' # Example usage:
#' options=list(results = "asis", size = "small", columns = 1)
#' zoutput.hook.tex("Example content",options )%>%cat
#' @keywords internal
rnw.output.hook <- function(x, options) {
  # Check if the results should be hidden
  if (options$results == 'hide') {
    return("")  # Return an empty string if results are to be hidden
  }

  # Determine font size; leave empty if 'normalsize'
  size = if (options$size == 'normalsize') '' else sprintf('\\%s', options$size)

  # Handle "asis" output
  if (output_asis(x, options)) {
    if (options$columns > 1) {
      # Multi-column output for "asis" results
      top = sprintf('\\begin{multicols}{%s}', options$columns)
      result = paste0('\\end{kframe}\n', '\\noindent\n', top, x, '\\end{multicols}\n\\begin{kframe>')
    } else {
      # Single-column "asis" output
      result = paste0('\\end{kframe}\n', x, '\n\\begin{kframe}')
    }
  } else {
    # Non-"asis" output; use verbatim formatting
    if (options$columns > 1) {
      # Multi-column verbatim output
      top = sprintf('\\setlength\\multicolsep{0pt}\n\\begin{multicols}{%s}', options$columns)
      result = paste0(top, '\\noindent\n', rnw.verb.hook(x, options), '\\end{multicols}')
    } else {
      # Single-column verbatim output
      result = paste0(rnw.verb.hook(x, options))
    }
  }

  result
}
