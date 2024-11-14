#' Set Custom Hooks for knitr in RNW Files
#'
#' The `rnw_hook_set` function customizes knitr hooks to control the formatting of code chunks,
#' output, inline values, and plot parameters in `.rnw` files. It is designed to improve
#' the appearance of compiled LaTeX documents by setting specific knitr hooks.
#'
#' @details
#' This function configures several hooks:
#' - `chunk`: Defines the formatting of chunks via the `rnw.chunk.hook` function.
#' - `output`: Sets output formatting via the `rnw.output.hook` function.
#' - `inline`: Formats inline numerical output to three decimal places.
#' - `crop`: Uses `hook_pdfcrop` to crop figures if `crop` is set to `TRUE` in chunk options.
#' - `par`: Controls graphical parameters like margins and text sizes for plots, with a light gray background.
#'
#' @importFrom knitr knit_hooks hook_pdfcrop
#' @importFrom graphics par
#'
#' @examples
#' \dontrun{
#'   rnw_hook_set() # Set hooks for knitr
#' }
#' @export
rnw_hook_set <- function() {
  # Set knitr hooks for customized behavior ##########
  knitr::knit_hooks$set(
    chunk = rnw.chunk.hook,         # Hook for chunk formatting
    output = rnw.output.hook,       # Hook for output formatting
    inline = function(x) {
      x <- sprintf("%1.3f", x)
      paste(x, collapse = ", ")
    },
    crop = function(before, options, envir) {
      if (options$crop)
        knitr::hook_pdfcrop(before, options, envir)
    }
  )

  ## par hook ######
  bg <- '#FAFAFA'
  knitr::knit_hooks$set(
    par = function(before, options, envir) {
      if (before && options$fig.show != 'none')
        graphics::par(
          mar = c(3, 3, 1, 1),
          cex.lab = 1.2,
          cex.axis = 1.2,
          cex.main = 1.2,
          mgp = c(1.5, 0.5, 0),
          tcl = -0.3,
          pch = 16,
          bg = bg,
          fg = 'black',
          col.axis = 'black',
          col.lab = 'black',
          col.main = 'black',
          col.sub = 'black'
        )
    }
  )
}
