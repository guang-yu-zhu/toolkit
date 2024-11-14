#' Set Global and knitr Options for RNW Files
#'
#' The `rnw_opts_set` function sets global R options and configures `knitr` options
#' for formatting and path management, tailored for compiling `.rnw` files.
#'
#' @details
#' This function configures:
#' - Global R options for numeric formatting, console width, and significant figures.
#' - `knitr` options including aliases for figure dimensions, chunk formatting, figure paths,
#'   caching, and figure alignment.
#' - Specific paths for figures and cache, creating directories if they don't exist.
#'
#' @importFrom knitr set_alias opts_knit opts_chunk current_input
#'
#' @examples
#' \dontrun{
#'   rnw_opts_set() # Set options for knitr and global R settings
#' }
#' @export
rnw_opts_set <- function() {
  # Set global options for output formatting ####
  options(
    scipen = 1,           # Disable scientific notation
    digits = 4,           # Number of significant digits
    pillar.sigfig = 4,    # Significant figures for pillar
    width = 95            # Width for console output
  )

  # Set options for knitr ####
  knitr::set_alias(h = 'fig.height', w = 'fig.width')  # Alias for figure dimensions
  knitr::opts_knit$set(
    concordance = FALSE,
    unnamed.chunk.label = "chunk",
    latex.tilde = r"(\hlopt{\tildemid})"  # Custom tilde for LaTeX output
  )

  # Define figure and cache paths ####
  infile <- knitr::current_input()
  infile <- sub("\\.[^.]*$", "", infile)  # Remove file extension from input file name
  fig.path <- paste0('fig/', infile, '/')  # Define path for figures
  cache.path <- paste0('cache/', infile, '/')  # Define path for cache

  # Create directories for figures and cache if they don't exist
  if (!file.exists('fig/')) dir.create('fig/')
  if (!file.exists('cache/')) dir.create('cache/')

  # Set chunk options for knitr ####
  knitr::opts_chunk$set(
    comment = "##",                  # Set comment character for code chunks
    par = TRUE,                      # Allow paragraph mode
    message = FALSE,                 # Suppress messages
    warning = FALSE,                 # Suppress warnings
    split = TRUE,                    # Allow chunk splitting
    include = TRUE,                  # Include chunk output in final document
    cache = TRUE,                    # Enable caching of chunk output
    fig.path = fig.path,             # Set figure path
    cache.path = cache.path,         # Set cache path
    fig.align = 'center',            # Center figures
    fig.pos = 'htbp',                # Figure placement
    fig.height = 4,                  # Default figure height
    fig.width = 7,                   # Default figure width
    out.width = '0.6\\textwidth',    # Output width in LaTeX
    fig.show = 'hold',               # Hold figures until all are plotted
    size = 'scriptsize',             # Set font size for code output
    outputsize = 'scriptsize',       # Set output size for text
    tidy = TRUE,                     # Tidy up code
    tidy.opts = list(
      width.cutoff = 65,             # Control width of code
      arrow = TRUE,                  # Enable arrow for tidy
      blank = FALSE                  # Disable blank lines
    ),
    columns = 1,                     # Number of columns for output
    crop = TRUE                      # Enable cropping for PDF output
  )
}
