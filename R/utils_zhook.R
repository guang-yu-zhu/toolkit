# utils functions #######################
# whether to write results as-is?
output_asis <- function(x, options) {
  # Returns TRUE if output should be suppressed based on conditions
  is_blank(x) || options$results == "asis" #|| options$echo == FALSE
}
is_blank = function(x) {
  if (length(x)) all(grepl('^\\s*$', x)) else TRUE
}

# collapse by \n
one_string = function(x, ...){
  paste(x, ..., collapse = '\n')
}


# define a color variable in TeX
color_def = function(col, variable = 'shadecolor') {
  if (all(is.na(col))) return('')  # no LaTeX code when color is NA
  x = if (length(col) == 1L) sc_split(col) else col
  if ((n <- length(x)) != 3L) {
    if (n == 1L) x = drop(col2rgb(x) / 255) else {
      x = switch(variable, shadecolor = rep(.97, 3), fgcolor = rep(0, 3))
      warning("the color '", col, "' is invalid;",
              'using default color...',
              'see https://yihui.org/knitr/options/')
    }
  }
  if (length(x) != 3L) stop('invalid color:', col)
  if (is.numeric(x)) x = round(x, 3L)
  xfun::decimal_dot(
    sprintf('\\definecolor{%s}{rgb}{%s, %s, %s}', variable, x[1], x[2], x[3])
  )
}

# split by semicolon or colon
sc_split = function(string) {
  if (is.call(string)) string = eval(string)
  if (is.numeric(string) || length(string) != 1L) return(string)
  trimws(strsplit(string, ';|,')[[1]])
}

# is tikz device without externalization?
is_tikz_dev = function(options) {
  'tikz' %in% options$dev && !options$external
}

tikz_dict = function(path) {
  paste(sans_ext(basename(path)), 'tikzDictionary', sep = '-')
}

.rm.empty.envir = function(x) {
  # Remove empty `kframe` environments, including optional `\scriptsize` or similar commands
  x = gsub('\\\\begin\\{(kframe)\\}(?:\\\\\\w+)?\\s*\\\\end\\{\\1\\}', '', x)
  
  # Remove empty `verbatim` or `alltt` environments
  gsub('\\\\end\\{(verbatim|alltt)\\}\\s*\\\\begin\\{\\1\\}[\n]?', '', x)
}
write_utf8<-function (text, con, ...) 
{
  if (is.null(text)) 
    text = character(0)
  if (identical(con, "")) {
    cat(text, sep = "\n", file = con)
  }
  else {
    opts = options(encoding = "native.enc")
    on.exit(options(opts), add = TRUE)
    writeLines(enc2utf8(text), con, ..., useBytes = TRUE)
  }
}