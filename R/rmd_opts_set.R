#' Set default options for code chunks in R Markdown
#'
#' This function sets default options for code chunks in R Markdown documents based on the output format.
#'
#' @details
#' The function determines the output format (LaTeX or HTML) and sets appropriate chunk options.
#' For LaTeX output, it sets options such as comment style, output width, figure settings, caching,
#' and code styling. For HTML output, it sets similar options tailored for HTML rendering.
#'
#' @return None
#'
#' @examples
#' rmd_opts_set()
#'
#' @importFrom knitr opts_chunk is_latex_output current_input set_alias
#' @importFrom tools file_path_sans_ext
#' @export
rmd_opts_set<-function(){
  # doc.type   ####
  doc.type <- knitr::opts_knit$get('rmarkdown.pandoc.to')
  if(is.null(doc.type)) doc.type<-'html'
  options(knitr.table.format = doc.type)

  # fig and cache path   ####
  knitr::set_alias(h = 'fig.height', w = 'fig.width')
  # get file name and use it to decide fig and cache path
  infile <- knitr::current_input()
  infile <- tools::file_path_sans_ext(infile) # remove extension
  fig.path<-paste0('fig/',infile,'/')
  cache.path<-paste0('cache/',infile,'/')

  knitr::opts_chunk$set(
    comment="##",par=TRUE,
    message=FALSE,warning=FALSE,error=FALSE,
    include=TRUE,cache=TRUE,echo = TRUE,
    fig.path=fig.path,cache.path=cache.path,
    fig.height=4, fig.width=6,
    fig.align='center',fig.show='hold',
    size='small',outputsize='small',
    split=TRUE, # Whether to split the output into separate files, only works for .Rnw, .Rtex, and .Rhtml documents.
    tidy=FALSE, # tidy=FALSE will keep how the code is typed
    columns=1,crop=TRUE
  )

  #  ---------
  if(knitr::is_latex_output()){
    options(digits=4,width=100)
    knitr::opts_chunk$set(
      out.width='0.6\\textwidth',fig.pos='htbp',
      tidy.opts = list(width.cutoff=60,arrow=TRUE,blank=FALSE,ft.keepnext = FALSE)
    )
  }else{
    options(digits = 4, width = 200,
            dplyr.print_min = 6,
            dplyr.print_max = 6,
            htmltools.dir.version = FALSE,
            formatR.indent = 2
    )
    #knit_theme$set("default") # peaksea, default, autumn
    knitr::opts_chunk$set(
      class.source="code-R-source",class.output="code-R-output",
      collapse=FALSE, # collapse all the source and output blocks from one code chunk into a single block
      fig.showtext = TRUE,
      out.width = '60%',dev='png', # svg is more clear than png
      tidy.opts = list(width.cutoff = 100,arrow=TRUE,blank=FALSE,ft.keepnext = FALSE)
    )
  }
}
