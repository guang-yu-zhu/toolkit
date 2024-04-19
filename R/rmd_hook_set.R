#' Set custom knit hooks for R Markdown
#'
#' This function sets custom knit hooks for R Markdown documents to customize behavior during knitting.
#'
#' @details
#' The function sets inline knit hook to format numeric values and par hook to customize plotting parameters.
#'
#' @return None
#'
#' @examples
#' rmd_hook_set()
#'
#' @importFrom knitr knit_hooks
#' @export
rmd_hook_set<-function(){
  # inline hook ####
  knitr::knit_hooks$set(inline = function(x) {
    if(is.numeric(x)){
      x <- sprintf("%1.2f", x)
    }
    paste(as.character(x), collapse = ', ')
  })
  # crop #########
  # Add the zhook_pdfcrop function to allow control over whether cropping will be applied by setting crop=TRUE.
  zhook_pdfcrop<-function (before, options, envir) {
    if (options$crop)
      knitr::hook_pdfcrop(before, options, envir)
  }
  knitr::knit_hooks$set(crop=zhook_pdfcrop)


  bg='#FAFAFA'
  knitr::knit_hooks$set(
    par=function(before, options, envir){
      if (before &&options$fig.show!='none')
        par(mar=c(3,3,1,1),cex.lab=1.2,cex.axis=1.2,cex.main=1.2,mgp=c(1.5,0.5,0),
            tcl=-.3,pch=16, bg=bg,fg='black',
            col.axis='black',col.lab='black',col.main='black',
            col.sub='black')
    }
  )
}
