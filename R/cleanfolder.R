#' Clean folder
#'
#' Clean folder after compiling latex
#'
#'
#' @export
#'
cleanFolder <- function() {
  answer <- NA
  #while(!(answer %in% c('y', 'n'))) {
  #  answer <- readline(paste("Clean ", getwd(),"? (y/n)"))
  #}
  #if(answer == 'y') 
  {
    rules <- c('.log', '.vrb', '.nav', '.snm', '.toc', 
               '-tikzDictionary', '.synctex.gz','.aux','.out','.bbl','.bak')
    file.remove(list.files(pattern = paste0('\\',rules ,'$', collapse = '|')))
  }
}
