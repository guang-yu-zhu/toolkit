#' Rounding of Numbers to a string
#'
#' Takes a single numeric argument x and returns a character vector containing strings rounded to specified number of digits after the decimal point (default 2
#' @param x a numeric vector
#' @param digits number of digits after the decimal point. Default is 2.
#'
#' @return
#' @export
#'
#' @examples
#' zround(c(pi,pi*10^5),3)
zround<-function(x,digits=2){
  x=sprintf(paste0("%.",digits,"f"),x)
  x
}



#' Calculate mean and SD of a vector
#'
#' @param x
#' @param digits number of digits after the decimal point. Default is 2.
#'
#' @return
#' @export
#'
#' @examples
#' meanSD(rnorm(100))
meanSD<-function(x,digits=2){
  res1<-c(mean(x),sd(x))
  res2<-sapply(res1,zround,digits=digits)
  res = paste0(res2[1],' (',res2[2],')')
  res
}



#' Get range of a vector
#'
#' @param x
#' @param digits number of digits after the decimal point. Default is 2.
#'
#' @export
#'
#' @examples
#' range(rnorm(100),3)
range<-function(x,digits=2){
  res1<-c(min(x),max(x))
  res2<-sapply(res1,zround,digits=digits)
  res = paste0('(',res2[1],', ',res2[2],')')
  res
}

