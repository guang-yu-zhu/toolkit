#' Round and Format Numbers to Specific Decimal Places
#'
#' This function takes a numeric vector or a single numeric value and
#' formats it to a specified number of decimal places. The function
#' returns a character vector with the numbers formatted as strings.
#'
#' @param x A numeric vector or a single numeric value.
#' @param digits An integer indicating the number of decimal places
#'   to format the numbers. Defaults to 2.
#'
#' @return A character vector with each number in `x` formatted to
#'   `digits` decimal places.
#'
#' @examples
#'
#' x=c(5.555,1.115,-0.002)
#' zround(x,method=1)
#' zround(x,method=2)
#' formatC(x, digits = 2, format = "f")
#' formattable::formattable(x, digits = 2, format = "f")
#'
#' @export
zround <- function(x, digits = 2,method=1) {
  if(method==1){
    x = format(round(x, digits), trim = T,nsmall = digits)
  }
  else{ x = sprintf(paste0("%.", digits, "f"), x)
  }
  x
}


#' Calculate Mean and Standard Deviation, and Format the Output
#'
#' This function computes the mean and standard deviation of a numeric vector
#' and returns a formatted string with the mean and standard deviation, rounded to
#' a specified number of decimal places.
#'
#' @param x A numeric vector for which the mean and standard deviation are to be computed.
#' @param digits An integer indicating the number of decimal places to which the
#'   results will be rounded. Defaults to 2.
#'
#' @return A character string containing the mean and standard deviation of `x`,
#'   formatted as 'mean (standard deviation)', each rounded to `digits` decimal places.
#'
#' @examples
#' meanSD(c(10.5, 5.3, 7.8), digits = 3)
#'
#' @export
meanSD <- function(x, digits = 2) {
  res1 <- c(mean(x), sd(x))
  res2 <- zround(res1, digits = digits)
  res = paste0(res2[1], ' (', res2[2], ')')
  res
}

#' Calculate and Format Range of a Numeric Vector
#'
#' This function calculates the range (minimum and maximum values) of a given numeric vector.
#' It then formats and rounds these values to a specified number of decimal places, returning them as a string.
#'
#' @param x A numeric vector for which the range (min and max) is to be calculated.
#' @param digits An integer indicating the number of decimal places to which the
#'   results will be rounded. Defaults to 2.
#'
#' @return A character string representing the range of `x`, formatted as
#'   '(min, max)', where 'min' and 'max' are the minimum and maximum values of `x`
#'   rounded to `digits` decimal places.
#'
#' @examples
#' zrange(c(1.234, 5.678, 9.012))
#' zrange(c(10, 20, 30, 40, 50), digits = 1)
#'
#' @export
zrange <- function(x, digits = 2) {
  res1 <- c(min(x), max(x))
  res2 <- zround(res1, digits = digits)
  res = paste0('(', res2[1], ', ', res2[2], ')')
  res
}


#' Calculate Counts and Percentages of Factor Levels
#'
#' This function computes the counts and percentages for each level of a factor or categorical variable.
#' It returns a character vector with each level's count and its percentage of the total, formatted to a specified number of decimal places.
#'
#' @param x A factor or a categorical variable.
#' @param digits An integer indicating the number of decimal places to which the
#'   percentages will be rounded. Defaults to 2.
#'
#' @return A character vector where each element corresponds to a level of `x`.
#'   Each element is formatted as 'count (percentage%)', where 'count' is the number of occurrences of the level
#'   and 'percentage' is the percentage of the total, rounded to `digits` decimal places.
#'
#' @examples
#' levels= LETTERS[1:3]
#' factor1 <- levels%>%sample(size = 10, replace = TRUE)%>%factor()
#' count_percent(factor1,1)
#'
#' @export
count_percent <- function(x,digits=2) {
  counts <- table(x)
  percent <- 100 * counts / sum(counts)
  result <- paste(counts, " (", zround(percent, digits), "%)", sep="")
  return(result)
}

