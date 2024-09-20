#' Format p-value for Display
#'
#' @description This function formats a p-value for display, rounding it to three decimal places. If the p-value is smaller than 0.001, it returns '<0.001'.
#'
#' @param x A numeric value representing a p-value.
#'
#' @return A character string. If `x > 0.001`, it returns the p-value rounded to three decimal places. If `x <= 0.001`, it returns '<0.001'.
#'
#' @examples
#' # Example usage of the function:
#' zpvalue(0.005)
#' zpvalue(0.0005)
#'
#' @export
zpvalue <- function(x) {
  if (x > 0.001) {
    res <- sprintf("%.3f", x)
  } else {
    res <- '<0.001'
  }
  return(res)
}
