#' Print a Matrix in LaTeX pmatrix Format
#'
#' This function prints a given matrix in LaTeX `pmatrix` format, rounded to a specified number of decimal places.
#'
#' @param matr A numeric matrix to be printed.
#' @param digits An integer specifying the number of decimal places to round the elements of the matrix. Default is 2.
#'
#' @return The function does not return a value; it prints the matrix in LaTeX `pmatrix` format.
#' 
#' @examples
#' mat <- matrix(c(1.123456, 2.654321, 3.987654), nrow = 3)
#' pmatrix(mat, digits = 3)
#'
#' @export
pmatrix <- function(matr, digits = 2) {
  matr <- round(x = matr, digits = digits)
  printmrow <- function(x) {
    cat(cat(x, sep = " & "), "\\\\ \n")
  }
  cat("\\begin{pmatrix}","\n")
  body <- apply(matr, 1, printmrow)
  cat("\\end{pmatrix}")
}