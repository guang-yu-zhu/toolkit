% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zkbl.R
\name{zkbl}
\alias{zkbl}
\title{Format and style a table for LaTeX output}
\usage{
zkbl(
  table,
  booktabs = TRUE,
  escape = TRUE,
  format.args = list(decimal.mark = ".", big.mark = ","),
  ...
)
}
\arguments{
\item{table}{The data table to be formatted.}

\item{booktabs}{Logical indicating whether to use the booktabs formatting (default: TRUE).}

\item{escape}{Logical indicating whether to escape special characters in the table (default: TRUE).}

\item{...}{Additional arguments to be passed to the kbl function.}
}
\value{
A LaTeX-formatted table with specified styling.
}
\description{
This function formats and styles a table for LaTeX output using the kableExtra package.
}
\examples{
zkbl(mtcars[1:5,])

}
