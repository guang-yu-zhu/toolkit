% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare_numerical.R
\name{compare_numerical}
\alias{compare_numerical}
\title{Compute Mean and SD for Two Samples by Grouping Variable and Perform One-Way ANOVA}
\usage{
compare_numerical(
  dat,
  var1,
  var2,
  total = FALSE,
  varname,
  colname = "Variables"
)
}
\arguments{
\item{dat}{A data frame containing the variables.}

\item{var1}{The grouping variable (factor or categorical).}

\item{var2}{The numerical variable for which mean and standard deviation are computed.}

\item{total}{Logical; if TRUE, includes overall mean and SD.}

\item{varname}{A character string to label the row in the output table.}

\item{colname}{A character string for naming the output table column.}
}
\value{
A data frame containing group-wise means and standard deviations of \code{var2} for each level of \code{var1}, along with the p-value from the one-way ANOVA test.
}
\description{
This function calculates the mean and standard deviation of a numerical variable for each level of a grouping variable and performs a one-way ANOVA test. The output is a formatted table that includes group-wise means, standard deviations, and a p-value.
}
\details{
The function computes the mean and standard deviation for \code{var2} within each group defined by \code{var1}, as well as overall. It performs a one-way ANOVA test to assess differences in means between groups. Results are presented in a table that includes the p-value.
}
\examples{
compare_numerical(mtcars, 'cyl', 'mpg', varname = 'Miles per Gallon')

}
