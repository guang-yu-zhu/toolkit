% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clip.R
\name{clip}
\alias{clip}
\title{Save output to clipboard}
\usage{
clip(obj, file = "output.txt")
}
\arguments{
\item{obj}{the object for save}

\item{file}{the file name}
}
\description{
Save string output to output.txt and save in clipboard
}
\examples{
library(dplyr,warn.conflicts = FALSE)
summary(glm(case ~ spontaneous+induced,data = infert,family = binomial()))\%>\%clip()

}
