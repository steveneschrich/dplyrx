% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/common_columns.R
\name{common_colnames}
\alias{common_colnames}
\title{Return colnames common among a list of data frames}
\usage{
common_colnames(tbls = NULL, ignore = NULL)
}
\arguments{
\item{tbls}{A list of data frames}

\item{ignore}{colnames to be ignored in common comparison}
}
\value{
A character vector of common colnames, in more than one
data frame of \code{tbls}.
}
\description{
Return colnames common among a list of data frames
}
\examples{
\dontrun{
common_colnames(list(iris,iris))
# [1] "Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width"  "Species"
}
}
