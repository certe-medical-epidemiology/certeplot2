% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_mapping.R
\name{add_mapping}
\alias{add_mapping}
\title{Add Additional Mapping}
\usage{
add_mapping(plot, ...)
}
\arguments{
\item{plot}{a \code{ggplot2} plot}

\item{...}{arguments passed on to \code{\link[ggplot2:aes]{ggplot2::aes()}}}
}
\description{
This function can be used to adjust the mapping of a plot.
}
\examples{
p <- iris |> plot2(Sepal.Length, Sepal.Width, Species, zoom = TRUE)
p

p |> add_mapping(shape = Species)
}
