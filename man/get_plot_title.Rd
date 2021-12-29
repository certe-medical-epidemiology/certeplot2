% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_plot_title.R
\name{get_plot_title}
\alias{get_plot_title}
\title{Get Plot Title}
\usage{
get_plot_title(plot, valid_filename = TRUE, default = NA_character_)
}
\arguments{
\item{plot}{a \code{ggplot2} plot}

\item{valid_filename}{a \link{logical} to indicate whether the returned value should be a valid filename, defaults to \code{TRUE}}

\item{default}{the default value, if a plot title is absent}
}
\description{
Get the title of the plot, or a default value.
}
\examples{
p <- plot2(mtcars, title = "Plotting **mpg** vs. **cyl**!")
get_plot_title(p)

get_plot_title(p, valid_filename = FALSE)

p <- plot2(mtcars)
# default is NA:
get_plot_title(p)
}