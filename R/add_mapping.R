# ===================================================================== #
#  An R package by Certe:                                               #
#  https://github.com/certe-medical-epidemiology                        #
#                                                                       #
#  Licensed as GPL-v2.0.                                                #
#                                                                       #
#  Developed at non-profit organisation Certe Medical Diagnostics &     #
#  Advice, department of Medical Epidemiology.                          #
#                                                                       #
#  This R package is free software; you can freely use and distribute   #
#  it for both personal and commercial purposes under the terms of the  #
#  GNU General Public License version 2.0 (GNU GPL-2), as published by  #
#  the Free Software Foundation.                                        #
#                                                                       #
#  We created this package for both routine data analysis and academic  #
#  research and it was publicly released in the hope that it will be    #
#  useful, but it comes WITHOUT ANY WARRANTY OR LIABILITY.              #
# ===================================================================== #

#' Add Additional Mapping
#' 
#' This function can be used to adjust the mapping of a plot.
#' @param plot a `ggplot2` plot
#' @param ... arguments passed on to [ggplot2::aes()]
#' @importFrom ggplot2 aes
#' @export
#' @examples 
#' p <- iris %>% plot2(Sepal.Length, Sepal.Width)
#' p
#' 
#' p %>% add_mapping(shape = Species)
add_mapping <- function(plot, ...) {
  plot$mapping <- utils::modifyList(plot$mapping, aes(...))
  plot
}
