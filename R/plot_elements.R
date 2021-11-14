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

#' Get Plot Element
#' 
#' These functions extract `ggplot` properties.
#' @param x a `ggplot` model
#' @name plot_elements
#' @rdname plot_elements
#' @export
#' @examples 
#' x <- plot2(iris)
#' 
#' get_mapping(x)
#' 
#' class(get_theme(x))
get_theme <- function(x) {
  x$theme
}

#' @rdname plot_elements
#' @export
get_mapping <- function(x) {
  x$mapping
}

#' @rdname plot_elements
#' @export
get_data <- function(x) {
  x$data
}

#' @rdname plot_elements
#' @export
get_layers <- function(x) {
  x$layers
}
