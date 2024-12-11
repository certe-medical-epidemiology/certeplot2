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

#' Certe Scales for Aesthetics
#' 
#' These scales apply the colours of Certe, using the 'certestyle' package.
#' @inheritParams ggplot2::scale_colour_gradientn
#' @importFrom ggplot2 scale_colour_gradientn scale_colour_discrete scale_fill_discrete
#' @name scale_certe
#' @rdname scale_certe
#' @export
scale_colour_certe_c <- function(..., 
                                 values = NULL,
                                 space = "Lab",
                                 na.value = "grey50", 
                                 guide = "colourbar",
                                 aesthetics = "colour") {
  scale_colour_gradientn(..., 
                         colours = get_colour(c("certeblauw0", "certegroen",
                                            "certegeel", "certeroze")),
                         values = values,
                         space = space,
                         na.value = na.value, 
                         guide = guide,
                         aesthetics = aesthetics)
}

#' @rdname scale_certe
#' @export
scale_color_certe_c <- scale_colour_certe_c

#' @rdname scale_certe
#' @export
scale_fill_certe_c <- function(..., 
                               values = NULL,
                               space = "Lab",
                               na.value = "grey50", 
                               guide = "colourbar",
                               aesthetics = "fill") {
  scale_colour_gradientn(..., 
                         colours = get_colour(c("certeblauw0", "certegroen",
                                            "certegeel", "certeroze")),
                         values = values,
                         space = space,
                         na.value = na.value, 
                         guide = guide,
                         aesthetics = aesthetics)
}

#' @rdname scale_certe
#' @param colour a Certe colour set: `"certe"`, `"certe2"`, `"certe3"`, etc. Will be evaluated with [get_colour()].
#' @export
scale_colour_certe_d <- function(colour = "certe") {
  scale_colour_discrete(type = get_colour(colour, 15))
}

#' @rdname scale_certe
#' @export
scale_color_certe_d <- scale_colour_certe_d

#' @rdname scale_certe
#' @export
scale_fill_certe_d <- function(colour = "certe") {
  scale_fill_discrete(type = get_colour(colour, 15))
}
