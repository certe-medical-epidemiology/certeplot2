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

#' Label Euro currencies
#' 
#' Format numbers as currency, rounding values to dollars or cents using a convenient heuristic. This function is similar to [label_dollar()] from the `scales` package.
#' @inheritParams scales::label_dollar
#' @importFrom scales label_dollar
#' @name labellers
#' @rdname labellers
#' @export
label_euro <- function(accuracy = NULL,
                       scale = 1,
                       prefix = "\u20ac",
                       suffix = "", 
                       big.mark = big_mark(),
                       decimal.mark = dec_mark(),
                       trim = TRUE,
                       largest_with_cents = 1e+05,
                       ...) {
  label_dollar(accuracy = accuracy,
               scale = scale,
               prefix = prefix, suffix = suffix,
               big.mark = big.mark,
               decimal.mark = decimal.mark,
               trim = trim,
               largest_with_cents = largest_with_cents,
               ...)
}

#' @importFrom scales label_dollar
#' @export
scales::label_dollar
