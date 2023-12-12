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
#' Format numbers as currency, rounding values to dollars or cents using a convenient heuristic.
#' @param x values
#' @inheritParams plot2
#' @name labellers
#' @rdname labellers
#' @export
#' @examples
#' profit <- data.frame(group = LETTERS[1:4],
#'                      profit = runif(4, 10000, 25000))
#' 
#' profit |>
#'   plot2(y.labels = euros,
#'         datalabels = FALSE)
#'         
#' profit |>
#'   plot2(y.labels = euros,
#'         datalabels.format = euros)
euros <- function(x,
                  big.mark = big_mark(),
                  decimal.mark = dec_mark(),
                  ...) {
  trimws(paste0("\u20ac ", trimws(format(x, decimal.mark = decimal.mark, big.mark = big.mark))))
}

#' @rdname labellers
#' @export
dollars <- function(x,
                    big.mark = big_mark(),
                    decimal.mark = dec_mark(),
                    ...) {
  trimws(paste0("$", format(trimws(x), decimal.mark = decimal.mark, big.mark = big.mark)))
}
