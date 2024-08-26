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

#' @importFrom plot2 register_colour
#' @importFrom certestyle certe.colours
.onLoad <- function(libname, pkgname) {
  suppressMessages(
    register_colour(certe.colours)
  )
  suppressMessages(
    register_colour(certe = c("certeblauw",  "certegroen",   "certeroze",   "certegeel",   "certelila",  "certebruin",
                              "certeblauw3", "certegroen3",  "certeroze3",  "certegeel3",  "certelila3", "certebruin3",
                              "certeblauw5", "certegroen5",  "certeroze5",  "certegeel5",  "certelila5", "certebruin5"),
                    certe2 = c("certeblauw2", "certegroen2",  "certeroze2",  "certegeel2",  "certelila2", "certebruin2",
                               "certeblauw4", "certegroen4",  "certeroze4",  "certegeel4",  "certelila4", "certebruin4",
                               "certeblauw6", "certegroen6",  "certeroze6",  "certegeel6",  "certelila6", "certebruin6"),
                    certe3 = c("certeblauw3", "certegroen3",  "certeroze3",  "certegeel3",  "certelila3", "certebruin3",
                               "certeblauw5", "certegroen5",  "certeroze5",  "certegeel5",  "certelila5", "certebruin5"),
                    certe_sir = c(S = "certegroen", SI = "certegroen", SDD = "certegeel", I = "certegeel", IR = "certeroze", R = "certeroze", N = "grey50"),
                    certe_sir2 = c(S = "certegroen2", SI = "certegroen2", SDD = "certegeel2", I = "certegeel2", IR = "certeroze2", R = "certeroze2", N = "grey50"))
  )
  
  options(plot2.colour = "certe",
          plot2.colour_font_secondary = "certeblauw")
}

.onUnload <- function(libpath) {
  if (identical(getOption("plot2.colour"), "certe")) {
    options(plot2.colour = NULL)
  }
  if (identical(getOption("plot2.colour_font_secondary"), "certeblauw")) {
    options(plot2.colour_font_secondary = NULL)
  }
}
