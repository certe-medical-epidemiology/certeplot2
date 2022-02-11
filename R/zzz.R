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

#' @importFrom showtext showtext_begin
.onLoad <- function(libname, pkgname) {
  # this will support any foreign font in e.g. R Markdown
  # sysfonts::font_add() can be used to add font files to R
  try(showtext_begin(), silent = TRUE)
}

#' @importFrom showtext showtext_end
.onUnload <- function(libpath) {
  # this will close support any foreign font in e.g. R Markdown
  try(showtext_end(), silent = TRUE)
}
