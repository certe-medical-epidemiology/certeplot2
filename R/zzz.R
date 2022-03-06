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

#' @importFrom showtext showtext_auto
#' @importFrom sysfonts font_files
.onLoad <- function(libname, pkgname) {
  # if (!identical(Sys.getenv("IN_PKGDOWN"), "true")) {
    # not in pkgdown (website generation):
    try({
      # this will allow support for any foreign font in e.g. R Markdown
      showtext_auto(enable = TRUE)
      # save current font map to environment to use for plot2():
      plot2_env$fonts <- font_files()
    }, silent = TRUE)
  # }
}

#' @importFrom showtext showtext_auto
.onUnload <- function(libpath) {
  # this will close support for any foreign font in e.g. R Markdown
  try(showtext_auto(enable = FALSE), silent = TRUE)
}
