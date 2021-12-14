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

#' Add Plot Element
#' 
#' Quickly add new 'geom' to an existing `plot2`/`ggplot` model.
#' @param plot a `ggplot2` plot
#' @param type a `ggplot2` geom name, all geoms are supported. Full function names can be used (e.g., `"geom_line"`), but they can also be abbreviated (e.g., `"l"`, `"line"`). These geoms can be abbreviated by their first character: area (`"a"`), boxplot (`"b"`), column (`"c"`), histogram (`"h"`), jitter (`"j"`), line (`"l"`), point (`"p"`), ribbon (`"r"`), violin (`"v"`).
#' @param ... arguments passed on to the geom function, set using `type`
#' @importFrom ggplot2 is.ggplot
#' @rdname add_type
#' @export
add_type <- function(plot, type = NULL, ...) {
  if (!is.ggplot(plot)) {
    stop("`plot` must be a ggplot2 model.", call. = FALSE)
  }
  type <- validate_type(type[1L])
  if (type == "") {
    stop("`type` must be set for `add_type()`", call. = FALSE)
  } else if (type == "geom_smooth") {
    plot2_warning("Adding a smooth using `add_type()` is less convenient than using `plot2(..., smooth = TRUE)")
  }
  geom_fn <- getExportedValue(name = type, ns = asNamespace("ggplot2"))
  
  plot +
    do.call(geom_fn,
            args = list(...))
}

#' @rdname add_type
#' @export
add_line <- function(plot, ...) {
  add_type(plot = plot, type = "line", ...)
}

#' @rdname add_type
#' @export
add_column <- function(plot, ...) {
  add_type(plot = plot, type = "col", ...)
}
