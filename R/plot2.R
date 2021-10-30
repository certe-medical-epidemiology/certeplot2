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

#' Conveniently Create a New `ggplot`
#'
#' These functions create [ggplot2:ggplot] objects, but work in a more convenient way than the `ggplot2` package. By design, the `ggplot2` package requires users to use a lot of functions, while the `certeplot2` package requires to use only arguments in one function.
#' @param .data data to plot
#' @details The [plot2()] function uses the `ggplot2` package for plotting and provides:
#'   * A convenient wrapper around many `ggplot2` functions such as [ggplot()], [geom_col()], [facet_wrap()], [labs()], etc.
#'   * Writing as few lines of codes as possible
#'   * Benefits from Microsoft Excel:
#'     * The y axis starts at 0
#'     * The y scale contains extra space to read all data points
#'     * Date breaks can be written in a readable format such as "d mmm yyyy"
#'     * Data labels can easily be printed
#'   * Easy plotting in three 'directions': `x` (the regular x axis), `category` (replaces 'fill' and 'colour') and `facet`
#'   * Easy way for sorting data in may ways (such as on alphabet, numeric value, frequency, original data order), by setting a single argument for the 'direction': `x.sort`, `category.sort` and `facet.sort`
#'   * Easy limiting values, e.g. by setting `x.max_items = 5`
#'   * Markdown support for any label, with any theme
#'   * An extra clean, minimalistic theme with a lot of whitespace that is ideal for printing
#'   
#' The `ggplot2` package in conjunction with the `tidyr`, `forcats` and `cleaner` packages can provide above functionalities, but the goal of the [plot2()] function is to generalise this into one function. Less typing, faster coding.
#' @rdname plot2
#' @importFrom dplyr `%>%` tibble
#' @importFrom ggplot2 ggplot geom_col
#' @export
#' @examples
#' head(admitted_patients)
#' 
#' plot2(iris, x = Sepal.Width, y = Sepal.Length, category = Species, type = "point")
#' 
#' plot2(iris, x = Species, y = Sepal.Length, type = "boxplot")
#' 
#' library(dplyr, warn.conflicts = FALSE)
#' 
#' mtcars %>% 
#'   plot2(mpg, hp,
#'         type = "point")
#'         
#' 
plot2 <- function(.data,
                  x = NULL,
                  y = NULL,
                  category = NULL,
                  facet = NULL,
                  type = "column",
                  title = NULL,
                  subtitle = NULL,
                  tag = NULL,
                  caption = NULL,
                  bins = NULL,
                  category.focus = NULL,
                  category.sort = TRUE,
                  colour = getOption("plot2.colours", "certe"),
                  colour.extended_spectrum = FALSE,
                  colour.fill = NULL,
                  colour.opacity = 0,
                  datalabels = TRUE,
                  datalabels.fill = "white",
                  datalabels.round = ifelse(y.percent, 1, 2),
                  datalabels.size = 3,
                  decimal_comma = ",",
                  facet.bold = TRUE,
                  facet.drop = FALSE,
                  facet.fill = NULL,
                  facet.fixed_y = FALSE,
                  facet.italic = FALSE,
                  facet.margin = 8,
                  facet.nrow = NULL,
                  facet.position = "top",
                  facet.relative = FALSE,
                  facet.repeat_lbls_x = TRUE,
                  facet.repeat_lbls_y = TRUE,
                  facet.size = 10,
                  facet.sort = TRUE,
                  horizontal = FALSE,
                  jitter_seed = 1,
                  legend.barheight = 5,
                  legend.barwidth = 1,
                  legend.italic = FALSE,
                  legend.position = "top",
                  legend.reverse = NULL,
                  legend.title = "",
                  linetype = 1,
                  markdown = TRUE,
                  na.replace = "(onbekend)",
                  na.rm = FALSE,
                  print = FALSE,
                  reverse = horizontal,
                  size = ifelse(type %in% c("point", "jitter", "boxplot", "violin"), 2, 0.75),
                  stacked = FALSE,
                  stackedpercent = FALSE,
                  subtitle.maxlength = 60,
                  summarise_function = base::sum,
                  text.factor = 1,
                  text.font_family = "Calibri",
                  theme = theme_minimal2(),
                  title.maxlength = 60,
                  width = ifelse(type %in% c("jitter", "boxplot", "violin"), 0.75, 0.5),
                  x.date_breaks = "1 day",
                  x.date_labels = "d mmm",
                  x.expand = 0.5,
                  x.lbl_align = NULL,
                  x.lbl_angle = 0,
                  x.lbl_italic = FALSE,
                  x.limits = NULL,
                  x.max_items = Inf,
                  x.max_txt = "(rest, x{n})",
                  x.position = "bottom",
                  x.remove = FALSE,
                  x.sort = TRUE,
                  x.title = NULL,
                  y.24h = FALSE,
                  y.age = FALSE,
                  y.breaks = NULL,
                  y.expand = 0.25,
                  y.labels = NULL,
                  y.limits = NULL,
                  y.percent = FALSE,
                  y.percent_break = 10,
                  y.position = "left",
                  y.remove = FALSE,
                  y.title = NULL,
                  y.trans = "identity",
                  x.category = NULL,
                  y.category = NULL,
                  ...) {
  UseMethod("plot2")
}

#' @rdname plot2
#' @export
plot2.default <- function(.data = NULL,
                          x = NULL,
                          y = NULL,
                          category = NULL,
                          facet = NULL,
                          ...) {
  if (is.null(.data)) {
    df <- tibble(`_var_x` = {{ x }},
                 `_var_y` = {{ y }},
                 `_var_x.category` = if (!is.null(x.category)) {{ x.category }} else {{ facet }},
                 `_var_y.category` = {{ y.category }},
                 `_var_datalabels` = {{ datalabels }}) %>% 
      as.data.frame(stringsAsFactors = FALSE)
    plot2(.data = df, ...)
  }
  stop("No supported method")
}

#' @rdname plot2
#' @importFrom dplyr `%>%` mutate tibble 
#' @export
plot2.data.frame <- function(.data = NULL,
                             x = NULL,
                             y = NULL,
                             category = NULL,
                             facet = NULL,
                             type = "column",
                             datalabels = TRUE,
                             x.category = NULL,
                             y.category = NULL,
                             ...) {
  
  .data <- .data %>%
    mutate(`_var_x` = {{ x }},
           `_var_y` = {{ y }},
           `_var_category` = if (tryCatch(!is.null(y.category), error = function(x) TRUE)) {{ y.category }} else {{ category }},
           `_var_facets` = if (tryCatch(!is.null(x.category), error = function(x) TRUE)) {{ x.category }} else {{ facet }},
           `_var_datalabels` = {{ datalabels }}) %>% 
    as.data.frame(stringsAsFactors = FALSE)
  validate_data(.data)
}

