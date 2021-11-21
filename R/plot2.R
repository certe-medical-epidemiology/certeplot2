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
#' @description  The [plot2()] function is a convenient wrapper around many [`ggplot2`][ggplot2::ggplot()] functions. By design, the `ggplot2` package requires users to use a lot of functions, while the [plot2()] function auto-determines needs and only requires to define arguments in one single function.
#' 
#' See [plot2-methods] for all implemented methods for different object classes.
#' @param .data data to plot
#' @param x plotting 'direction': the x axis
#' @param y values to use for plotting along the y axis
#' @param category plotting 'direction': the category (called 'fill' and 'colour' in `ggplot2`)
#' @param facet plotting 'direction': the facet
#' @param type type of visualisation to use, supports all `ggplot2` geoms. It will be determined automatically if left blank.
#' 
#' In `ggplot2`, 'bars' and 'columns' are equal, while it is common to many people that 'bars' are oriented horizontally and 'columns' are oriented vertically. For this reason, `type = "bar"` will set `type = "col"` and `horizontal = TRUE`.
#' 
#' There is one special case for this `type` argument: the shortcut `type = "barpercent"`, which will set `type = "col"` and `horizontal = TRUE` and `x.max_items = 10` and `x.sort = "freq-desc"` and `datalabels.format = "%n (%p)"`.
#' @param x.title text to show on the x asis
#' @param y.title text to show on the y asis
#' @param title title to show
#' @param subtitle subtitle to show
#' @param caption caption to show
#' @param tag tag to show
#' @param title.linelength maximum number of characters per line in the title, before a linebreak occurs
#' @param title.colour text colour of the title
#' @param subtitle.linelength maximum number of characters per line in the subtitle, before a linebreak occurs
#' @param subtitle.colour text colour of the subtitle
#' @param na.replace character to put in place of `NA` values if `na.rm = FALSE`
#' @param na.rm remove `NA` values from showing in the plot
#' @param facet.position text
#' @param facet.fill text
#' @param facet.bold text
#' @param facet.italic text
#' @param facet.size text
#' @param facet.margin text
#' @param facet.repeat_lbls_x text
#' @param facet.repeat_lbls_y text
#' @param facet.fixed_y text
#' @param facet.drop text
#' @param facet.nrow text
#' @param facet.relative text
#' @param x.date_breaks text
#' @param x.date_labels text
#' @param category.focus text
#' @param colour text
#' @param colour_fill text
#' @param x.lbl_angle text
#' @param x.lbl_align text
#' @param x.lbl_italic text
#' @param x.remove text
#' @param x.position text
#' @param x.max_items text
#' @param x.max_txt text
#' @param category.max_items text
#' @param category.max_txt text
#' @param facet.max_items text
#' @param facet.max_txt text
#' @param x.breaks text
#' @param x.breaks_n text
#' @param x.trans text
#' @param x.expand text
#' @param x.limits text
#' @param x.character text
#' @param y.remove text
#' @param y.24h text
#' @param y.age text
#' @param y.percent text
#' @param y.percent_break text
#' @param y.breaks text
#' @param y.limits text
#' @param y.labels text
#' @param y.expand text
#' @param y.trans text
#' @param y.position text
#' @param category.labels,category.percent,category.breaks,category.limits,category.expand,category.midpoint,category.trans settings for the plotting direction `category`
#' @param x.sort,category.sort,facet.sort sorting of the plotting direction, defaults to `TRUE`, except for continuous values on the x axis (such as dates and numbers). Applying one of the sorting methods will transform the values to an ordered [factor], which `ggplot2` uses to orient the data. Valid values are:
#' 
#' - `TRUE`: sort [factor] on their levels, otherwise sort as `"asc"`
#' - `FALSE`: keep order as it is in the data
#' - `NULL`: do not sort/transform at all
#' - `"asc"` or `"alpha"`: sort ascending on alphabet, while maintaining numbers in the text (*numeric* sort)
#' - `"desc"`: sort descending on alphabet, while maintaining numbers in the text (*numeric* sort)
#' - `"order"` or `"inorder"`: sort as `FALSE`
#' - `"freq"` or `"freq-desc"`: sort descending according to the frequencies of `y` computed by `summarise_function` (highest value first)
#' - `"freq-asc"`: sort ascending according to the frequencies of `y` computed by `summarise_function` (loewest value first)
#' @param datalabels variables or character vector to use as datalabels - if left blank, will take the first character column in 'sf' plots, and values of `y` otherwise
#' @param datalabels.round number of digits to round the datalabels
#' @param datalabels.format format to use for datalabels - `"%n"` will be replaced by the count number, `"%p"` will be replaced by the percentage of the total count. Use `datalabels.format = NULL` to not transform the datalabels.
#' @param datalabels.colour,datalabels.fill,datalabels.size,datalabels.angle settings for the datalabels
#' @param decimal.mark decimal mark, defaults to Dutch use (a comma)
#' @param big.mark thousands separator, defaults to Dutch use (a full stop)
#' @param summarise_function a [function] to use if the data has to be summarised, see *Examples*
#' @param stacked text
#' @param stackedpercent text
#' @param horizontal a [logical] to turn the plot 90 defrees using [`coord_flip()`][ggplot2::coord_flip()]
#' @param reverse a [logical] to reverse all values on the x axis
#' @param smooth a [logical] to add a smooth. In histograms, this will add the density count as an overlaying line (default: `TRUE`). In all other cases, a smooth will be added using [`geom_smooth()`][ggplot2::geom_smooth()] (default: `FALSE`).
#' @param smooth.method,smooth.formula,smooth.se,smooth.level,smooth.alpha,smooth.size,smooth.linetype settings for `smooth`
#' @param size size of the geom
#' @param linetype linetype of the geom, only suitable for geoms that draw lines
#' @param binwidth width of bins (only useful for `type = "histogram"`), can be specified as a numeric value or as a function that calculates width from `x`, see [`geom_histogram()`][ggplot2::geom_histogram()]
#' @param width width of the geom
#' @param jitter_seed seed (randomisation factor) to be set when using `type = "jitter"`
#' @param violin_scale scale to be set when using `type = "violin"`, can also be set to `"area"`
#' @param legend.position,legend.title,legend.reverse,legend.barheight,legend.barwidth,legend.nbin,legend.italic settings for the legend
#' @param zoom a [logical] to indicate if the plot should be scaled to the data, i.e., not having the x and y axes to start at 0
#' @param sep separator character to use if multiple columns are given to either of the three directions: `x`, `category` and `facet`, e.g. `facet = c(column1, column2)`
#' @param print a [logical] to indicate if the result should be [printed][print()] instead of just returned
#' @param text_factor text factor to use, which will apply to all texts shown in the plot
#' @param family font family to use
#' @param theme a valid `ggplot2` [theme][ggplot2::theme()] to apply, or `NULL` to use the default [`theme_grey()`][ggplot2::theme_grey()]
#' @param markdown text
#' @param x.category old argument for `facet`, now deprecated
#' @param y.category old argument for `category`, now deprecated
#' @param ... arguments passed on to methods
#' @details The [plot2()] function is a convenient wrapper around many [`ggplot2`][ggplot2::ggplot()] functions such as [`ggplot()`][ggplot2::ggplot()], [`aes()`][ggplot2::aes()], [`geom_col()`][ggplot2::geom_col()], [`facet_wrap()`][ggplot2::facet_wrap()], [`labs()`][ggplot2::labs()], etc., and provides:
#'   * Writing as few lines of codes as possible
#'   * Easy plotting in three 'directions': `x` (the regular x axis), `category` (replaces 'fill' and 'colour') and `facet`
#'   * Automatic setting of these 'directions' based on the input data
#'   * Easy way for sorting data in many ways (such as on alphabet, numeric value, frequency, original data order), by setting a single argument for the 'direction': `x.sort`, `category.sort` and `facet.sort`
#'   * Easy limiting values, e.g. by setting `x.max_items = 5` or `category.max_items = 5`
#'   * Markdown support for any label, with any theme
#'   * An extra clean, minimalistic theme with a lot of whitespace (but without unnecessary margins) that is ideal for printing: [theme_minimal2()]
#'   * Some conveniences from Microsoft Excel:
#'     * The y axis starts at 0 if possible
#'     * The y scale expands at the top to be better able to interpret all data points
#'     * Date breaks can be written in a human-readable format (such as "d mmm yyyy")
#'     * Labels with data values can easily be printed and are automatically determined
#'   * Support for any `ggplot2` extension based on [ggplot2::fortify()]
#'   
#' The `ggplot2` package in conjunction with the `tidyr`, `forcats` and `cleaner` packages can provide above functionalities, but the goal of the [plot2()] function is to generalise this into one function. For [data.frame]s, [plot2()] currently has `r length(formals(plot2.data.frame)) - 1` arguments, all with a default value. **Less typing, faster coding.**
#' @return The [plot2()] function adds new variables to the data for each mapping: any combination of `_var_x`, `_var_y`, `_var_category`, `_var_facet`. These columns are internally set as mapping with [`aes()`][ggplot2::aes()].
#' @export
#' @examples
#' head(iris)
#' 
#' # no variables determined, so plot2() will try for itself -
#' # the type will be points since the first two variables are numeric
#' plot2(iris)
#' 
#' # only view the data part, like ggplot2 normally does
#' plot2(iris, zoom = TRUE)
#' 
#' # if x and y are set, no addition mapping will be set:
#' plot2(iris, Sepal.Width, Sepal.Length)
#' plot2(iris, Species, Sepal.Length)
#' 
#' # the arguments are in this order: x, y, category, facet
#' plot2(iris, Sepal.Length, Sepal.Width, Petal.Length, Species)
#' 
#' plot2(iris, Sepal.Length, Sepal.Width, Petal.Length, Species,
#'       colour = "viridis") # set the viridis colours
#'       
#' plot2(iris, Sepal.Length, Sepal.Width, Petal.Length, Species,
#'       colour = c("white", "red", "black")) # set own colours
#'       
#' plot2(iris, Sepal.Length, Sepal.Width, Petal.Length, Species,
#'       colour = c("white", "red", "black"), # set own colours
#'       category.midpoint = 3)               # with an own midpoint
#' 
#' # change to any type
#' plot2(iris, Species, Sepal.Length, type = "violin")
#' 
#' library(dplyr, warn.conflicts = FALSE)
#'   
#' head(admitted_patients)
#' 
#' # if there are more Y values than groups, the default will be boxplot
#' admitted_patients %>%
#'   plot2(x = hospital)
#' 
#' # the arguments are in this order: x, y, category, facet
#' admitted_patients %>%
#'   plot2(hospital, age, gender)
#'   
#' admitted_patients %>%
#'   plot2(hospital, age, gender, ward)
#'   
#' # use summarise_function to apply a function for continuous data
#' admitted_patients %>%
#'   plot2(hospital, age, gender, ward,
#'         type = "col", summarise_function = median)
#'
#' admitted_patients %>%
#'   plot2(x = hospital,
#'         category = gender,
#'         colour = c("F" = "orange3", "M" = "purple3"),
#'         colour_fill = "white",
#'         y.age = TRUE)
#'         
#' admitted_patients %>%
#'   plot2(age, type = "hist")
#' admitted_patients %>%
#'   plot2(age, type = "density")
#'  
#' # the default type is column, datalabels are automatically
#' # set in non-continuous types:
#' patients_per_hospital_gender <- admitted_patients %>%
#'   count(hospital, gender)
#'   
#' head(patients_per_hospital_gender)
#'   
#' patients_per_hospital_gender %>%
#'   plot2()
#'   
#' patients_per_hospital_gender %>%
#'   plot2(stacked = TRUE)
#'   
#' patients_per_hospital_gender %>%
#'   plot2(stackedpercent = TRUE)
#'   
#' # sort any direction
#' patients_per_hospital_gender %>%
#'   plot2(category.sort = "desc")
#'   
#' patients_per_hospital_gender %>%
#'   plot2(category.sort = "desc",
#'         x.sort = "freq-asc",
#'         stacked = TRUE)
#' 
#' # plot2() supports all S3 extensions available through ggplot2::fortify():
#' mtcars %>% 
#'   lm(mpg ~ hp, data = .) %>% 
#'   plot2(x = mpg ^ 2,
#'         y = hp ^ 3,
#'         smooth = TRUE,
#'         title = "Titles/captions *support* **markdown**",
#'         subtitle = "Axis titles contain the square notation: ^2")
#'         
#' # sf objects (geographic plots, 'simple features') are also supported
#' if (require("sf")) {
#'   netherlands %>% 
#'     plot2(datalabels = TRUE)
#' }
plot2 <- function(.data,
                  x = NULL,
                  y = NULL,
                  category = NULL,
                  facet = NULL,
                  type = NULL,
                  x.title = NULL,
                  y.title = NULL,
                  title = NULL,
                  subtitle = NULL,
                  caption = NULL,
                  tag = NULL,
                  title.linelength = 60,
                  title.colour = "black",
                  subtitle.linelength = 60,
                  subtitle.colour = "grey35",
                  na.replace = "(??)",
                  na.rm = FALSE,
                  facet.position = "top",
                  facet.fill = NULL,
                  facet.bold = TRUE,
                  facet.italic = FALSE,
                  facet.size = 10,
                  facet.margin = 8,
                  facet.repeat_lbls_x = TRUE,
                  facet.repeat_lbls_y = FALSE,
                  facet.fixed_y = FALSE,
                  facet.drop = FALSE,
                  facet.nrow = NULL,
                  facet.relative = FALSE,
                  x.date_breaks = NULL,
                  x.date_labels = NULL,
                  category.focus = NULL,
                  colour = "certe",
                  colour_fill = NULL,
                  x.lbl_angle = 0,
                  x.lbl_align = NULL,
                  x.lbl_italic = FALSE,
                  x.remove = FALSE,
                  x.position = "bottom",
                  x.max_items = Inf,
                  x.max_txt = "(rest, x %n)",
                  category.max_items = Inf,
                  category.max_txt = "(rest, x %n)",
                  facet.max_items = Inf,
                  facet.max_txt = "(rest, x %n)",
                  x.breaks = NULL,
                  x.breaks_n = NULL,
                  x.trans = "identity",
                  x.expand = 0.5,
                  x.limits = NULL,
                  x.character = NULL,
                  y.remove = FALSE,
                  y.24h = FALSE,
                  y.age = FALSE,
                  y.percent = FALSE,
                  y.percent_break = 10,
                  y.breaks = NULL,
                  y.limits = NULL,
                  y.labels = NULL,
                  y.expand = 0.25,
                  y.trans = "identity",
                  y.position = "left",
                  category.labels = NULL,
                  category.percent = FALSE,
                  category.breaks = NULL,
                  category.limits = NULL,
                  category.expand = 0,
                  category.midpoint = NULL,
                  category.trans = "identity",
                  x.sort = NULL,
                  category.sort = TRUE,
                  facet.sort = TRUE,
                  datalabels = TRUE,
                  datalabels.round = ifelse(y.percent, 2, 1),
                  datalabels.format = "%n",
                  datalabels.colour = "grey25",
                  datalabels.fill = NULL,
                  datalabels.size = (3 * text_factor),
                  datalabels.angle = 0,
                  decimal.mark = ",",
                  big.mark = ifelse(decimal.mark == ",", ".", ","),
                  summarise_function = base::sum,
                  stacked = FALSE,
                  stackedpercent = FALSE,
                  horizontal = FALSE,
                  reverse = horizontal,
                  smooth = NULL,
                  smooth.method = NULL,
                  smooth.formula = NULL,
                  smooth.se = TRUE,
                  smooth.level = 0.95,
                  smooth.alpha = 0.15,
                  smooth.size = 0.75,
                  smooth.linetype = 3,
                  size = NULL,
                  linetype = 1,
                  binwidth = NULL,
                  width = NULL,
                  jitter_seed = NA,
                  violin_scale = "count",
                  legend.position = "top",
                  legend.title = NULL, # TRUE in numeric categories
                  legend.reverse = FALSE,
                  legend.barheight = 6,
                  legend.barwidth = 1.5,
                  legend.nbin = 300,
                  legend.italic = FALSE,
                  zoom = FALSE,
                  sep = "/",
                  print = FALSE,
                  text_factor = 1,
                  family = "Calibri",
                  theme = theme_minimal2(),
                  markdown = TRUE,
                  # old certetools pkg support
                  x.category = NULL,
                  y.category = NULL,
                  ...) {
  UseMethod("plot2")
}

#' @importFrom dplyr `%>%` mutate vars group_by across summarise
#' @importFrom ggplot2 ggplot aes aes_string labs stat_boxplot scale_colour_manual scale_fill_manual coord_flip geom_smooth geom_density guides guide_legend
#' @importFrom certestyle format2 font_red font_black font_blue
plot2_exec <- function(.data,
                       x,
                       y,
                       category,
                       facet,
                       type,
                       x.title,
                       y.title,
                       title,
                       subtitle,
                       caption,
                       tag,
                       title.linelength,
                       title.colour,
                       subtitle.linelength,
                       subtitle.colour,
                       na.replace,
                       na.rm,
                       facet.position,
                       facet.fill,
                       facet.bold,
                       facet.italic,
                       facet.size,
                       facet.margin,
                       facet.repeat_lbls_x,
                       facet.repeat_lbls_y,
                       facet.fixed_y,
                       facet.drop,
                       facet.nrow,
                       facet.relative,
                       x.date_breaks,
                       x.date_labels,
                       category.focus,
                       colour,
                       colour_fill,
                       x.lbl_angle,
                       x.lbl_align,
                       x.lbl_italic,
                       x.remove,
                       x.position,
                       x.max_items,
                       x.max_txt,
                       category.max_items,
                       category.max_txt,
                       facet.max_items,
                       facet.max_txt,
                       x.breaks,
                       x.breaks_n,
                       x.trans,
                       x.expand,
                       x.limits,
                       x.character,
                       y.remove,
                       y.24h,
                       y.age,
                       y.percent,
                       y.percent_break,
                       y.breaks,
                       y.limits,
                       y.labels,
                       y.expand,
                       y.trans,
                       y.position,
                       category.labels,
                       category.percent,
                       category.breaks,
                       category.limits,
                       category.expand,
                       category.midpoint,
                       category.trans,
                       x.sort,
                       category.sort,
                       facet.sort,
                       datalabels,
                       datalabels.round,
                       datalabels.colour,
                       datalabels.format,
                       datalabels.fill,
                       datalabels.size,
                       datalabels.angle,
                       decimal.mark,
                       big.mark,
                       summarise_function,
                       stacked,
                       stackedpercent,
                       horizontal,
                       reverse,
                       smooth,
                       smooth.method,
                       smooth.formula,
                       smooth.se,
                       smooth.level,
                       smooth.alpha,
                       smooth.size,
                       smooth.linetype,
                       size,
                       linetype,
                       binwidth,
                       width,
                       jitter_seed,
                       violin_scale,
                       legend.position,
                       legend.title,
                       legend.reverse,
                       legend.barheight,
                       legend.barwidth,
                       legend.nbin,
                       legend.italic,
                       zoom,
                       sep,
                       print,
                       text_factor,
                       family,
                       theme,
                       markdown,
                       x.category,
                       y.category,
                       ...) {
  
  if (NROW(.data) == 0) {
    warning("No observations to plot.", call. = FALSE)
    return(invisible())
  }
  
  dots <- list(...)
  
  # record missing arguments ----
  misses_x <- isTRUE(dots$misses.x)
  misses_y <- isTRUE(dots$misses.y)
  misses_category <- isTRUE(dots$misses.category)
  misses_facet <- isTRUE(dots$misses.facet)
  misses_datalabels <- isTRUE(dots$misses.datalabels)
  misses_colour_fill <- isTRUE(dots$misses.colour_fill)
  misses_x.title <- isTRUE(dots$misses.x.title)
  misses_y.title <- isTRUE(dots$misses.y.title)
  misses_title <- isTRUE(dots$misses.title)
  misses_subtitle <- isTRUE(dots$misses.subtitle)
  misses_tag <- isTRUE(dots$misses.tag)
  misses_caption <- isTRUE(dots$misses.caption)
  misses_zoom <- isTRUE(dots$misses.zoom)
  misses_y.percent <- isTRUE(dots$misses.y.percent)
  
  # old arguments, from previous package ----
  if (tryCatch(!is.null(y.category), error = function(e) TRUE)) {
    plot2_warning("Using ", font_red("'y.category' is deprecated"), " - use ", font_blue("'category'"), " instead")
    .data <- .data %>% 
      mutate(across({{ y.category }}, .names = "_var_category_{col}")) %>% 
      summarise_variable("_var_category", sep = sep)
    category <- "_var_category"
  }
  if (tryCatch(!is.null(x.category), error = function(e) TRUE)) {
    plot2_warning("Using ", font_red("'x.category' is deprecated"), " - use ", font_blue("'facet'"), " instead")
    .data <- .data %>% 
      mutate(across({{ x.category }}, .names = "_var_facet_{col}")) %>% 
      summarise_variable("_var_facet", sep = sep)
    facet <- "_var_facet"
  }
  if (!is.null(dots$type)) {
    plot2_warning("Using ", font_red("'type' is deprecated"), " - use ", font_blue("'type'"), " instead")
    type <- dots$type
  }
  if (!is.null(dots$sort.x)) {
    plot2_warning("Using ", font_red("'sort.x' is deprecated"), " - use ", font_blue("'x.sort'"), " instead")
    x.sort <- dots$sort.x
  }
  if (!is.null(dots$sort.category)) {
    plot2_warning("Using ", font_red("'sort.category' is deprecated"), " - use ", font_blue("'category.sort'"), " instead")
    category.sort <- dots$sort.category
  }
  if (!is.null(dots$sort.facet)) {
    plot2_warning("Using ", font_red("'sort.facet' is deprecated"), " - use ", font_blue("'facet.sort'"), " instead")
    facet.sort <- dots$sort.facet
  }
  if (!is.null(dots$category.title)) {
    legend.title <- dots$category.title
  }
  
  # prevalidate types for special types ----
  if (isTRUE(type[1L] %like% "barpercent")) {
    if (is.infinite(x.max_items)) {
      x.max_items <- 10
    }
    x.sort <- "freq-desc"
    datalabels.format <- "%n (%p)"
  }
  if (isTRUE(type[1L] %like% "bar")) {
    type <- "col"
    horizontal <- TRUE
  }
  
  # prepare data ----
  df <- .data %>%
    mutate(`_var_y` = {{ y }},
           `_var_datalabels` = {{ datalabels }}) %>% 
    # add the three directions, these functions also support Tidyverse selections: `facet = where(is.character)`
    add_direction(direction = {{ x }},
                  var_name = "x",
                  sep = sep) %>% 
    add_direction(direction = {{ category }}, 
                  var_name = "category",
                  sep = sep) %>% 
    add_direction(direction = {{ facet }}, 
                  var_name = "facet",
                  sep = sep) %>% 
    # this part will transform the data as needed
    validate_data(misses_x = misses_x,
                  misses_category = misses_category,
                  label_x = dots$label_x,
                  label_y = dots$label_y,
                  label_category = dots$label_category,
                  label_facet = dots$label_facet,
                  decimal.mark = decimal.mark,
                  big.mark = big.mark,
                  type = type,
                  datalabels.round = datalabels.round,
                  datalabels.format = datalabels.format,
                  x.sort = x.sort,
                  category.sort = category.sort,
                  facet.sort = facet.sort,
                  summarise_function = summarise_function,
                  horizontal = horizontal,
                  x.max_items = x.max_items,
                  x.max_txt = x.max_txt,
                  category.max_items = category.max_items,
                  category.max_txt = category.max_txt,
                  facet.max_items = facet.max_items,
                  facet.max_txt = facet.max_txt,
                  ...)
  
  # validate type ----
  type <- validate_type(type = type, df = df) # this will automatically determine the type if is.null(type)
  # transform data if not a continuous geom but group sizes are > 1
  if (any(group_sizes(df) > 1) && !geom_is_continuous(type)) {
    plot2_message("Duplicate observations in discrete plot type (", font_blue(type), "), applying ",
                  font_blue("summarise_function = " ), font_blue(dots$summarise_fn_name))
    df <- summarise_data(df = df, summarise_function = summarise_function,
                         decimal.mark = decimal.mark, big.mark = big.mark,
                         datalabels.round = datalabels.round, datalabels.format = datalabels.format)
  }
  
  # remove datalabels in continuous geoms
  if (isTRUE(misses_datalabels) && geom_is_continuous(type) && type != "geom_sf") {
    df <- df %>% select(-`_var_datalabels`)
  }
  if (!isTRUE(misses_y) && geom_is_continuous_x(type)) {
    plot2_message("Ignoring ", font_blue("y"), " for plot type ", font_blue(gsub("geom_", "", type)))
    df$`_var_y` <- df$`_var_x`
  }
  # remove x from sf geom
  if (type == "geom_sf") {
    df <- df %>% select(-`_var_x`)
  }
  
  # set default size and width ----
  size <- validate_size(size = size, type = type)
  width <- validate_width(width = width, type = type)
  
  # generate colour vectors ----
  cols <- validate_colour(df = df,
                          type = type,
                          colour = colour,
                          colour_fill = colour_fill,
                          misses_colour_fill = misses_colour_fill,
                          horizontal = horizontal)
  
  # generate mapping ----
  if (type == "geom_sf" && !is.null(dots$sf_column)) {
    mapping <- aes_string(geometry = dots$sf_column)
  } else if (!geom_is_continuous_x(type)) {
    # histograms etc. have a continuous x variable, so only set y if not a histogram-like
    mapping <- aes(y = `_var_y`, group = 1)
  } else {
    mapping <- aes()
    if (misses_zoom) {
      zoom <- TRUE
    }
  }
  if (has_x(df)) {
    mapping <- utils::modifyList(mapping, aes(x = `_var_x`,
                                              group = `_var_x`))
  }
  if (has_category(df)) {
    if (type == "geom_sf") {
      # no colour in sf's
      mapping <- utils::modifyList(mapping, aes(fill = `_var_category`,
                                                group = `_var_category`))
    } else {
      mapping <- utils::modifyList(mapping, aes(fill = `_var_category`,
                                                colour = `_var_category`,
                                                group = `_var_category`))
    }
  }
  if (geom_is_continuous(type)) {
    # remove the group from the mapping
    mapping <- utils::modifyList(mapping, aes(group = NULL))
  }
  
  # generate ggplot ----
  p <- ggplot(data = df, mapping = mapping, colour = cols$colour, fill = cols$colour_fill)
  
  # generate geom ----
  if (type == "geom_boxplot") {
    # first add the whiskers
    p <- p +
      stat_boxplot(geom = "errorbar",
                   coef = 1.5, # 1.5 * IQR
                   width = width * ifelse(has_category(df), 1, 0.75),
                   lwd = size,
                   colour = cols$colour)
  }
  p <- p +
    generate_geom(type = type,
                  df = df,
                  stacked = stacked,
                  stackedpercent = stackedpercent,
                  horizontal = horizontal,
                  width = width,
                  size = size,
                  linetype = linetype,
                  reverse = reverse,
                  na.rm = na.rm,
                  violin_scale = violin_scale,
                  jitter_seed = jitter_seed,
                  binwidth = binwidth,
                  cols = cols)
  if (is.null(smooth) && type == "geom_histogram") {
    plot2_message("Assuming ", font_blue("smooth = TRUE"), " for ", font_blue("type = \"histogram\""))
    smooth <- TRUE
  }
  if (isTRUE(smooth)) {
    if (type == "geom_histogram") {
      # add a density count
      set_binwidth <- p$layers[[1]]$stat_params$binwidth
      p <- p +
        do.call(geom_density,
                c(list(mapping = aes(y = ..count.. * set_binwidth),
                       alpha = smooth.alpha,
                       linetype = smooth.linetype,
                       size = smooth.size,
                       na.rm = na.rm),
                  list(colour = cols$colour[1L])[!has_category(df)]))
    } else {
      # add smooth with geom_smooth()
      p <- p +
        do.call(geom_smooth,
                c(list(mapping = mapping,
                       fill = "grey60",
                       formula = smooth.formula,
                       se = smooth.se,
                       method = smooth.method,
                       level = smooth.level,
                       alpha = smooth.alpha,
                       linetype = smooth.linetype,
                       size = smooth.size,
                       na.rm = na.rm),
                  list(colour = cols$colour[1L])[!has_category(df)]))
    }
  }
  
  # add axis labels ----
  p <- p +
    labs(x = get_x_name(df),
         y = get_y_name(df),
         fill = get_category_name(df),
         colour = get_category_name(df)) # will return NULL if not available, so always works
  if (geom_is_continuous_x(type)) {
    if (type %like% "density") {
      p <- p +
        labs(y = "Density")
      if (misses_y.percent) {
        y.percent <- TRUE
      }
    } else {
      p <- p +
        labs(y = "Frequency")
    }
  }
  
  # add the right scales ----
  
  if (has_category(df) && is.numeric(get_category(df))) {
    p <- p + 
      validate_category_scale(df = df,
                              type = type,
                              cols = cols,
                              category.labels = category.labels,
                              category.percent = category.percent,
                              category.breaks = category.breaks,
                              category.limits = category.limits,
                              category.expand = category.expand,
                              category.midpoint = category.midpoint,
                              category.trans = category.trans,
                              stackedpercent = stackedpercent,
                              legend.nbin = legend.nbin,
                              legend.barheight = legend.barheight,
                              legend.barwidth = legend.barwidth,
                              legend.reverse = legend.reverse,
                              decimal.mark = decimal.mark,
                              big.mark = big.mark,
                              family = family)
    if (is.null(legend.title)) {
      legend.title <- TRUE
    }
  } else if (type != "geom_sf") {
    p <- p +
      scale_colour_manual(values = cols$colour) +
      scale_fill_manual(values = cols$colour_fill)
  }
  if (type != "geom_sf") {
    # x axis
    p <- p + 
      validate_x_scale(df = df,
                       x.date_breaks = x.date_breaks,
                       x.date_labels = x.date_labels,
                       x.breaks = x.breaks,
                       x.expand = x.expand,
                       x.breaks_n = x.breaks_n,
                       x.limits = x.limits,
                       x.position = x.position,
                       x.trans = x.trans,
                       decimal.mark = decimal.mark,
                       big.mark = big.mark,
                       horizontal = horizontal,
                       zoom = zoom)
    # y axis
    p <- p +
      validate_y_scale(df = df,
                       y.24h = y.24h,
                       y.age = y.age,
                       y.breaks = y.breaks,
                       y.expand = y.expand,
                       y.labels = y.labels,
                       y.limits = y.limits,
                       y.percent = y.percent,
                       y.percent_break = y.percent_break,
                       y.position = y.position,
                       y.trans = y.trans,
                       stackedpercent = stackedpercent,
                       facet.fixed_y = facet.fixed_y,
                       decimal.mark = decimal.mark,
                       big.mark = big.mark,
                       zoom = zoom)
  }
  
  # validate theme and add markdown support ----
  theme <- validate_theme(theme = theme,
                          markdown = markdown,
                          text_factor = text_factor,
                          family = family,
                          horizontal = horizontal,
                          x.remove = x.remove,
                          x.lbl_angle = x.lbl_angle,
                          x.lbl_align = x.lbl_align,
                          x.lbl_italic = x.lbl_italic,
                          facet.fill = facet.fill,
                          facet.bold = facet.bold,
                          facet.italic = facet.italic,
                          facet.size = facet.size,
                          facet.margin = facet.margin,
                          legend.italic = legend.italic,
                          title.colour = title.colour,
                          subtitle.colour = subtitle.colour)
  if (!is_empty(theme)) {
    p <- p + theme
  }
  
  # add titles ----
  if (!misses_x.title) p <- p + labs(x = validate_titles(x.title)) # this will overwrite the var name
  if (!misses_y.title) p <- p + labs(y = validate_titles(y.title)) # this will overwrite the var name
  if (!misses_title) p <- p + labs(title = validate_titles(title, markdown = markdown, max_length = title.linelength))
  if (!misses_subtitle) p <- p + labs(subtitle = validate_titles(subtitle, markdown = markdown, max_length = subtitle.linelength))
  if (!misses_tag) p <- p + labs(tag = validate_titles(tag))
  if (!misses_caption) p <- p + labs(caption = validate_titles(caption))
  if (has_category(df)) {
    # legend
    if (isTRUE(legend.title)) {
      legend.title <- get_category_name(df)
    }
    if ("colour" %in% names(mapping)) {
      p <- p + labs(colour = validate_titles(legend.title))
    }
    if ("fill" %in% names(mapping)) {
      p <- p + labs(fill = validate_titles(legend.title))
    }
  }
  
  # set legend ----
  if (!(has_category(df) && is.numeric(get_category(df)))) {
    # only change this when there is no guide_colourbar(), see validate_category_scale()
    legend.position <- validate_legend.position(legend.position)
    p <- p + theme(legend.position = legend.position)
    if (!is.null(legend.reverse)) {
      p <- p +
        guides(fill = guide_legend(reverse = isTRUE(legend.reverse)),
               colour = guide_legend(reverse = isTRUE(legend.reverse)))
    }
    if (isTRUE(horizontal)) {
      if (legend.position %in% c("top", "bottom") &&
          validate_sorting(category.sort, horizontal = horizontal) %unlike% "freq") {
        # turn legend items when on top or bottom, but not when sorting is freq, freq-asc or freq-desc
        p <- p +
          guides(fill = guide_legend(reverse = TRUE),
                 colour = guide_legend(reverse = TRUE))
      }
    }
  }
  
  # set facet ----
  if (has_facet(df)) {
    p <- p +
      validate_facet(df = df,
                     type = type,
                     facet.repeat_lbls_x = facet.repeat_lbls_x,
                     facet.repeat_lbls_y = facet.repeat_lbls_y,
                     facet.relative = facet.relative,
                     facet.drop = facet.drop,
                     facet.nrow = facet.nrow,
                     facet.position = facet.position,
                     horizontal = horizontal)
  }
  
  # set datalabels ----
  if (has_datalabels(df)) {
    p <- set_datalabels(p = p,
                        df = df,
                        type = type,
                        width = width,
                        stacked = stacked,
                        stackedpercent = stackedpercent,
                        datalabels.fill = datalabels.fill,
                        datalabels.colour = datalabels.colour,
                        datalabels.size = datalabels.size,
                        datalabels.angle = datalabels.angle,
                        family = family,
                        reverse = reverse,
                        horizontal = horizontal,
                        misses_datalabels = misses_datalabels)
  }
  
  # turn plot horizontal if required ----
  if (isTRUE(horizontal)) {
    p <- p + coord_flip()
  }
  
  # return plot ----
  if (isTRUE(print)) {
    print(p)
  } else {
    p
  }
}
