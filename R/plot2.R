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
#' @details The [plot2()] function is a convenient wrapper around many [`ggplot2`][ggplot2::ggplot()] functions such as [`ggplot()`][ggplot2::ggplot()], [`geom_col()`][ggplot2::geom_col()], [`facet_wrap()`][ggplot2::facet_wrap()], [`labs()`][ggplot2::labs()], etc., and provides:
#'   * Writing as few lines of codes as possible
#'   * Easy plotting in three 'directions': `x` (the regular x axis), `category` (replaces 'fill' and 'colour') and `facet`
#'   * Automatic setting of these 'directions' based on the input data
#'   * Easy way for sorting data in many ways (such as on alphabet, numeric value, frequency, original data order), by setting a single argument for the 'direction': `x.sort`, `category.sort` and `facet.sort`
#'   * Easy limiting values, e.g. by setting `x.max_items = 5` or `category.max_items = 5`
#'   * Markdown support for any label, with any theme
#'   * An extra clean, minimalistic theme with a lot of whitespace (but without unnecessary margins) that is ideal for printing: [theme_minimal2()]
#'   * Some conveniences from Microsoft Excel:
#'     * The y axis starts at 0
#'     * The y scale expands at the top to be better able to interpret all data points
#'     * Date breaks can be written in a readable format such as "d mmm yyyy"
#'     * Data labels can easily be printed and are automatically determined
#'   * Support for any `ggplot2` extension based on [ggplot2::fortify()]
#'   
#' The `ggplot2` package in conjunction with the `tidyr`, `forcats` and `cleaner` packages can provide above functionalities, but the goal of the [plot2()] function is to generalise this into one function. Less typing, faster coding.
#' @importFrom dplyr `%>%` tibble
#' @importFrom ggplot2 ggplot geom_col
#' @export
#' @examples
#' head(admitted_patients)
#' 
#' # no variables determined, plot2() does a try:
#' plot2(iris)
#' 
#' # x and y set, no addition mapping will be set:
#' plot2(iris, Sepal.Width, Sepal.Length)
#' 
#' plot2(iris, x = Species, y = Sepal.Length, type = "boxplot")
#' 
#' library(dplyr, warn.conflicts = FALSE)
#' 
#' # automatically takes first columns, will be points since both are numeric
#' mtcars %>% 
#'   plot2()
#' mtcars %>% 
#'   plot2(mpg, hp)
#'   
#' # if there are more Y values than groups, the default will be boxplot
#' mtcars %>% 
#'   mutate(cyl = as.character(cyl)) %>%
#'   plot2()
#'   
#' # plot2() supports all S3 extensions available through ggplot2::fortify():
#' mtcars %>% 
#'   lm(mpg ~ hp, data = .) %>% 
#'   plot2(title = "Titles/captions *support* **markdown**",
#'         x.title = "*hp*")
plot2 <- function(.data, ...) {
  # TO DO - copy arguments here from plot2.data.frame()
  UseMethod("plot2")
}

#' Methods for [plot2()]
#' 
#' These are the implemented methods for different S3 classes to be used in [plot2()].
# @param object data object which will be transformed with [ggplot2::fortify()], which allows S3 extensions by other packages
#' @rdname plot2-methods
#' @name plot2-methods
#' @inheritParams plot2
#' @importFrom dplyr tibble
#' @importFrom ggplot2 fortify
#' @export
plot2.default <- function(.data, ...) {
  # ggplot2's fortify() will try to make this a data.frame,
  # so that plot2.data.frame() can be called
  plot2(fortify(.data), ...)
}

#' @rdname plot2-methods
#' @export
plot2.numeric <- function(y, ...) {
  y_deparse <- deparse(substitute(y))
  df <- tibble(y = y)
  colnames(df) <- y_deparse
  plot2(df, x = NULL, category = NULL, facet = NULL, ...)
}

#' @rdname plot2-methods
#' @export
plot2.freq <- function(.data,
                       x = .data$item,
                       y = .data$count,
                       x.sort = "freq-desc",
                       ...) {
  plot2(as.data.frame(.data, stringsAsFactors = FALSE)[, 1:2, drop = FALSE],
        x = x,
        y = y,
        x.sort = x.sort,
        ...)
}

#' @rdname plot2-methods
#' @export
plot2.sf <- function(.data,
                     colour = "grey50",
                     x.expand = 0,
                     y.expand = 0,
                     datalabels = FALSE,
                     datalabels.colour = "black",
                     legend.position = "right",
                     ...) {
  
}

#' @rdname plot2-methods
#' @importFrom dplyr `%>%` mutate tibble 
#' @importFrom ggplot2 ggplot aes labs scale_x_discrete scale_x_continuous stat_boxplot scale_colour_continuous
#' @importFrom certestyle format2 font_red font_black
#' @export
plot2.data.frame <- function(.data = NULL,
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
                             title_maxlength = 60,
                             subtitle_maxlength = 60,
                             na.replace = "(??)",
                             na.rm = FALSE,
                             facet.fill = NULL,
                             facet.position = "top",
                             facet.bold = TRUE,
                             facet.size = 10,
                             facet.repeat_lbls_x = TRUE,
                             facet.repeat_lbls_y = TRUE,
                             facet.fixed_y = FALSE,
                             facet.drop = FALSE,
                             facet.nrow = NULL,
                             facet.margin = 8,
                             facet.relative = FALSE,
                             facet.italic = FALSE,
                             x.date_breaks = NULL,
                             x.date_labels = NULL,
                             category.focus = NULL,
                             colour = getOption("plot2.colours", "certe"),
                             colour_fill = NULL,
                             colour.opacity = 0,
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
                             x.sort = NULL,
                             category.sort = TRUE,
                             facet.sort = TRUE,
                             datalabels = TRUE,
                             datalabels.round = ifelse(y.percent, 2, 1),
                             datalabels.colour = "grey25",
                             datalabels.fill = "white",
                             datalabels.size = 3,
                             decimal.mark = ",",
                             big.mark = ".",
                             summarise_function = base::sum,
                             stacked = FALSE,
                             stackedpercent = FALSE,
                             horizontal = FALSE,
                             reverse = horizontal,
                             smooth = FALSE,
                             smooth.method = NULL,
                             smooth.formula = NULL,
                             smooth.se = TRUE,
                             smooth.ci = 0.95,
                             smooth.alpha = 0.15,
                             smooth.size = 0.5,
                             smooth.linetype = 3,
                             size = NULL,
                             linetype = 1,
                             bins = NULL,
                             width = NULL,
                             jitter_seed = 1,
                             legend.position = "top",
                             legend.title = "",
                             legend.reverse = NULL,
                             legend.barheight = 5,
                             legend.barwidth = 1,
                             legend.nbin = 300,
                             legend.italic = FALSE,
                             zoom = FALSE,
                             print = FALSE,
                             text.factor = 1,
                             text.font_family = "Verdana",
                             theme = theme_minimal2(),
                             markdown = TRUE,
                             taxonomy.italic = markdown,
                             # old certetools support
                             x.category = NULL,
                             y.category = NULL,
                             ...) {
  
  if (NROW(.data) == 0) {
    warning("No observations to plot.", call. = FALSE)
    return(invisible())
  }
  
  misses_x <- missing(x)
  misses_category <- missing(category) & missing(y.category)
  if (!missing(y.category)) {
    warning(font_black("Using "), font_red("'y.category' is deprecated"), font_black(" - use 'category' instead"), call. = FALSE) 
  }
  if (!missing(x.category)) {
    warning(font_black("Using "), font_red("'x.category' is deprecated"), font_black(" - use 'facet' instead"), call. = FALSE) 
  }
  
  label_x <- deparse(substitute(x))
  label_y <- deparse(substitute(y))
  label_category <- deparse(substitute(category))
  label_facet <- deparse(substitute(facet))
  
  # prepare data ----
  df <- .data %>%
    mutate(`_var_x` = {{ x }},
           `_var_y` = {{ y }},
           `_var_category` = if (tryCatch(!is.null(y.category), error = function(x) TRUE)) {{ y.category }} else {{ category }},
           `_var_facet` = if (tryCatch(!is.null(x.category), error = function(x) TRUE)) {{ x.category }} else {{ facet }},
           `_var_datalabels` = {{ datalabels }}) %>% 
    as.data.frame(stringsAsFactors = FALSE) %>% 
    # this part will transform the data as needed
    validate_data(misses_x = misses_x,
                  misses_category = misses_category,
                  label_x = label_x,
                  label_y = label_y,
                  label_category = label_category,
                  label_facet = label_facet,
                  decimal.mark = decimal.mark,
                  big.mark = big.mark,
                  datalabels.round = datalabels.round,
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
  type <- validate_type(type, df = df) # this will automatically determine type if is.null(type)
  
  # generate mapping ----
  mapping <- aes(y = `_var_y`, group = 1)
  if (has_x(df)) {
    mapping <- utils::modifyList(mapping, aes(x = `_var_x`,
                                              group = `_var_x`))
  }
  if (has_category(df)) {
    mapping <- utils::modifyList(mapping, aes(fill = `_var_category`,
                                              colour = `_var_category`,
                                              group = `_var_category`))
  }
  if (type %in% c("geom_boxplot", "geom_violin")) {
    # remove the group from the mapping
    mapping <- utils::modifyList(mapping, aes(group = NULL))
  }
  
  # generate colour vectors
  # colour <- validate_colour(colour = colour, colour_fill = colour_fill)
  # colour_fill <- validate_colour_fill(colour = colour, colour_fill = colour_fill)
  
  # generate ggplot ----
  p <- ggplot(data = df, mapping = mapping)
  
  # add geom ----
  if (type == "geom_boxplot") {
    # first add the whiskers
    p <- p + stat_boxplot(geom = "errorbar",
                          coef = 1.5, # 1.5 * IQR
                          width = ifelse(is.null(width), 0.75, width) * 0.75 * 0.5,
                          lwd = (ifelse(is.null(size), 2, size) * 0.75) / 2.5)
  }
  p <- p + validate_geom(type = type,
                         df = df,
                         stacked = stacked,
                         stackedpercent = stackedpercent,
                         horizontal = horizontal,
                         width = width,
                         size = size,
                         linetype = linetype,
                         reverse = reverse)
  
  # add colours
  if (has_category(df)) {
    if (is.numeric(get_category(df))) {
      p <- p + scale_colour_continuous()
    }
    p <- p +
      scale_colour_manual(values = colour_set) +
      scale_fill_manual(values = colour_set.fill)
  }
  
  # add axis labels ----
  p <- p +
    labs(x = get_x_name(df),
         y = get_y_name(df),
         fill = get_category_name(df),
         colour = get_category_name(df)) # will return NULL if not available, so always works
  
  # add the right scales ----
  
  # x axis
  p <- p + 
    validate_x_scale(df = df,
                     x.date_breaks = x.date_breaks,
                     x.date_labels = x.date_labels,
                     x.breaks = x.breaks,
                     x.expand = x.expand,
                     x.breaks_n = x.breaks_n,
                     x.limit = x.limits,
                     x.position = x.position,
                     x.trans = x.trans,
                     decimal.mark = decimal.mark,
                     big.mark = big.mark,
                     horizontal = horizontal)
  
  # y axis
  p <- p +
    validate_y_scale(df = df,
                     y.24h = y.24h,
                     y.age = y.age,
                     y.breaks = y.breaks,
                     y.expand = y.expand,
                     y.fixed = y.fixed,
                     y.labels = y.labels,
                     y.limits = y.limits,
                     y.percent = y.percent,
                     y.position = y.position,
                     y.trans = y.trans,
                     stackedpercent = stackedpercent,
                     facet.fixed_y = facet.fixed_y,
                     decimal.mark = decimal.mark,
                     big.mark = big.mark)
  
  
  # add theme / markdown support ----
  theme <- validate_theme(theme = theme, markdown = markdown)
  if (!is_empty(theme)) {
    p <- p + theme
  }
  
  # add titles ----
  if (!is_empty(x.title)) p <- p + labs(x = x.title) # this wil overwrite the var name
  if (!is_empty(y.title)) p <- p + labs(y = y.title) # this wil overwrite the var name
  if (!is_empty(title)) p <- p + labs(title = title)
  if (!is_empty(subtitle)) p <- p + labs(subtitle = subtitle)
  if (!is_empty(tag)) p <- p + labs(tag = tag)
  if (!is_empty(caption)) p <- p + labs(caption = caption)
  
  # set positions ----
  p <- p + theme(legend.position = validate_legend.position(legend.position))
  
  # return plot ----
  if (isTRUE(print)) {
    print(p)
  } else {
    p
  }
}
