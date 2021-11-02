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
#' These functions create [ggplot2::ggplot()] objects, but work in a more convenient way than the `ggplot2` package. By design, the `ggplot2` package requires users to use a lot of functions, while the `certeplot2` package requires to use only arguments in one function.
#' @param .data data to plot
#' @details The [plot2()] function uses the `ggplot2` package for plotting and provides:
#'   * A convenient wrapper around many `ggplot2` functions such as [ggplot()], [geom_col()], [facet_wrap()], [labs()], etc.
#'   * Writing as few lines of codes as possible
#'   * Some benefits from Microsoft Excel:
#'     * The y axis starts at 0
#'     * The y scale contains extra space at the top to be able to interpret all data points
#'     * Date breaks can be written in a readable format such as "d mmm yyyy"
#'     * Data labels can easily be printed
#'   * Easy plotting in three 'directions': `x` (the regular x axis), `category` (replaces 'fill' and 'colour') and `facet`
#'   * Easy way for sorting data in may ways (such as on alphabet, numeric value, frequency, original data order), by setting a single argument for the 'direction': `x.sort`, `category.sort` and `facet.sort`
#'   * Easy limiting values, e.g. by setting `x.max_items = 5` or `category.max_items = 5`
#'   * Markdown support for any label, with any theme
#'   * An extra clean, minimalistic theme with a lot of whitespace (but without unnecessary margins) that is ideal for printing: [theme_minimal2()]
#'   * Support for any `ggplot2` extension based on [ggplot2::fortify()]
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
plot2 <- function(...) {
  UseMethod("plot2")
}

#' @rdname plot2
#' @param object data object which will be transformed with [ggplot2::fortify()], which allows S3 extensions by other packages
#' @importFrom dplyr tibble
#' @importFrom ggplot2 fortify
#' @export
plot2.default <- function(object, ...) {
  # ggplot2's fortify() will try to make this a data.frame,
  # so that plot2.data.frame() can be called
  plot2(fortify(object), ...)
}

#' @rdname plot2
#' @export
plot2.numeric <- function(y, ...) {
  y_deparse <- deparse(substitute(y))
  df <- tibble(y = y)
  colnames(df) <- y_deparse
  plot2(df, x = NULL, category = NULL, facet = NULL, ...)
}

#' @rdname plot2
#' @importFrom dplyr `%>%` mutate tibble 
#' @importFrom ggplot2 ggplot aes labs scale_x_discrete scale_x_continuous theme_grey
#' @importFrom ggtext element_markdown
#' @importFrom certestyle format2
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
                             colour.fill = NULL,
                             colour.opacity = 0,
                             x.lbl_angle = 0,
                             x.lbl_align = NULL,
                             x.lbl_italic = FALSE,
                             x.remove = FALSE,
                             x.position = "bottom",
                             x.max = Inf,
                             x.max_txt = "(rest, x %n)",
                             category.max = Inf,
                             category.max_txt = "(rest, x %n)",
                             facet.max = Inf,
                             facet.max.txt = "(rest, x %n)",
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
    warning("Using 'y.category' is deprecated - use 'category' instead", call. = FALSE) 
  }
  if (!missing(x.category)) {
    warning("Using 'x.category' is deprecated - use 'facet' instead", call. = FALSE) 
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
                  ...)
  
  # generate mapping ----
  mapping <- aes(y = `_var_y`)
  if (has_x(df)) {
    mapping <- utils::modifyList(mapping, aes(x = `_var_x`))
  }
  if (has_category(df)) {
    mapping <- utils::modifyList(mapping, aes(fill = `_var_category`,
                                              colour = `_var_category`))
  }
  
  # generate ggplot ----
  p <- ggplot(data = df, mapping = mapping)
  
  # add geom ----
  type <- validate_type(type, df = df) # this will automatically determine type if is.null(type)
  geom_fn <- getExportedValue(name = type, ns = asNamespace("ggplot2"))
  p <- p + geom_fn()
  
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
  
  
  # add the theme and markdown support ----
  if (is_empty(theme)) {
    # turn to default ggplot2 theme, so we can:
    # - extend all element_text() classes with element_markdown()
    # - add all theme options set as parameters, like legend position
    theme <- theme_grey()
  }
  if (inherits(theme, "theme")) {
    if (isTRUE(markdown)) {
      # add 'element_markdown' to all text classes, which the ggtext pkg will use to print in markdown
      # for this, the ggtext pkg has at least to be installed, but not loaded
      attr_bak <- attributes(theme)
      theme <- lapply(theme, function(el) {
        if (inherits(el, "element_text")) {
          class(el) <- c("element_markdown", class(el))
        }
        el
      })
      attributes(theme) <- attr_bak # restore class and all other attributes
    }
    p <- p + theme
  } else if (!is_empty(theme)) {
    # not a valid ggplot2 theme
    stop("'theme' must be a valid ggplot2 theme", call. = FALSE)
  }
  
  # add titles ----
  if (!is_empty(x.title)) p <- p + labs(x = x.title) # this wil overwrite the var name
  if (!is_empty(y.title)) p <- p + labs(y = y.title) # this wil overwrite the var name
  if (!is_empty(title)) p <- p + labs(title = title)
  if (!is_empty(subtitle)) p <- p + labs(subtitle = subtitle)
  if (!is_empty(tag)) p <- p + labs(tag = tag)
  if (!is_empty(caption)) p <- p + labs(caption = caption)
  
  # return plot ----
  p
}

#' @rdname plot2
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

#' @rdname plot2
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
