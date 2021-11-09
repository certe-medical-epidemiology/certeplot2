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
#' @export
#' @examples
#' head(iris)
#' 
#' # no variables determined, plot2() tries for itself:
#' # the type will be points since the first two variables are numeric
#' plot2(iris)
#' 
#' # only view the data part, like ggplot2 normally does
#' plot2(iris, zoom = TRUE)
#' 
#' # if x and y are set, no addition mapping will be set:
#' plot2(iris, Sepal.Width, Sepal.Length)
#' 
#' # change to any type
#' plot2(iris, x = Species, y = Sepal.Length, type = "boxplot")
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
  df <- data.frame(y = y, stringsAsFactors = FALSE)
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
#' @importFrom dplyr `%>%` mutate vars group_by across summarise
#' @importFrom ggplot2 ggplot aes labs stat_boxplot scale_colour_manual scale_fill_manual coord_flip facet_grid facet_wrap coord_flip geom_smooth
#' @importFrom certestyle format2 font_red font_black font_blue
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
                             title.linelength = 60,
                             subtitle.linelength = 60,
                             na.replace = "(??)",
                             na.rm = FALSE,
                             facet.fill = NULL,
                             facet.position = "top",
                             facet.bold = TRUE,
                             facet.size = 10,
                             facet.repeat_lbls_x = TRUE,
                             facet.repeat_lbls_y = FALSE,
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
                             datalabels.size = 3.5,
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
                             smooth.level = 0.95,
                             smooth.alpha = 0.15,
                             smooth.size = 0.75,
                             smooth.linetype = 3,
                             size = NULL,
                             linetype = 1,
                             bins = NULL,
                             width = NULL,
                             jitter_seed = NA,
                             violin_scale = "count",
                             legend.position = "top",
                             legend.title = FALSE,
                             legend.reverse = NULL,
                             legend.barheight = 5,
                             legend.barwidth = 1,
                             legend.nbin = 300,
                             legend.italic = FALSE,
                             zoom = FALSE,
                             print = FALSE,
                             text.factor = 1,
                             text.font_family = "Verdana",
                             theme = theme_minimal2(horizontal = horizontal,
                                                    text.font_family = text.font_family,
                                                    text.factor = text.factor),
                             markdown = TRUE,
                             taxonomy.italic = markdown,
                             # old certetools pkg support
                             x.category = NULL,
                             y.category = NULL,
                             ...) {
  
  if (NROW(.data) == 0) {
    warning("No observations to plot.", call. = FALSE)
    return(invisible())
  }
  
  misses_x <- missing(x)
  misses_y <- missing(y)
  misses_category <- missing(category) & missing(y.category)
  misses_datalabels <- missing(datalabels)
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
                  type = type,
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
  type <- validate_type(type = type, df = df) # this will automatically determine type if is.null(type)
  # transform data if not a continuous type but group sizes are > 1
  if (any(group_sizes(df) > 1) && !type_is_continuous(type)) {
    plot_warning("Duplicate observations in discrete plot type (", font_blue(type), "), applying ",
                 font_blue("summarise_function = " ), font_blue(deparse(substitute(summarise_function))))
    df <- summarise_data(df = df, summarise_function = summarise_function,
                         decimal.mark = decimal.mark, big.mark = big.mark,
                         datalabels.round = datalabels.round)
  }
  # remove datalabels in continuous types
  if (isTRUE(misses_datalabels) && type_is_continuous(type)) {
    df <- df %>% select(-`_var_datalabels`)
  }
  if (!isTRUE(misses_y) && type_is_continuous_x(type)) {
    plot_message("Ignoring ", font_blue("y"), " for plot type ", font_blue(gsub("geom_", "", type)))
    df$`_var_y` <- df$`_var_x`
  }
  
  # set default size and width ----
  size <- validate_size(size = size, type = type)
  width <- validate_width(width = width, type = type)
  
  # generate colour vectors ----
  cols <- validate_colour(df = df,
                          colour = colour,
                          colour_fill = colour_fill,
                          misses_colour_fill = missing(colour_fill),
                          horizontal = horizontal,
                          type = type)
  
  mapping <- aes()
  # generate mapping ----
  if (!type_is_continuous_x(type)) {
    # histograms etc. have a continuous x variable, so only set y if not a histogram-like
    mapping <- aes(y = `_var_y`, group = 1)
  } else {
    if (missing(zoom)) {
      zoom <- TRUE
    }
  }
  if (has_x(df)) {
    mapping <- utils::modifyList(mapping, aes(x = `_var_x`,
                                              group = `_var_x`))
  }
  if (has_category(df)) {
    mapping <- utils::modifyList(mapping, aes(fill = `_var_category`,
                                              colour = `_var_category`,
                                              group = `_var_category`))
  }
  if (type_is_continuous(type)) {
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
    validate_geom(type = type,
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
                  bins = bins,
                  cols = cols)
  if (isTRUE(smooth)) {
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
  
  # add colours
  p <- p +
    scale_colour_manual(values = cols$colour) +
    scale_fill_manual(values = cols$colour_fill)
  
  # add axis labels ----
  p <- p +
    labs(x = get_x_name(df),
         y = get_y_name(df),
         fill = get_category_name(df),
         colour = get_category_name(df)) # will return NULL if not available, so always works
  if (type_is_continuous_x(type)) {
    if (type %like% "density") {
      p <- p +
        labs(y = "Density")
      if (missing(y.percent)) {
        y.percent <- TRUE
      }
    } else {
      p <- p +
        labs(y = "Frequency")
    }
  }
  
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
                     horizontal = horizontal,
                     zoom = zoom)
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
                     y.percent_break = y.percent_break,
                     y.position = y.position,
                     y.trans = y.trans,
                     stackedpercent = stackedpercent,
                     facet.fixed_y = facet.fixed_y,
                     decimal.mark = decimal.mark,
                     big.mark = big.mark,
                     zoom = zoom)
  
  # add theme + markdown support ----
  theme <- validate_theme(theme = theme, markdown = markdown)
  if (!is_empty(theme)) {
    p <- p + theme
  }
  
  # add titles ----
  if (!missing(x.title)) p <- p + labs(x = validate_titles(x.title)) # this will overwrite the var name
  if (!missing(y.title)) p <- p + labs(y = validate_titles(y.title)) # this will overwrite the var name
  if (!missing(title)) p <- p + labs(title = validate_titles(title, markdown = markdown, max_length = title.linelength))
  if (!missing(subtitle)) p <- p + labs(subtitle = validate_titles(subtitle, markdown = markdown, max_length = subtitle.linelength))
  if (!missing(tag)) p <- p + labs(tag = validate_titles(tag))
  if (!missing(caption)) p <- p + labs(caption = validate_titles(caption))
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
  
  # set positions ----
  p <- p + theme(legend.position = validate_legend.position(legend.position))
  
  # set facets ----
  if (has_facet(df)) {
    scales <- "fixed"
    if (isTRUE(facet.repeat_lbls_x) & isTRUE(facet.repeat_lbls_y)) {
      scales <- "free"
    } else if (isTRUE(facet.repeat_lbls_y)) {
      scales <- "free_y"
      if (isTRUE(horizontal)) {
        scales <- "free_x"
      }
    } else if (isTRUE(facet.repeat_lbls_x)) {
      scales <- "free_x"
      if (isTRUE(horizontal)) {
        scales <- "free_y"
      }
    }
    
    if (any(is.na(get_facet(df)))) {
      # drop is droppen van factors levels. Als dit FALSE is en de kolom bevat NA, geeft het een fout:
      # Error in scale_apply(layer_data, x_vars, "train", SCALE_X, x_scales)
      facet.drop <- TRUE
    }
    if (isTRUE(facet.relative)) {
      switch <- "x"
      if (isTRUE(horizontal)) {
        switch <- "y"
      }
      p <- p +
        facet_grid(cols = vars(`_var_facet`),
                   space = scales,
                   drop = facet.drop,
                   scales = scales,
                   switch = switch)
    } else {
      p <- p +
        facet_wrap("`_var_facet`",
                   scales = scales,
                   strip.position = facet.position,
                   drop = facet.drop,
                   nrow = facet.nrow)
    }
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
                        text.factor = text.factor,
                        text.font_family = text.font_family,
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
