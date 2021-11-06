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

#' @importFrom dplyr `%>%` group_by across group_size
validate_type <- function(type, df) {
  type.bak <- type
  if (is.null(type)) {
    if (!has_x(df)) {
      # only numeric values, make it a boxplot
      type <- "geom_boxplot"
      plot_message("Using ", font_blue("type = \"", gsub("geom_", "", type), "\"", collapse = NULL),
                   font_black(" since there is no x axis"))
    } else if (has_x(df) && is.numeric(get_x(df))) {
      # make it points if x and y are both numeric
      type <- "geom_point"
      plot_message("Using ", font_blue("type = \"", gsub("geom_", "", type), "\"", collapse = NULL), 
                   font_black(" since both axes are numeric"))
    } else {
      # check if y has multiple values across groups, then make it boxplot
      sizes <- df %>% 
        group_by(across(c(get_x_name(df), get_category_name(df), get_facet_name(df)))) %>%
        group_size()
      if (all(sizes > 3)) {
        type <- "geom_boxplot"
        plot_message("Using ", font_blue("type = \"", gsub("geom_", "", type), "\"", collapse = NULL),
                     font_black(" since all groups have size > 3"))
      } else {
        # otherwise: column
        type <- "geom_col"
        plot_message("Using ", font_blue("type = \"", gsub("geom_", "", type), "\"", collapse = NULL), 
                     font_black(" as default"))
      }
    }
  } else {
    type <- trimws(tolower(type[1L]))
    type <- gsub("[^a-z0-9_]", "", type)
    if (type == "p") type <- "point"
    if (type == "l") type <- "line"
    if (type == "b") type <- "boxplot"
    if (type == "v") type <- "violin"
    if (type %in% c("c", "column")) type <- "col"
    if (type %unlike% "^geom_") {
      type <- paste0("geom_", type)
    }
    # replace 'points' etc. with 'point' etc.
    type <- gsub("s$", "", type)
  }
  
  valid_geoms <- ls(pattern = "^geom_", env = asNamespace("ggplot2"))
  if (!type %in% valid_geoms) {
    stop("plot type '", type.bak, "' is invalid, since ggplot2::", type, "() does not exist", call. = FALSE)
  }
  type
}

validate_legend.position <- function(legend.position) {
  if (is.na(legend.position[1L])) {
    legend.position <- "none"
  }
  legend.position <- trimws(tolower(legend.position[1L]))
  legend.position <- gsub("^t$", "top", legend.position)
  legend.position <- gsub("^r$", "right", legend.position)
  legend.position <- gsub("^b$", "bottom", legend.position)
  legend.position <- gsub("^l$", "left", legend.position)
  
  if (!legend.position %in% c("top", "right", "bottom", "left")) {
    stop("legend.position must be 'top', 'right', 'bottom' or 'left'", call. = FALSE)
  }
  legend.position
}

#' @importFrom dplyr `%>%` select pull mutate arrange across
#' @importFrom certestyle format2 font_bold font_blue
validate_data <- function(df,
                          misses_x,
                          misses_category,
                          label_x,
                          label_y,
                          label_category,
                          label_facet,
                          ...) {
  
  dots <- list(...)
  
  if (!has_y(df)) {
    # try to find numeric column for y
    numeric_cols <- names(which(vapply(FUN.VALUE = logical(1), df, is.numeric)))
    if (is.na(numeric_cols[1L])) {
      stop("no numeric column found to use for y", call. = FALSE)
    }
    if (length(numeric_cols) > 1) {
      # first check if there is also no x axis
      if (!has_x(df)) {
        # make x first numeric column and y second numeric column
        plot_message("Using ", font_blue("x = ", numeric_cols[1L], collapse = NULL))
        plot_message("Using ", font_blue("y = ", numeric_cols[2L], collapse = NULL))
        df <- df %>% 
          mutate(`_var_x` = df %>% pull(numeric_cols[1L]),
                 `_var_y` = df %>% pull(numeric_cols[2L]))
      } else {
        plot_message("Using ", font_blue("y = ", numeric_cols[1L], collapse = NULL))
        df <- df %>% 
          mutate(`_var_y` = df %>% pull(numeric_cols[1L]))
      }
    } else {
      plot_message("Using ", font_blue("y = ", numeric_cols, collapse = NULL))
      df <- df %>% 
        mutate(`_var_y` = df %>% pull(numeric_cols))
    }
  }
  
  if (misses_x && !has_x(df) && ncol(df) > 1) {
    # take first column if it's not used for y
    if (identical(pull(df, 1), get_y(df))) {
      x_col <- colnames(df)[2L]
    } else {
      x_col <- colnames(df)[1L]
    }
    plot_message("Using ", font_blue("x = ", x_col, collapse = NULL))
    df <- df %>% 
      mutate(`_var_x` = df %>% pull(x_col))
  }
  
  if (misses_x && misses_category && !has_category(df) && ncol(df) > 2) {
    # category must only be used if factor or character
    # and if x was also missing
    cols <- sapply(df, function(col) (is.factor(col) | is.character(col)) &
                     !identical(get_y(df), col) &
                     !(has_x(df) && identical(get_x(df), col)))
    cols <- names(cols)[cols]
    if (length(cols) > 0) {
      plot_message("Using ", font_blue("category = ", cols[1L], collapse = NULL))
      df <- df %>% 
        mutate(`_var_category` = df %>% pull(cols[1L]))
    }
  }
  
  # create surrogate labels to df
  if (has_x(df) & !label_x %in% colnames(df) & label_x != "NULL") {
    df$`_label_x` <- get_x(df)
    colnames(df)[colnames(df) == "_label_x"] <- label_x
  }
  if (has_y(df) & !label_y %in% colnames(df) & label_y != "NULL") {
    df$`_label_y` <- get_y(df)
    colnames(df)[colnames(df) == "_label_y"] <- label_y
  }
  if (has_category(df) & !label_category %in% colnames(df) & label_category != "NULL") {
    df$`_label_category` <- get_category(df)
    colnames(df)[colnames(df) == "_label_category"] <- label_category
  }
  if (has_facet(df) & !label_facet %in% colnames(df) & label_facet != "NULL") {
    df$`_label_facet` <- get_facet(df)
    colnames(df)[colnames(df) == "_label_facet"] <- label_facet
  }
  
  # remove datalabels if all are FALSE
  if (has_datalabels(df) && all(get_datalabels(df) == FALSE)) {
    df <- df %>% select(-`_var_datalabels`)
  }
  # take datalabels from y axis if all are TRUE
  if (has_datalabels(df) && all(get_datalabels(df) == TRUE)) {
    df <- df %>% mutate(`_var_datalabels` = `_var_y`)
  }
  
  # format datalabels
  if (has_datalabels(df)) {
    df <- df %>% mutate(`_var_datalabels` = format2(`_var_datalabels`,
                                                    decimal.mark = dots$decimal.mark,
                                                    big.mark = dots$big.mark,
                                                    round = dots$datalabels.round,
                                                    force.decimals = TRUE))
  }
  
  # apply sortings
  if (has_x(df)) {
    if (is.null(dots$x.sort) && inherits(get_x(df), c("character", "factor"))) {
      dots$x.sort <- TRUE
    }
    
    df <- df %>% 
      mutate(`_var_x` = sort_data(original_values = get_x(df),
                                  sort_method = dots$x.sort,
                                  datapoints = get_y(df),
                                  summarise_function = dots$summarise_function,
                                  horizontal = dots$horizontal)) %>%
      arrange(across(`_var_x`))
  }
  if (has_category(df)) {
    df <- df %>% 
      mutate(`_var_category` = sort_data(original_values = get_category(df),
                                         sort_method = dots$category.sort,
                                         datapoints = get_y(df),
                                         summarise_function = dots$summarise_function,
                                         horizontal = dots$horizontal))
  }
  if (has_facet(df)) {
    df <- df %>% 
      mutate(`_var_facet` = sort_data(original_values = get_facet(df),
                                      sort_method = dots$facet.sort,
                                      datapoints = get_y(df),
                                      summarise_function = dots$summarise_function,
                                      horizontal = FALSE)) # never reversely sort when horizontal
  }
  
  # apply limitations (have to been after sorting, e.g. on frequency)
  df <- set_max_items(df = df,
                      y = get_y(df),
                      x = get_x_name(df),
                      x.max_items = dots$x.max_items,
                      x.max_txt = dots$x.max_text,
                      category = get_category_name(df), 
                      category.max_items = dots$category.max_items,
                      category.max_txt = dots$category.max_txt,
                      facet = get_facet_name(df), 
                      facet.max_items = dots$facet.max_items, 
                      facet.max_txt = dots$facet.max_txt,
                      horizontal = dots$horizontal)
  
  # sort on x, important when piping plot2()'s after plot2()'s
  df <- df %>% 
    arrange(across(`_var_x`))
  
  # output
  df
}

#' @importFrom ggplot2 scale_x_discrete scale_x_date scale_x_datetime scale_x_discrete scale_x_continuous expansion waiver
#' @importFrom scales reverse_trans
#' @importFrom cleaner format_datetime
validate_x_scale <- function(df,
                             x.date_breaks,
                             x.date_labels,
                             x.breaks,
                             x.breaks_n,
                             x.expand,
                             x.limits,
                             x.position,
                             x.trans,
                             decimal.mark,
                             big.mark,
                             horizontal) {
  if (!has_x(df)) {
    scale_x_discrete(labels = NULL, breaks = NULL)
  } else {
    if (!is.null(x.limits)) {
      if (length(x.limits) != 2) {
        stop("`x.limits` must be of length 2", call. = FALSE)
      }
      if (inherits(get_x(df), "Date")) {
        x.limits <- as.Date(x.limits, origin = "1970-01-01")
      } else if (inherits(get_x(df), "POSIXt")) {
        x.limits <- as.POSIXct(x.limits, origin = "1970-01-01")
      }
      if (inherits(get_x(df), c("Date", "POSIXt"))) {
        # edit limits so x.limits has one spare one at each side
        x.limits[1] <- x.limits[1] - 1
        x.limits[2] <- x.limits[2] + 1
        # strip that extra ones from x.expand, so that all columns will plot
        if (!is.function(x.expand)) {
          x.expand <- x.expand - 1
        }
      }
    }
    if (!is.function(x.expand)) {
      if (length(x.expand) == 1) {
        x.expand <- c(x.expand, x.expand)
      }
      x.expand <- expansion(add = x.expand)
    }
    if (inherits(get_x(df), c("Date", "POSIXt"))) {
      auto_breaks_labels <- determine_date_breaks_labels(get_x(df))
      if (is.null(x.date_breaks)) {
        x.date_breaks <- auto_breaks_labels$breaks
      }
      if (is.null(x.date_labels)) {
        x.date_labels <- auto_breaks_labels$labels
      }
    }
    if (inherits(get_x(df), "Date")) {
      scale_x_date(position = x.position,
                   date_breaks = x.date_breaks,
                   date_labels = format_datetime(x.date_labels),
                   expand = x.expand,
                   limits = x.limits)
    } else if (inherits(get_x(df), "POSIXt")) {
      scale_x_datetime(position = x.position,
                       date_breaks = x.date_breaks,
                       date_labels = format_datetime(x.date_labels),
                       expand = x.expand,
                       limits = x.limits)
    } else {
      if (!is.numeric(get_x(df))) {
        scale_x_discrete(position = x.position)
      } else {
        if (x.trans == "identity" & horizontal == TRUE) {
          x.trans <- reverse_trans()
        }
        scale_x_continuous(labels = function(x, ...) format2(x, decimal.mark = decimal.mark, big.mark = big.mark),
                           breaks = if (!is.null(x.breaks)) x.breaks else waiver(),
                           n.breaks = x.breaks_n,
                           trans = x.trans,
                           position = x.position,
                           limits = c(ifelse(min(get_x(df)) < 0, NA_real_, 0), NA),
                           expand = expansion(mult = c(0.05, 0.05)))
      }
    }
  }
}

#' @importFrom ggplot2 waiver expansion scale_y_continuous
#' @importFrom cleaner as.percentage
#' @importFrom scales pretty_breaks
validate_y_scale <- function(df,
                             y.24h,
                             y.age,
                             y.breaks,
                             y.expand,
                             y.fixed,
                             y.labels,
                             y.limits,
                             y.percent,
                             y.position,
                             y.trans,
                             stackedpercent,
                             facet.fixed_y,
                             decimal.mark,
                             big.mark,
                             ...) {
  
  breaks_fn <- function(df, waiver,
                        y.breaks = NULL, y.expand = 0.25, stackedpercent = FALSE,
                        y.age = FALSE, y.percent = FALSE, y.percent.break = 10, y.24h = FALSE, y.limits = NULL,
                        ...) {
    data_min <- min(0, df) * -(1 + y.expand)
    data_max <- max(df) * (1 + y.expand)
    
    if (!is.null(y.breaks)) {
      y.breaks
    } else if (isTRUE(y.age)) {
      # no decimal numbers, generate max 12 labels
      function(x, ...) seq(from = min(0, x),
                           to = min(120, max(x)),
                           by = 10)
    } else if (isTRUE(y.24h)) {
      function(x, ...) seq(from = min(0, x),
                           to = max(x),
                           by = 24)
    } else if (isTRUE(y.percent)) {
      # calculate how many labels will be printed, keep around 10
      if (is.null(y.limits)) {
        y.limits <- c(data_min, data_max)
      }
      labels_n <- (max(y.limits) - min(y.limits)) / y.percent.break
      if (as.integer(labels_n) > 10) {
        warning("printing at most 10 labels for y axis, set with `y.percent.break`", call. = FALSE)
        y.percent.break <- round((max(y.limits) - min(y.limits)) / 10, 2)
      }
      function(x, y_percent_break = y.percent.break, ...) seq(from = min(0, x),
                                                              to = max(x),
                                                              by = y_percent_break)
    } else if (isTRUE(stackedpercent)) {
      seq(from = 0,
          to = 1,
          by = 0.1)
    } else {
      # this makes sure that integers will actually print as integers
      pretty_breaks()
    }
  }
  
  labels_fn <- function(df, waiver,
                        y.labels,
                        y.age, y.percent, y.24h, stackedpercent,
                        decimal.mark, big.mark, ...) {
    if (!is.null(y.labels)) {
      y.labels
    } else if (isTRUE(y.24h)) {
      function(x, dec = decimal.mark, big = big.mark, ...) paste0(format2(x, decimal.mark = dec, big.mark = big), "u (", x / 24, "d)")
    } else if (isTRUE(y.age)) {
      function(x, dec = decimal.mark, big = big.mark, ...) paste0(format2(x, decimal.mark = dec, big.mark = big, round = 0),
                                                                  ifelse(Sys.getlocale("LC_COLLATE") %like% "nl|dutch", " jr", " yrs"))
    } else if (isTRUE(y.percent) | isTRUE(stackedpercent)) {
      function(x, dec = decimal.mark, big = big.mark, ...) format2(as.percentage(x), decimal.mark = dec, big.mark = big)
    } else {
      function(x, dec = decimal.mark, big = big.mark, ...) format2(x, decimal.mark = dec, big.mark = big)
    }
  }
  
  limits_fn <- function(df, y.limits,
                        y.expand, facet.fixed_y, y.age,
                        ...) {
    if (!is.null(y.limits)) {
      y.limits
    } else if (isTRUE(y.age)) {
      # geen functie dus, maar vector forceren
      c(0, max(df) * (1 + y.expand))
    } else if (isTRUE(facet.fixed_y)) {
      # geen functie dus, maar vector forceren
      c(NA, max(df) * (1 + y.expand))
    } else {
      function(x, y_expand = y.expand, ...) c(min(0, x), max(x))
    }
  }
  
  expand_fn <- function(df, y.expand, y.age, stackedpercent, ...) {
    if (is.function(y.expand)) {
      y.expand
    } else if (isTRUE(y.age) | isTRUE(stackedpercent)) {
      expansion(mult = c(0, 0))
    } else {
      # ingestelde percentage toevoegen aan bovenkant bij positieve waarden en aan onderkant bij negatieve waarden
      expansion(mult = c(ifelse(any(df < 0), y.expand, 0),
                         ifelse(any(df > 0), y.expand, 0)))
    }
  }
  
  scale_y_continuous(
    breaks = breaks_fn(df = get_y(df),
                       waiver = waiver(),
                       y.breaks = y.breaks,
                       y.expand = y.expand,
                       stackedpercent = stackedpercent,
                       y.age = y.age,
                       y.percent = y.percent,
                       y.percent.break = y.percent.break,
                       y.24h = y.24h,
                       y.limits = y.limits,
                       ...),
    labels = labels_fn(df = get_y(df),
                       waiver = waiver(),
                       y.labels,
                       y.percent = y.percent,
                       y.age = y.age,
                       y.24h = y.24h,
                       stackedpercent = stackedpercent,
                       decimal.mark = decimal.mark,
                       big.mark = big.mark,
                       ...),
    limits = limits_fn(df = get_y(df),
                       y.limits,
                       y.expand = y.expand,
                       facet.fixed_y = facet.fixed_y,
                       y.age = y.age,
                       ...),
    expand = expand_fn(df = get_y(df),
                       y.expand = y.expand,
                       y.age = y.age,
                       stackedpercent = stackedpercent,
                       ...),
    trans = y.trans,
    position = y.position
  )
}

#' @importFrom ggplot2 position_stack position_fill position_dodge2
validate_geom <- function(type,
                          df,
                          stacked,
                          stackedpercent,
                          horizontal,
                          width,
                          size,
                          linetype,
                          reverse,
                          na.rm,
                          violin_scale,
                          cols) {
  
  if (type == "geom_col") {
    type <- "geom_bar"
  }
  geom_fn <- getFromNamespace(x = type, ns = asNamespace("ggplot2"))
  
  if (type == "geom_bar") {
    # set position
    if (isTRUE(stacked)) {
      position <- position_stack(reverse = reverse)
    } else if (isTRUE(stackedpercent)) {
      position <- position_fill(reverse = reverse)
    } else {
      # small whitespace between columns:
      position <- position_dodge2(width = width * 1.05, preserve = "single")
    }
    do.call(geom_fn,
            args = c(list(width = width,
                          stat = "identity",
                          position = position,
                          na.rm = na.rm),
                     list(colour = cols$colour)[!has_category(df)],
                     list(fill = cols$colour_fill)[!has_category(df)]))
    
  } else if (type %in% c("geom_line", "geom_path")) {
    do.call(geom_fn,
            args = c(list(lineend = "round",
                          size = size,
                          linetype = linetype,
                          na.rm = na.rm),
                     list(colour = cols$colour)[!has_category(df)]))
    
  } else if (type %in% c("geom_point", "geom_path")) {
    do.call(geom_fn,
            args = c(list(size = size,
                          na.rm = na.rm),
                     list(colour = cols$colour)[!has_category(df)]))
    
  } else if (type == "geom_boxplot") {
    do.call(geom_fn,
            args = c(list(outlier.size = size * 3,
                          outlier.alpha = 0.75,
                          width = width,
                          lwd = size, # line width, of whole box
                          fatten = 1.5, # factor to make median thicker compared to lwd
                          na.rm = na.rm),
                     list(colour = cols$colour)[!has_category(df)],
                     list(fill = cols$colour_fill)[!has_category(df)]))
    
  } else if (type == "geom_violin") {
    do.call(geom_fn,
            args = c(list(width = width,
                          lwd = size, # line width, of whole violin
                          scale = violin_scale,
                          trim = TRUE,
                          draw_quantiles = c(0.25, 0.5, 0.75),
                          na.rm = na.rm),
                     list(colour = cols$colour)[!has_category(df)],
                     list(fill = cols$colour_fill)[!has_category(df)]))
    
  } else {
    # try to put some arguments into the requested geom
    warning("'", type, "' is currently only loosely supported in plot2()", call. = FALSE)
    do.call(geom_fn,
            args = c(list(size = size,
                          width = width,
                          linetype = linetype,
                          na.rm = na.rm),
                     list(colour = cols$colour)[!has_category(df)],
                     list(fill = cols$colour_fill)[!has_category(df)]))
  }
}

#' @importFrom certestyle colourpicker
#' @importFrom dplyr `%>%` count pull
validate_colour <- function(df, colour, colour_fill, horizontal, type) {
  
  minimum_length <- df %>%
    count(across(c(get_x_name(df), get_category_name(df), get_facet_name(df)))) %>% 
    nrow()
  
  if (type_is_continuous(type) && length(colour) == 1 && colour %like% "certe[a-z]*" & is.null(colour_fill)) {
    # specific treatment for Certe boxplots/violins/...
    # certeblauw (colour) -> certeblauw6 (colour_fill)
    colour_fill <- paste0(colour, "6")
  }
  
  if (!has_category(df)) {
    # has no category
    if (has_x(df) && length(unique(get_x(df))) != length(colour)) {
      # take only the first
      colour <- colour[1]
      colour_fill <- colour_fill[1]
    }
    colour <- colourpicker(colour)
    colour_fill <- colourpicker(colour_fill)
  } else {
    # has also category
    colour <- colourpicker(colour,
                           length = ifelse(length(colour) == 1, length(unique(get_category(df))), 1))
    colour_fill <- colourpicker(colour_fill,
                                length = ifelse(length(colour_fill) == 1, length(unique(get_category(df))), 1))
    if (horizontal == TRUE) {
      colour <- rev(colour)
      colour_fill <- rev(colour_fill)
    }
    
    if (length(colour) > 1 && length(colour_fill) == 1) {
      colour_fill <- colour
    }
    
    if (length(colour) < minimum_length) {
      colour <- rep(colour, minimum_length / length(colour))
    }
    if (length(colour_fill) < minimum_length) {
      colour_fill <- rep(colour_fill, minimum_length / length(colour_fill))
    }
  }
  
  list(colour = colour,
       colour_fill = colour_fill)
}

validate_size <- function(size, type) {
  if (is.null(size)) {
    if (type %in% c("geom_boxplot", "geom_violin")) {
      size <- 0.5
    } else if (type %in% c("geom_point", "geom_jitter")) {
      size <- 2
    } else {
      size <- 0.75
    }
  }
  size
}

validate_width <- function(width, type) {
  if (is.null(width)) {
    if (type %in% c("geom_boxplot", "geom_violin", "geom_jitter")) {
      width <- 0.75
    } else {
      width <- 0.5
    }
  }
  width
}

validate_titles <- function(text, markdown = TRUE, max_length = NULL) {
  if (is_empty(text)) {
    return(NULL)
  } else {
    if (is.expression(text)) {
      return(text)
    } else {
      if (isTRUE(markdown)) {
        text <- gsub("\n", "<br>", text, fixed = TRUE)
      }
      if (is.null(max_length)) {
        return(text)
      } else {
        return(paste(strwrap(x = text, width = max_length),
                     collapse = ifelse(markdown, "<br>", "\n")))
      }
    }
  }
}

#' @importFrom ggplot2 theme_grey
#' @importFrom ggtext element_markdown
validate_theme <- function(theme, markdown) {
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
    theme
  } else {
    NULL
  }
}

#' @importFrom forcats fct_inorder fct_reorder
#' @importFrom stringr str_sort
sort_data <- function(original_values, sort_method, datapoints, summarise_function, horizontal, last_level) {
  if (is.null(sort_method) || (isTRUE(sort_method) & is.factor(original_values) & horizontal == FALSE)) {
    # don't sort at all
    return(original_values)
  } else if (isTRUE(sort_method) & is.factor(original_values) & horizontal == TRUE) {
    # reverse the levels of the current factor since horizontal == TRUE
    return(factor(as.character(original_values),
                  levels = rev(levels(original_values)),
                  ordered = is.ordered(original_values)))
  }
  
  if (!is.numeric(original_values)) {
    # force characters for anything else than numbers
    original_values <- as.character(original_values)
  }
  
  # set up sort_method
  sort_method.bak <- sort_method[1L]
  sort_method <- tolower(sort_method[1L])
  sort_method <- gsub("[^a-z-]+", "", sort_method)
  sort_method <- gsub("true", "asc", sort_method)      # when sort_method = TRUE
  sort_method <- gsub("false", "inorder", sort_method) # when sort_method = FALSE
  sort_method <- gsub("asc[a-z]+", "asc", sort_method)
  sort_method <- gsub("desc[a-z]+", "desc", sort_method)
  if (grepl("freq$", sort_method)) {
    sort_method <- paste0(sort_method, "-desc")
  }
  if (isTRUE(horizontal)) {
    # reverse asc and desc
    sort_method <- gsub("asc", "asc2", sort_method)
    sort_method <- gsub("desc", "asc", sort_method)
    sort_method <- gsub("asc2", "desc", sort_method)
  }
  
  # start the sorting
  numeric_sort <- any(grepl("[0-9]", original_values), na.rm = TRUE)
  if (sort_method %in% c("alpha", "alpha-asc", "asc")) {
    # alphabetical, or ascending
    df <- factor(original_values,
                 levels = str_sort(unique(original_values),
                                   numeric = numeric_sort))
  } else if (sort_method %in% c("alpha-desc", "desc")) {
    df <- factor(original_values,
                 levels = str_sort(unique(original_values),
                                   numeric = numeric_sort,
                                   decreasing = TRUE))
  } else if (sort_method %in% c("false", "order", "inorder")) {
    df <- fct_inorder(as.character(original_values))
  } else if (sort_method %in% c("freq-asc", "infreq-asc")) {
    df <- fct_reorder(.f = as.character(original_values),
                      .x = datapoints,
                      .fun = summarise_function,
                      .desc = FALSE)
  } else if (sort_method %in% c("freq-desc", "infreq-desc")) {
    df <- fct_reorder(.f = as.character(original_values),
                      .x = datapoints,
                      .fun = summarise_function,
                      .desc = TRUE)
  } else {
    stop("invalid sorting option: '", sort_method.bak, "'")
  }
  
  df
}

#' @importFrom forcats fct_relevel
#' @importFrom dplyr `%>%` group_by across group_size
set_max_items <- function(df, y,
                          x, x.max_items, x.max_txt,
                          category, category.max_items, category.max_txt,
                          facet, facet.max_items, facet.max_txt,
                          horizontal, type) {
  
  if (is.infinite(x.max_items) && is.infinite(category.max_items) && is.infinite(facet.max_items)) {
    return(df)
  }
  if (is.null(x) && is.null(category) && is.null(facet)) {
    return(df)
  }
  
  # helper function
  set_max <- function(values, n_max, txt, horizontal) {
    if (is.null(n_max) || is.infinite(n_max)) {
      return(values)
    }
    if (!is.factor(values)) {
      plot_message("Maximising 'x', 'category' or 'facet' only works when they are sorted, or a factor")
      return(values)
    }
    if (n_max < length(levels(values))) {
      if (horizontal == TRUE) {
        lvls_remove <- rev(levels(values))[c(n_max:length(levels(values)))]
      } else {
        lvls_remove <- levels(values)[c(n_max:length(levels(values)))]
      }
      lvls_remove <- lvls_remove[order(-nchar(lvls_remove))]
      value_new <- gsub("%n", length(lvls_remove), txt, fixed = TRUE)
      # add new factor level
      levels(values) <- c(levels(values), value_new)
      # replace all values that must be removed
      values[as.character(values) %in% lvls_remove] <- value_new
      # drop unused factor levels
      values <- droplevels(values)
      # set new level to last place, taking into account 'horizontal'
      if (horizontal == TRUE) {
        values <- fct_relevel(values, value_new, after = 0)
      } else {
        values <- fct_relevel(values, value_new, after = Inf)
      }
    }
    values
  }
  
  # set new factor levels
  if (!is.null(x)) {
    df[, x] <- set_max(values = get_x(df),
                       n_max = x.max_items,
                       txt = x.max_txt,
                       horizontal = horizontal)
    df$`_var_x` <- df[, x, drop = TRUE]
  }
  if (!is.null(category)) {
    df[, category] <- set_max(values = get_category(df),
                              n_max = category.max_items,
                              txt = category.max_txt,
                              horizontal = horizontal)
    df$`_var_category` <- df[, category, drop = TRUE]
  }
  if (!is.null(facet)) {
    df[, facet] <- set_max(values = get_facet(df),
                           n_max = facet.max_items,
                           txt = facet.max_txt,
                           horizontal = FALSE)
    df$`_var_facet` <- df[, facet, drop = TRUE]
  }
  
  sizes <- df %>% 
    group_by(across(c(get_x_name(df), get_category_name(df), get_facet_name(df)))) %>%
    group_size()
  if (all(sizes == 1)) {
    # summarise again
    df <- df %>%
      mutate(n = get_y(df)) %>%
      group_by(across(c(get_x_name(df), get_category_name(df), get_facet_name(df)))) %>%
      summarise(n = sum(n, na.rm = TRUE))
    colnames(df)[colnames(df) == "n"] <- get_y_name(df)
  }
  df
  
}
