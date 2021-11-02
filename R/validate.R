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
  
  if (misses_category && !has_category(df) && ncol(df) > 2) {
    # category must only be used if factor or character
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

  # create surrogate labels to data
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
  
  df
}

#' @importFrom ggplot2 scale_x_discrete expansion scale_x_date scale_x_datetime waiver
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
  
  breaks_fn <- function(data, waiver,
                        y.breaks = NULL, y.expand = 0.25, stackedpercent = FALSE,
                        y.age = FALSE, y.percent = FALSE, y.percent.break = 10, y.24h = FALSE, y.limits = NULL,
                        ...) {
    data_min <- min(0, data) * -(1 + y.expand)
    data_max <- max(data) * (1 + y.expand)
    
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
  
  labels_fn <- function(data, waiver,
                        y.labels,
                        y.age, y.percent, y.24h, stackedpercent,
                        decimal.mark, big.mark, ...) {
    if (!is.null(y.labels)) {
      y.labels
    } else if (isTRUE(y.24h)) {
      function(x, dec = decimal.mark, big = big.mark, ...) paste0(format2(x, decimal.mark = dec, big.mark = big), "u (", x / 24, "d)")
    } else if (isTRUE(y.age)) {
      function(x, dec = decimal.mark, big = big.mark, ...) paste0(format2(x, decimal.mark = dec, big.mark = big, round = 0), " jr")
    } else if (isTRUE(y.percent) | isTRUE(stackedpercent)) {
      function(x, dec = decimal.mark, big = big.mark, ...) format2(as.percentage(x), decimal.mark = dec, big.mark = big)
    } else {
      function(x, dec = decimal.mark, big = big.mark, ...) format2(x, decimal.mark = dec, big.mark = big)
    }
  }
  
  limits_fn <- function(data, y.limits,
                        y.expand, facet.fixed_y, y.age,
                        ...) {
    if (!is.null(y.limits)) {
      y.limits
    } else if (isTRUE(y.age)) {
      # geen functie dus, maar vector forceren
      c(0, max(data) * (1 + y.expand))
    } else if (isTRUE(facet.fixed_y)) {
      # geen functie dus, maar vector forceren
      c(NA, max(data) * (1 + y.expand))
    } else {
      function(x, y_expand = y.expand, ...) c(min(0, x), max(x))
    }
  }
  
  expand_fn <- function(data, y.expand, y.age, stackedpercent, ...) {
    if (is.function(y.expand)) {
      y.expand
    } else if (isTRUE(y.age) | isTRUE(stackedpercent)) {
      expansion(mult = c(0, 0))
    } else {
      # ingestelde percentage toevoegen aan bovenkant bij positieve waarden en aan onderkant bij negatieve waarden
      expansion(mult = c(ifelse(any(data < 0), y.expand, 0),
                         ifelse(any(data > 0), y.expand, 0)))
    }
  }
  
  scale_y_continuous(
    breaks = breaks_fn(data = get_y(df),
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
    labels = labels_fn(data = get_y(df),
                       waiver = waiver(),
                       y.labels,
                       y.percent = y.percent,
                       y.age = y.age,
                       y.24h = y.24h,
                       stackedpercent = stackedpercent,
                       decimal.mark = decimal.mark,
                       big.mark = big.mark,
                       ...),
    limits = limits_fn(data = get_y(df),
                       y.limits,
                       y.expand = y.expand,
                       facet.fixed_y = facet.fixed_y,
                       y.age = y.age,
                       ...),
    expand = expand_fn(data = get_y(df),
                       y.expand = y.expand,
                       y.age = y.age,
                       stackedpercent = stackedpercent,
                       ...),
    trans = y.trans,
    position = y.position
  )
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
    out <- factor(original_values,
                  levels = str_sort(unique(original_values),
                                             numeric = numeric_sort))
  } else if (sort_method %in% c("alpha-desc", "desc")) {
    out <- factor(original_values,
                  levels = str_sort(unique(original_values),
                                             numeric = numeric_sort,
                                             decreasing = TRUE))
  } else if (sort_method %in% c("false", "order", "inorder")) {
    out <- fct_inorder(as.character(original_values))
  } else if (sort_method %in% c("freq-asc", "infreq-asc")) {
    out <- fct_reorder(.f = as.character(original_values),
                       .x = datapoints,
                       .fun = summarise_function,
                       .desc = FALSE)
  } else if (sort_method %in% c("freq-desc", "infreq-desc")) {
    out <- fct_reorder(.f = as.character(original_values),
                       .x = datapoints,
                       .fun = summarise_function,
                       .desc = TRUE)
  } else {
    stop("invalid sorting option: '", sort_method.bak, "'")
  }
  
  out
}

set_max_items <- function(data, y,
                          x, x.max, x.max.txt,
                          category, category.max, category.max.txt,
                          facet, facet.max, facet.max.txt,
                          horizontal, type) {
  
  if (is.infinite(x.max) && is.infinite(category.max) && is.infinite(facet.max)) {
    return(data)
  }
  if (is.null(x) && is.null(category) && is.null(facet)) {
    return(data)
  }
  
  out <- data
  
  # helper function
  set_max <- function(values, n_max, txt, horizontal) {
    if (is.null(n_max) || is.infinite(n_max)) {
      return(values)
    }
    if (!is.factor(values)) {
      warning("Maximising x, category or facet only works when they are sorted, or a factor", call. = FALSE)
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
    out[, x] <- set_max(values = out %>% pull(x),
                        n_max = x.max,
                        txt = x.max.txt,
                        horizontal = horizontal)
  }
  if (!is.null(category)) {
    out[, category] <- set_max(values = out %>% pull(category),
                               n_max = category.max,
                               txt = category.max.txt,
                               horizontal = horizontal)
  }
  if (!is.null(facet)) {
    out[, facet] <- set_max(values = out %>% pull(facet),
                            n_max = facet.max,
                            txt = facet.max.txt,
                            horizontal = FALSE)
  }
  
  if (!type %in% c("boxplot", "jitter", "violin")) {
    # summarise again
    out <- out %>%
      mutate(n = out %>% pull(y)) %>%
      group_by(across(c(x, category, facet))) %>% # if one is NULL, no problem for across()!
      summarise(n = sum(n, na.rm = TRUE))
    colnames(out)[colnames(out) == "n"] <- y
  }
  out
}
