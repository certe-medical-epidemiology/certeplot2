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

#' @importFrom certestyle font_blue font_black  
validate_type <- function(type, df = NULL) {
  type.bak <- type
  type_unset <- (is.null(type) || identical(type, ""))
  if (type_unset && !is.null(df)) {
    if (!has_x(df)) {
      # only numeric values, make it a boxplot
      type <- "geom_boxplot"
      plot2_message("Using ", font_blue("type = \"", gsub("geom_", "", type), "\"", collapse = NULL),
                    font_black(" since there is no x axis"))
    } else if (has_x(df) && is.numeric(get_x(df))) {
      if (identical(get_x(df), get_y(df))) {
        # both x and y are numeric - if they are equal then use histogram
        # see plot_message below 'since the variable ... is the only numeric variable'
        type <- "geom_histogram"
      } else {
        # make it points if x and y are both numeric
        type <- "geom_point"
        plot2_message("Using ", font_blue("type = \"", gsub("geom_", "", type), "\"", collapse = NULL), 
                      font_black(" since both axes are numeric"))
      }
    } else {
      # check if y has multiple values across groups, then make it boxplot
      if (all(group_sizes(df) >= 3)) {
        type <- "geom_boxplot"
        plot2_message("Using ", font_blue("type = \"", gsub("geom_", "", type), "\"", collapse = NULL),
                      font_black(" since all groups have size >= 3"))
      } else {
        # otherwise: column
        type <- "geom_col"
        plot2_message("Using ", font_blue("type = \"", gsub("geom_", "", type), "\"", collapse = NULL), 
                      font_black(" as default"))
      }
    }
  } else if (type_unset && is.null(df)) {
    return("") # for quick validation
  } else {
    if (length(type) > 1) {
      plot2_warning(font_blue("type"), " can only be of length 1")
    }
    type <- trimws(tolower(type[1L]))
    type <- gsub(".*::", "", type) # for `type = "ggplot2::geom_col()"`
    type <- gsub("[^a-z0-9_]", "", type)
    if (type == "a") type <- "area"
    if (type == "b") type <- "boxplot"
    if (type == "c") type <- "column"
    if (type == "h") type <- "histogram"
    if (type == "j") type <- "jitter"
    if (type == "l") type <- "line"
    if (type == "p") type <- "point"
    if (type == "r") type <- "ribbon"
    if (type == "v") type <- "violin"
    if (type == "column") {
      type <- "col"
    }
    if (type %unlike% "^geom_") {
      type <- paste0("geom_", type)
    }
    # replace 'points' etc. with 'point' etc.
    type <- gsub("s$", "", type)
  }
  
  valid_geoms <- ls(pattern = "^geom_", envir = asNamespace("ggplot2"))
  if (!type %in% valid_geoms) {
    if (any(valid_geoms %like% type)) {
      type <- valid_geoms[valid_geoms %like% type][1L]
    } else {
      stop("plot type \"", type.bak, "\" is invalid, since ggplot2::", type, "() does not exist", call. = FALSE)
    }
  }
  type
}

validate_legend.position <- function(legend.position) {
  if (is.null(legend.position) || is.na(legend.position[1L])) {
    legend.position <- "none"
  }
  legend.position <- trimws(tolower(legend.position[1L]))
  legend.position <- gsub("^t$", "top", legend.position)
  legend.position <- gsub("^r$", "right", legend.position)
  legend.position <- gsub("^b$", "bottom", legend.position)
  legend.position <- gsub("^l$", "left", legend.position)
  
  if (!legend.position %in% c("top", "right", "bottom", "left", "none")) {
    stop("`legend.position` must be 'top', 'right', 'bottom', 'left' or 'none'", call. = FALSE)
  }
  legend.position
}

#' @importFrom dplyr `%>%` select pull mutate arrange across
#' @importFrom certestyle font_bold font_blue font_red
validate_data <- function(df,
                          misses_x,
                          misses_category,
                          ...) {
  dots <- list(...)
  type <- validate_type(dots$type, df = NULL) # quick validation
  
  numeric_cols <- names(which(vapply(FUN.VALUE = logical(1), df, function(col) mode(col) == "numeric" & !inherits(col, c("Date", "POSIXTt", "factor")))))
  numeric_cols <- numeric_cols[numeric_cols %unlike% "^_var_"]
  character_cols <- names(which(vapply(FUN.VALUE = logical(1), df, function(col) is.character(col) | is.factor(col))))
  character_cols <- character_cols[character_cols %unlike% "^_var_"]
  non_numeric_cols <- names(which(vapply(FUN.VALUE = logical(1), df, function(col) mode(col) != "numeric" | inherits(col, c("Date", "POSIXTt", "factor")))))
  non_numeric_cols <- non_numeric_cols[non_numeric_cols %unlike% "^_var_"]
  
  if (!has_y(df) && "n" %in% numeric_cols && mode(df$n) == "numeric") {
    # give preference to "n" for the y axis
    plot2_message("Using ", font_blue("y = n"))
    df <- df %>% 
      mutate(`_var_y` = df %>% pull(n))
  }
  
  if (!has_y(df)) {
    # try to find numeric column for y
    if (is.na(numeric_cols[1L])) {
      stop("no numeric column found to use for y", call. = FALSE)
    }
    if (length(numeric_cols) > 1) {
      # first check if there is also no x axis
      if (!has_x(df)) {
        # make x first numeric column and y second numeric column
        plot2_message("Using ", font_blue("x = ", numeric_cols[1L], collapse = NULL))
        if (!geom_is_continuous_x(type)) {
          # don't show when type for density geoms - y will not be used
          plot2_message("Using ", font_blue("y = ", numeric_cols[2L], collapse = NULL))
        }
        df <- df %>% 
          mutate(`_var_x` = df %>% pull(numeric_cols[1L]),
                 `_var_y` = df %>% pull(numeric_cols[2L]))
      } else {
        if (!geom_is_continuous_x(type)) {
          # don't show when type for density geoms - y will not be used
          plot2_message("Using ", font_blue("y = ", numeric_cols[1L], collapse = NULL))
        }
        df <- df %>% 
          mutate(`_var_y` = df %>% pull(numeric_cols[1L]))
      }
    } else {
      # only one numeric column
      if (geom_is_continuous_x(type)) {
        if (!has_x(df)) {
          plot2_message("Using ", font_blue("x = ", numeric_cols, collapse = NULL))
          df <- df %>% 
            mutate(`_var_x` = df %>% pull(numeric_cols))
        }
        # don't show when type for density geoms - y will not be used
        df <- df %>% 
          mutate(`_var_y` = df %>% pull(`_var_x`))
      } else if (!has_x(df) && type == "" && length(non_numeric_cols) == 0) {
        # has no x and no y, make it a histogram
        plot2_message("Using ", font_blue("x = ", numeric_cols, collapse = NULL))
        plot2_message("Assuming ", font_blue("type = \"histogram\""),
                      " since the data has only one numeric variable and no other variables")
        type <- "geom_histogram"
        df <- df %>% 
          mutate(`_var_x` = df %>% pull(numeric_cols),
                 `_var_y` = df %>% pull(numeric_cols))
      } else {
        if (has_x(df) && get_x_name(df) == numeric_cols) {
          if (type == "") {
            plot2_message("Assuming ", font_blue("type = \"histogram\""),
                          " since the ", font_blue("x"),
                          " variable (", font_blue(get_x_name(df)), ") is the only numeric variable")
            type <- "geom_histogram"
            df <- df %>% 
              mutate(`_var_y` = df %>% pull(`_var_x`))
          } else {
            stop("No variable found for y, since the x variable (", get_x_name(df),
                 ") is the only numeric variable in the data set.\nDid you mean type = \"histogram\"?", call. = FALSE)
          }
        } else {
          plot2_message("Using ", font_blue("y = ", numeric_cols, collapse = NULL))
          df <- df %>% 
            mutate(`_var_y` = df %>% pull(numeric_cols))
        }
      }
    }
  }
  
  # this is required to plot e.g. difftime
  # integers and doubles both return TRUE for is.numeric() 
  if (has_y(df) && requires_numeric_coercion(get_y(df))) {
    df <- df %>% 
      mutate(`_var_y` = as.double(`_var_y`))
  }
  if (has_x(df) && requires_numeric_coercion(get_x(df))) {
    df <- df %>% 
      mutate(`_var_x` = as.double(`_var_x`))
  }
  
  if (misses_x && !has_x(df) && ncol(df) > 1) {
    # take first column if it's not used for y
    if (identical(pull(df, 1), get_y(df))) {
      x_col <- colnames(df)[2L]
    } else {
      x_col <- colnames(df)[1L]
    }
    plot2_message("Using ", font_blue("x = ", x_col, collapse = NULL))
    df <- df %>% 
      mutate(`_var_x` = df %>% pull(x_col))
  }
  
  if (misses_x && misses_category && !has_category(df) && ncol(df) > 2 && type != "geom_sf") {
    # category must only be used if factor or character
    # and if x was also missing
    cols <- vapply(FUN.VALUE = logical(1),
                   df,
                   function(col) (is.factor(col) | is.character(col)) &
                     !identical(get_y(df), col) &
                     !(has_x(df) && identical(get_x(df), col)))
    cols <- names(cols)[cols]
    if (has_facet(df)) {
      # remove columns that are already used for facet
      cols <- cols[!cols %in% c("_var_facet", get_facet_name(df))]
    }
    if (length(cols) > 0) {
      plot2_message("Using ", font_blue("category = ", cols[1L], collapse = NULL))
      df <- df %>% 
        mutate(`_var_category` = df %>% pull(cols[1L]))
    }
  }
  if (type == "geom_sf" && misses_category && !has_category(df) && !is.na(numeric_cols[1L])) {
    # try to take the first numeric column for 'sf' plots
    plot2_message("Using ", font_blue("category = ", numeric_cols[1L], collapse = NULL))
    df <- df %>% 
      mutate(`_var_category` = df %>% pull(numeric_cols[1L]))
  }
  
  # if given FALSE for a direction (e.g., category = FALSE), remove these columns
  if (has_category(df) && all(get_category(df) == FALSE)) {
    df <- df %>% select(-`_var_category`)
  }
  if (has_facet(df) && all(get_facet(df) == FALSE)) {
    df <- df %>% select(-`_var_facet`)
  }
  if (has_datalabels(df) && 
      (all(get_datalabels(df) == FALSE) ||
       (!is.null(dots$type) && dots$type != "sf" &&
        geom_is_continuous(suppressMessages(validate_type(dots$type, df)))))) {
    # remove datalabels if `datalabels = FALSE`, or if the type now seems to be continuous
    df <- df %>% select(-`_var_datalabels`)
  }
  
  # add surrogate columns to df
  if (has_x(df) && !dots$label_x %in% colnames(df) && dots$label_x != "NULL") {
    df$`_label_x` <- get_x(df)
    colnames(df)[colnames(df) == "_label_x"] <- concat(dots$label_x)
  }
  if (has_y(df) && !dots$label_y %in% colnames(df) && dots$label_y != "NULL") {
    df$`_label_y` <- get_y(df)
    colnames(df)[colnames(df) == "_label_y"] <- concat(dots$label_y)
  }
  if (has_category(df) && !dots$label_category %in% colnames(df) && dots$label_category != "NULL") {
    df$`_label_category` <- get_category(df)
    colnames(df)[colnames(df) == "_label_category"] <- concat(dots$label_category)
  }
  if (has_facet(df) && !dots$label_facet %in% colnames(df) && dots$label_facet != "NULL") {
    df$`_label_facet` <- get_facet(df)
    colnames(df)[colnames(df) == "_label_facet"] <- concat(dots$label_facet)
  }
  
  if (has_datalabels(df)) {
    if (all(get_datalabels(df) == TRUE)) {
      # for when given: datalabels = TRUE, guess the results
      if (type == "geom_sf") {
        # take values from first character column in case of sf plots
        if (!is.na(character_cols[1L])) {
          plot2_message("Using ", font_blue("datalabels = ", character_cols[1L], collapse = NULL))
          df <- df %>% mutate(`_var_datalabels` = df %>% pull(character_cols[1L]))
        } else {
          plot2_warning("No suitable column found for ", font_blue("datalabels = TRUE"))
          df <- df %>% select(-`_var_datalabels`)
        }
      } else {
        # otherwise take values from the y column
        df <- df %>% mutate(`_var_datalabels` = `_var_y`)
      }
    }
    # format datalabels
    if (requires_numeric_coercion(get_datalabels(df))) {
      # force double for e.g. difftime
      df <- df %>% 
        mutate(`_var_datalabels` = as.double(`_var_datalabels`))
    }
    df <- df %>%
      mutate(`_var_datalabels` = format_datalabels(`_var_datalabels`, 
                                                   datalabels.round = dots$datalabels.round,
                                                   datalabels.format = dots$datalabels.format,
                                                   decimal.mark = dots$decimal.mark,
                                                   big.mark = dots$big.mark))
  }
  
  if (is.null(dots$x.character) &&
      has_x(df) &&
      is.numeric(get_x(df)) &&
      all(get_x(df, na.rm = TRUE) >= 2000) &&
      all(get_x(df, na.rm = TRUE) <= 2050)) {
    plot2_message("Assuming ", font_blue("x.character = TRUE"),
                  " since the ", font_blue("x"), " labels seem to be years")
    dots$x.character <- TRUE
  } else if (has_x(df) && 
             is.numeric(get_x(df)) &&
             !type %in% c("", "geom_blank") &&
             !geom_is_continuous(type)) {
    plot2_message("Assuming ", font_blue("x.character = TRUE"),
                  " for discrete plot type (", font_blue(type), ")",
                  " since ", font_blue(get_x_name(df)), " is numeric")
    dots$x.character <- TRUE
  }
  if (isTRUE(dots$x.character)) {
    df <- df %>%
      mutate(`_var_x` = as.character(`_var_x`))
  }
  
  # remove or replace NAs
  is_numeric <- function(x) {
    mode(x) == "numeric" || is.numeric(x) || inherits(x, c("Date", "POSIXt"))
  }
  df_noNA <- df %>%
    filter(across(c(get_x_name(.), get_category_name(.), get_facet_name(.),
                    matches("_var_(x|category|facet)")),
                  function(x) {
                    if (is_numeric(x) & !is.factor(x)) {
                      TRUE
                    } else {
                      !is.na(x)
                    }}))
  if (nrow(df_noNA) < nrow(df)) {
    # so some are NAs
    if (isTRUE(dots$na.rm)) {
      plot2_message("Removed ", nrow(df) - nrow(df_noNA), " rows since ",
                    font_blue("na.rm = TRUE"))
      df <- df_noNA
    } else {
      # replace NAs
      plot2_env$na_replaced <- 0
      df <- df %>%
        mutate(across(c(get_x_name(.), get_category_name(.), get_facet_name(.),
                        matches("_var_(x|category|facet)")),
                      function(x) {
                        if (is.factor(x)) {
                          # add as last factor level
                          levels(x) <- c(levels(x), dots$na.replace)
                        }
                        if (!is_numeric(x) | is.factor(x)) {
                          plot2_env$na_replaced <- plot2_env$na_replaced + sum(is.na(x))
                          x[is.na(x)] <- dots$na.replace
                        }
                        x
                      }))
      if (plot2_env$na_replaced > 0) {
        plot2_message("Replacing ", font_red("NA"), " using ",
                      font_blue(paste0("na.replace = \"", dots$na.replace, "\"")))
      }
    }
  }
  
  # apply sortings
  if (has_x(df) && type != "geom_sf") {
    if (is.null(dots$x.sort) && inherits(get_x(df), c("character", "factor"))) {
      dots$x.sort <- TRUE
    }
    
    df <- df %>% 
      mutate(`_var_x` = sort_data(original_values = get_x(df),
                                  sort_method = dots$x.sort,
                                  datapoints = get_y(df),
                                  summarise_function = dots$summarise_function,
                                  summarise_fn_name = dots$summarise_fn_name,
                                  horizontal = dots$horizontal,
                                  drop = dots$x.drop)) %>%
      arrange(across(`_var_x`))
    df[, get_x_name(df)] <- df$`_var_x` # required to keep sorting after summarising
  }
  if (has_category(df)) {
    df <- df %>% 
      mutate(`_var_category` = sort_data(original_values = get_category(df),
                                         sort_method = dots$category.sort,
                                         datapoints = get_y(df),
                                         summarise_function = dots$summarise_function,
                                         summarise_fn_name = dots$summarise_fn_name,
                                         horizontal = dots$horizontal,
                                         drop = TRUE))
    df[, get_category_name(df)] <- df$`_var_category` # required to keep sorting after summarising
  }
  if (has_facet(df)) {
    df <- df %>% 
      mutate(`_var_facet` = sort_data(original_values = get_facet(df),
                                      sort_method = dots$facet.sort,
                                      datapoints = get_y(df),
                                      summarise_function = dots$summarise_function,
                                      summarise_fn_name = dots$summarise_fn_name,
                                      horizontal = FALSE, # never reversely sort when horizontal
                                      drop = TRUE))
    df[, get_facet_name(df)] <- df$`_var_facet` # required to keep sorting after summarising
  }
  
  if (type != "geom_sf") {
    # apply limitations (have to been after sorting, e.g. on frequency)
    df <- set_max_items(df = df,
                        y = get_y(df),
                        x = get_x_name(df),
                        x.max_items = dots$x.max_items,
                        x.max_txt = dots$x.max_txt,
                        category = get_category_name(df), 
                        category.max_items = dots$category.max_items,
                        category.max_txt = dots$category.max_txt,
                        facet = get_facet_name(df), 
                        facet.max_items = dots$facet.max_items, 
                        facet.max_txt = dots$facet.max_txt,
                        horizontal = dots$horizontal,
                        summarise_function = dots$summarise_function,
                        decimal.mark = dots$decimal.mark,
                        big.mark = dots$big.mark,
                        datalabels.round = dots$datalabels.round,
                        datalabels.format = dots$datalabels.format)
    # sort on x, important when piping plot2()'s after plot2()'s
    df <- df %>% 
      arrange(across(`_var_x`))
  }
  
  # output
  df
}

#' @importFrom ggplot2 scale_x_discrete scale_x_date scale_x_datetime scale_x_discrete scale_x_continuous expansion waiver
#' @importFrom scales reverse_trans
#' @importFrom cleaner format_datetime
#' @importFrom certestyle format2
validate_x_scale <- function(values,
                             x.date_breaks,
                             x.date_labels,
                             x.breaks,
                             x.breaks_n,
                             x.expand,
                             x.limits,
                             x.position,
                             x.trans,
                             x.drop,
                             decimal.mark,
                             big.mark,
                             horizontal,
                             zoom) {
  
  if (isTRUE(zoom)) {
    x.limits <- c(NA_real_, NA_real_)
  }
  if (!is.null(x.limits)) {
    if (length(x.limits) != 2) {
      stop("`x.limits` must be of length 2", call. = FALSE)
    }
    if (inherits(values, "Date")) {
      x.limits <- as.Date(x.limits, origin = "1970-01-01")
    } else if (inherits(values, "POSIXt")) {
      x.limits <- as.POSIXct(x.limits, origin = "1970-01-01")
    }
    if (inherits(values, c("Date", "POSIXt"))) {
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
  if (inherits(values, c("Date", "POSIXt"))) {
    auto_breaks_labels <- determine_date_breaks_labels(values)
    if (is.null(x.date_breaks)) {
      x.date_breaks <- auto_breaks_labels$breaks
      plot2_message("Using ", font_blue("x.date_breaks = \"", x.date_breaks, "\"", collapse = ""),
                    " based on data")
    }
    if (is.null(x.date_labels)) {
      x.date_labels <- auto_breaks_labels$labels
      plot2_message("Using ", font_blue("x.date_labels = \"", x.date_labels, "\"", collapse = ""),
                    " based on data")
    }
  }
  if (inherits(values, "Date")) {
    scale_x_date(position = x.position,
                 date_breaks = x.date_breaks,
                 date_labels = format_datetime(x.date_labels),
                 expand = x.expand,
                 limits = x.limits)
  } else if (inherits(values, "POSIXt")) {
    scale_x_datetime(position = x.position,
                     date_breaks = x.date_breaks,
                     date_labels = format_datetime(x.date_labels),
                     expand = x.expand,
                     limits = x.limits)
  } else {
    if (!is.numeric(values)) {
      scale_x_discrete(position = x.position, drop = x.drop)
    } else {
      if (x.trans == "identity" && isTRUE(horizontal)) {
        x.trans <- reverse_trans()
      }
      if (is.null(x.limits)) {
        x.limits <- c(ifelse(min(values) < 0, NA_real_, 0), NA)
      }
      scale_x_continuous(labels = function(x, ...) format2(x, decimal.mark = decimal.mark, big.mark = big.mark),
                         breaks = if (!is.null(x.breaks)) x.breaks else waiver(),
                         n.breaks = x.breaks_n,
                         trans = x.trans,
                         position = x.position,
                         limits = x.limits,
                         expand = expansion(mult = c(0.05, 0.05)))
    }
  }
}

#' @importFrom ggplot2 waiver expansion scale_y_continuous
#' @importFrom cleaner as.percentage
#' @importFrom scales pretty_breaks
#' @importFrom certestyle format2
validate_y_scale <- function(values,
                             y.24h,
                             y.age,
                             y.breaks,
                             y.expand,
                             y.labels,
                             y.limits,
                             y.percent,
                             y.percent_break,
                             y.position,
                             y.trans,
                             stackedpercent,
                             facet.fixed_y,
                             decimal.mark,
                             big.mark,
                             zoom,
                             ...) {
  
  breaks_fn <- function(values, waiver,
                        y.breaks = NULL, y.expand = 0.25, stackedpercent = FALSE,
                        y.age = FALSE, y.percent = FALSE, y.percent_break = 10, y.24h = FALSE, y.limits = NULL,
                        ...) {
    data_min <- min(0, values) * -(1 + y.expand)
    data_max <- max(values)
    if (!inherits(values, c("Date", "POSIXt"))) {
      data_max <- data_max * (1 + y.expand)
    }
    
    if (y.percent_break >= 1) {
      # for `y.percent_break = 25`, probably `y.percent_break = 0.25` was meant
      y.percent_break <- y.percent_break / 100
      plot2_message("Assuming ", font_blue("y.percent_break = ", y.percent_break, collapse = ""))
    }
    
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
    } else if (isTRUE(stackedpercent)) {
      # special case of y.percent, where the y scale is always 0 to 1
      function(x, y_percent_break = y.percent_break, ...) {
        seq(from = min(0, x),
            to = max(x),
            by = y_percent_break)
      }
    } else if (isTRUE(y.percent)) {
      # calculate how many labels will be printed, keep around 10
      if (is.null(y.limits)) {
        y.limits <- c(data_min, data_max)
      }
      labels_n <- (max(y.limits) - min(y.limits)) / y.percent_break
      if (is.na(labels_n)) {
        labels_n <- 10
      }
      if (as.integer(labels_n) > 10) {
        plot2_warning("Printing at most 10 labels for ", font_blue("y"), ", set with ", font_blue("y.percent_break"))
        y.percent_break <- round((max(y.limits) - min(y.limits)) / 10, 2)
      }
      function(x, y_percent_break = y.percent_break, ...) {
        if (y_percent_break >= max(x)) {
          y_percent_break <- max(x) / 10
        }
        seq(from = min(0, x),
            to = max(x),
            by = y_percent_break)
      }
    } else if (isTRUE(stackedpercent)) {
      seq(from = 0,
          to = 1,
          by = 0.1)
    } else if (all(values %% 1 == 0) && data_max < 5) {
      # whole numbers - only strip decimal numbers if total y range is low
      function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 3))))
    } else {
      pretty_breaks()
    }
  }
  
  labels_fn <- function(values, waiver,
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
  
  limits_fn <- function(values, y.limits,
                        y.expand, facet.fixed_y, y.age,
                        ...) {
    if (!is.null(y.limits)) {
      y.limits
    } else if (isTRUE(y.age)) {
      # so no function, but force a vector
      c(0, max(values) * (1 + y.expand))
    } else if (isTRUE(facet.fixed_y)) {
      # so no function, but force a vector
      c(NA, max(values) * (1 + y.expand))
    } else {
      function(x, y_expand = y.expand, ...) c(min(0, x), max(x))
    }
  }
  
  expand_fn <- function(values, y.expand, y.age, stackedpercent, ...) {
    if (is.function(y.expand)) {
      y.expand
    } else if (isTRUE(y.age) | isTRUE(stackedpercent)) {
      expansion(mult = c(0, 0))
    } else {
      # ingestelde percentage toevoegen aan bovenkant bij positieve waarden en aan onderkant bij negatieve waarden
      expansion(mult = c(ifelse(any(values < 0), y.expand, 0),
                         ifelse(any(values > 0), y.expand, 0)))
    }
  }
  
  if (isTRUE(zoom)) {
    y.limits <- c(NA_real_, NA_real_)
  }
  
  scale_y_continuous(
    breaks = breaks_fn(values = values,
                       waiver = waiver(),
                       y.breaks = y.breaks,
                       y.expand = y.expand,
                       stackedpercent = stackedpercent,
                       y.age = y.age,
                       y.percent = y.percent,
                       y.percent_break = y.percent_break,
                       y.24h = y.24h,
                       y.limits = y.limits,
                       ...),
    labels = labels_fn(values = values,
                       waiver = waiver(),
                       y.labels,
                       y.percent = y.percent,
                       y.age = y.age,
                       y.24h = y.24h,
                       stackedpercent = stackedpercent,
                       decimal.mark = decimal.mark,
                       big.mark = big.mark,
                       ...),
    limits = limits_fn(values = values,
                       y.limits,
                       y.expand = y.expand,
                       facet.fixed_y = facet.fixed_y,
                       y.age = y.age,
                       ...),
    expand = expand_fn(values = values,
                       y.expand = y.expand,
                       y.age = y.age,
                       stackedpercent = stackedpercent,
                       ...),
    trans = y.trans,
    position = y.position
  )
}

#' @importFrom ggplot2 scale_colour_gradient2 scale_colour_gradient scale_colour_gradientn expansion guide_colourbar element_text
#' @importFrom certestyle format2
#' @importFrom cleaner as.percentage
validate_category_scale <- function(values,
                                    type,
                                    cols,
                                    category.labels,
                                    category.percent,
                                    category.breaks,
                                    category.limits,
                                    category.expand,
                                    category.midpoint,
                                    category.trans,
                                    stackedpercent,
                                    legend.nbin,
                                    legend.barheight,
                                    legend.barwidth,
                                    legend.reverse,
                                    legend.position,
                                    decimal.mark,
                                    big.mark,
                                    family,
                                    ...) {
  # only for a numeric category scale
  
  if (is.null(legend.position)) {
    legend.position <- "right"
  } else {
    legend.position <- validate_legend.position(legend.position)
  }
  
  labels_fn <- function(values, category.labels, category.percent, stackedpercent, decimal.mark, big.mark, ...) {
    if (!is.null(category.labels)) {
      category.labels
    } else if (isTRUE(category.percent) | isTRUE(stackedpercent)) {
      function(x, dec = decimal.mark, big = big.mark, ...) format2(as.percentage(x), decimal.mark = dec, big.mark = big)
    } else {
      function(x, dec = decimal.mark, big = big.mark, ...) format2(x, decimal.mark = dec, big.mark = big)
    }
  }
  breaks_fn <- function(category.breaks = NULL, category.percent = FALSE, waiver = NULL, ...) {
    if (!is.null(category.breaks)) {
      category.breaks
    } else if (isTRUE(category.percent)) {
      if (max(c(1, values)) == 1) {
        seq(0, 1, 0.25)
      } else {
        # print 5 labels nicely
        scales::pretty_breaks()(values, 5)
      }
    } else {
      scales::pretty_breaks()
    }
  }
  limits_fn <- function(category.limits, category.percent = FALSE, ...) {
    if (!is.null(category.limits)) {
      category.limits
    } else if (isTRUE(category.percent)) {
      function(x, ...) c(min(0, x), max(1, x))
    } else {
      function(x, ...) c(min(x), max(x))
    }
  }
  if (is.numeric(category.expand)) {
    category.expand <- expansion(mult = c(0, category.expand))
  }
  
  if (geom_has_only_colour(type) || identical(cols$colour, cols$colour_fill)) {
    aest <- c("colour", "fill")
    cols_category <- cols$colour
  } else {
    aest <- "fill"
    cols_category <- cols$colour_fill
  }
  
  # general arguments for any scale function below (they are called with do.call())
  args <- list(aesthetics = aest,
               na.value = "white",
               guide = guide_colourbar(ticks = FALSE,
                                       draw.ulim = TRUE,
                                       draw.llim = TRUE,
                                       reverse = isTRUE(legend.reverse),
                                       nbin = legend.nbin,
                                       barheight = ifelse(legend.position %in% c("top", "bottom"),
                                                          legend.barwidth,
                                                          legend.barheight),
                                       barwidth = ifelse(legend.position %in% c("top", "bottom"),
                                                         legend.barheight,
                                                         legend.barwidth)),
               labels = labels_fn(values = values,
                                  category.labels = category.labels,
                                  category.percent = category.percent,
                                  stackedpercent = stackedpercent,
                                  decimal.mark = decimal.mark,
                                  big.mark = big.mark,
                                  ...),
               breaks = breaks_fn(category.breaks = category.breaks,
                                  category.percent = category.percent,
                                  waiver = waiver(),
                                  ...),
               limits = limits_fn(category.limits = category.limits,
                                  category.percent = category.percent,
                                  ...),
               trans = category.trans)
  
  if (length(cols_category) == 1) {
    # 1 colour, start with white
    do.call(scale_colour_gradient,
            args = c(list(low = "white",
                          high =  cols_category),
                     args))
    
  } else if (length(cols_category) == 2) {
    # 2 colours, low and high
    do.call(scale_colour_gradient,
            args = c(list(low = cols_category[1],
                          high = cols_category[2]),
                     args))
    
  } else if (length(cols_category) == 3) {
    # 3 colours, so low, mid (set as vector name) and high
    if (!is.null(category.midpoint)) {
      mid_point <- as.double(category.midpoint)
    } else {
      # default to the middle of the set limits
      if (is.function(args$limits)) {
        rng <- args$limits(values)
      } else {
        rng <- args$limits
      }
      mid_point <- sum(rng) / 2
      plot2_message("Using ", font_blue("category.midpoint =", round(mid_point, 2)),
                    ifelse(isTRUE(category.percent),
                           paste0(" (", format2(as.percentage(mid_point)), ", "),
                           " ("),
                    "the current ", font_blue("category"), " scale centre)")
    }
    do.call(scale_colour_gradient2,
            args = c(list(low = cols_category[1],
                          mid = cols_category[2],
                          high = cols_category[3],
                          midpoint = mid_point),
                     args))
    
  } else {
    # more than 3 colours, create own divergent scale
    # this can also be because one of the viridis colours was set with `colour` and/or `colour_fill`
    do.call(scale_colour_gradientn,
            args = c(list(colours = cols_category),
                     args))
  }
}

#' @importFrom ggplot2 position_stack position_fill position_dodge2 position_jitter
#' @importFrom certestyle font_blue font_black
generate_geom <- function(type,
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
                          jitter_seed,
                          binwidth,
                          cols) {
  
  if (type == "geom_col") {
    type <- "geom_bar"
  }
  geom_fn <- getExportedValue(name = type, ns = asNamespace("ggplot2"))
  
  # set position
  if (isTRUE(stacked)) {
    position <- position_stack(reverse = reverse)
  } else if (isTRUE(stackedpercent)) {
    position <- position_fill(reverse = reverse)
  } else {
    # small whitespace between columns:
    position <- position_dodge2(width = width * 1.05, preserve = "single")
  }
  
  # set geoms - do.call() applies all arguments to the geom_fn function
  if (type == "geom_bar") {
    do.call(geom_fn,
            args = c(list(width = width,
                          stat = "identity",
                          position = position,
                          na.rm = na.rm),
                     list(colour = cols$colour)[!has_category(df)],
                     list(fill = cols$colour_fill)[!has_category(df)]))
    
  } else if (type == "geom_area") {
    do.call(geom_fn,
            args = c(list(linetype = linetype,
                          stat = "identity",
                          position = position,
                          size = size,
                          na.rm = na.rm),
                     list(colour = cols$colour)[!has_category(df)],
                     list(fill = cols$colour_fill)[!has_category(df)]))
    
  } else if (type %in% c("geom_line", "geom_path")) {
    do.call(geom_fn,
            args = c(list(size = size,
                          lineend = "round",
                          linetype = linetype,
                          na.rm = na.rm),
                     list(colour = cols$colour)[!has_category(df)]))
    
  } else if (type == "geom_point") {
    do.call(geom_fn,
            args = c(list(size = size,
                          na.rm = na.rm),
                     list(colour = cols$colour)[!has_category(df)]))
    
  } else if (type == "geom_jitter") {
    do.call(geom_fn,
            args = c(list(size = size,
                          position = position_jitter(seed = jitter_seed),
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
    
  } else if (type == "geom_histogram") {
    if (is.null(binwidth)) {
      # this will be the default binwidth: the difference in the range, divided by 12 to 22.
      values <- get_x(df)
      values <- values[!is.infinite(values)]
      binwidth <- as.double(diff(range(values, na.rm = TRUE))) / (12 + min(10, length(unique(values)) / 20))
      if (binwidth < 0) {
        binwidth <- round(binwidth, 3)
      } else if (binwidth > 10) {
        binwidth <- round(binwidth, 0)
      } else {
        binwidth <- round(binwidth, 1)
      }
      plot2_message("Using ", font_blue("binwidth =", binwidth), " based on data")
    }
    do.call(geom_fn,
            args = c(list(size = size,
                          binwidth = binwidth,
                          na.rm = na.rm),
                     list(colour = cols$colour)[!has_category(df)],
                     list(fill = cols$colour_fill)[!has_category(df)]))
    
  } else if (type == "geom_density") {
    do.call(geom_fn,
            args = c(list(linetype = linetype,
                          size = size,
                          na.rm = na.rm),
                     list(colour = cols$colour)[!has_category(df)],
                     list(fill = cols$colour_fill)[!has_category(df)]))
    
  } else if (type == "geom_sf") {
    do.call(geom_fn,
            args = c(list(linetype = linetype,
                          size = size,
                          na.rm = na.rm),
                     list(colour = cols$colour)[length(cols$colour) == 1],
                     list(fill = cols$colour_fill)[!has_category(df)]))
    
  } else if (type == "geom_blank") {
    do.call(geom_fn,
            args = list(na.rm = na.rm))
    
  } else {
    # try to put some arguments into the requested geom
    plot2_warning(font_blue("type = \"", type, "\"", collapse = ""), " is currently only loosely supported")
    do.call(geom_fn,
            args = c(list(width = width,
                          size = size,
                          na.rm = na.rm),
                     list(colour = cols$colour)[!has_category(df)],
                     list(fill = cols$colour_fill)[!has_category(df)]))
  }
}

#' @importFrom certestyle colourpicker add_white
validate_colour <- function(df,
                            type,
                            colour,
                            colour_fill,
                            colour_opacity,
                            misses_colour_fill,
                            horizontal) {
  
  if (geom_is_continuous(type) && is.numeric(get_category(df))) {
    viridis_colours <- c("viridis", "magma", "inferno", "plasma", "cividis", "rocket", "mako", "turbo")
    colour.bak <- colour
    # this is for validate_category_scale()
    if (length(colour) == 1) {
      if (colour == "certe") {
        # divergent Certe scale
        colour <- colourpicker(c("certeblauw0", "certegroen", "certegeel", "certeroze"),
                               opacity = colour_opacity)
      } else if (colour %like% "certe([1-6]+)$") {
        # take Certe colours
        colour <- colourpicker(colour, 4, opacity = colour_opacity)
      } else if (colour %in% viridis_colours) {
        # generate viridis colour
        colour <- colourpicker(colour, 5, opacity = colour_opacity)
      } else {
        colour <- colourpicker(colour, opacity = colour_opacity)
      }
    } else {
      colour <- colourpicker(colour, opacity = colour_opacity)
    }

    if (is.null(colour_fill) || identical(colour.bak, colour_fill)) {
      colour_fill <- colour
    } else {
      if (length(colour_fill) == 1) {
        if (colour_fill == "certe") {
          # divergent Certe scale
          colour_fill <- colourpicker(c("certeblauw0", "certegroen", "certegeel", "certeroze"),
                                      opacity = colour_opacity)
        } else if (colour_fill %like% "certe([1-6]+)$") {
          # take Certe colours
          colour_fill <- colourpicker(colour_fill, 4, opacity = colour_opacity)
        } else if (colour_fill %in% viridis_colours) {
          # generate viridis colour
          colour_fill <- colourpicker(colour_fill, 5, opacity = colour_opacity)
        } else {
          colour_fill <- colourpicker(colour_fill, opacity = colour_opacity)
        }
      } else {
        colour_fill <- colourpicker(colour_fill, opacity = colour_opacity)
      }
    }
    return(list(colour = colour,
                colour_fill = colour_fill))
  }
  
  if (geom_is_continuous(type) && is.null(colour_fill) && any(colour %like% "certe")) {
    # exception for Certe: "certeblauw" (colour) -> "certeblauw6" (colour_fill)
    colour_fill <- colourpicker(colour, opacity = colour_opacity)
    if (type == "geom_sf") {
      colour_fill[colour %like% "certe[a-z]*"] <- paste0(colour[colour %like% "certe[a-z]*"], "3")
    } else {
      colour_fill[colour %like% "certe[a-z]*"] <- paste0(colour[colour %like% "certe[a-z]*"], "6")
    }
  }
  if (isTRUE(misses_colour_fill) && is.null(colour_fill) && !geom_is_continuous(type)) {
    colour_fill <- colour
  }
  
  if (!has_category(df)) {
    # has no category
    if (has_x(df) && length(unique(get_x(df))) != length(colour)) {
      # take only the first
      colour <- colour[1]
      colour_fill <- colour_fill[1]
    }
    colour <- colourpicker(colour, opacity = colour_opacity)
    if (geom_is_continuous(type) && is.null(colour_fill)) {
      # specific treatment for continuous geoms (such as boxplots/violins/histograms/...)
      colour_fill <- add_white(colour, white = 0.35)
    } else {
      colour_fill <- colourpicker(colour_fill, opacity = colour_opacity)
    }
    
  } else {
    # has also category, and it's not numeric
    n_unique <- length(unique(get_category(df)))
    colour <- colourpicker(colour,
                           length = ifelse(length(colour) == 1, n_unique, 1),
                           opacity = colour_opacity)
    if (geom_is_continuous(type) && is.null(colour_fill)) {
      # specific treatment for continuous geoms (such as boxplots/violins/histograms/...)
      colour_fill <- add_white(colour, white = 0.35)
    } else {
      colour_fill <- colourpicker(colour_fill,
                                  length = ifelse(length(colour_fill) == 1, n_unique, 1),
                                  opacity = colour_opacity)
    }
    
    if (isTRUE(horizontal)) {
      colour <- rev(colour)
      colour_fill <- rev(colour_fill)
    }
    
    if (length(colour) > 1 && length(colour_fill) == 1) {
      colour_fill <- colour
    }
    
    minimum_length <- length(group_sizes(df))
    if (length(colour) < minimum_length) {
      colour <- c(colour, rep(colour, minimum_length)[seq_len(minimum_length - length(colour))])
    }
    if (length(colour_fill) < minimum_length) {
      colour_fill <- c(colour_fill, rep(colour_fill, minimum_length)[seq_len(minimum_length - length(colour_fill))])
    }
  }
  
  if (type == "geom_sf" && !has_category(df)) {
    colour_fill <- colour_fill[1]
  }
  
  list(colour = colour,
       colour_fill = colour_fill)
}

validate_size <- function(size, type) {
  if (is.null(size)) {
    if (type %in% c("geom_boxplot", "geom_violin", "geom_area", "geom_ribbon") | geom_is_continuous_x(type)) {
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
        text <- gsub(" ^ ", "^", text, fixed = TRUE)
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

#' @importFrom ggplot2 theme_grey element_blank margin
#' @importFrom ggtext element_markdown
#' @importFrom certestyle colourpicker
validate_theme <- function(theme,
                           type,
                           markdown,
                           text_factor,
                           family,
                           horizontal,
                           x.remove,
                           x.lbl_angle,
                           x.lbl_align,
                           x.lbl_italic,
                           facet.fill,
                           facet.bold,
                           facet.italic,
                           facet.size,
                           facet.margin,
                           legend.italic,
                           title.colour,
                           subtitle.colour) {
  
  if (!is.null(theme)) {
    if (is.character(theme)) {
      # for `theme = "theme_bw"` and `theme = "theme_bw()"`
      theme <- eval(parse(text = theme))
    }
    if (is.function(theme)) {
      # for `theme = theme_bw`
      theme <- theme()
    }
    if (!inherits(theme, "theme")) {
      plot2_warning("No valid ggplot2 theme, using ", font_blue("theme = ggplot2::theme_grey()"))
      theme <- NULL
    }
  }
  
  if (is_empty(theme)) {
    # turn to default ggplot2 theme, so we can at least:
    # - extend all element_text() classes with element_markdown()
    # - add all theme options set as arguments, like x.lbl_angle
    theme <- theme_grey()
  }
  
  if (isTRUE(markdown)) {
    # add 'element_markdown' to all titles, which the ggtext pkg will use to print in markdown
    # for this, the ggtext pkg has at least to be installed, but not loaded
    add_markdown <- function(el) {
      if (!is.null(el)) {
        class(el) <- c("element_markdown", class(el))
      }
      el
    }
    theme$plot.title <- add_markdown(theme$plot.title)
    theme$plot.subtitle <- add_markdown(theme$plot.subtitle)
    theme$plot.caption <- add_markdown(theme$plot.caption)
    theme$plot.tag <- add_markdown(theme$plot.tag)
    theme$strip.text <- add_markdown(theme$strip.text)
    theme$axis.title.x <- add_markdown(theme$axis.title.x)
    theme$axis.title.y <- add_markdown(theme$axis.title.y)
    theme$legend.title <- add_markdown(theme$legend.title)
    if (type != "geom_sf") {
      if (isTRUE(horizontal)) {
        # values of the turned x axis
        theme$axis.text.y <- add_markdown(theme$axis.text.y)
      } else {
        # values of the x axis
        theme$axis.text.x <- add_markdown(theme$axis.text.x)
      }
    }
  }
  
  # set other properties to theme, that are set in plot2(...)
  if (isTRUE(horizontal)) {
    if (isTRUE(x.lbl_italic)) {
      theme$axis.text.y$face <- "italic"
    }
    if (isTRUE(x.remove)) {
      theme$axis.text.y <- element_blank()
    }
  } else {
    if (isTRUE(x.lbl_italic)) {
      theme$axis.text.x$face <- "italic"
    }
    if (isTRUE(x.remove)) {
      theme$axis.text.x <- element_blank()
    }
  }
  
  theme$axis.text.x$angle <- x.lbl_angle
  if (is.null(x.lbl_align) && x.lbl_angle != 0) {
    # determine the better alignment
    if (abs(x.lbl_angle) %in% c(0:10, 171:190, 351:360)) {
      x.lbl_align <- 0.5 # centre
    }
    if (abs(x.lbl_angle) %in% c(191:350)) {
      x.lbl_align <- 0 # left
    }
    if (abs(x.lbl_angle) %in% c(11:170)) {
      x.lbl_align <- 1 # right
    }
    if (x.lbl_angle < 0) {
      x.lbl_align <- 1 - x.lbl_align
    }
  }
  if (!is.null(x.lbl_align)) {
    theme$axis.text.x$hjust <- x.lbl_align
  }
  
  if (isTRUE(legend.italic)) {
    theme$legend.text$face <- "italic"
  }
  
  theme$plot.title$colour <- colourpicker(title.colour)
  theme$plot.subtitle$colour <- colourpicker(subtitle.colour)
  # facet
  theme$strip.background$fill <- facet.fill
  if (isTRUE(facet.bold) && isTRUE(facet.italic)) {
    theme$strip.text$face <- "bold.italic"
  } else if (isTRUE(facet.bold)) {
    theme$strip.text$face <- "bold"
  } else if (isTRUE(facet.bold)) {
    theme$strip.text$face <- "italic"
  } else {
    theme$strip.text$face <- "plain"
  }
  theme$strip.text$margin <- margin(t = facet.margin, b = facet.margin / 2)
  theme$strip.text$size <- unit(facet.size, "pt")
  
  # set the font family and font size, taking text_factor into account
  attr_bak <- attributes(theme)
  theme <- lapply(theme, function(el) {
    if (inherits(el, "element_text")) {
      el$family <- family
      if (text_factor != 1 && !is.null(el$size) && is.numeric(el$size)) {
        attr_el_bak <- attributes(el$size)
        el$size <- as.double(el$size) * text_factor
        attributes(el$size) <- attr_el_bak
      }
    }
    el
  })
  attributes(theme) <- attr_bak # restore class and all other attributes
  
  # if horizontal, all x and y grid lines etc. should be exchanged
  if (isTRUE(horizontal)) {
    theme.bak <- theme
    theme$panel.grid.major.x <- theme$panel.grid.major.y
    theme$panel.grid.major.y <- theme.bak$panel.grid.major.x
    theme$panel.grid.minor.x <- theme$panel.grid.minor.y
    theme$panel.grid.minor.y <- theme.bak$panel.grid.minor.x
    theme$axis.ticks.x <- theme$axis.ticks.y
    theme$axis.ticks.y <- theme.bak$axis.ticks.x
    theme$axis.line.x <- theme$axis.line.y
    theme$axis.line.y <- theme.bak$axis.line.x
  }
  
  # return the theme
  return(theme)
}

#' @importFrom ggplot2 facet_grid facet_wrap
validate_facet <- function(df,
                           type,
                           facet.repeat_lbls_x,
                           facet.repeat_lbls_y,
                           facet.relative,
                           facet.drop,
                           facet.nrow,
                           facet.position,
                           horizontal) {
  scales <- "fixed"
  if (isTRUE(facet.repeat_lbls_x) && isTRUE(facet.repeat_lbls_y)) {
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
  
  if (type == "geom_sf") {
    # force fixes scales, otherwise throws an error: coord_sf doesn't support free scales
    scales <- "fixed"
  }
  
  if (any(is.na(get_facet(df)))) {
    # 'drop' means dropping of factor levels. If this is FALSE and the columns contains NA, this throws an error:
    # Error in scale_apply(layer_data, x_vars, "train", SCALE_X, x_scales)
    facet.drop <- TRUE
  }
  if (isTRUE(facet.relative)) {
    if (facet.position == "top") {
      switch <- "y"  
    } else {
      switch <- "x"
    }
    if (is.null(facet.nrow) || facet.nrow == 1) {
      return(facet_grid(cols = vars(`_var_facet`),
                        space = scales, # <- this makes the facet.relative happen
                        drop = facet.drop,
                        scales = scales,
                        switch = switch))
    } else {
      plot2_warning("When using ", font_blue("facet.relative = TRUE"), ", the number of columns cannot be > 1 when ",
                    font_blue("facet.nrow"), " is larger than 1")
      return(facet_grid(rows = vars(`_var_facet`),
                        space = scales,
                        drop = facet.drop,
                        scales = scales,
                        switch = switch))
    }
  } else {
    return(facet_wrap("`_var_facet`",
                      scales = scales,
                      strip.position = facet.position,
                      drop = facet.drop,
                      nrow = facet.nrow))
  }
}

#' @importFrom ggplot2 geom_text geom_label geom_sf_text geom_sf_label aes position_fill position_stack position_dodge2
#' @importFrom certestyle colourpicker
set_datalabels <- function(p,
                           df,
                           type,
                           width,
                           stacked,
                           stackedpercent,
                           datalabels.colour_fill,
                           datalabels.colour,
                           datalabels.size,
                           datalabels.angle,
                           family,
                           reverse,
                           horizontal,
                           misses_datalabels) {
  
  if (isTRUE(misses_datalabels) && nrow(df) > 50) {
    plot2_warning("Omitting printing of ", nrow(df), " datalabels - use ",
                  font_blue("datalabels = TRUE"), " to force printing")
    return(p)
  }
  
  if (is.null(datalabels.colour_fill)) {
    # try to get from current theme
    datalabels.colour_fill <- p$theme$panel.background$fill
    if (is.null(datalabels.colour_fill)) {
      # still NULL, then make fill invisible (NA)
      datalabels.colour_fill <- NA
    }
  }
  
  if (!isTRUE(stacked) && !isTRUE(stackedpercent) && type != "geom_sf") {
    datalabels.colour_fill <- colourpicker(datalabels.colour_fill, opacity = 0.25) # 25% transparency
  } else {
    datalabels.colour_fill <- colourpicker(datalabels.colour_fill, opacity = 0.75) # 75% transparency
  }
  datalabels.colour <- colourpicker(datalabels.colour)
  
  is_sf <- (type == "geom_sf")
  
  # set label and text sizes
  text_horizontal <- 0.5
  text_vertical <- -0.75
  label_horizontal <- 0.5
  label_vertical <- -0.1
  if (isTRUE(horizontal) | datalabels.angle == 90) {
    text_horizontal <- -0.25
    text_vertical <- 0.5
    label_horizontal <- -0.1
    label_vertical <- 0.5
  }
  
  # set positioning function
  if (isTRUE(stackedpercent)) {
    position_fn <- position_fill(reverse = reverse, vjust = 0.5)
  } else if (isTRUE(stacked)) {
    position_fn <- position_stack(reverse = reverse, vjust = 0.5)
  } else {
    position_fn <- position_dodge2(width = width, preserve = "single")
  }
  
  # set label and text functions
  geom_label_fn <- ifelse(isTRUE(is_sf),  geom_sf_label, geom_label)
  geom_text_fn <- ifelse(isTRUE(is_sf),  geom_sf_text, geom_text)
  
  if (!isTRUE(is_sf)) {
    geom_label_fn <- geom_label
    geom_text_fn <- geom_text
    geometry_fix_fn <- NULL
  } else {
    geom_label_fn <- geom_sf_label
    geom_text_fn <- geom_sf_text
    # these functions from the 'sf' package fix invalid geometries
    st_is_valid <- getExportedValue(name = "st_is_valid", ns = asNamespace("sf"))
    st_point <- getExportedValue(name = "st_point", ns = asNamespace("sf"))
    st_point_on_surface <- getExportedValue(name = "st_point_on_surface", ns = asNamespace("sf"))
    st_zm <- getExportedValue(name = "st_zm", ns = asNamespace("sf"))
    geometry_fix_fn <- function(x) {
      x[!st_is_valid(x)] <- st_point()
      suppressWarnings(st_point_on_surface(st_zm(x)))
    }
  }
  
  # generate the datalabels
  p <- p +
    # set background label
    do.call(geom_label_fn,
            args = c(list(mapping = aes(label = ifelse(is.na(`_var_datalabels`),
                                                       NA_character_,
                                                       paste0(`_var_datalabels`,
                                                              strrep("-", ceiling(nchar(`_var_datalabels`) * 0.33))))),
                          colour = NA,
                          fill = datalabels.colour_fill,
                          size = datalabels.size,
                          family = family,
                          angle = datalabels.angle,
                          na.rm = TRUE),
                     # only when there's a category:
                     list(position = position_fn)[has_category(df) & !isTRUE(is_sf)],
                     # only when not stacked at all:
                     list(label.padding = unit(0.25, "lines"))[!isTRUE(stacked) & !isTRUE(stackedpercent) & !isTRUE(is_sf)],
                     list(label.r = unit(0, "lines"))[!isTRUE(stacked) & !isTRUE(stackedpercent) & !isTRUE(is_sf)],
                     list(vjust = label_vertical)[!isTRUE(stacked) & !isTRUE(stackedpercent) & !isTRUE(is_sf)],
                     list(hjust = label_horizontal)[!isTRUE(stacked) & !isTRUE(stackedpercent) & !isTRUE(is_sf)],
                     # only when stackedpercent:
                     list(vjust = 0.5)[isTRUE(stackedpercent) | isTRUE(is_sf)],
                     list(hjust = 0.5)[isTRUE(stackedpercent) | isTRUE(is_sf)],
                     # only when sf:
                     list(fun.geometry = geometry_fix_fn)[isTRUE(is_sf)])) +
    # set text
    do.call(geom_text_fn,
            args = c(list(mapping = aes(label = `_var_datalabels`),
                          colour = datalabels.colour,
                          size = datalabels.size,
                          family = family,
                          angle = datalabels.angle,
                          na.rm = TRUE),
                     # only when there's a category:
                     list(position = position_fn)[has_category(df) & !isTRUE(is_sf)],
                     # only when not stacked at all:
                     list(vjust = text_vertical)[!isTRUE(stacked) & !isTRUE(stackedpercent) & !isTRUE(is_sf)],
                     list(hjust = text_horizontal)[!isTRUE(stacked) & !isTRUE(stackedpercent) & !isTRUE(is_sf)],
                     # only when stackedpercent:
                     list(vjust = 0.5)[isTRUE(stackedpercent) | isTRUE(is_sf)],
                     list(hjust = 0.5)[isTRUE(stackedpercent) | isTRUE(is_sf)],
                     # only when sf:
                     list(fun.geometry = geometry_fix_fn)[isTRUE(is_sf)]))
  
  if (!isTRUE(is_sf)) {
    if (!isTRUE(stacked) && !isTRUE(stackedpercent)) {
      # move label layer to back + 1;
      # this will make the labels only interfere with plot lines,
      # not with the data (such as columns)
      layer_n <- seq_len(length(p$layers))
      layer_label <- length(layer_n) - 1
      layer_others <- layer_n[-layer_label]
      p$layers <- p$layers[c(layer_label, layer_others)]
    }
  }
  
  p
}

validate_sorting <- function(sort_method, horizontal) {
  if (is.null(sort_method)) {
    return(sort_method)
  }
  sort_method <- tolower(sort_method[1L])
  sort_method <- gsub("[^a-z-]+", "", sort_method)
  sort_method <- gsub("true", "asc", sort_method)      # when sort_method = TRUE
  sort_method <- gsub("false", "inorder", sort_method) # when sort_method = FALSE
  sort_method <- gsub("asc[a-z]+", "asc", sort_method)
  sort_method <- gsub("desc[a-z]+", "desc", sort_method)
  if (sort_method %like% "freq$") {
    sort_method <- paste0(sort_method, "-desc")
  }
  if (isTRUE(horizontal)) {
    # reverse asc and desc
    sort_method <- gsub("asc", "asc2", sort_method)
    sort_method <- gsub("desc", "asc", sort_method)
    sort_method <- gsub("asc2", "desc", sort_method)
  }
  sort_method
}

#' @importFrom forcats fct_inorder fct_reorder
#' @importFrom stringr str_sort
#' @importFrom certestyle font_blue
sort_data <- function(original_values,
                      sort_method,
                      datapoints,
                      summarise_function,
                      summarise_fn_name,
                      horizontal,
                      drop) {
  if (is.null(sort_method) ||
      is.numeric(original_values) ||
      ((isTRUE(sort_method) && is.factor(original_values) && !isTRUE(horizontal)))) {
    # don't sort at all
    return(original_values)
  }
  
  # set up sort_method
  sort_method.bak <- sort_method[1L]
  sort_method <- validate_sorting(sort_method = sort_method, horizontal = horizontal)
  
  # factors get a special treatment - they are sorted on their levels
  if (is.factor(original_values)) {
    if (sort_method %in% c("alpha", "alpha-asc", "asc")) {
      if (isTRUE(horizontal)) {
        lvls <- rev(levels(original_values))
      } else {
        lvls <- levels(original_values)
      }
      return(factor(as.character(original_values),
                    levels = lvls,
                    ordered = is.ordered(original_values)))
    } else if (sort_method %in% c("alpha-desc", "desc")) {
      if (isTRUE(horizontal)) {
        lvls <- levels(original_values)
      } else {
        lvls <- rev(levels(original_values))
      }
      return(factor(as.character(original_values),
                    levels = lvls,
                    ordered = is.ordered(original_values)))
    }
  }
  if (!isTRUE(drop)) {
    levels <- levels(original_values)
  }
  
  if (!is.numeric(original_values)) {
    # force characters for anything else than numbers
    original_values <- as.character(original_values)
  }
  
  # start the sorting
  numeric_sort <- any(original_values %like% "[0-9]", na.rm = TRUE)
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
    plot2_message("Applying sorting ", font_blue(paste0("\"", sort_method, "\"")), " using ",
                  font_blue(paste0("summarise_function = ", summarise_fn_name)))
    out <- fct_reorder(.f = as.character(original_values),
                       .x = datapoints,
                       .fun = summarise_function,
                       .desc = FALSE)
  } else if (sort_method %in% c("freq-desc", "infreq-desc")) {
    plot2_message("Applying sorting ", font_blue(paste0("\"", sort_method, "\"")), " using ",
                  font_blue(paste0("summarise_function = ", summarise_fn_name)))
    out <- fct_reorder(.f = as.character(original_values),
                       .x = datapoints,
                       .fun = summarise_function,
                       .desc = TRUE)
  } else {
    stop("invalid sorting option: '", sort_method.bak, "'")
  }
  
  if (!isTRUE(drop) && !is.null(levels)) {
    levels(out) <- c(levels(out), sort(levels[!levels %in% levels(out)]))
  }
  
  out
}

#' @importFrom forcats fct_relevel
#' @importFrom dplyr `%>%` group_by across group_size mutate summarise
#' @importFrom certestyle font_blue font_red format2
set_max_items <- function(df,
                          y,
                          x,
                          x.max_items,
                          x.max_txt,
                          category,
                          category.max_items,
                          category.max_txt,
                          facet,
                          facet.max_items,
                          facet.max_txt,
                          horizontal,
                          summarise_function,
                          decimal.mark,
                          big.mark,
                          datalabels.round,
                          datalabels.format) {
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
      plot2_warning("Setting ", font_blue("*.max_items"), " only works when values are a character or (sorted) factor, not ", font_red(paste0(class(values), collapse = "/")))
      return(values)
    }
    if (n_max < length(levels(values))) {
      if (isTRUE(horizontal)) {
        lvls_remove <- rev(levels(values))[c(n_max:length(levels(values)))]
      } else {
        lvls_remove <- levels(values)[c(n_max:length(levels(values)))]
      }
      lvls_remove <- lvls_remove[order(-nchar(lvls_remove))]
      value_new <- gsub("%n", format2(length(lvls_remove)), txt, fixed = TRUE)
      pct <- format2(length(values[values %in% lvls_remove]) / length(values), percent = TRUE)
      value_new <- gsub("%p", pct, value_new, fixed = TRUE)
      # add new factor level
      levels(values) <- c(levels(values), value_new)
      # replace all values that must be removed
      values[as.character(values) %in% lvls_remove] <- value_new
      # drop unused factor levels
      values <- droplevels(values)
      # set new level to last place, taking into account 'horizontal'
      if (isTRUE(horizontal)) {
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
  
  if (all(group_sizes(df) == 1)) {
    # summarise again
    df <- summarise_data(df = df,
                         summarise_function = summarise_function,
                         decimal.mark = decimal.mark,
                         big.mark = big.mark,
                         datalabels.round = datalabels.round,
                         datalabels.format = datalabels.format)
  }
  df
  
}

summarise_data <- function(df,
                           summarise_function,
                           decimal.mark,
                           big.mark,
                           datalabels.round,
                           datalabels.format) {
  x <- get_x_name(df)
  y <- get_y_name(df)
  category <- get_category_name(df)
  facet <- get_facet_name(df)
  has_datalbls <- has_datalabels(df)
  df <- df %>%
    mutate(n = get_y(df)) %>%
    group_by(across(c(x, category, facet))) %>%
    summarise(n = summarise_function(n, na.rm = TRUE),
              .groups = "drop")
  colnames(df)[colnames(df) == "n"] <- y
  df$`_var_y` <- df[, y, drop = TRUE]
  if (!is.null(x)) df$`_var_x` <- df[, x, drop = TRUE]
  if (!is.null(category)) df$`_var_category` <- df[, category, drop = TRUE]
  if (!is.null(facet)) df$`_var_facet` <- df[, facet, drop = TRUE]
  if (isTRUE(has_datalbls)) {
    df <- df %>%
      mutate(`_var_datalabels` = format_datalabels(`_var_y`,
                                                   datalabels.round = datalabels.round,
                                                   datalabels.format = datalabels.format,
                                                   decimal.mark = decimal.mark,
                                                   big.mark = big.mark))
  }
  df
}

#' @importFrom certestyle format2
format_datalabels <- function(datalabels,
                              datalabels.round,
                              datalabels.format,
                              decimal.mark,
                              big.mark) {
  datalabels[as.character(datalabels) %in% c("", "0")] <- NA
  datalabels_out <- datalabels
  if (!is.null(datalabels.format) &&
      mode(datalabels) == "numeric" &&
      !inherits(datalabels, c("factor", "Date", "POSIXt"))) {
    datalabels <- as.double(datalabels)
    datalabels_out <- rep(datalabels.format, length(datalabels_out))
    if (datalabels.format %like% "%p") {
      datalabels_p <- trimws(format2(datalabels / sum(datalabels, na.rm = TRUE),
                                     round = datalabels.round,
                                     decimal.mark = decimal.mark,
                                     big.mark = big.mark,
                                     percent = TRUE))
      datalabels_out <- mapply(gsub,
                               x = datalabels_out,
                               pattern = "%p",
                               replacement = datalabels_p,
                               USE.NAMES = FALSE)
    }
    if (datalabels.format %like% "%n") {
      datalabels_n <- trimws(format2(datalabels,
                                     decimal.mark = decimal.mark,
                                     big.mark = big.mark,
                                     round = datalabels.round,
                                     force.decimals = FALSE))
      datalabels_out <- mapply(gsub,
                               x = datalabels_out,
                               pattern = "%n",
                               replacement = datalabels_n,
                               USE.NAMES = FALSE)
    }
  }
  datalabels_out
}
