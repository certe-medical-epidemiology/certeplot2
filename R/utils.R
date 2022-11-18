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

plot2_env <- new.env(hash = FALSE)

globalVariables(c("_new_title",
                  "_var_category",
                  "_var_datalabels",
                  "_var_facet",
                  "_var_y_secondary",
                  "_var_x",
                  "_var_y",
                  "ab",
                  "antibiotic", 
                  "count",
                  "geom",
                  "interpretation",
                  "isolates",
                  "mo",
                  "n",
                  "name",
                  "R",
                  "rowname",
                  "total",
                  "value",
                  "where"))

#' @importFrom dplyr n
#' @export
dplyr::n

#' @importFrom dplyr n_distinct
#' @export
dplyr::n_distinct

#' @importFrom tidyselect everything
#' @export
tidyselect::everything

#' @importFrom tidyselect starts_with
#' @export
tidyselect::starts_with

#' @importFrom tidyselect ends_with
#' @export
tidyselect::ends_with

#' @importFrom tidyselect matches
#' @export
tidyselect::matches

#' @importFrom tidyselect where
#' @export
tidyselect::where

#' @importFrom dplyr first
#' @export
dplyr::first

#' @importFrom dplyr last
#' @export
dplyr::last

#' @importFrom dplyr all_of
#' @export
dplyr::all_of

#' @importFrom dplyr any_of
#' @export
dplyr::any_of

#' @importFrom certestyle dec_mark
#' @export
certestyle::dec_mark

#' @importFrom certestyle big_mark
#' @export
certestyle::big_mark

#' @importFrom certestyle font_black font_blue font_red font_white font_bold
plot2_message <- function(..., print = interactive() | Sys.getenv("IN_PKGDOWN") != "", type = "info") {
  # at default, only prints in interactive mode and for the website generation
  if (isTRUE(print)) {
    # get info icon
    if (isTRUE(base::l10n_info()$`UTF-8`) && interactive()) {
      # \u2139 is a symbol officially named 'information source'
      icon <- "\u2139"
    } else {
      icon <- "i"
    }
    if (type == "info") {
      fn <- font_black
      icon <- font_blue(icon)
    } else {
      fn <- font_red
      icon <- font_red("!")
    }
    msg <- paste0(fn(c(...), collapse = NULL), collapse = "")
    message(paste(icon, fn(msg)))
  }
}

plot2_warning <- function(..., print = interactive() | Sys.getenv("IN_PKGDOWN") != "") {
  plot2_message(..., print = print, type = "warning")
}

requires_numeric_coercion <- function(x) {
  !is.null(x) && mode(x) == "numeric" && !is.numeric(x) && !inherits(x, c("factor", "Date", "POSIXt"))
}

summarise_variable <- function(df, var, sep) {
  # combined with add_direction(), this will add support for multiple vars in one direction:
  # e.g., `category = c(col1, col2)`
  cols <- colnames(df)
  old_vars <- cols[cols %like% paste0(var, "_")]
  if (length(old_vars) == 0) {
    return(df)
  } else if (length(old_vars) > 1) {
    new_var <- do.call(paste, c(df[old_vars], sep = sep))
  } else {
    new_var <- df[, old_vars, drop = TRUE]
  }
  df <- df[, cols[!cols %in% old_vars], drop = FALSE]
  df[, var] <- new_var
  df
}

#' @importFrom dplyr select mutate across
add_direction <- function(df, direction, var_name, var_label, sep) {
  tryCatch({
    # this for using Tidyverse selectors, such as `facet = where(is.character)`
    selected_cols <- df |>
      as.data.frame(stringsAsFactors = FALSE) |> # for sf data
      select({{ direction }}) |> 
      colnames()
    selected_cols <- selected_cols[selected_cols %unlike% "^_var_"]
    if (length(selected_cols) > 1 && is.character(selected_cols) && !all(var_label %like% selected_cols)) {
      # replace e.g. `facet = where(is.character)` with `facet = c(var1, var2, var3)`
      # in labels for columns, but also in mapping
      new_var_name <- paste0("c(", paste0(selected_cols, collapse = ", "), ")")
      plot2_message("Using ", font_blue(paste0(var_name, " = ", new_var_name)))
      if (var_name == "x") plot2_env$mapping_x <- new_var_name
      if (var_name == "category") plot2_env$mapping_category <- new_var_name
      if (var_name == "facet") plot2_env$mapping_facet <- new_var_name
      var_label <- new_var_name
    }
  }, error = function(e) invisible())

  df <- tryCatch({
    out <- df |> 
      mutate(`_var_` = {{ direction }})
    colnames(out)[colnames(out) == "_var_"] <- paste0("_var_", var_name)
    out
  }, error = function(e) {
    # multiple columns selected
    df |> 
      mutate(across({{ direction }}, .names = paste0("_var_", var_name, "_{col}"))) |> 
      summarise_variable(paste0("_var_", var_name), sep = sep)
  })
  
  # this adds the column again with the right label
  if (var_label != "NULL" && !var_label %in% colnames(df)) {
    df$`_var_new` <- df[, paste0("_var_", var_name), drop = TRUE]
    colnames(df)[colnames(df) == "_var_new"] <- var_label
  }
  
  df
}

#' @importFrom dplyr pull
get_column_name <- function(df, column_var) {
  out <- vapply(FUN.VALUE = logical(1), df, function(col) {
    identical(col,
              df |> pull({{column_var}}))
  })
  if (all(out[names(out) %unlike% "^_var_"] == FALSE)) {
    # no column found, probably due to sorting (i.e., factors), try again with character comparison
    out <- vapply(FUN.VALUE = logical(1), df, function(col) {
      identical(col |> as.character(),
                df |> pull({{column_var}}) |> as.character())
    })
  }
  out <- names(out)[out & names(out) %unlike% "^_var_"][1L]
  if (is.na(out)) {
    return(NULL)
  }
  out
}

get_x <- function(df, na.rm = FALSE) {
  if (has_x(df)) {
    out <- df$`_var_x`
    if (isTRUE(na.rm)) {
      out <- out[!is.na(out)]
    }
    out
  } else {
    NULL
  }
}
get_x_name <- function(df) {
  if (has_x(df)) {
    if (!is.null(plot2_env$mapping_x) && plot2_env$mapping_x != "NULL" && plot2_env$mapping_x %in% colnames(df)) {
      plot2_env$mapping_x
    } else {
      get_column_name(df, `_var_x`)
    }
  } else {
    NULL
  }
}
has_x <- function(df) {
  "_var_x" %in% colnames(df)
}

get_y <- function(df) {
  if (has_y(df)) {
    df$`_var_y`
  } else {
    NULL
  }
}
get_y_name <- function(df) {
  if (has_y(df)) {
    if (!is.null(plot2_env$mapping_y) && plot2_env$mapping_y != "NULL" && plot2_env$mapping_y %in% colnames(df)) {
      plot2_env$mapping_y
    } else {
      get_column_name(df, `_var_y`)
    }
  } else {
    NULL
  }
}
has_y <- function(df) {
  "_var_y" %in% colnames(df)
}

get_category <- function(df, na.rm = FALSE) {
  if (has_category(df)) {
    out <- df$`_var_category`
    if (isTRUE(na.rm)) {
      out <- out[!is.na(out)]
    }
    out
  } else {
    NULL
  }
}
get_category_name <- function(df) {
  if (has_category(df)) {
    if (!is.null(plot2_env$mapping_category) && plot2_env$mapping_category != "NULL" && plot2_env$mapping_category %in% colnames(df)) {
      plot2_env$mapping_category
    } else {
      get_column_name(df, `_var_category`)
    }
  } else {
    NULL
  }
}
has_category <- function(df) {
  "_var_category" %in% colnames(df)
}

get_facet <- function(df) {
  if (has_facet(df)) {
    df$`_var_facet`
  } else {
    NULL
  }
}
get_facet_name <- function(df) {
  if (has_facet(df)) {
    if (!is.null(plot2_env$mapping_facet) && plot2_env$mapping_facet != "NULL" && plot2_env$mapping_facet %in% colnames(df)) {
      plot2_env$mapping_facet
    } else {
      get_column_name(df, `_var_facet`)
    }
  } else {
    NULL
  }
}
has_facet <- function(df) {
  "_var_facet" %in% colnames(df)
}

get_y_secondary <- function(df) {
  if (has_y_secondary(df)) {
    df$`_var_y_secondary`
  } else {
    NULL
  }
}
get_y_secondary_name <- function(df) {
  if (has_y_secondary(df)) {
    if (!is.null(plot2_env$mapping_y_secondary) && plot2_env$mapping_y_secondary != "NULL" && plot2_env$mapping_y_secondary %in% colnames(df)) {
      plot2_env$mapping_y_secondary
    } else {
      get_column_name(df, `_var_y_secondary`)
    }
  } else {
    NULL
  }
}
has_y_secondary <- function(df) {
  "_var_y_secondary" %in% colnames(df)
}

get_datalabels <- function(df) {
  if (has_datalabels(df)) {
    df$`_var_datalabels`
  } else {
    NULL
  }
}
has_datalabels <- function(df) {
  "_var_datalabels" %in% colnames(df)
}

determine_date_breaks_labels <- function(x) {
  diff_range <- diff(range(x, na.rm = TRUE))
  if (diff_range < 30) {
    # 1 month
    out <- list(breaks = "1 day",
                labels = "d mmm")
  } else if (diff_range < 92) {
    # quarter
    out <- list(breaks = "4 days",
                labels = "d mmm")
  } else if (diff_range < 183) {
    # half year
    out <- list(breaks = "2 weeks",
                labels = "d mmm")
  } else if (diff_range < 365) {
    # years
    out <- list(breaks = "1 month",
                labels = "mmmm yyyy")
  } else if (diff_range < 730) {
    # 2 years
    out <- list(breaks = "3 months",
                labels = "mmm yyyy")
  } else if (diff_range < 1095) {
    # 3 years
    out <- list(breaks = "6 months",
                labels = "mmm yyyy")
  } else if (diff_range < 2556) {
    # 7 years
    out <- list(breaks = "1 year",
                labels = "mmm yyyy")
  } else {
    # even longer, all other cases
    out <- list(breaks = "2 years",
                labels = "yyyy")
  }
  out
}

is_empty <- function(x) {
  is.null(x) || isFALSE(x) || identical(x, "") || all(is.na(as.character(x)))
}

geom_is_continuous <- function(geom) {
  geom %in% c("geom_boxplot", "geom_violin", "geom_point", "geom_jitter", "geom_histogram", "geom_density", "geom_sf", "geom_line", "geom_area", "geom_ribbon", "geom_tile", "geom_raster", "geom_rect")
}
geom_is_continuous_x <- function(geom) {
  geom %in% c("geom_histogram", "geom_density")
}
geom_has_colour <- function(geom) {
  geom %in% c("geom_line", "geom_hline", "geom_vline", "geom_path", "geom_qq_line", "geom_linerange", "geom_area", "geom_ribbon", "geom_tile", "geom_raster", "geom_rect")
}
geom_has_only_colour <- function(geom) {
  geom %in% c("geom_point", "geom_jitter", "geom_line", "geom_hline", "geom_vline",
              "geom_path", "geom_qq_line", "geom_linerange", "geom_pointrange")
}

#' @importFrom dplyr group_by across group_size
group_sizes <- function(df) {
  if (inherits(df, "sf")) {
    nrow(df)
  } else {
    df |> 
      group_by(across(c(get_x_name(df), get_category_name(df), get_facet_name(df))),
               .drop = FALSE) |>
      group_size()
  }
}

restore_mapping <- function(p, df) {
  # help function
  new_mapping <- function(mapping, df) {
    if (is.null(mapping)) {
      return(mapping)
    }
    att <- attributes(mapping)
    new_mapping <- lapply(mapping,
                          function(map) {
                            if (any(deparse(map) %like% "_var_x")) {
                              aes_string(as.name(get_x_name(df)))[[1]] 
                            } else if (any(deparse(map) %like% "_var_y_secondary")) {
                              aes_string(as.name(get_y_secondary_name(df)))[[1]] 
                            } else if (any(deparse(map) %like% "_var_y")) {
                              aes_string(as.name(get_y_name(df)))[[1]] 
                            } else if (any(deparse(map) %like% "_var_category")) {
                              aes_string(as.name(get_category_name(df)))[[1]] 
                            } else if (any(deparse(map) %like% "_var_facet")) {
                              aes_string(as.name(get_facet_name(df)))[[1]] 
                            } else {
                              map
                            }})
    attributes(new_mapping) <- att
    new_mapping
  }
  
  # general plot mapping
  p$mapping <- new_mapping(mapping = p$mapping, df = df)
  # mapping for each extra layer, such as geom_smooth()
  for (i in seq_len(length(p$layers))) {
    p$layers[[i]]$mapping <- new_mapping(mapping = p$layers[[i]]$mapping, df = df)
  }
  
  # facet mapping
  if (has_facet(df)) {
    p$facet$params$facets[[1]] <- aes_string(as.name(get_facet_name(df)))[[1]]
    names(p$facet$params$facets)[1] <- get_facet_name(df)
    # required for ggplot2::facet_grid(), which is used when facet.relative = TRUE:
    if (length(p$facet$params$rows) > 0) {
      p$facet$params$rows[[1]] <- aes_string(as.name(get_facet_name(df)))[[1]]
      names(p$facet$params$rows)[1] <- get_facet_name(df)
    }
    if (length(p$facet$params$cols) > 0) {
      p$facet$params$cols[[1]] <- aes_string(as.name(get_facet_name(df)))[[1]]
      names(p$facet$params$cols)[1] <- get_facet_name(df)
    }
  }
  
  # now remove anonymous these `_var_*` columns from the data
  p$data <- p$data[, colnames(p$data)[colnames(p$data) %unlike% "^_var_(x|y|category|facet)$"], drop = FALSE]
  
  # return the plot object
  p
}

set_plot2_env <- function(x = NULL, y = NULL, category = NULL, facet = NULL, y_secondary = NULL) {
  x <- paste0(x, collapse = " ")
  y <- paste0(y, collapse = " ")
  category <- paste0(category, collapse = " ")
  facet <- paste0(facet, collapse = " ")
  y_secondary <- paste0(y_secondary, collapse = " ")
  if (!x %in% c("NULL", "") && is.null(plot2_env$mapping_x)) {
    plot2_env$mapping_x <- x
  }
  if (!y %in% c("NULL", "") && is.null(plot2_env$mapping_y)) {
    plot2_env$mapping_y <- y
  }
  if (!category %in% c("NULL", "") && is.null(plot2_env$mapping_category)) {
    plot2_env$mapping_category <- category
  }
  if (!facet %in% c("NULL", "") && is.null(plot2_env$mapping_facet)) {
    plot2_env$mapping_facet <- facet
  }
  if (!y_secondary %in% c("NULL", "") && is.null(plot2_env$mapping_y_secondary)) {
    plot2_env$mapping_y_secondary <- y_secondary
  }
}
clean_plot2_env <- function() {
  plot2_env$mapping_x <- NULL
  plot2_env$mapping_y <- NULL
  plot2_env$mapping_category <- NULL
  plot2_env$mapping_facet <- NULL
  plot2_env$mapping_y_secondary <- NULL
  plot2_env$y_secondary_factor <- NULL
}

sigfigs <- function(x) {
  vapply(FUN.VALUE = double(1), x, function(val) {
    frm <- format(val, scientific = FALSE)
    if (frm %unlike% "[.]" | frm %like% "[.]0+$") {
      0
    } else if (frm %like% "[.]0") {
      nchar(gsub(".*[.](0+).*$", "\\1", frm)) + 1
    } else {
      nchar(gsub(".*[.]([0-9]+)$", "\\1", frm))
    }
  })
}

data_is_numeric <- function(x) {
  all(x %like% "^[0-9.,-]+(e[+][0-9.,-]+)?$", na.rm = TRUE)
}

digit_to_text <- function(x) {
  out <- switch(x,
                "one",
                "two",
                "three",
                "four",
                "five",
                "six",
                "seven",
                "eight",
                "nine",
                "ten")
  if (is.null(out)) {
    out <- as.character(x)
  }
  out
}

format_error <- function(e, replace = character(0), by = character(0)) {
  if (is.null(e$bullets)) {
    txt <- c(e$message, e$parent$message)
  } else {
    txt <- e$bullets
  }
  txt <- txt[txt %unlike% "^Problem while"]
  if (length(txt) == 0) {
    # return original error
    stop(e, call. = FALSE)
  }
  out <- gsub(".*(\033\\[31m)?x(\033\\[39m)? ", "", paste(txt, collapse = "\n"))
  for (i in seq_len(length(replace))) {
    out <- gsub(replace[i], by[i], out)
  }
  if (out == "") {
    out <- "Plot cannot be generated due to unknown error"
  }
  out
}
