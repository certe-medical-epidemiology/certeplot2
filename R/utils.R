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

globalVariables(c(".",
                  "..count..",
                  "ab",
                  "geom",
                  "mo",
                  "n",
                  "name",
                  "R",
                  "total",
                  "value",
                  "where",
                  "_var_category",
                  "_var_datalabels",
                  "_var_facet",
                  "_var_x",
                  "_var_y"))

#' @importFrom dplyr `%>%`
#' @export
dplyr::`%>%`

#' @importFrom dplyr n
#' @export
dplyr::n

#' @importFrom dplyr n_distinct
#' @export
dplyr::n_distinct

#' @importFrom certestyle font_black font_blue font_red_bg font_white font_bold
plot2_message <- function(..., print = interactive() | Sys.getenv("IN_PKGDOWN") != "", geom = "info") {
  # at default, only prints in interactive mode and for the website generation
  if (isTRUE(print)) {
    msg <- paste0(font_black(c(...), collapse = NULL), collapse = "")
    # get info icon
    if (isTRUE(base::l10n_info()$`UTF-8`) && interactive()) {
      # \u2139 is a symbol officially named 'information source'
      icon <- "\u2139"
    } else {
      icon <- "i"
    }
    if (geom == "info") {
      icon <- font_blue(icon)
    } else {
      icon <- font_red(icon)
    }
    message(paste(icon, font_black(msg)))
  }
}

plot2_warning <- function(..., print = interactive() | Sys.getenv("IN_PKGDOWN") != "") {
  plot2_message(..., print = print, geom = "warning")
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

#' @importFrom dplyr `%>%` select mutate across
add_direction <- function(df, direction, var_name, var_label, sep) {
  tryCatch({
    # this for using Tidyverse selectors, such as `facet = where(is.character)`
    selected_cols <- df %>%
      as.data.frame(stringsAsFactors = FALSE) %>% # for sf data
      select({{ direction }}) %>% 
      colnames()
    selected_cols <- selected_cols[selected_cols %unlike% "^_var_"]
    if (length(selected_cols) > 1 && is.character(selected_cols) && !all(var_label %like% selected_cols)) {
      plot2_message("Using ", font_blue(paste0(var_name, " = c(", paste0(selected_cols, collapse = ", "), ")")))
    }
  }, error = function(e) invisible())
  
  df <- tryCatch({
    df %>% 
      mutate(across({{ direction }}, .names = paste0("_var_", var_name, "_{col}"))) %>% 
      summarise_variable(paste0("_var_", var_name), sep = sep)
  }, error = function(e) {
    out <- df %>% 
      mutate(`_var_` = {{ direction }})
    colnames(out)[colnames(out) == "_var_"] <- paste0("_var_", var_name)
    out
  })
  
  if (var_label != "NULL" && !var_label %in% colnames(df)) {
    df$`_var_new` <- df[, paste0("_var_", var_name), drop = TRUE]
    colnames(df)[colnames(df) == "_var_new"] <- var_label
  }
  df
}

#' @importFrom dplyr `%>%` pull
get_column_name <- function(df, column_var) {
  out <- vapply(FUN.VALUE = logical(1), df, function(col) {
    identical(col,
              df %>% pull({{column_var}}))
  })
  if (all(out[names(out) %unlike% "^_var_"] == FALSE)) {
    # no column found, probably due to sorting (i.e., factors), try again with character comparison
    out <- vapply(FUN.VALUE = logical(1), df, function(col) {
      identical(col %>% as.character(),
                df %>% pull({{column_var}}) %>% as.character())
    })
  }
  names(out)[out & names(out) %unlike% "^_var_"][1L]
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
    if (plot2_env$mapping_x != "NULL" && plot2_env$mapping_x %in% colnames(df)) {
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
    if (plot2_env$mapping_y != "NULL" && plot2_env$mapping_y %in% colnames(df)) {
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

get_category <- function(df) {
  if (has_category(df)) {
    df$`_var_category`
  } else {
    NULL
  }
}
get_category_name <- function(df) {
  if (has_category(df)) {
    if (plot2_env$mapping_category != "NULL" && plot2_env$mapping_category %in% colnames(df)) {
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
    if (plot2_env$mapping_facet != "NULL" && plot2_env$mapping_facet %in% colnames(df)) {
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

get_datalabels <- function(df) {
  if (has_datalabels(df)) {
    df$`_var_datalabels`
  } else {
    NULL
  }
}
get_datalabels_name <- function(df) {
  if (has_datalabels(df)) {
    get_column_name(df, `_var_datalabels`)
  } else {
    NULL
  }
}
has_datalabels <- function(df) {
  "_var_datalabels" %in% colnames(df)
}

determine_date_breaks_labels <- function(x) {
  diff_range <- diff(range(x))
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
    out <- list(breaks = "2 months",
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
  geom %in% c("geom_boxplot", "geom_violin", "geom_point", "geom_jitter", "geom_histogram", "geom_density", "geom_sf", "geom_line")
}
geom_is_continuous_x <- function(geom) {
  geom %in% c("geom_histogram", "geom_density")
}
geom_is_line <- function(geom) {
  geom %in% c("geom_line", "geom_hline", "geom_vline", "geom_path", "geom_qq_line", "geom_linerange")
}
geom_has_only_colour <- function(geom) {
  geom %in% c("geom_point", "geom_jitter", "geom_line", "geom_hline", "geom_vline",
              "geom_path", "geom_qq_line", "geom_linerange", "geom_pointrange")
}

#' @importFrom dplyr `%>%` group_by across group_size
group_sizes <- function(df) {
  if (inherits(df, "sf")) {
    nrow(df)
  } else {
    df %>% 
      group_by(across(c(get_x_name(df), get_category_name(df), get_facet_name(df)))) %>%
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
                          function(map) 
                            if (any(deparse(map) %like% "_var_x")) {
                              aes_string(as.name(get_x_name(df)))[[1]] 
                            } else if (any(deparse(map) %like% "_var_y")) {
                              aes_string(as.name(get_y_name(df)))[[1]] 
                            } else if (any(deparse(map) %like% "_var_category")) {
                              aes_string(as.name(get_category_name(df)))[[1]] 
                            } else if (any(deparse(map) %like% "_var_facet")) {
                              aes_string(as.name(get_facet_name(df)))[[1]] 
                            } else {
                              map
                            })
    attributes(new_mapping) <- att
    new_mapping
  }
  
  # general plot mapping
  p$mapping <- new_mapping(mapping = p$mapping, df = df)
  # mapping for each extra layer, such as geom_smooth()
  for (i in seq_len(length(p$layers))) {
    p$layers[[i]]$mapping <- new_mapping(mapping = p$layers[[i]]$mapping, df = df)
  }
  # mapping in facets
  if (has_facet(df)) {
    p$facet$params$facets[[1]] <- aes_string(as.name(get_facet_name(df)))[[1]]
    names(p$facet$params$facets)[1] <- get_facet_name(df)
  }
  
  # now remove anonymous these `_var_*` columns from the data
  p$data <- p$data[, colnames(p$data)[colnames(p$data) %unlike% "^_var_(x|y|category|facet)$"], drop = FALSE]
  # return the plot object
  p
}
