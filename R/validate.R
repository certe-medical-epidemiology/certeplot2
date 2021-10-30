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

validate_type <- function(type) {
  type <- trimws(tolower(type[1L]))
  type <- gsub("geom_", "", type, fixed = TRUE)
  type <- gsub("[^a-z0-9_]", "", type)
  
  if (type == "p") type <- "point"
  if (type == "l") type <- "line"
  if (type %in% c("c", "column")) type <- "col"
  # replace 'points' etc. with 'point' etc.
  type <- gsub("s$", "", type)
  
  valid_geoms <- gsub("^geom_", "", ls(pattern = "^geom_", env = asNamespace("ggplot2")))
  
  if (!type %in% valid_geoms) {
    stop("plot type '", type, "' is invalid, since ggplot2::geom_", type, "() does not exist", call. = FALSE)
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

#' @importFrom dplyr `%>%` select pull
#' @importFrom certestyle format2
validate_data <- function(df,
                          decimal.mark = ",",
                          big.mark = ".",
                          datalabels.round = 1) {
  
  if (!"_var_y" %in% colnames(df)) {
    # try to find numeric column for y
    first_numeric_col <- names(which(vapply(FUN.VALUE = logical(1), df, is.numeric))[1])
    if (is.na(first_numeric_col)) {
      stop("no numeric column found to use for y", call. = FALSE)
    }
    df <- df %>% 
      mutate(`_var_y` = df %>% pull(first_numeric_col))
  }
  
  # remove datalabels if all are FALSE
  if ("_var_datalabels" %in% colnames(df) && all(df$`_var_datalabels` == FALSE)) {
    df <- df %>% select(-`_var_datalabels`)
  }
  # take datalabels from y axis if all are TRUE
  if ("_var_datalabels" %in% colnames(df) && all(df$`_var_datalabels` == TRUE)) {
    df <- df %>% mutate(`_var_datalabels` = `_var_y`)
  }
  # format datalabels
  if ("_var_datalabels" %in% colnames(df)) {
    df <- df %>% mutate(`_var_datalabels` = format2(`_var_datalabels`,
                                                    decimal.mark = decimal.mark,
                                                    big.mark = big.mark,
                                                    round = datalabels.round,
                                                    force.decimals = TRUE))
  }
  df
}
