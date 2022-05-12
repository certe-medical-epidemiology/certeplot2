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

#' Get Plot Title
#' 
#' Get the title of the plot, or a default value.
#' @param plot a `ggplot2` plot
#' @param valid_filename a [logical] to indicate whether the returned value should be a valid filename, defaults to `TRUE`
#' @param default the default value, if a plot title is absent
#' @importFrom ggplot2 is.ggplot
#' @export
#' @examples 
#' p <- plot2(mtcars, title = "Plotting **mpg** vs. **cyl**!")
#' get_plot_title(p)
#' 
#' get_plot_title(p, valid_filename = FALSE)
#' 
#' p <- plot2(mtcars)
#' # default is a guess:
#' get_plot_title(p)
#' 
#' # unless default is set:
#' get_plot_title(p, default = NA)
#' get_plot_title(p, default = "title")
get_plot_title <- function(plot,
                           valid_filename = TRUE,
                           default = NULL) {
  
  if (!is.ggplot(plot)) {
    stop("`plot` must be a ggplot2 model.", call. = FALSE)
  }
  
  get_default_title <- function(plot, default) {
    if (!is.null(default)) {
      return(default)
    }
    get_mapping <- function(plot) gsub("~", "", sapply(plot$mapping, deparse))
    mapp <- get_mapping(plot)
    val <- mapp[names(mapp) == "y"]
    if (!is.na(val)) {
      val <- paste(val, "per ")
      substr(val, 1, 1) <- toupper(substr(val, 1, 1))
    } else {
      val <- ""
    }
    mapp <- mapp[names(mapp) %in% c("x", "category", "facet")]
    paste0(val, paste(mapp, collapse = ", "))
  }
  
  title <- plot$labels$title
  if (is.null(title)) {
    title <- get_default_title(plot = plot, default = default)
  } else {
    title <- gsub("\"", "***", as.character(title)) |>
      strsplit("***", fixed = TRUE) |>
      unlist()
    title <- title[which(title != "" & title != "paste(" & 
                           title != ", italic(" & title != ", bold(" & title != ", bolditalic(" &
                           title != ")" & title != "), ")]
    title <- gsub("_+", " ", concat(title)) |> 
      trimws()
    if (title == "") {
      title <- get_default_title(plot = plot, default = default)
    }
  }
  
  if (!is.na(title) && isTRUE(valid_filename)) {
    title <- gsub("[ .]+", "_", 
                  gsub("[?!|<>|:/\\*]", "", title)) |> 
      tolower()
  }
  
  caption <- plot$labels$caption
  if (!is.null(caption) && caption %like% "^[0-9a-f]+$") {
    if (is.na(title)) {
      title <- NULL
    }
    title <- trimws(paste(title, "-", caption))
  }
  
  title
}
