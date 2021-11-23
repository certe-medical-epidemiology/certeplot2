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

#' Retrieve Plot Title
#' 
#' Retrieve the title of the plot, or a default value.
#' @param plot a `ggplot2` plot
#' @param valid_filename a [logical] to indicate whether the returned values should be a valid filename, defaults to `TRUE`
#' @param default the default value, if a plot title is absent
#' @importFrom ggplot2 is.ggplot
#' @export
#' @examples 
#' p <- plot2(mtcars, title = "Plotting **mpg** vs. **cyl**!")
#' get_plot_title(p)
#' get_plot_title(p, valid_filename = FALSE)
#' 
#' p <- plot2(mtcars)
#' # default is date and time:
#' get_plot_title(p)
get_plot_title <- function(plot,
                           valid_filename = TRUE,
                           default = format(Sys.time(), "%Y_%m_%d_%H%M%S")) {
  
  if (!is.ggplot(plot)) {
    stop("`plot` must be a ggplot2 model.", call. = FALSE)
  }
  
  title <- plot$labels$title
  if (is.null(title)) {
    title <- default
  } else {
    title <- title %>%
      as.character() %>%
      gsub("\"", "***", .) %>%
      strsplit("***", fixed = TRUE) %>%
      unlist()
    title <- title[which(title != "" & title != "paste(" & 
                           title != ", italic(" & title != ")" & title != "), ")]
    title <- title %>% 
      concat() %>%
      gsub("_+", " ", .) %>% 
      trimws()
    if (valid_filename == TRUE) {
      title <- title %>% 
        gsub("[?!|<>|:/\\*]", "", .) %>% 
        gsub("[ .]+", "_", .) %>% 
        tolower()
    }
    if (title == "") {
      title <- default
    }
  }
  caption <- plot$labels$caption
  if (!is.null(caption)) {
    if (caption %like% "^[0-9a-f]+$") {
      title <- paste0(title, " (", caption, ")")
    }
  }
  title
}
