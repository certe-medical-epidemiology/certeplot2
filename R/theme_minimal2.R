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

#' An Even More Minimal Theme
#' @param ... arguments passed on to [ggplot2::theme()]
#' @importFrom ggplot2 element_text `%+replace%` theme_bw theme margin element_line element_blank unit element_rect
#' @export
#' @examples 
#' plot2(iris)
#' plot2(iris, x = Species)
theme_minimal2 <- function(...) {
  t <- theme_bw(base_size = 11) %+replace%
    theme(
      axis.text.x = element_text(margin = margin(3, 0, 0, 0)),
      axis.title.x = element_text(margin = margin(14, 0, 0, 0)),
      axis.title.y = element_text(margin = margin(0, 14, 0, 0), angle = 90),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_line(size = 0.75, colour = "grey75"),
      axis.ticks.length = unit(2, "pt"),
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.key.size = unit(11, "pt"), # squares and lines left to legend text
      legend.text = element_text(size = unit(9, "pt"), # text itself
                                 margin = margin(l = 1, r = 6, unit = "pt")), # left and right of text
      legend.title = element_text(face = "bold", size = unit(10, "pt")),
      panel.background = element_rect(fill = "white", linetype = 0),
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(size = 0.375, colour = "grey75"),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_line(size = 0.25, colour = "grey85"),
      axis.line = element_line(size = 0.375, colour = "grey75"),
      axis.line.y = element_blank(),
      plot.margin = unit(c(5,   # top
                           5,   # right
                           5,   # bottom
                           5),  # left
                         units = "pt"),
      plot.background = element_blank(),
      plot.subtitle = element_text(size = unit(11, "pt"),
                                    #margin = margin(0, 0, ifelse(has_subtitle == TRUE, 15, 7), 0),
                                    hjust = 0.5,
                                   margin = margin(5, 0, 10, 0)),
      plot.title = element_text(size = unit(13, "pt"),
                                 #margin = margin(0, 0, ifelse(has_subtitle == TRUE, 7, 15), 0),
                                 hjust = 0.5),
      plot.caption = element_text(colour = "grey50",
                                   size = unit(10, "pt"),
                                   hjust = 1),
      plot.tag = element_text(size = unit(14, "pt"),
                               margin = margin(0, 0, 0, 0),
                               hjust = 0,
                               colour = "black",
                               face = "bold"),
      # for facet (facet_wrap):
      strip.background = element_rect(colour = "#FFFFFF00"),
      strip.switch.pad.wrap = unit(10, "pt"),
      strip.placement = "outside",
      complete = TRUE)
 
  if (length(list(...)) > 0) {
    t <- t %+replace%
      theme(...)
  }
  
  t
}
