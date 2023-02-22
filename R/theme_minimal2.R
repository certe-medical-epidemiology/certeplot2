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
#' 
#' This `ggplot2` theme provide even more white area and less clutter than [`theme_minimal()`][ggplot2::theme_minimal()].
#' @param ... arguments passed on to [ggplot2::theme()]
#' @param font_colour,font_colour2 defaults colours for the plot texts
#' @importFrom ggplot2 element_text `%+replace%` theme_bw theme margin element_line element_blank unit element_rect
#' @export
#' @examples 
#' plot2(iris)
#' plot2(admitted_patients, x = hospital, category = gender)
theme_minimal2 <- function(...,
                           font_colour = getOption("plot2.font_colour", "grey20"),
                           font_colour2 = getOption("plot2.font_colour2", "grey35")) {
  t <- theme_bw(base_size = 11) %+replace%
    theme(
      axis.text.x = element_text(margin = margin(3, 0, 0, 0), colour = font_colour2),
      axis.text.y = element_text(colour = font_colour2),
      axis.title.x = element_text(margin = margin(14, 0, 0, 0), colour = font_colour),
      axis.title.y = element_text(margin = margin(0, 14, 0, 0), angle = 90, colour = font_colour),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_line(size = 0.75, colour = "grey75"),
      axis.ticks.length = unit(2, "pt"),
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.key.size = unit(11, "pt"), # squares and lines left to legend text
      # values in legend:
      legend.text = element_text(size = unit(9, "pt"),
                                 colour = font_colour2,
                                 margin = margin(l = 1, r = 6, unit = "pt")), # left and right of text
      legend.title = element_text(face = "bold", size = unit(10, "pt"), colour = font_colour),
      panel.background = element_rect(fill = "white", linetype = 0),
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(size = 0.375, colour = "grey75"),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_line(size = 0.25, colour = "grey85"),
      axis.line = element_line(size = 0.375, colour = "grey75"),
      axis.line.y = element_blank(),
      plot.margin = unit(c(5, 12, 5, 5), units = "pt"),
      plot.background = element_rect(fill = "white", linetype = 0),
      plot.subtitle = element_text(size = unit(11, "pt"),
                                   hjust = 0.5,
                                   margin = margin(0, 0, 10, 0),
                                   colour = font_colour2),
      plot.title = element_text(size = unit(13, "pt"),
                                hjust = 0.5,
                                margin = margin(0, 0, 10, 0),
                                colour = font_colour),
      plot.caption = element_text(colour = font_colour2,
                                  size = unit(10, "pt"),
                                  hjust = 1),
      plot.tag = element_text(size = unit(14, "pt"),
                              margin = margin(0, 0, 0, 0),
                              hjust = 0,
                              colour = font_colour,
                              face = "bold"),
      # for facet (ggh4x::facet_wrap2):
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
