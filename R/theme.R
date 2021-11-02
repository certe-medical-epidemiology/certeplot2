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
#' @importFrom ggplot2 element_text `%+replace%` theme_bw theme margin element_line element_blank unit element_rect
#' @importFrom dplyr case_when
#' @export
#' @examples 
#' plot2(iris)
#' plot2(iris, x = Species)
theme_minimal2 <- function(subtitle.colour = "black",
                           x.lbl.angle = 0,
                           x.lbl.align = 0.5,
                           horizontal = FALSE,
                           text.font_family = "Verdana",
                           legend.position = "top",
                           legend.italic = FALSE,
                           text.factor = 1,
                           facet.fill = NA,
                           facet.bold = TRUE,
                           facet.italic = FALSE,
                           facet.size = 10,
                           facet.margin = 8,
                           has_subtitle = FALSE,
                           ...) {
  
  legend.position <- validate_legend.position(legend.position)
  if (legend.italic == TRUE) {
    legend.italic <- "italic"
  } else {
    legend.italic <- NULL
  }
  
  t <- theme_bw(base_size = 11 * text.factor,
                base_family = text.font_family) %+replace%
    theme(
      axis.text.x = element_text(angle = x.lbl.angle, hjust = x.lbl.align, margin = margin(3, 0, 0, 0)),
      # # getallen van y-as op de lijn plaatsen, links uitgelijnd
      # axis.text.y = element_text(margin = margin(l = 10 * text.factor,
      #                                            r = -21 * text.factor, unit = "pt"),
      #                            hjust = 0,
      #                            vjust = -0.5),
      axis.title.x = element_text(margin = margin(14, 0, 0, 0)),
      axis.title.y = element_text(margin = margin(0, 14, 0, 0), angle = 90),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_line(size = 0.75, colour = "grey75"),
      axis.ticks.length = unit(2, "pt"),
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.key.size = unit(11 * text.factor, "pt"), # blokjes en lijnen links van tekst in legenda
      legend.text = element_text(size = unit(9 * text.factor, "pt"), # tekst zelf
                                 margin = margin(l = 1, r = 6, unit = "pt"), # ruimte links en rechts van tekst
                                 face = legend.italic),
      legend.position = legend.position,
      legend.title = element_text(face = "bold", size = unit(10 * text.factor, "pt")),
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(size = 0.375, colour = "grey75"),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_line(size = 0.25, colour = "grey85"),
      axis.line = element_line(size = 0.375, colour = "grey75"),
      axis.line.y = element_blank(),
      plot.margin = unit(c(5,                                  # top
                           ifelse(horizontal == TRUE, 25, 5),  # right
                           5,                                  # bottom
                           5),                                 # left
                         units = "pt"),
      plot.background = element_blank(),
      plot.subtitle = element_text(size = unit(11 * text.factor, "pt"),
                                    margin = margin(0, 0, ifelse(has_subtitle == TRUE, 15, 7), 0),
                                    hjust = 0.5,
                                    colour = subtitle.colour),
      plot.title = element_text(size = unit(13 * text.factor, "pt"),
                                 margin = margin(0, 0, ifelse(has_subtitle == TRUE, 7, 15), 0),
                                 hjust = 0.5,
                                 colour = "black"),
      plot.caption = element_text(colour = "grey50",
                                   size = unit(10 * text.factor, "pt"),
                                   hjust = 1),
      plot.tag = element_text(size = unit(14 * text.factor, "pt"),
                               margin = margin(0, 0, 0, 0),
                               hjust = 0,
                               colour = "black",
                               face = "bold"),
      # voor facet (facet_wrap):
      strip.background = element_rect(fill = facet.fill, colour = "#FFFFFF00"),
      strip.text = element_text(face = case_when(facet.bold & facet.italic ~ "bold.italic",
                                                 facet.bold ~ "bold",
                                                 facet.italic ~ "italic",
                                                 TRUE ~ "plain"),
                                size = unit(facet.size * text.factor, "pt"),
                                margin = margin(t = facet.margin, b = facet.margin / 2)),
      strip.switch.pad.wrap = unit(10 * text.factor, "pt"),
      strip.placement = "outside",
      complete = TRUE)
  
  if (x.lbl.angle < 90 & x.lbl.angle > 10) {
    t <- t +
      theme(axis.text.x = element_text(margin = margin(-5, 0, 0, 0)))
  }
  
  if (horizontal == TRUE) {
    t <- t %+replace%
      theme(panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_line(size = 0.375, colour = "grey75"),
            panel.grid.minor.x = element_line(size = 0.25, colour = "grey85"),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_line(size = 0.75, colour = "grey75"),
            # tekst op y-as (wat x-as was) rechts align en minder ruimte geven
            axis.text.y = element_text(hjust = 1.0, vjust = 0.3, margin = margin(0, 3, 0, 0)),
            axis.text.x = element_text(margin = margin(0, 0, 0, 0)),
            axis.line.y = element_line(size = 0.375, colour = "grey75"),
            axis.line.x = element_blank())
  }
  
  if (length(list(...)) > 0) {
    t <- t %+replace% theme(...)
  }
  
  t
}
