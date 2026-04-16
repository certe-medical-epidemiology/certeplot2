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

globalVariables(c(".",
                  "ab",
                  "antibiotic",
                  "cases",
                  "cluster",
                  "coverage",
                  "day_in_period",
                  "interpretation",
                  "isolates",
                  "lower_ci",
                  "markdown",
                  "md_to_expression",
                  "mo",
                  "move_layer",
                  "moving_avg",
                  "moving_avg_limit",
                  "moving_avg_max",
                  "n_cases",
                  "name",
                  "period",
                  "period_date",
                  "period_txt",
                  "plot2_warning",
                  "R",
                  "SI",
                  "syndromic_group",
                  "total",
                  "upper_ci",
                  "validate_markdown",
                  "validate_title",
                  "value",
                  "xmax",
                  "xmin"))

#' @importFrom certestyle colourpicker
interactive <- function() {
  plot2::create_interactively(logo_path = system.file("logo.svg", package = "certeplot2"),
                              pretty_labels = FALSE,
                              hide_generated_code = FALSE,
                              hide_export_buttons = TRUE,
                              upload_tab = FALSE,
                              css_code = paste0(
                                "#sidebar {",
                                "  background-color: ", colourpicker("certeblauw6"), ";",
                                "}",
                                ".irs-to, .irs-single {",
                                "  background-color: ", colourpicker("certeblauw"), " !important;",
                                "}",
                                ".irs--shiny .irs-bar {",
                                "  border-top: 1px solid ", colourpicker("certeblauw"), " !important;",
                                "  border-bottom: 1px solid ", colourpicker("certeblauw"), " !important;",
                                "  background-color: ", colourpicker("certeblauw"), " !important;",
                                "}",
                                "a, .optgroup-header {",
                                "  color: ", colourpicker("certeblauw"), " !important;",
                                "}",
                                ".selectize-dropdown .selected {",
                                "  background-color: ", colourpicker("certeblauw"), " !important;",
                                "}",
                                "#copy_btn {",
                                "  border-color: ", colourpicker("certeblauw"), " !important;",
                                "  background-color: ", colourpicker("certeblauw"), " !important;",
                                "}",
                                ".form-check-input:checked, .shiny-input-container .checkbox input:checked, .shiny-input-container .checkbox-inline input:checked, .shiny-input-container .radio input:checked, .shiny-input-container .radio-inline input:checked {",
                                "  background-color: ", colourpicker("certeblauw"), ";",
                                "  border-color: ", colourpicker("certeblauw"), ";",
                                "}", collapse = "\n"))
}
