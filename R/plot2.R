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

#' Conveniently Create a New `ggplot`
#'
#' @description  The [plot2()] function is a convenient wrapper around many [`ggplot2`][ggplot2::ggplot()] functions. By design, the `ggplot2` package requires users to use a lot of functions, while the [plot2()] function auto-determines needs and only requires to define arguments in one single function.
#' 
#' See [plot2-methods] for all implemented methods for different object classes.
#' @param .data data to plot
#' @param x plotting 'direction': the x axis
#' @param y values to use for plotting along the y axis - can also be [n()] for the row count or a calculation of a variable, e.g. `max(column1)`, `median(column2)` or `n_distinct(person_id)`
#' @param category plotting 'direction': the category (called 'fill' and 'colour' in `ggplot2`)
#' @param facet plotting 'direction': the facet
#' @param type type of visualisation to use. This can be:
#' 
#' * A `ggplot2` geom name, all geoms are supported (including [`geom_blank()`][ggplot2::geom_blank()]). Full function names can be used (e.g., `"geom_histogram"`), but they can also be abbreviated (e.g., `"h"`, `"hist"`). These geoms can be abbreviated by their first character: area (`"a"`), boxplot (`"b"`), column (`"c"`), histogram (`"h"`), jitter (`"j"`), line (`"l"`), point (`"p"`), ribbon (`"r"`), violin (`"v"`). **Please note:** in `ggplot2`, 'bars' and 'columns' are equal, while it is common to many people that 'bars' are oriented horizontally and 'columns' are oriented vertically. For this reason, `type = "bar"` will set `type = "col"` and `horizontal = TRUE`.
#' 
#' * A shortcut. There is currently one supported shortcut: `"barpercent"`, which will set `type = "col"` and `horizontal = TRUE` and `x.max_items = 10` and `x.sort = "freq-desc"` and `datalabels.format = "%n (%p)"`.
#' 
#' * Left blank. In this case, the type will be determined automatically: `"boxplot"` if there is no X axis or if the length of unique values per X axis item is at least 3, `"point"` if both the Y and X axes are numeric, and `"col"` otherwise. Use `type = "blank"` or `type = "geom_blank"` to *not* print a geom.
#' @param x.title text to show on the x axis
#' @param y.title text to show on the y axis
#' @param category.title title of the legend (if `legend.title` is not set), defaults to `TRUE` if the legend items are numeric.
#' @param title title to show
#' @param subtitle subtitle to show
#' @param caption caption to show
#' @param tag tag to show
#' @param title.linelength maximum number of characters per line in the title, before a linebreak occurs
#' @param title.colour text colour of the title
#' @param subtitle.linelength maximum number of characters per line in the subtitle, before a linebreak occurs
#' @param subtitle.colour text colour of the subtitle
#' @param na.replace character to put in place of `NA` values if `na.rm = FALSE`
#' @param na.rm remove `NA` values from showing in the plot
#' @param facet.position,facet.fill,facet.bold,facet.italic,facet.size,facet.margin,facet.repeat_lbls_x,facet.repeat_lbls_y,facet.fixed_y,facet.drop,facet.nrow,facet.relative settings for the plotting direction `facet`
#' @param x.date_breaks breaks to use when the x axis contains dates, will be determined automatically if left blank
#' @param x.date_labels labels to use when the x axis contains dates, will be determined automatically if left blank
#' @param category.focus a value of `category` that should be highlighted, meaning that all other values in `category` will be greyed out. This can also be a numeric value between 1 and the length of unique values of `category`, e.g. `category.focus = 2` to focus on the second legend item.
#' @param colour colour(s) to set, will be evaluated with [`colourpicker()`][certestyle::colourpicker()] and defaults to Certe colours. This can also be one of the viridis colours for a continuous scale: `"viridis"`, `"magma"`, `"inferno"`, `"plasma"`, `"cividis"`, `"rocket"`, `"mako"` or `"turbo"`. This can also be a named vector to match values of `category`, see *Examples*. Using a named vector can also be used to manually sort the values of `category`.
#' @param colour_fill colour(s) to be used for filling, will be determined automatically if left blank and will be evaluated with [`colourpicker()`][certestyle::colourpicker()]
#' @param x.lbl_angle angle to use for the x axis in a counter-clockwise direction (i.e., a value of `90` will orient the axis labels from bottom to top, a value of `270` will orient the axis labels from top to bottom)
#' @param x.lbl_align alignment for the x axis between `0` (left aligned) and `1` (right aligned)
#' @param x.lbl_italic [logical] to indicate whether the x labels should in in *italics*
#' @param x.remove [logical] to indicate whether the x labels and title should be removed
#' @param x.position position of the x axis, defaults to `"bottom"`
#' @param x.breaks breaks function or numeric vector to use for the x axis
#' @param x.breaks_n number of breaks to use for the x axis
#' @param x.trans transformation function to use for the x axis, e.g. `"log2"`
#' @param x.expand expansion to use for the x axis, can be length 1 or 2
#' @param x.limits limits to use for the x axis, can be length 1 or 2. Use `NA` for the highest or lowest value in the data, e.g. `x.limits = c(0, NA)` to have the scale start at zero.
#' @param x.character a [logical] to indicate whether the values of the x axis should be forced to [character]. The default is `FALSE`, except for years (x values between 2000 and 2050)
#' @param x.drop [logical] to indicate whether factor levels should be dropped
#' @param y.remove a [logical] to indicate whether the y labels and title should be removed
#' @param y.24h a [logical] to indicate whether the y labels and breaks should be formatted as 24-hour sequences
#' @param y.age a [logical] to indicate whether the y labels and breaks should be formatted as ages in years
#' @param y.percent a [logical] to indicate whether the y labels should be formatted as percentages
#' @param y.percent_break number of percentages on which the y axis should have breaks
#' @param y.breaks a breaks function or numeric vector to use for the y axis
#' @param y.limits limits to use for the y axis, can be length 1 or 2. Use `NA` for the highest or lowest value in the data, e.g. `y.limits = c(0, NA)` to have the scale start at zero.
#' @param y.labels a labels function or character vector to use for the y axis
#' @param y.expand expansion to use for the y axis, can be length 1 or 2
#' @param y.trans a transformation function to use for the y axis, e.g. `"log2"`
#' @param y.position position of the x axis, defaults to `"left"`
#' @param category.labels,category.percent,category.breaks,category.expand,category.midpoint,category.trans settings for the plotting direction `category`.
#' @param category.limits limits to use for a numeric category, can be length 1 or 2. Use `NA` for the highest or lowest value in the data, e.g. `category.limits = c(0, NA)` to have the scale start at zero.
#' @param x.max_items,category.max_items,facet.max_items number of maximum items to use, defaults to infinite. All other values will be grouped and summarised using the `summarise_function` function. **Please note:** the sorting will be applied first, allowing to e.g. plot the top *n* most frequent values of the x axis by combining `x.sort = "freq-desc"` with `x.max_items =` *n*.
#' @param x.max_txt,category.max_txt,facet.max_txt the text to use of values not included number of `*.max_items`. The placeholder `%n` will be replaced with the outcome of the `summarise_function` function, the placeholder `%p` will be replaced with the percentage.
#' @param x.sort,category.sort,facet.sort sorting of the plotting direction, defaults to `TRUE`, except for continuous values on the x axis (such as dates and numbers). Applying one of the sorting methods will transform the values to an ordered [factor], which `ggplot2` uses to orient the data. Valid values are:
#' 
#' - `TRUE`: sort [factor]s on their levels, otherwise sort ascending on alphabet, while maintaining numbers in the text (*numeric* sort)
#' - `FALSE`: sort according to the order in the data
#' - `NULL`: do not sort/transform at all
#' - `"asc"` or `"alpha"`: sort as `TRUE`
#' - `"desc"`: sort [factor]s on their [reversed][rev()] levels, otherwise sort descending on alphabet, while maintaining numbers in the text (*numeric* sort)
#' - `"order"` or `"inorder"`: sort as `FALSE`
#' - `"freq"` or `"freq-desc"`: sort descending according to the frequencies of `y` computed by `summarise_function` (highest value first)
#' - `"freq-asc"`: sort ascending according to the frequencies of `y` computed by `summarise_function` (lowest value first)
#' @param datalabels variables or character vector to use as datalabels - if left blank, will take the first character column in 'sf' data, and values of `y` otherwise. It will print a maximum of 50 labels at default, which can be enforced by explicitly adding `datalabels = TRUE`.
#' @param datalabels.round number of digits to round the datalabels, applies to both `"%n"` and `"%p"` for replacement (see `datalabels.format`)
#' @param datalabels.format format to use for datalabels - `"%n"` will be replaced by the count number, `"%p"` will be replaced by the percentage of the total count. Use `datalabels.format = NULL` to not transform the datalabels.
#' @param datalabels.colour,datalabels.colour_fill,datalabels.size,datalabels.angle settings for the datalabels
#' @param decimal.mark decimal mark, defaults to Dutch use (a comma)
#' @param big.mark thousands separator, defaults to Dutch use (a full stop)
#' @param summarise_function a [function] to use if the data has to be summarised, see *Examples*
#' @param stacked a [logical] to indicate that values must be stacked
#' @param stackedpercent a [logical] to indicate that values must be 100% stacked
#' @param horizontal a [logical] to turn the plot 90 degrees using [`coord_flip()`][ggplot2::coord_flip()]. This option also updates some theme options, so that e.g., `x.lbl_italic` will still apply to the original x axis.
#' @param reverse a [logical] to reverse the *values* of `category`. Use `legend.reverse` to reverse the *legend* of `category`.
#' @param smooth a [logical] to add a smooth. In histograms, this will add the density count as an overlaying line (default: `TRUE`). In all other cases, a smooth will be added using [`geom_smooth()`][ggplot2::geom_smooth()] (default: `FALSE`).
#' @param smooth.method,smooth.formula,smooth.se,smooth.level,smooth.alpha,smooth.size,smooth.linetype settings for `smooth`
#' @param size size of the geom
#' @param linetype linetype of the geom, only suitable for geoms that draw lines
#' @param binwidth width of bins (only useful for `geom = "histogram"`), can be specified as a numeric value or as a function that calculates width from `x`, see [`geom_histogram()`][ggplot2::geom_histogram()]. It defaults to approx. `diff(range(x))` divided by 12 to 22 based on the data.
#' @param width width of the geom
#' @param jitter_seed seed (randomisation factor) to be set when using `type = "jitter"`
#' @param violin_scale scale to be set when using `type = "violin"`, can also be set to `"area"`
#' @param legend.position position of the legend, must be `"top"`, `"right"`, `"bottom"`, `"left"` or `"none"` (of `NA` or `NULL`), can be abbreviated. Defaults to `"right"` for numeric `category` values and 'sf' plots, and `"top"` otherwise.
#' @param legend.title title of the legend (if `category.title` is not set), defaults to `TRUE` if the legend items are numeric.
#' @param legend.reverse,legend.barheight,legend.barwidth,legend.nbin,legend.italic other settings for the legend
#' @param zoom a [logical] to indicate if the plot should be scaled to the data, i.e., not having the x and y axes to start at 0
#' @param sep separator character to use if multiple columns are given to either of the three directions: `x`, `category` and `facet`, e.g. `facet = c(column1, column2)`
#' @param print a [logical] to indicate if the result should be [printed][print()] instead of just returned
#' @param text_factor text factor to use, which will apply to all texts shown in the plot
#' @param family font family to use, can be set with `options(plot2.family = "...")`
#' @param theme a valid `ggplot2` [theme][ggplot2::theme()] to apply, or `NULL` to use the default [`theme_grey()`][ggplot2::theme_grey()]. This argument accepts themes (e.g., `theme_bw()`), functions (e.g., `theme_bw`) and characters themes (e.g., `"theme_bw"`). Can be set with `options(plot2.theme = "...")`.
#' @param markdown a [logical] to turn all labels and titles into markdown-supported labels, by extending their S3 classes with [`"element_markdown"`][ggtext::element_markdown()], a feature of the `ggtext` package
#' @param taxonomy_italic a [logical] to transform all labels and titles into italics that are in the `microorganisms` data set of the `AMR` package
#' @param ... arguments passed on to methods
#' @details The [plot2()] function is a convenient wrapper around many [`ggplot2`][ggplot2::ggplot()] functions such as [`ggplot()`][ggplot2::ggplot()], [`aes()`][ggplot2::aes()], [`geom_col()`][ggplot2::geom_col()], [`facet_wrap()`][ggplot2::facet_wrap()], [`labs()`][ggplot2::labs()], etc., and provides:
#'   * Writing as few lines of codes as possible
#'   * Easy plotting in three 'directions': `x` (the regular x axis), `category` (replaces 'fill' and 'colour') and `facet`
#'   * Automatic setting of these 'directions' based on the input data
#'   * Setting in-place calculations for all plotting direction and even `y`
#'   * Easy way for sorting data in many ways (such as on alphabet, numeric value, frequency, original data order), by setting a single argument for the 'direction': `x.sort`, `category.sort` and `facet.sort`
#'   * Easy limiting values, e.g. by setting `x.max_items = 5` or `category.max_items = 5`
#'   * Markdown support for any label, with any theme
#'   * An extra clean, minimalistic theme with a lot of whitespace (but without unnecessary margins) that is ideal for printing: `theme_minimal2()`
#'   * Some conveniences from Microsoft Excel:
#'     * The y axis starts at 0 if possible
#'     * The y scale expands at the top to be better able to interpret all data points
#'     * Date breaks can be written in a human-readable format (such as "d mmm yyyy")
#'     * Labels with data values can easily be printed and are automatically determined
#'   * Support for any `ggplot2` extension based on [ggplot2::fortify()]
#'   
#' The `ggplot2` package in conjunction with the `tidyr`, `forcats` and `cleaner` packages can provide above functionalities, but the goal of the [plot2()] function is to generalise this into one function. The generic [plot2()] function currently has `r length(formals(plot2)) - 1` arguments, all with a default value. **Less typing, faster coding.**
#' @return a `ggplot` object
#' @export
#' @examples
#' head(iris)
#' 
#' # no variables determined, so plot2() will try for itself -
#' # the type will be points since the first two variables are numeric
#' iris %>%
#'   plot2()
#' 
#' # ggplot2 defaults (more or less):
#' iris %>% 
#'   plot2(theme = NULL,
#'         zoom = TRUE,
#'         legend.title = TRUE,
#'         legend.position = "right")
#' 
#' # if x and y are set, no additional mapping will be set:
#' iris %>% 
#'   plot2(Sepal.Width, Sepal.Length)
#' iris %>% 
#'   plot2(Species, Sepal.Length)
#' 
#' # the arguments are in this order: x, y, category, facet
#' iris %>% 
#'   plot2(Sepal.Length, Sepal.Width, Petal.Length, Species)
#' 
#' iris %>% 
#'   plot2(Sepal.Length, Sepal.Width, Petal.Length, Species,
#'         colour = "viridis") # set the viridis colours
#'       
#' iris %>% 
#'   plot2(Sepal.Length, Sepal.Width, Petal.Length, Species,
#'         colour = c("white", "red", "black")) # set own colours
#'   
#' admitted_patients
#' 
#' # the arguments are in this order: x, y, category, facet
#' admitted_patients %>%
#'   plot2(hospital, age)
#' 
#' admitted_patients %>%
#'   plot2(hospital, age, gender)
#'   
#' admitted_patients %>%
#'   plot2(hospital, age, gender, ward)
#'   
#' # or use any function for y
#' admitted_patients %>%
#'   plot2(hospital, median(age), gender, ward)
#' admitted_patients %>%
#'   plot2(hospital, n(), gender, ward)
#'
#' admitted_patients %>%
#'   plot2(x = hospital,
#'         category = gender,
#'         colour = c("F" = "green4", "M" = "red4"),
#'         colour_fill = "lightyellow",
#'         y.age = TRUE)
#'         
#' admitted_patients %>%
#'   plot2(age, type = "hist")
#'  
#' # the default type is column, datalabels are automatically
#' # set in non-continuous types:
#' admitted_patients %>% 
#'   plot2(hospital, n(), gender)
#'   
#' admitted_patients %>% 
#'   plot2(hospital, n(), gender,
#'         stacked = TRUE)
#'         
#' admitted_patients %>% 
#'   plot2(hospital, n(), gender,
#'         stackedpercent = TRUE)
#'  
#' # sort on any direction:       
#' admitted_patients %>% 
#'   plot2(hospital, n(), gender,
#'         x.sort = "freq-asc",
#'         stacked = TRUE)
#' 
#' # plot2() supports all S3 extensions available through
#' # ggplot2::fortify(), such as regression models:
#' mtcars %>% 
#'   lm(mpg ~ hp, data = .) %>% 
#'   plot2(x = mpg ^ -3,
#'         y = hp ^ 2,
#'         smooth = TRUE,
#'         smooth.method = "lm",
#'         smooth.formula = "y ~ log(x)",
#'         title = "Titles/captions *support* **markdown**",
#'         subtitle = "Axis titles contain the square notation: ^2")
#' 
#' # plot2() also has various other S3 implementations:
#' 
#' # QC plots, according to e.g. Nelson's Quality Control Rules
#' if (require("certestats", warn.conflicts = FALSE)) {
#'   rnorm(250) %>% 
#'     qc_test() %>% 
#'     plot2()
#' }
#'         
#' # sf objects (geographic plots, 'simple features') are also supported
#' if (require("sf")) {
#'   netherlands %>% 
#'     plot2(datalabels = paste0(province, "\n", round(area_km2)))
#' }
#' 
#' # Antimicrobial resistance (AMR) data analysis
#' if (require("AMR")) {
#'   options(AMR_locale = "nl")
#'   
#'   example_isolates %>% 
#'     .[, c("mo", penicillins())] %>% 
#'     bug_drug_combinations(FUN = mo_gramstain) %>%
#'     plot2(y.percent_break = 0.25)
#' }
plot2 <- function(.data,
                  x = NULL,
                  y = NULL,
                  category = NULL,
                  facet = NULL,
                  type = NULL,
                  x.title = NULL,
                  y.title = NULL,
                  category.title = NULL,
                  title = NULL,
                  subtitle = NULL,
                  caption = NULL,
                  tag = NULL,
                  title.linelength = 60,
                  title.colour = "black",
                  subtitle.linelength = 60,
                  subtitle.colour = "grey35",
                  na.replace = "(??)",
                  na.rm = FALSE,
                  facet.position = "top",
                  facet.fill = NULL,
                  facet.bold = TRUE,
                  facet.italic = FALSE,
                  facet.size = 10,
                  facet.margin = 8,
                  facet.repeat_lbls_x = TRUE,
                  facet.repeat_lbls_y = FALSE,
                  facet.fixed_y = FALSE,
                  facet.drop = FALSE,
                  facet.nrow = NULL,
                  facet.relative = FALSE,
                  x.date_breaks = NULL,
                  x.date_labels = NULL,
                  category.focus = NULL,
                  colour = "certe",
                  colour_fill = NULL,
                  x.lbl_angle = 0,
                  x.lbl_align = NULL,
                  x.lbl_italic = FALSE,
                  x.remove = FALSE,
                  x.position = "bottom",
                  x.max_items = Inf,
                  x.max_txt = "(rest, x %n)",
                  category.max_items = Inf,
                  category.max_txt = "(rest, x %n)",
                  facet.max_items = Inf,
                  facet.max_txt = "(rest, x %n)",
                  x.breaks = NULL,
                  x.breaks_n = NULL,
                  x.trans = "identity",
                  x.expand = 0.5,
                  x.limits = NULL,
                  x.character = NULL,
                  x.drop = FALSE,
                  y.remove = FALSE,
                  y.24h = FALSE,
                  y.age = FALSE,
                  y.percent = FALSE,
                  y.percent_break = 0.1,
                  y.breaks = NULL,
                  y.limits = NULL,
                  y.labels = NULL,
                  y.expand = 0.25,
                  y.trans = "identity",
                  y.position = "left",
                  category.labels = NULL,
                  category.percent = FALSE,
                  category.breaks = NULL,
                  category.limits = NULL,
                  category.expand = 0,
                  category.midpoint = NULL,
                  category.trans = "identity",
                  x.sort = NULL,
                  category.sort = TRUE,
                  facet.sort = TRUE,
                  datalabels = TRUE,
                  datalabels.round = ifelse(y.percent, 2, 1),
                  datalabels.format = "%n",
                  datalabels.colour = "grey25",
                  datalabels.colour_fill = NULL,
                  datalabels.size = (3 * text_factor),
                  datalabels.angle = 0,
                  decimal.mark = ",",
                  big.mark = ifelse(decimal.mark == ",", ".", ","),
                  summarise_function = base::sum,
                  stacked = FALSE,
                  stackedpercent = FALSE,
                  horizontal = FALSE,
                  reverse = horizontal,
                  smooth = NULL,
                  smooth.method = NULL,
                  smooth.formula = NULL,
                  smooth.se = TRUE,
                  smooth.level = 0.95,
                  smooth.alpha = 0.15,
                  smooth.size = 0.75,
                  smooth.linetype = 3,
                  size = NULL,
                  linetype = 1,
                  binwidth = NULL,
                  width = NULL,
                  jitter_seed = NA,
                  violin_scale = "count",
                  legend.position = NULL,
                  legend.title = NULL, # will become TRUE in numeric categories if left NULL
                  legend.reverse = FALSE,
                  legend.barheight = 6,
                  legend.barwidth = 1.5,
                  legend.nbin = 300,
                  legend.italic = FALSE,
                  zoom = FALSE,
                  sep = " / ",
                  print = FALSE,
                  text_factor = 1,
                  family = getOption("plot2.family"),
                  theme = getOption("plot2.theme", "theme_minimal2"),
                  markdown = TRUE,
                  taxonomy_italic = FALSE,
                  ...) {
  UseMethod("plot2")
}

#' @importFrom dplyr `%>%` mutate vars group_by across summarise select matches
#' @importFrom forcats fct_relabel
#' @importFrom ggplot2 ggplot aes aes_string labs stat_boxplot scale_colour_manual scale_fill_manual coord_flip geom_smooth geom_density guides guide_legend scale_x_discrete
#' @importFrom certestyle format2 font_red font_black font_blue
plot2_exec <- function(.data,
                       x,
                       y,
                       category,
                       facet,
                       type,
                       x.title,
                       y.title,
                       category.title,
                       title,
                       subtitle,
                       caption,
                       tag,
                       title.linelength,
                       title.colour,
                       subtitle.linelength,
                       subtitle.colour,
                       na.replace,
                       na.rm,
                       facet.position,
                       facet.fill,
                       facet.bold,
                       facet.italic,
                       facet.size,
                       facet.margin,
                       facet.repeat_lbls_x,
                       facet.repeat_lbls_y,
                       facet.fixed_y,
                       facet.drop,
                       facet.nrow,
                       facet.relative,
                       x.date_breaks,
                       x.date_labels,
                       category.focus,
                       colour,
                       colour_fill,
                       x.lbl_angle,
                       x.lbl_align,
                       x.lbl_italic,
                       x.remove,
                       x.position,
                       x.max_items,
                       x.max_txt,
                       category.max_items,
                       category.max_txt,
                       facet.max_items,
                       facet.max_txt,
                       x.breaks,
                       x.breaks_n,
                       x.trans,
                       x.expand,
                       x.limits,
                       x.character,
                       x.drop,
                       y.remove,
                       y.24h,
                       y.age,
                       y.percent,
                       y.percent_break,
                       y.breaks,
                       y.limits,
                       y.labels,
                       y.expand,
                       y.trans,
                       y.position,
                       category.labels,
                       category.percent,
                       category.breaks,
                       category.limits,
                       category.expand,
                       category.midpoint,
                       category.trans,
                       x.sort,
                       category.sort,
                       facet.sort,
                       datalabels,
                       datalabels.round,
                       datalabels.colour,
                       datalabels.format,
                       datalabels.colour_fill,
                       datalabels.size,
                       datalabels.angle,
                       decimal.mark,
                       big.mark,
                       summarise_function,
                       stacked,
                       stackedpercent,
                       horizontal,
                       reverse,
                       smooth,
                       smooth.method,
                       smooth.formula,
                       smooth.se,
                       smooth.level,
                       smooth.alpha,
                       smooth.size,
                       smooth.linetype,
                       size,
                       linetype,
                       binwidth,
                       width,
                       jitter_seed,
                       violin_scale,
                       legend.position,
                       legend.title,
                       legend.reverse,
                       legend.barheight,
                       legend.barwidth,
                       legend.nbin,
                       legend.italic,
                       zoom,
                       sep,
                       print,
                       text_factor,
                       family,
                       theme,
                       markdown,
                       taxonomy_italic,
                       ...) {
  
  dots <- list(...)
  dots_unknown <- names(dots) %unlike% "^_(label[.]|misses[.]|sf.column|summarise_fn_name)"
  if (any(dots_unknown)) {
    plot2_warning(ifelse(sum(dots_unknown) == 1,
                         "This argument is unknown and was ignored: ",
                         "These arguments are unknown and were ignored: "),
                  paste0(font_red(names(dots[dots_unknown]), collapse = NULL), collapse = font_black(", ")))
  }
  
  # record missing arguments ----
  misses_x <- isTRUE(dots$`_misses.x`)
  misses_y <- isTRUE(dots$`_misses.y`)
  misses_category <- isTRUE(dots$`_misses.category`)
  misses_facet <- isTRUE(dots$`_misses.facet`)
  misses_datalabels <- isTRUE(dots$`_misses.datalabels`)
  misses_colour_fill <- isTRUE(dots$`_misses.colour_fill`)
  misses_x.title <- isTRUE(dots$`_misses.x.title`)
  misses_y.title <- isTRUE(dots$`_misses.y.title`)
  misses_title <- isTRUE(dots$`_misses.title`)
  misses_subtitle <- isTRUE(dots$`_misses.subtitle`)
  misses_tag <- isTRUE(dots$`_misses.tag`)
  misses_caption <- isTRUE(dots$`_misses.caption`)
  misses_zoom <- isTRUE(dots$`_misses.zoom`)
  misses_y.percent <- isTRUE(dots$`_misses.y.percent`)
  
  # no observations, return empty plot ----
  if (NROW(.data) == 0) {
    plot2_warning("No observations, returning an empty plot")
    p <- ggplot() +
      validate_theme(theme = theme,
                     type = "",
                     markdown = markdown,
                     text_factor = text_factor,
                     family = family,
                     horizontal = horizontal,
                     x.remove = x.remove,
                     x.lbl_angle = x.lbl_angle,
                     x.lbl_align = x.lbl_align,
                     x.lbl_italic = x.lbl_italic,
                     facet.fill = facet.fill,
                     facet.bold = facet.bold,
                     facet.italic = facet.italic,
                     facet.size = facet.size,
                     facet.margin = facet.margin,
                     legend.italic = legend.italic,
                     title.colour = title.colour,
                     subtitle.colour = subtitle.colour)
    if (!misses_x.title) p <- p + labs(x = validate_titles(x.title))
    if (!misses_y.title) p <- p + labs(y = validate_titles(y.title))
    if (!misses_title) p <- p + labs(title = validate_titles(title, markdown = markdown, max_length = title.linelength))
    if (!misses_subtitle) p <- p + labs(subtitle = validate_titles(subtitle, markdown = markdown, max_length = subtitle.linelength))
    if (!misses_tag) p <- p + labs(tag = validate_titles(tag))
    if (!misses_caption) p <- p + labs(caption = validate_titles(caption))
    return(p)
  }
  
  # prevalidate types for special types ----
  if (!is_empty(type) && !is.character(type)) {
    stop("'type' must be a character", call. = FALSE)
  }
  type_backup <- type
  if (isTRUE(type[1L] %like% "^(barpercent|bp)$")) {
    type_backup <- "barpercent"
    if (is.infinite(x.max_items)) {
      x.max_items <- 10
    }
    x.sort <- "freq-desc"
    datalabels.format <- "%n (%p)"
    type <- "col"
    horizontal <- TRUE
  }
  if (isTRUE(type[1L] %like% "bar")) {
    type <- "col"
    horizontal <- TRUE
  }
  
  set_plot2_env(dots$`_label.x`, 
                dots$`_label.y`,
                dots$`_label.category`,
                dots$`_label.facet`)
  on.exit(clean_plot2_env())
  
  # prepare data ----
  # IMPORTANT: in this part, the data for mapping will be generated anonymously, e.g. as `_var_x` and `_var_category`;
  # this is done for convenience - this is restored before returning the `ggplot` object in the end
  df <- .data %>%
    # add the three directions, these functions also support tidyverse selections: `facet = where(is.character)`
    add_direction(direction = {{ x }},
                  var_name = "x",
                  var_label = dots$`_label.x`,
                  sep = sep) %>% 
    add_direction(direction = {{ category }}, 
                  var_name = "category",
                  var_label = dots$`_label.category`,
                  sep = sep) %>% 
    add_direction(direction = {{ facet }}, 
                  var_name = "facet",
                  var_label = dots$`_label.facet`,
                  sep = sep) %>% 
    # add y
    { function(.data) {
      if (dots$`_label.y` %like% ".+\\(.*\\)" &&
          !(length(group_sizes(.data) == 1) && group_sizes(.data) == nrow(.data))) {
        # seems like a function with multiple groups, so calculate it over all groups that are available
        # - this will support e.g. `data %>% plot2(y = n_distinct(id))`
        .data %>% 
          group_by(across(c(get_x_name(.), get_category_name(.), get_facet_name(.),
                            matches("_var_(x|category|facet)")))) %>%
          summarise(`_var_y` = {{ y }},
                    .groups = "drop")
      } else {
        # nothing special, just run it as mutate
        .data %>% 
          mutate(`_var_y` = {{ y }})
      }}}() %>% 
    mutate(`_var_datalabels` = {{ datalabels }}) %>% 
    # this part will transform the data as needed
    validate_data(misses_x = misses_x,
                  misses_category = misses_category,
                  label_x = dots$`_label.x`,
                  label_y = dots$`_label.y`,
                  label_category = dots$`_label.category`,
                  label_facet = dots$`_label.facet`,
                  decimal.mark = decimal.mark,
                  big.mark = big.mark,
                  type = type,
                  datalabels.round = datalabels.round,
                  datalabels.format = datalabels.format,
                  x.sort = x.sort,
                  category.sort = category.sort,
                  facet.sort = facet.sort,
                  summarise_function = summarise_function,
                  summarise_fn_name = dots$`_summarise_fn_name`,
                  horizontal = horizontal,
                  x.max_items = x.max_items,
                  x.max_txt = x.max_txt,
                  x.character = x.character,
                  x.drop = x.drop,
                  category.max_items = category.max_items,
                  category.max_txt = category.max_txt,
                  facet.max_items = facet.max_items,
                  facet.max_txt = facet.max_txt,
                  na.rm = na.rm,
                  na.replace = na.replace,
                  ...)
  
  # apply taxonomic italics ----
  if (isTRUE(taxonomy_italic) && isTRUE(markdown)) {
    if ("AMR" %in% rownames(utils::installed.packages())) {
      requireNamespace("AMR")
      taxonomic_nms <- unique(c(AMR::microorganisms$family,
                                AMR::microorganisms$genus,
                                AMR::microorganisms$species,
                                AMR::microorganisms$subspecies,
                                AMR::microorganisms.old$fullname))
      make_taxonomy_italic <- function(x, nms = taxonomic_nms) {
        if (is.null(x)) {
          return(NULL)
        }
        vapply(FUN.VALUE = character(1),
               X = strsplit(x, " "),
               FUN = function(nm) {
                 if (!all(is.na(nm))) {
                   nm[nm %in% nms] <- paste0("*", nm[nm %in% nms], "*")
                   nm <- paste0(nm, collapse = " ")
                   nm <- gsub("(.*)([A-Z][.]) [*]([a-z]+)[*](.*)", "\\1*\\2 \\3*\\4", nm, perl = TRUE)
                 } else if (length(nm) == 0) {
                   # this is because of `strsplit("", " ")`
                   nm <- ""
                 }
                 nm
               },
               USE.NAMES = FALSE)
      }
      df <- df %>%
        mutate(across(where(is.character), make_taxonomy_italic),
               across(where(is.factor), ~fct_relabel(.x, make_taxonomy_italic)))
      if (!misses_x.title) x.title <- make_taxonomy_italic(validate_titles(x.title))
      if (!misses_y.title) y.title <- make_taxonomy_italic(validate_titles(y.title))
      if (!misses_title) title <- make_taxonomy_italic(validate_titles(title))
      if (!misses_subtitle) subtitle <- make_taxonomy_italic(validate_titles(subtitle))
      if (!misses_tag) tag <- make_taxonomy_italic(validate_titles(tag))
      if (!misses_caption) caption <- make_taxonomy_italic(validate_titles(caption))
    }
  }
  
  # validate type ----
  type <- validate_type(type = type, df = df) # this will automatically determine the type if is.null(type)
  # transform data if not a continuous geom but group sizes are > 1
  if (any(group_sizes(df) > 1) && !geom_is_continuous(type)) {
    if (identical(type_backup, "barpercent")) {
      plot2_message("Duplicate observations in discrete plot type (", font_blue(type), "), applying ",
                    font_blue(paste0("summarise_function = ", dots$`_summarise_fn_name`)))
    }
    df <- summarise_data(df = df, summarise_function = summarise_function,
                         decimal.mark = decimal.mark, big.mark = big.mark,
                         datalabels.round = datalabels.round, datalabels.format = datalabels.format)
  }
  
  # various cleaning steps ----
  # remove datalabels in continuous geoms
  if (has_datalabels(df) && isTRUE(misses_datalabels) && (geom_is_continuous(type) | type %like% "path|line") && type != "geom_sf") {
    df <- df %>% select(-`_var_datalabels`)
  }
  if (!isTRUE(misses_y) && geom_is_continuous_x(type)) {
    plot2_message("Ignoring ", font_blue("y"), " for plot type ", font_blue(gsub("geom_", "", type)))
    df$`_var_y` <- df$`_var_x`
  }
  # remove x from sf geom
  if (type == "geom_sf") {
    df <- df %>% select(-`_var_x`)
  }
  # keep only one of `stacked` and `stackedpercent`
  if (isTRUE(stacked) && isTRUE(stackedpercent)) {
    plot2_warning("Ignoring ", font_blue("stacked = TRUE"), ", since ", font_blue("stackedpercent = TRUE"))
    stacked <- FALSE
  }
  # category.title and legend.title both exist for convenience
  legend.title <- if (is.null(category.title)) legend.title else category.title
  
  # set default size and width ----
  size <- validate_size(size = size, type = type)
  width <- validate_width(width = width, type = type)
  
  # generate colour vectors ----
  if (has_category(df) && !is.null(category.focus)) {
    category.focus <- category.focus[1L]
    # check if value is actually in category
    if (!category.focus %in% get_category(df) && !is.numeric(category.focus)) {
      plot2_warning("Value \"", category.focus, "\" not found in ", font_blue("category"))
    } else {
      category_unique <- sort(unique(get_category(df)))
      if (is.numeric(category.focus)) {
        # support `category.focus = 3` to choose the third value
        category.focus <- category_unique[category.focus]
      }
      cols <- rep(as.character(colourpicker("grey85")), length(category_unique))
      nms <- as.character(category_unique)
      cols[nms == category.focus] <- colourpicker(colour[1L])
      colour <- stats::setNames(cols, nms)
    }
  }
  cols <- validate_colour(df = df,
                          type = type,
                          colour = colour,
                          colour_fill = colour_fill,
                          misses_colour_fill = misses_colour_fill,
                          horizontal = horizontal)
  
  # generate mapping / aesthetics ----
  # IMPORTANT: in this part, the mapping will be generated anonymously, e.g. as `_var_x` and `_var_category`;
  # this is done for convenience - this is restored before returning the `ggplot` object in the end
  if (type == "geom_sf" && !is.null(dots$`_sf.column`)) {
    mapping <- aes_string(geometry = dots$`_sf.column`)
  } else if (!geom_is_continuous_x(type)) {
    # histograms etc. have a continuous x variable, so only set y if not a histogram-like
    mapping <- aes(y = `_var_y`, group = 1)
  } else {
    mapping <- aes()
    if (misses_zoom) {
      zoom <- TRUE
    }
  }
  if (has_x(df)) {
    mapping <- utils::modifyList(mapping, aes(x = `_var_x`,
                                              group = `_var_x`))
  }
  if (has_category(df)) {
    if (type == "geom_sf") {
      # no colour in sf's
      mapping <- utils::modifyList(mapping, aes(fill = `_var_category`,
                                                group = `_var_category`))
    } else {
      mapping <- utils::modifyList(mapping, aes(fill = `_var_category`,
                                                colour = `_var_category`,
                                                group = `_var_category`))
    }
  }
  if (geom_is_continuous(type) && !geom_is_line(type)) {
    # remove the group from the mapping
    mapping <- utils::modifyList(mapping, aes(group = NULL))
  } else if (geom_is_line(type) && !has_category(df)) {
    # exception for line plots without colour/fill, force group = 1
    mapping <- utils::modifyList(mapping, aes(group = 1))
  }
  
  # generate ggplot ----
  p <- ggplot(data = df, mapping = mapping, colour = cols$colour, fill = cols$colour_fill)
  
  # generate geom ----
  if (type == "geom_boxplot") {
    # first add the whiskers
    p <- p +
      stat_boxplot(geom = "errorbar",
                   coef = 1.5, # 1.5 * IQR
                   width = width * ifelse(has_category(df), 1, 0.75),
                   lwd = size,
                   colour = cols$colour)
  }
  p <- p +
    generate_geom(type = type,
                  df = df,
                  stacked = stacked,
                  stackedpercent = stackedpercent,
                  horizontal = horizontal,
                  width = width,
                  size = size,
                  linetype = linetype,
                  reverse = reverse,
                  na.rm = na.rm,
                  violin_scale = violin_scale,
                  jitter_seed = jitter_seed,
                  binwidth = binwidth,
                  cols = cols)
  if (is.null(smooth) && type == "geom_histogram") {
    plot2_message("Assuming ", font_blue("smooth = TRUE"), " for ", font_blue("type = \"histogram\""))
    smooth <- TRUE
  }
  if (isTRUE(smooth)) {
    if (type == "geom_histogram") {
      # add a density count
      set_binwidth <- p$layers[[1]]$stat_params$binwidth
      p <- p +
        do.call(geom_density,
                c(list(mapping = aes(y = ..count.. * set_binwidth),
                       alpha = smooth.alpha,
                       linetype = smooth.linetype,
                       size = smooth.size,
                       na.rm = na.rm),
                  list(colour = cols$colour[1L])[!has_category(df)]))
    } else {
      # add smooth with geom_smooth()
      p <- p +
        do.call(geom_smooth,
                c(list(mapping = mapping,
                       fill = "grey60",
                       formula = smooth.formula,
                       se = smooth.se,
                       method = smooth.method,
                       level = smooth.level,
                       alpha = smooth.alpha,
                       linetype = smooth.linetype,
                       size = smooth.size,
                       na.rm = na.rm),
                  list(colour = cols$colour[1L])[!has_category(df)]))
    }
  }
  
  # add axis labels ----
  p <- p +
    labs(x = get_x_name(df),
         y = get_y_name(df),
         fill = get_category_name(df),
         colour = get_category_name(df)) # will return NULL if not available, so always works
  if (geom_is_continuous_x(type)) {
    if (type %like% "density") {
      p <- p +
        labs(y = "Density")
      if (misses_y.percent) {
        y.percent <- TRUE
      }
    } else {
      p <- p +
        labs(y = "Frequency")
    }
  }
  
  # add the right scales ----
  if (is_empty(family)) {
    family <- ""
  }
  if (has_category(df) && is.numeric(get_category(df))) {
    p <- p +
      validate_category_scale(values = get_category(df),
                              type = type,
                              cols = cols,
                              category.labels = category.labels,
                              category.percent = category.percent,
                              category.breaks = category.breaks,
                              category.limits = category.limits,
                              category.expand = category.expand,
                              category.midpoint = category.midpoint,
                              category.trans = category.trans,
                              stackedpercent = stackedpercent,
                              legend.nbin = legend.nbin,
                              legend.barheight = legend.barheight,
                              legend.barwidth = legend.barwidth,
                              legend.reverse = legend.reverse,
                              legend.position = legend.position,
                              decimal.mark = decimal.mark,
                              big.mark = big.mark,
                              family = family)
  } else if (type != "geom_sf") {
    p <- p +
      scale_colour_manual(values = cols$colour,
                          limits = if (is.null(names(cols$colour))) {
                            NULL
                          } else {
                            # remove unneeded labels
                            base::force
                          }) + 
      scale_fill_manual(values = cols$colour_fill,
                        limits = if (is.null(names(cols$colour))) {
                          NULL
                        } else {
                          # remove unneeded labels
                          base::force
                        })
  }
  if (type != "geom_sf") {
    if (has_x(df)) {
      p <- p + 
        validate_x_scale(values = get_x(df),
                         x.date_breaks = x.date_breaks,
                         x.date_labels = x.date_labels,
                         x.breaks = x.breaks,
                         x.expand = x.expand,
                         x.breaks_n = x.breaks_n,
                         x.limits = x.limits,
                         x.position = x.position,
                         x.trans = x.trans,
                         x.drop = x.drop,
                         decimal.mark = decimal.mark,
                         big.mark = big.mark,
                         horizontal = horizontal,
                         zoom = zoom)
    } else {
      # no x
      p <- p +
        scale_x_discrete(labels = NULL, breaks = NULL, drop = x.drop)
    }
    if (has_y(df)) {
      p <- p +
        validate_y_scale(values = get_y(df),
                         y.24h = y.24h,
                         y.age = y.age,
                         y.breaks = y.breaks,
                         y.expand = y.expand,
                         y.labels = y.labels,
                         y.limits = y.limits,
                         y.percent = y.percent,
                         y.percent_break = y.percent_break,
                         y.position = y.position,
                         y.trans = y.trans,
                         stackedpercent = stackedpercent,
                         facet.fixed_y = facet.fixed_y,
                         decimal.mark = decimal.mark,
                         big.mark = big.mark,
                         zoom = zoom)
    }
  }
  
  # validate theme and add markdown support ----
  p <- p + 
    validate_theme(theme = theme,
                   type = type,
                   markdown = markdown,
                   text_factor = text_factor,
                   family = family,
                   horizontal = horizontal,
                   x.remove = x.remove,
                   x.lbl_angle = x.lbl_angle,
                   x.lbl_align = x.lbl_align,
                   x.lbl_italic = x.lbl_italic,
                   facet.fill = facet.fill,
                   facet.bold = facet.bold,
                   facet.italic = facet.italic,
                   facet.size = facet.size,
                   facet.margin = facet.margin,
                   legend.italic = legend.italic,
                   title.colour = title.colour,
                   subtitle.colour = subtitle.colour)
  
  # add titles ----
  if (!misses_x.title) p <- p + labs(x = validate_titles(x.title)) # this will overwrite the var name
  if (!misses_y.title) p <- p + labs(y = validate_titles(y.title)) # this will overwrite the var name
  if (!misses_title) p <- p + labs(title = validate_titles(title, markdown = markdown, max_length = title.linelength))
  if (!misses_subtitle) p <- p + labs(subtitle = validate_titles(subtitle, markdown = markdown, max_length = subtitle.linelength))
  if (!misses_tag) p <- p + labs(tag = validate_titles(tag))
  if (!misses_caption) p <- p + labs(caption = validate_titles(caption))
  if (has_category(df)) {
    # legend
    if (is.null(legend.title) && all(get_category(df) %like% "^[0-9.,]+$", na.rm = TRUE)) {
      legend.title <- TRUE
    }
    if (isTRUE(legend.title)) {
      legend.title <- get_category_name(df)
    }
    if ("colour" %in% names(mapping)) {
      p <- p + labs(colour = validate_titles(legend.title))
    }
    if ("fill" %in% names(mapping)) {
      p <- p + labs(fill = validate_titles(legend.title))
    }
    if ("group" %in% names(mapping)) {
      p <- p + labs(group = validate_titles(legend.title))
    }
  }
  
  # set legend ----
  if (is.null(legend.position)) {
    if (has_category(df) && all(get_category(df) %like% "^[0-9.,]+$", na.rm = TRUE)) {
      legend.position <- "right"
    } else {
      legend.position <- "top"
    }
  }
  legend.position <- validate_legend.position(legend.position)
  p <- p + theme(legend.position = legend.position)
  
  if (!(has_category(df) && is.numeric(get_category(df)))) {
    # only change this when there is no guide_colourbar(), see validate_category_scale()
    if (!is.null(legend.reverse)) {
      p <- p +
        guides(fill = guide_legend(reverse = isTRUE(legend.reverse)),
               colour = guide_legend(reverse = isTRUE(legend.reverse)))
    }
    if (isTRUE(horizontal)) {
      if (legend.position %in% c("top", "bottom") &&
          validate_sorting(category.sort, horizontal = horizontal) %unlike% "freq") {
        # turn legend items when on top or bottom, but not when sorting is freq, freq-asc or freq-desc
        p <- p +
          guides(fill = guide_legend(reverse = TRUE),
                 colour = guide_legend(reverse = TRUE))
      }
    }
  }
  
  # set facet ----
  if (has_facet(df)) {
    p <- p +
      validate_facet(df = df,
                     type = type,
                     facet.repeat_lbls_x = facet.repeat_lbls_x,
                     facet.repeat_lbls_y = facet.repeat_lbls_y,
                     facet.relative = facet.relative,
                     facet.drop = facet.drop,
                     facet.nrow = facet.nrow,
                     facet.position = facet.position,
                     horizontal = horizontal)
  }
  
  # set datalabels ----
  if (has_datalabels(df) && type != "geom_blank") {
    p <- set_datalabels(p = p,
                        df = df,
                        type = type,
                        width = width,
                        stacked = stacked,
                        stackedpercent = stackedpercent,
                        datalabels.colour_fill = datalabels.colour_fill,
                        datalabels.colour = datalabels.colour,
                        datalabels.size = datalabels.size,
                        datalabels.angle = datalabels.angle,
                        family = family,
                        reverse = reverse,
                        horizontal = horizontal,
                        misses_datalabels = misses_datalabels)
  }
  
  # turn plot horizontal if required ----
  # up until this point, a lot has been done already for `horizontal`.
  # such as switching some x and y axis properties of the theme
  if (isTRUE(horizontal)) {
    p <- p + coord_flip()
  }
  
  # restore mapping to original names ----
  # this will replace e.g. `_var_x` and `_var_category` in the mapping and remove them from the data
  p <- restore_mapping(p = p,
                       df = df)
  
  # return plot ----
  if (isTRUE(print)) {
    print(p)
  } else {
    p
  }
}
