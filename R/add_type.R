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

#' Add Plot Element
#' 
#' Quickly add a new 'geom' to an existing `plot2`/`ggplot` model. Like [plot2()], they support tidy evaluation, meaning that variables can be unquoted. They can be added using the pipe (`%>%`).
#' @param plot a `ggplot2` plot
#' @param type a `ggplot2` geom name, all geoms are supported. Full function names can be used (e.g., `"geom_line"`), but they can also be abbreviated (e.g., `"l"`, `"line"`). These geoms can be abbreviated by their first character: area (`"a"`), boxplot (`"b"`), column (`"c"`), histogram (`"h"`), jitter (`"j"`), line (`"l"`), point (`"p"`), ribbon (`"r"`), violin (`"v"`).
#' @param mapping a mapping created with [ggplot2::aes()] to pass on to the geom
#' @param group,linetype,size,width,... arguments passed on to the geom
#' @importFrom ggplot2 is.ggplot aes
#' @rdname add_type
#' @export
#' @examples 
#' df <- data.frame(var_1 = c(1:100),
#'                  var_2 = rnorm(100, 100, 25))
#' df %>%
#'   plot2() %>% 
#'   add_line(mean(var_2))
#'   
#' df %>%
#'   plot2() %>% 
#'   add_line(y = mean(var_2), 
#'            size = 2) %>%
#'   add_col(y = var_2 / 5,
#'           width = 0.25,
#'           colour = "certeroze")
#'    
#' if (require("certestats", warn.conflicts = FALSE)) {
#'    df %>%
#'      plot2(caption = "EWMA in pink :)") %>% 
#'      add_line(y = ewma(var_2, 0.75),
#'               colour = "certeroze",
#'               linetype = 3,
#'               size = 1)
#' }
#' 
#' if (require("certegis")) {
#'   hospitals <- geocode(c("Martini Ziekenhuis",
#'                          "Medisch Centrum Leeuwarden",
#'                          "Tjongerschans Heerenveen",
#'                          "Treant Emmen"))
#'   geo_gemeenten %>%
#'     crop_certe() %>%
#'     plot2(datalabels = FALSE) %>%
#'     add_sf(hospitals, colour = "certeroze", datalabels = place)
#' }
add_type <- function(plot, type = NULL, mapping = aes(), ...) {
  if (!is.ggplot(plot)) {
    stop("`plot` must be a ggplot2 model.", call. = FALSE)
  }
  type <- validate_type(type[1L])
  if (type == "") {
    stop("`type` must be set for `add_type()`", call. = FALSE)
  } else if (type == "geom_smooth") {
    plot2_warning("Adding a smooth using `add_type()` is less convenient than using `plot2(..., smooth = TRUE)")
  }
  geom_fn <- getExportedValue(name = type, ns = asNamespace("ggplot2"))
  
  plot +
    do.call(geom_fn,
            args = c(list(mapping = mapping),
                     c(...)))
}

#' @rdname add_type
#' @param x,y aesthetic arguments
#' @param colour,colour_fill colour of the line or column, will be evaluated with [certestyle::colourpicker()]. If `colour_fill` is missing but `colour` is given, `colour_fill` will inherit the colour set with `colour`.
#' @param inherit.aes a [logical] to indicate whether the default aesthetics should be inherited, rather than combining with them
#' @importFrom dplyr `%>%` mutate
#' @importFrom ggplot2 aes_string
#' @importFrom certestyle colourpicker
#' @export
add_line <- function(plot, y, x, group = 1, colour = "certeblauw", size, linetype, ..., inherit.aes = TRUE) {
  if (!is.ggplot(plot)) {
    stop("`plot` must be a ggplot2 model.", call. = FALSE)
  }
  label_line_y <- deparse(substitute(y))
  label_line_x <- deparse(substitute(x))
  
  df <- plot$data %>% 
    mutate(`_var_line_y` = {{ y }},
           `_var_line_x` = {{ x }})
  colnames(df)[colnames(df) == "_var_line_y"] <- label_line_y
  colnames(df)[colnames(df) == "_var_line_x"] <- label_line_x
  
  # build mapping
  if (missing(group) && !"group" %in% names(plot$mapping) && "colour" %in% names(plot$mapping)) {
    # be sure to add the group as the category
    mapping <- aes_string(group = gsub("^~", "", deparse(plot$mapping$colour)))
  } else if (!missing(group) || !isTRUE(inherit.aes) || !"group" %in% names(plot$mapping)) {
    mapping <- aes_string(group = group)
  } else {
    mapping <- aes()
  }
  if (label_line_y != "") {
    mapping <- utils::modifyList(mapping, aes_string(y = label_line_y))
  }
  if (label_line_x != "") {
    mapping <- utils::modifyList(mapping, aes_string(x = label_line_x))
  }
  
  # build additional parameters
  params <- list(inherit.aes = inherit.aes)
  if (!missing(colour) || !isTRUE(inherit.aes) || !"colour" %in% names(plot$mapping)) {
    params <- c(params, list(colour = colourpicker(colour)))
  }
  if (!missing(size)) {
    params <- c(params, list(size = size))
  }
  if (!missing(linetype)) {
    params <- c(params, list(linetype = linetype))
  }
  if (length(list(...)) > 0) {
    params <- c(params, list(...))
  }
  
  # add the geom
  add_type(plot = plot,
           type = "line",
           mapping = mapping,
           params)
}

#' @rdname add_type
#' @importFrom dplyr `%>%` mutate
#' @importFrom ggplot2 aes_string
#' @importFrom certestyle colourpicker
#' @export
add_col <- function(plot, y, x, colour = "certeblauw", colour_fill = "certeblauw", width, ..., inherit.aes = TRUE) {
  if (!is.ggplot(plot)) {
    stop("`plot` must be a ggplot2 model.", call. = FALSE)
  }
  label_col_y <- deparse(substitute(y))
  label_col_x <- deparse(substitute(x))
  
  df <- plot$data %>% 
    mutate(`_var_line_y` = {{ y }},
           `_var_line_x` = {{ x }})
  colnames(df)[colnames(df) == "_var_line_y"] <- label_col_y
  colnames(df)[colnames(df) == "_var_line_x"] <- label_col_x
  
  # build mapping
  mapping <- aes_string()
  if (label_col_y != "") {
    mapping <- utils::modifyList(mapping, aes_string(y = label_col_y))
  }
  if (label_col_x != "") {
    mapping <- utils::modifyList(mapping, aes_string(x = label_col_x))
  }
  
  if (!missing(colour) && missing(colour_fill)) {
    colour_fill <- colour
  }
  # build additional parameters
  params <- list(inherit.aes = inherit.aes)
  if (!missing(colour) || !isTRUE(inherit.aes) || !"colour" %in% names(plot$mapping)) {
    params <- c(params, list(colour = colourpicker(colour)))
  }
  if (!missing(colour) || !isTRUE(inherit.aes) || !"fill" %in% names(plot$mapping)) {
    params <- c(params, list(fill = colourpicker(colour_fill)))
  }
  if (!missing(width)) {
    params <- c(params, list(width = width))
  }
  if (length(list(...)) > 0) {
    params <- c(params, list(...))
  }
  
  # add the geom
  add_type(plot = plot,
           type = "column",
           mapping = mapping,
           params)
}

#' @rdname add_type
#' @param sf_data an 'sf' [data.frame], such as the outcome of [certegis::geocode()]
#' @param datalabels a column of `sf_data` to add as label below the points
#' @param nudge_y is `datalabels` is not `NULL`, the amount of vertical adjustment of the datalabels
#' @importFrom dplyr `%>%` mutate
#' @importFrom ggplot2 geom_sf geom_sf_text aes is.ggplot
#' @importFrom certestyle colourpicker
#' @export
add_sf <- function(plot,
                   sf_data,
                   colour = "certeblauw",
                   colour_fill = "certeblauw",
                   size = 3,
                   datalabels = NULL,
                   nudge_y = -0.025,
                   ...,
                   inherit.aes = FALSE) {
  if (!is.ggplot(plot)) {
    stop("`plot` must be a ggplot2 model.", call. = FALSE)
  }
  
  p <- plot +
    geom_sf(data = sf_data,
            inherit.aes = inherit.aes,
            size = size,
            colour = colourpicker(colour),
            fill = colourpicker(colour_fill),
            ...)
  
  if (tryCatch(!is.null(datalabels), error = function(e) TRUE)) {
    sf_data <- sf_data %>% 
      mutate(`_var_datalabels` = {{ datalabels }})
    # these functions from the 'sf' package fix invalid geometries
    st_is_valid <- getExportedValue(name = "st_is_valid", ns = asNamespace("sf"))
    st_point <- getExportedValue(name = "st_point", ns = asNamespace("sf"))
    st_point_on_surface <- getExportedValue(name = "st_point_on_surface", ns = asNamespace("sf"))
    st_zm <- getExportedValue(name = "st_zm", ns = asNamespace("sf"))
    p <- p +
      geom_sf_text(aes(label = `_var_datalabels`),
                   data = sf_data,
                   inherit.aes = inherit.aes,
                   size = size,
                   nudge_y = nudge_y,
                   colour = colourpicker(colour),
                   fun.geometry = function(x) {
                     x[!st_is_valid(x)] <- st_point()
                     suppressWarnings(st_point_on_surface(st_zm(x)))
                   })
  }
  p
}