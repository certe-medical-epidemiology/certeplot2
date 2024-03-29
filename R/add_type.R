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
#' Quickly add a new 'geom' to an existing `plot2`/`ggplot` model. Like [plot2()], they support tidy evaluation, meaning that variables can be unquoted. They can be added using the pipe (new base \R `|>` or tidyverse `%>%`).
#' @param plot a `ggplot2` plot
#' @param type a `ggplot2` geom name, all geoms are supported. Full function names can be used (e.g., `"geom_line"`), but they can also be abbreviated (e.g., `"l"`, `"line"`). These geoms can be abbreviated by their first character: area (`"a"`), boxplot (`"b"`), column (`"c"`), histogram (`"h"`), jitter (`"j"`), line (`"l"`), point (`"p"`), ribbon (`"r"`), violin (`"v"`).
#' @param mapping a mapping created with [`aes()`][ggplot2::aes()] to pass on to the geom
#' @param group,linetype,linewidth,shape,size,width,... arguments passed on to the geom
#' @param data data to use in mapping
#' @importFrom ggplot2 is.ggplot aes
#' @rdname add_type
#' @export
#' @examples 
#' df <- data.frame(var_1 = c(1:100),
#'                  var_2 = rnorm(100, 100, 25))
#' df |>
#'   plot2() |> 
#'   add_line(mean(var_2))
#'   
#' df |>
#'   plot2() |> 
#'   add_line(y = mean(var_2), 
#'            linewidth = 2,
#'            linetype = 3) |>
#'   add_col(y = var_2 / 5,
#'           width = 0.25,
#'           colour = "certeroze")
#'        
#' if (require("dplyr", warn.conflicts = FALSE)) {   
#'   df2 <- df |> 
#'     as_tibble() |> 
#'     filter(var_1 <= 50) |> 
#'     mutate(error1 = var_2 * 0.9,
#'            error2 = var_2 * 1.1)
#'   
#'   print(df2)
#'   
#'   df2 |> 
#'     plot2(type = "c", colour = "certeroze4") |> 
#'     # add the error bars, simply by referencing the lower and upper values
#'     add_errorbar(error1, error2)
#' }
#' 
#' if (require("certestats", warn.conflicts = FALSE)) {
#'    df |>
#'      plot2() |> 
#'      add_line(y = ewma(var_2, 0.75),
#'               colour = "certeroze",
#'               linewidth = 1)
#' }
#' 
#' if (require("certegis")) {
#'   hospitals <- geocode(c("Martini Ziekenhuis",
#'                          "Medisch Centrum Leeuwarden",
#'                          "Tjongerschans Heerenveen",
#'                          "Treant Emmen"))
#'   geo_gemeenten |>
#'     crop_certe() |>
#'     plot2(datalabels = FALSE,
#'           category.title = "Inhabitants",
#'           colour_fill = c("white", "certeblauw2")) |>
#'     add_sf(hospitals,
#'            colour = "certeroze",
#'            datalabels = place) |> 
#'     add_sf(geo_provincies |> crop_certe(),
#'            colour_fill = NA,
#'            colour = "certeblauw",
#'            linetype = 2,
#'            linewidth = 0.5)
#' }
add_type <- function(plot, type = NULL, mapping = aes(), ..., data = NULL) {
  if (!is.ggplot(plot)) {
    stop("`plot` must be a ggplot2 model.", call. = FALSE)
  }
  type <- validate_type(type[1L])
  if (type == "") {
    stop("`type` must be set for `add_type()`", call. = FALSE)
  } else if (type == "geom_smooth") {
    plot2_caution("Adding a smooth using `add_type()` is less convenient than using `plot2(..., smooth = TRUE)")
  }
  
  args <- list(...)
  if (length(args) == 1 && is.list(args[[1]])) {
    args <- args[[1]]
  }
  args <- utils::modifyList(list(mapping = mapping, data = data), args)
  args <- args[!vapply(FUN.VALUE = logical(1), args, is.null)]
  
  geom_fn <- getExportedValue(name = type, ns = asNamespace("ggplot2"))
  plot +
    do.call(geom_fn, args = args)
}

#' @rdname add_type
#' @param x,y aesthetic arguments
#' @param geom_type type of line, can be "vline" or "hline" to force the type of line
#' @param colour,colour_fill colour of the line or column, will be evaluated with [certestyle::colourpicker()]. If `colour_fill` is missing but `colour` is given, `colour_fill` will inherit the colour set with `colour`.
#' @param inherit.aes a [logical] to indicate whether the default aesthetics should be inherited, rather than combining with them
#' @importFrom dplyr mutate
#' @importFrom ggplot2 aes
#' @importFrom certestyle colourpicker
#' @details The function [add_line()] will add:
#' * [`geom_hline()`][ggplot2::geom_hline()] if only `y` is provided and `y` contains one unique value;
#' * [`geom_vline()`][ggplot2::geom_vline()] if only `x` is provided and `x` contains one unique value;
#' * [`geom_line()`][ggplot2::geom_line()] in all other cases.
#' @export
add_line <- function(plot, y = NULL, x = NULL, group = 1, colour = "certeblauw", geom_type = NULL, linetype, linewidth, ..., inherit.aes = TRUE) {
  if (!is.ggplot(plot)) {
    stop("`plot` must be a ggplot2 model.", call. = FALSE)
  }
  label_line_y <- paste(trimws(deparse(substitute(y))), collapse = " ")
  label_line_x <- paste(trimws(deparse(substitute(x))), collapse = " ")
  
  df <- plot$data |> 
    mutate(`_var_line_y` = {{ y }},
           `_var_line_x` = {{ x }})
  suppressWarnings(values_y <- df$`_var_line_y`)
  suppressWarnings(values_x <- df$`_var_line_x`)
  colnames(df)[colnames(df) == "_var_line_y"] <- label_line_y
  colnames(df)[colnames(df) == "_var_line_x"] <- label_line_x
  
  # build mapping
  if (missing(group) && !"group" %in% names(plot$mapping) && "colour" %in% names(plot$mapping)) {
    # be sure to add the group as the category
    mapping <- update_aes(group = gsub("^~", "", deparse(plot$mapping$colour)))
  } else if (!missing(group) || !isTRUE(inherit.aes) || !"group" %in% names(plot$mapping)) {
    mapping <- update_aes(group = group)
  } else {
    mapping <- aes()
  }
  if (label_line_y != "") {
    mapping <- update_aes(mapping, y = label_line_y)
  }
  if (label_line_x != "") {
    mapping <- update_aes(mapping, x = label_line_x)
  }
  
  # build additional parameters
  params <- list(inherit.aes = inherit.aes)
  if (!missing(colour) || !isTRUE(inherit.aes) || !"colour" %in% names(plot$mapping)) {
    params <- c(params, list(colour = colourpicker(colour)))
  }
  if (!missing(linewidth)) {
    params <- c(params, list(linewidth = linewidth))
  }
  if (!missing(linetype)) {
    params <- c(params, list(linetype = linetype))
  }
  if (length(list(...)) > 0) {
    params <- c(params, list(...))
  }
  
  if (identical(geom_type, "hline") || (is.null(geom_type) && is.null(values_x) && length(unique(values_y)) == 1)) {
    # check if y are all 1 value, then make it hline
    if (is.null(geom_type)) {
      plot2_message("Adding type ", font_blue("hline"))
    }
    add_type(plot = plot,
             type = "hline",
             mapping = update_aes(mapping, y = NULL, yintercept = mapping$y),
             utils::modifyList(params, list(inherit.aes = NULL)))
    
  } else if (identical(geom_type, "vline") || (is.null(geom_type) && is.null(values_y) && length(unique(values_x)) == 1)) {
    # check if x are all 1 value, then make it vline
    if (is.null(geom_type)) {
      plot2_message("Adding type ", font_blue("vline"))
    }
    add_type(plot = plot,
             type = "vline",
             mapping = update_aes(mapping, x = NULL, xintercept = mapping$x),
             utils::modifyList(params, list(inherit.aes = NULL)))
  } else {
    # add the geom
    add_type(plot = plot,
             type = "line",
             mapping = mapping,
             params)
  }
}

#' @rdname add_type
#' @export
add_point <- function(plot, y = NULL, x = NULL, group = 1, colour = "certeblauw", size, shape, ..., inherit.aes = TRUE) {
  if (!is.ggplot(plot)) {
    stop("`plot` must be a ggplot2 model.", call. = FALSE)
  }
  label_line_y <- paste(trimws(deparse(substitute(y))), collapse = " ")
  label_line_x <- paste(trimws(deparse(substitute(x))), collapse = " ")
  
  df <- plot$data |> 
    mutate(`_var_line_y` = {{ y }},
           `_var_line_x` = {{ x }})
  colnames(df)[colnames(df) == "_var_line_y"] <- label_line_y
  colnames(df)[colnames(df) == "_var_line_x"] <- label_line_x
  
  # build mapping
  if (missing(group) && !"group" %in% names(plot$mapping) && "colour" %in% names(plot$mapping)) {
    # be sure to add the group as the category
    mapping <- aes(group = gsub("^~", "", deparse(plot$mapping$colour)))
  } else if (!missing(group) || !isTRUE(inherit.aes) || !"group" %in% names(plot$mapping)) {
    mapping <- aes(group = group)
  } else {
    mapping <- aes()
  }
  if (label_line_y != "") {
    mapping <- update_aes(mapping, y = label_line_y)
  }
  if (label_line_x != "") {
    mapping <- update_aes(mapping, x = label_line_x)
  }
  
  # build additional parameters
  params <- list(inherit.aes = inherit.aes)
  if (!missing(colour) || !isTRUE(inherit.aes) || !"colour" %in% names(plot$mapping)) {
    params <- c(params, list(colour = colourpicker(colour)))
  }
  if (!missing(size)) {
    params <- c(params, list(size = size))
  }
  if (!missing(shape)) {
    params <- c(params, list(shape = shape))
  }
  if (length(list(...)) > 0) {
    params <- c(params, list(...))
  }
  
  # add the geom
  add_type(plot = plot,
           type = "point",
           mapping = mapping,
           params)
}

#' @rdname add_type
#' @importFrom dplyr mutate
#' @importFrom ggplot2 aes
#' @importFrom certestyle colourpicker
#' @export
add_col <- function(plot, y = NULL, x = NULL, colour = "certeblauw", colour_fill = "certeblauw", width, ..., inherit.aes = TRUE) {
  if (!is.ggplot(plot)) {
    stop("`plot` must be a ggplot2 model.", call. = FALSE)
  }
  label_col_y <- paste(trimws(deparse(substitute(y))), collapse = " ")
  label_col_x <- paste(trimws(deparse(substitute(x))), collapse = " ")
  
  df <- plot$data |> 
    mutate(`_var_line_y` = {{ y }},
           `_var_line_x` = {{ x }})
  colnames(df)[colnames(df) == "_var_line_y"] <- label_col_y
  colnames(df)[colnames(df) == "_var_line_x"] <- label_col_x
  
  # build mapping
  mapping <- aes()
  if (label_col_y != "") {
    mapping <- update_aes(mapping, y = label_col_y)
  }
  if (label_col_x != "") {
    mapping <- update_aes(mapping, x = label_col_x)
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
#' @param min,max minimum (lower) and maximum (upper) values of the error bars
#' @importFrom dplyr mutate
#' @importFrom ggplot2 aes
#' @importFrom certestyle colourpicker
#' @details
#' The function [add_errorbar()] only adds error bars to the `y` values, see *Examples*.
#' @export
add_errorbar <- function(plot, min = NULL, max = NULL, y = NULL, x = NULL, colour = "certeroze", width = 0.5, ..., inherit.aes = TRUE) {
  if (!is.ggplot(plot)) {
    stop("`plot` must be a ggplot2 model.", call. = FALSE)
  }
  label_col_min <- paste(trimws(deparse(substitute(min))), collapse = " ")
  label_col_max <- paste(trimws(deparse(substitute(max))), collapse = " ")
  label_col_y <- paste(trimws(deparse(substitute(y))), collapse = " ")
  label_col_x <- paste(trimws(deparse(substitute(x))), collapse = " ")
  
  
  df <- plot$data |> 
    mutate(`_var_line_y` = {{ y }},
           `_var_line_x` = {{ x }})
  colnames(df)[colnames(df) == "_var_line_y"] <- label_col_y
  colnames(df)[colnames(df) == "_var_line_x"] <- label_col_x
  
  # build mapping
  mapping <- aes()
  if (label_col_min != "") {
    mapping <- update_aes(mapping, ymin = label_col_min)
  }
  if (label_col_max != "") {
    mapping <- update_aes(mapping, ymax = label_col_max)
  }
  if (label_col_y != "") {
    mapping <- update_aes(mapping, y = label_col_y)
  }
  if (label_col_x != "") {
    mapping <- update_aes(mapping, x = label_col_x)
  }
  
  # build additional parameters
  params <- list(inherit.aes = inherit.aes)
  if (!missing(colour) || !isTRUE(inherit.aes) || !"colour" %in% names(plot$mapping)) {
    params <- c(params, list(colour = colourpicker(colour)))
  }
  params <- c(params, list(width = width))
  if (length(list(...)) > 0) {
    params <- c(params, list(...))
  }
  
  # add the geom
  add_type(plot = plot,
           type = "errorbar",
           mapping = mapping,
           params)
}

#' @rdname add_type
#' @param sf_data an 'sf' [data.frame], such as the outcome of [certegis::geocode()]
#' @param datalabels a column of `sf_data` to add as label below the points
#' @param datalabels.colour,datalabels.size,datalabels.angle,datalabels.font properties of `datalabels`
#' @param datalabels.nudge_y is `datalabels` is not `NULL`, the amount of vertical adjustment of the datalabels (positive value: more to the North, negative value: more to the South)
#' @importFrom dplyr mutate
#' @importFrom ggplot2 geom_sf geom_sf_text aes is.ggplot
#' @importFrom certestyle colourpicker
#' @export
add_sf <- function(plot,
                   sf_data,
                   colour = "certeblauw",
                   colour_fill = "certeblauw",
                   size = 2,
                   linewidth = 0.1,
                   datalabels = NULL,
                   datalabels.colour = colour,
                   datalabels.size = 3,
                   datalabels.angle = 0,
                   datalabels.font = getOption("plot2.font"),
                   datalabels.nudge_y = 2500,
                   ...,
                   inherit.aes = FALSE) {
  
  loadNamespace("sf") # will throw an error if not installed
  
  if (!is.ggplot(plot)) {
    stop("`plot` must be a ggplot2 model.", call. = FALSE)
  }
  if (!"geometry" %in% colnames(plot$data)) {
    stop("`plot` must be a ggplot2 model based on geographic data.", call. = FALSE)
  }
  
  # force sf type
  sf_data <- sf::st_as_sf(sf_data)
  
  crs <- c(plot = as.character(sf::st_crs(plot$data$geometry))[1],
           add = as.character(sf::st_crs(sf_data))[1])
  if (n_distinct(crs) > 1) {
    plot2_warning("The coordinate reference system (CRS) of `plot` and `sf_data` are different, transforming `sf_data` to ", crs[1])
    sf_data <- sf::st_transform(sf_data, crs = crs[1])
  }
  crs <- crs[1]
  
  p <- plot +
    geom_sf(data = sf_data,
            inherit.aes = inherit.aes,
            size = size,
            linewidth = linewidth,
            colour = colourpicker(colour),
            fill = colourpicker(colour_fill),
            ...)
  
  if (tryCatch(!is.null(datalabels), error = function(e) TRUE)) {
    
    if (abs(datalabels.nudge_y) > 0.25 && crs %unlike% "28992") {
      plot2_message(font_blue(paste0("datalabels.nudge_y = ", datalabels.nudge_y)),
                    " might be very ", ifelse(datalabels.nudge_y < 0, "low", "high"),
                    " for the current coordinate reference system (", crs, ")")
    }
    
    sf_data <- sf_data |> 
      mutate(`_var_datalabels` = {{ datalabels }})
    
    datalabels.font <- suppressMessages(validate_font(datalabels.font))
    
    p <- p +
      geom_sf_text(aes(label = `_var_datalabels`),
                   data = sf_data,
                   inherit.aes = inherit.aes,
                   size = datalabels.size,
                   family = datalabels.font,
                   angle = datalabels.angle,
                   nudge_y = datalabels.nudge_y,
                   colour = colourpicker(colour),
                   fun.geometry = function(x) {
                     x[!sf::st_is_valid(x)] <- sf::st_point()
                     suppressWarnings(sf::st_point_on_surface(sf::st_zm(x)))
                   })
  }
  p
}
