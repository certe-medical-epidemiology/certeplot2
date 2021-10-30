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

#' Plotting functions
#'
#' text text
#' @inheritParams graphics::plot
#' @details The [plot3()] function uses the `ggplot2` package for plotting and provides:
#'   * A convenient wrapper around many `ggplot2` functions such as [ggplot()], [geom_col()], [facet_wrap()], [labs()], etc.
#'   * Writing as few lines of codes as possible
#'   * A drop-in replacement for most [plot()] calls
#'   * Benefits from Excel: the y axis starts at 0, the y scale contains extra space to read all data points, date breaks can be written in a readable format such as "d mmm yyyy", and data labels can easily be printed
#'   * Easy plotting in three 'directions': `x` (the regular x axis), `category` (replaces 'fill' and 'colour') and `facet`
#'   * Easy way for sorting data in may ways (such as on alphabet, numeric value, frequency, original data order), by setting a single argument for the 'direction': `x.sort`, `category.sort` and `facet.sort`
#'   * Easy limiting values for `x` by setting `code{x.max_items`
#'   * Markdown support for any label, with any theme
#'   * An extra clean, minimalistic theme with a lot of whitespace that is ideal for printing
#'
#' The [plot3()] function is arguably what the [qplot()] function of the `ggplot2` package could have been.
#'
#' @section Comparison with the `ggplot2` package:
#'
#' The `ggplot2` package provides a wide range of functions for very advanced plotting, but it lacks convenience for simple plotting. For example, a common data set could look like this:
#'
#' ```
#' df
#' #>   carmodel            mpg
#' #>   <chr>             <dbl>
#' #> 1 Mazda RX4          21.0
#' #> 2 Mazda RX4 Wag      21.0
#' #> 3 Datsun 710         22.8
#' #> 4 Hornet 4 Drive     21.4
#' ```
#'
#' The `ggplot2` package sets no default for plotting types, requiring you to type at least:
#'
#' ```
#' ggplot(df) +
#'   geom_col(aes(x = carmodel, y = mpg))
#' ```
#'
#' That's a lot of code containing three functions for something so obvious. Any addition (labels, scales, etc.) would require you to write more functions and their parameters. The [plot3()] function guesses what you want to do and contains many parameters to alter your plots. In other words, the `ggplot2` package requires you to write many functions and parameters for plotting, the [plot3()] function only requires you to write the parameters that are not already set:
#'
#' ```
#' df %>%
#'   plot3()
#' ```
#'
#' Moreover, the `ggplot2` package requires a [data.frame] as input or forces input to a [data.frame]. The [plot3()] function uses S3 classes for extensions, allowing `base` [plot()] users to change their plots to `ggplot2` plots by literally adding one number to the function they are calling. And [plot3()] functions are even extendible with new [plot3()] functions and with other packages.
#'
#' @section Working with the three 'directions':
#'
#' @rdname plot3
#' @export
plot3 <- function (.data,
                   type = "column",
                   x = NULL,
                   y = NULL,
                   category = NULL,
                   facet = NULL,
                   title = NULL,
                   subtitle = NULL,
                   tag = NULL,
                   caption = NULL,
                   bins = NULL,
                   category.focus = NULL,
                   category.sort = TRUE,
                   colour = getOption("plot2.colours", "certe"),
                   colour.extended_spectrum = FALSE,
                   colour.fill = NULL,
                   colour.opacity = 0,
                   datalabels = TRUE,
                   datalabels.fill = "white",
                   datalabels.round = if_else(y.percent, 1, 2),
                   datalabels.size = 3,
                   decimal_comma = uses_decimal_comma(),
                   facet.bold = TRUE,
                   facet.drop = FALSE,
                   facet.fill = NULL,
                   facet.fixed_y = FALSE,
                   facet.italic = FALSE,
                   facet.margin = 8,
                   facet.nrow = NULL,
                   facet.position = "top",
                   facet.relative = FALSE,
                   facet.repeat_lbls_x = TRUE,
                   facet.repeat_lbls_y = TRUE,
                   facet.size = 10,
                   facet.sort = TRUE,
                   horizontal = FALSE,
                   jitter_seed = 1,
                   legend.barheight = 5,
                   legend.barwidth = 1,
                   legend.italic = FALSE,
                   legend.position = "top",
                   legend.reverse = NULL,
                   legend.title = "",
                   linetype = 1,
                   markdown = TRUE,
                   na.replace = "(onbekend)",
                   na.rm = FALSE,
                   print = FALSE,
                   reverse = horizontal,
                   size = ifelse(type %in% c("point", "jitter", "boxplot", "violin"), 2, 0.75),
                   stacked = FALSE,
                   stackedpercent = FALSE,
                   subtitle.maxlength = 60,
                   summarise_function = base::sum,
                   text.factor = 1,
                   text.font_family = "Calibri",
                   theme = theme_minimal2(),
                   title.maxlength = 60,
                   width = ifelse(type %in% c("jitter", "boxplot", "violin"), 0.75, 0.5),
                   x.date_breaks = "1 day",
                   x.date_labels = "d mmm",
                   x.expand = 0.5,
                   x.lbl_align = NULL,
                   x.lbl_angle = 0,
                   x.lbl_italic = FALSE,
                   x.limits = NULL,
                   x.max_items = Inf,
                   x.max_txt = "(rest, x{n})",
                   x.position = "bottom",
                   x.remove = FALSE,
                   x.sort = TRUE,
                   x.title = NULL,
                   y.24h = FALSE,
                   y.age = FALSE,
                   y.breaks = NULL,
                   y.expand = 0.25,
                   y.labels = NULL,
                   y.limits = NULL,
                   y.percent = FALSE,
                   y.percent_break = 10,
                   y.position = "left",
                   y.remove = FALSE,
                   y.title = NULL,
                   y.trans = "identity",
                   ...) {
  UseMethod("plot3")
}

#' @rdname plot3
#' @export
plot3.default <- function(x, y = NULL, type = "p",  xlim = NULL, ylim = NULL,
                          log = "", main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
                          ann = par("ann"), axes = TRUE, frame.plot = axes,
                          panel.first = NULL, panel.last = NULL, asp = NA,
                          xgap.axis = NA, ygap.axis = NA,
                          ...) {
  print("starts plot3.default")

}

#' @rdname plot3
#' @export
plot3.grouped_df <- function(.data,
                             ...,
                             category = NULL) {
  if (isTRUE(is.null(category))) {
    # take first group from data
    plot3.data.frame(.data = .data, ..., category = group_vars(.data)[1L])
  } else {
    plot3.data.frame(.data = .data, ..., category = category)
  }
}

#' @noRd
#' @importFrom rlang !! enquo
#' @importFrom methods is
#' @export
plot3.data.frame <- function(.data,
                             type = "column",
                             x = NULL,
                             y = NULL,
                             category = NULL,
                             facet = NULL,
                             title = NULL,
                             subtitle = NULL,
                             tag = NULL,
                             caption = NULL,
                             bins = NULL,
                             category.focus = NULL,
                             category.sort = TRUE,
                             colour = getOption("plot2.colours", "certe"),
                             colour.extended_spectrum = FALSE,
                             colour.fill = NULL,
                             colour.opacity = 0,
                             datalabels = TRUE,
                             datalabels.fill = "white",
                             datalabels.round = if_else(y.percent, 1, 2),
                             datalabels.size = 3,
                             decimal_comma = uses_decimal_comma(),
                             facet.bold = TRUE,
                             facet.drop = FALSE,
                             facet.fill = NULL,
                             facet.fixed_y = FALSE,
                             facet.italic = FALSE,
                             facet.margin = 8,
                             facet.nrow = NULL,
                             facet.position = "top",
                             facet.relative = FALSE,
                             facet.repeat_lbls_x = TRUE,
                             facet.repeat_lbls_y = TRUE,
                             facet.size = 10,
                             facet.sort = TRUE,
                             horizontal = FALSE,
                             jitter_seed = 1,
                             legend.barheight = 5,
                             legend.barwidth = 1,
                             legend.italic = FALSE,
                             legend.position = "top",
                             legend.reverse = NULL,
                             legend.title = "",
                             linetype = 1,
                             markdown = TRUE,
                             na.replace = "(onbekend)",
                             na.rm = FALSE,
                             print = FALSE,
                             reverse = horizontal,
                             size = ifelse(type %in% c("point", "jitter", "boxplot", "violin"), 2, 0.75),
                             stacked = FALSE,
                             stackedpercent = FALSE,
                             subtitle.maxlength = 60,
                             summarise_function = base::sum,
                             text.factor = 1,
                             text.font_family = "Calibri",
                             theme = theme_minimal2(),
                             title.maxlength = 60,
                             width = ifelse(type %in% c("jitter", "boxplot", "violin"), 0.75, 0.5),
                             x.date_breaks = "1 day",
                             x.date_labels = "d mmm",
                             x.expand = 0.5,
                             x.lbl_align = NULL,
                             x.lbl_angle = 0,
                             x.lbl_italic = FALSE,
                             x.limits = NULL,
                             x.max_items = Inf,
                             x.max_txt = "(rest, x{n})",
                             x.position = "bottom",
                             x.remove = FALSE,
                             x.sort = TRUE,
                             x.title = NULL,
                             y.24h = FALSE,
                             y.age = FALSE,
                             y.breaks = NULL,
                             y.expand = 0.25,
                             y.labels = NULL,
                             y.limits = NULL,
                             y.percent = FALSE,
                             y.percent_break = 10,
                             y.position = "left",
                             y.remove = FALSE,
                             y.title = NULL,
                             y.trans = "identity",
                             ...) {

  type <- validate_type(type)

  x_name <- gsub('"', "", deparse(substitute(x)))
  # if (x_name %in% ls(envir = globalenv())) message("Taking variable '", x_name, "' from Global Environment")
  y_name <- gsub('"', "", deparse(substitute(y)))
  # if (y_name %in% ls(envir = globalenv())) message("Taking variable '", y_name, "' from Global Environment")
  category_name <- gsub('"', "", deparse(substitute(category)))
  # if (category_name %in% ls(envir = globalenv())) message("Taking variable '", category_name, "' from Global Environment")

  # support quoted and unquoted parameters for x, y, category and facet
  x_set <- !missing(x)
  x_enquo <- enquo(x)
  x <- eval_plot3(var = x_enquo, data = .data, misses_var = !x_set)
  y_set <- !missing(y)
  y_enquo <- enquo(y)
  y <- eval_plot3(var = y_enquo, data = .data, misses_var = !y_set)
  category_set <- !missing(category)
  category_enquo <- enquo(category)
  if (category_set & !inherits(try(eval(category), silent = TRUE), "try-error")) {
    # coming from plot3.grouped_df
    category_name <- eval(category)
  }
  category <- eval_plot3(var = category_enquo, data = .data, misses_var = !category_set)

  facet_set <- !missing(facet)
  facet_enquo <- enquo(facet)
  facet <- eval_plot3(var = facet_enquo, data = .data, misses_var = !facet_set)

  cat(crayon::red("x_set:\n"))
  print(x_set)
  cat(crayon::red("x_name:\n"))
  print(x_name)
  cat(crayon::red("x_enquo:\n"))
  print(x_enquo)
  cat(crayon::red("x:\n"))
  print(x)
  cat(crayon::red("y_set:\n"))
  print(y_set)
  cat(crayon::red("y_name:\n"))
  print(y_name)
  cat(crayon::red("y_enquo:\n"))
  print(y_enquo)
  cat(crayon::red("y:\n"))
  print(y)
  cat(crayon::red("category_set:\n"))
  print(category_set)
  cat(crayon::red("category_name:\n"))
  print(category_name)
  cat(crayon::red("category_enquo:\n"))
  print(category_enquo)
  cat(crayon::red("category:\n"))
  print(category)
  cat(crayon::red("facet_set:\n"))
  print(facet_set)
  cat(crayon::red("facet_enquo:\n"))
  print(facet_enquo)
  cat(crayon::red("facet:\n"))
  print(facet)
  print("---")

  data_list <- list()
  has_x <- FALSE
  has_y <- FALSE
  has_category <- FALSE
  has_facet <- FALSE
  if (length(x) > 0 & !isFALSE(x)) {
    data_list$x <- x
    has_x <- TRUE
  }
  if (length(y) > 0) {
    data_list$y <- y
    has_y <- TRUE
  }
  if (length(category) > 0 & !isFALSE(category)) {
    data_list$category <- category
    has_category <- TRUE
  }
  if (length(facet) > 0 & !isFALSE(facet)) {
    data_list$facet <- facet
    has_facet <- TRUE
  }

  # Fortify -----------------------------------------------------------------

  # fortify all data, first try to determine x, y and category if they're missing
  cols_numeric <- sapply(.data, function(col) is.numeric(col) | is(col, "difftime"))
  cols_non_numeric <- !cols_numeric

  if (!has_y) {
    # first try to find numeric cols
    y_name <- colnames(.data)[cols_numeric][1L]
    if (!is.na(y_name) && y_name == x_name & sum(cols_numeric) > 1) {
      # y was already set, so change to other numeric column
      y_name <- colnames(.data)[cols_numeric][2L]
    }
    if (!is.na(y_name)) {
      data_list$y <- .data[, y_name, drop = TRUE]
      has_y <- TRUE
    }
  }

  if (!has_x & !isFALSE(x)) {
    # first try to find non-numeric cols
    x_name <- colnames(.data)[cols_non_numeric][1L]
    # make x numeric if more than 1 numeric column - then y will become the second numeric column
    if (is.na(x_name) & sum(cols_numeric) > 1) {
      x_name <- colnames(.data)[cols_numeric][1L]
      if (y_name == x_name) {
        # y was already set, so change to other numeric column
        x_name <- colnames(.data)[cols_numeric][2L]
      }
    }
    if (is.na(x_name)) {
      # make 'x' just 1:n
      data_list$x <- as.integer(seq_len(length(data_list$y)))
      x_name <- "x"
    } else {
      data_list$x <- .data[, x_name, drop = TRUE]
    }
    has_x <- TRUE
  }

  if (!has_y) {
    # still no y, now we'll create it
    data_list$y <- 1
    y_name <- "y"
    has_y <- TRUE
  }

  if (!has_category & has_x & !isFALSE(category) & sum(cols_non_numeric) > 1) {
    # first try to find non-numeric cols
    category_cols <- (cols_non_numeric & colnames(.data) != x_name)
    # if (which(category_cols)[1L] < which(names(data_list) == "y")[1L]) {
    # when auto-determining, the category column must be before the y column
    category_name <- colnames(.data)[which(category_cols)][1L]
    data_list$category <- .data[, category_name, drop = TRUE]
    has_category <- TRUE
    # }
  }

  # Create data and transform if needed -------------------------------------

  # create new data set based on original data and set variables
  new_data <- as.data.frame(data_list, stringsAsFactors = FALSE)

  # sort the data, always return factors in case of non-numeric values
  new_data$x <- sort_data(var = new_data$x,
                              sort_method = x.sort,
                              datapoints = new_data$y,
                              summarise_function = summarise_function,
                              horizontal = horizontal)
  # maximum number of x must be limited
  if (x.max_items < length(unique(new_data$x))) {
    keep_x <- sort(unique(new_data$x))[seq_len(x.max_items - 1)] # minus one for the extra created group 'Other'
    new_level_name <- gsub("{n}", length(unique(new_data$x)) - length(keep_x), x.max_txt, fixed = TRUE)
    levels(new_data$x) <- c(levels(new_data$x), new_level_name)
    new_data$x[which(!new_data$x %in% keep_x)] <- new_level_name

    # summarise on new groups

    new_data <- new_data %>%
      (function(df) {
        if (has_category & has_facet) {
          group_by(df, x, category, facet)
        } else if (has_category) {
          group_by(df, x, category)
        } else if (has_facet) {
          group_by(df, x, facet)
        } else {
          group_by(df, x)
        }}) %>%
      summarise(y = summarise_function(y)) %>%
      ungroup()
  }
  # sort on x, important when piping plot3()'s after plot3()'s
  new_data <- new_data[order(new_data$x), , drop = FALSE]

  if (has_category) {
    new_data$category <- sort_data(var = new_data$category,
                                       sort_method = category.sort,
                                       datapoints = new_data$y,
                                       summarise_function = summarise_function,
                                       horizontal = horizontal)
  }
  if (has_facet) {
    new_data$facet <- sort_data(var = new_data$facet,
                                    sort_method = facet.sort,
                                    datapoints = new_data$y,
                                    summarise_function = summarise_function,
                                    horizontal = horizontal)
  }

  print(as_tibble(new_data))

  # Create datalabels -------------------------------------------------------

  # datalabels <- as.character(datalabels)
  # datalabels[datalabels %in% c("", "0")] <- NA
  # grafiek$data$lbls.nieuw <- datalabels



  # Set ggplot model --------------------------------------------------------

  # sort column order: x, category, facet, y
  new_data <- new_data[, na.omit(c("x",
                                   ifelse(has_category, "category", character(0)),
                                   ifelse(has_facet, "facet", character(0)),
                                   "y")), drop = FALSE]

  # assign original names to data
  colnames(new_data)[colnames(new_data) == "x"] <- x_name
  colnames(new_data)[colnames(new_data) == "y"] <- y_name
  if (has_category) colnames(new_data)[colnames(new_data) == "category"] <- category_name

  entick <- function(x) paste0("`", x, "`")
  p <- ggplot(data = new_data,
              mapping = if (has_category) {
                aes_string(x = entick(x_name),
                           y = entick(y_name),
                           group = 1,
                           colour = entick(category_name),
                           fill = entick(category_name))
              } else {
                aes_string(x = entick(x_name),
                           y = entick(y_name),
                           group = 1)
              })

  if (is.null(type)) type <- FALSE
  if (type %in% c("column", "col", "bar")) {
    p <- p +
      geom_bar(width = width,
               stat = "identity",
               # small whitespace between columns:
               position = if (isTRUE(stacked)) {
                 position_stack(reverse = reverse)
               } else if (isTRUE(stackedpercent)) {
                 position_fill(reverse = reverse)
               } else {
                 position_dodge2(width = width * 1.05,
                                 preserve = "single")
               },
               na.rm = TRUE)
  } else if (type %in% c("line")) {
    p <- p +
      geom_line(lineend = 'round',
                size = size,
                linetype = linetype,
                na.rm = TRUE)
  } else if (!isFALSE(type)) {
    # try to put some parameters into the requested geom
    warning("'geom_", type, "' currently only loosely supported in plot3()", call. = FALSE)
    geom_other <- get(paste0("geom_", type), envir = asNamespace("ggplot2"))
    p <- p +
      geom_other(size = size,
                 width = width,
                 linetype = linetype,
                 na.rm = TRUE)
  }

  # Add datalabels ----------------------------------------------------------


  # Markdown and theme ------------------------------------------------------
  if (empty(theme)) {
    # turn to default ggplot2 theme, so we can:
    # - extend all element_text() classes with element_markdown()
    # - add all theme options set as parameters, like legend position
    theme <- theme_gray()
  }
  if (inherits(theme, "theme")) {
    if (isTRUE(markdown)) {
      # add 'element_markdown' to all text classes, which the ggtext pkg will use to print in markdown
      # for this, the ggtext pkg has at least to be installed, but not loaded
      attr_bak <- attributes(t)
      theme <- lapply(theme, function(el) {
        if (inherits(el, "element_text")) {
          class(el) <- c("element_markdown", class(el))
        }
        el
      })
      attributes(t) <- attr_bak # restore class and all other attributes
    }
    p <- p + theme
  } else if (!empty(theme)) {
    stop("'theme' must be a valid ggplot theme")
  }

  # Titles ------------------------------------------------------------------
  if (!empty(x.title)) p <- p + labs(x = x.title)
  if (!empty(y.title)) p <- p + labs(y = y.title)
  if (!empty(title)) p <- p + labs(title = title)
  if (!empty(subtitle)) p <- p + labs(subtitle = subtitle)
  if (!empty(tag)) p <- p + labs(tag = tag)
  if (!empty(caption)) p <- p + labs(caption = caption)

  # Positions ---------------------------------------------------------------
  p <- p + theme(legend.position = validate_legendposition(legend.position))


  # Return ------------------------------------------------------------------
  if (isTRUE(print)) {
    print(p)
  } else {
    p
  }
}

#' @rdname plot3
#' @export
plot3.ggplot <- function(plot, type, ...) {
  plot + geom_line()
}

#' @rdname plot3
#' @export
plot3.sf <- function(.data,
                     ...,
                     datalabels = FALSE,
                     legend.position = "right",
                     colour = "grey50",
                     y.expand = 0,
                     x.expand = 0) {

}

#' Check if _locale_ uses comma as decimal separator
#'
#' This function returns `TRUE` if [Sys.getlocale()] contains one of the countries where a comma "," is used as decimal separator (see Source). This function returns `FALSE` otherwise.
#' @return logical
#' @source <https://en.wikipedia.org/wiki/Decimal_mark#Countries_using_Arabic_numerals_with_decimal_comma>
#' @export
uses_decimal_comma <- function() {
  countries <-
    c("Albania", "Algeria", "Andorra", "Angola", "Argentina", "Armenia", "Austria",
      "Azerbaijan", "Belarus", "Belgium", "Bolivia", "Bosnia", "Brazil",
      "Bulgaria", "Cameroon", "Chile", "Colombia", "CostaRica", "Croatia", "Cuba",
      "Cyprus", "Czech", "Denmark", "East.*Timor", "Ecuador", "Estonia", "Faroes",
      "Finland", "France", "Germany", "Georgia", "Greece", "Greenland", "Hungary",
      "Iceland", "Indonesia", "Italy", "Kazakhstan", "Kosovo", "Kyrgyzstan", "Latvia",
      "Lebanon", "Lithuania", "Macedonia", "Moldova", "Mongolia", "Morocco", "Mozambique",
      "Namibia", "Netherlands", "Norway", "Paraguay", "Peru", "Poland", "Portugal",
      "Romania", "Russia", "Serbia", "Slovakia", "Slovenia", "South.*Africa", "Spain",
      "Sweden", "Switzerland", "Tunisia", "Turkey", "Ukraine", "Uruguay", "Uzbekistan",
      "Venezuela", "Vietnam")
  any(sapply(countries, grepl, Sys.getlocale(), ignore.case = TRUE))
}

#' @importFrom rlang eval_tidy as_name
eval_plot3 <- function(var, data, misses_var) {
  var <- tryCatch(eval_tidy(var),
                  error = function(e) tryCatch(eval_tidy(var, data = data),
                                               error = function(e) as_name(var)))
  if (!is.null(var) && var %in% colnames(data)) {
    var <- data[, var, drop = TRUE]
  }
  if (is.null(var) && isFALSE(misses_var)) {
    return(FALSE)
  }
  var
}

theme_minimal2 <- function(subtitle.colour = "black",
                           x.lbl.angle = 0,
                           x.lbl.align = 0.5,
                           horizontal = FALSE,
                           font.family = 'Verdana',
                           legend.position = 'top',
                           legend.italic = FALSE,
                           text.factor = 1,
                           x.category.fill = NA,
                           x.category.bold = TRUE,
                           x.category.italic = FALSE,
                           x.category.size = 10,
                           x.category.margin = 8,
                           has_subtitle = FALSE,
                           ...) {

  text_function <- ggplot2::element_text

  legend.position <- validate_legendposition(legend.position)
  if (legend.italic == TRUE) {
    legend.italic <- "italic"
  } else {
    legend.italic <- NULL
  }

  t <- theme_bw(base_size = 11 * text.factor,
                base_family = font.family) %+replace%
    theme(
      axis.text.x = element_text(angle = x.lbl.angle, hjust = x.lbl.align, margin = margin(3, 0, 0, 0)),
      # # getallen van y-as op de lijn plaatsen, links uitgelijnd
      # axis.text.y = element_text(margin = margin(l = 10 * text.factor,
      #                                            r = -21 * text.factor, unit = "pt"),
      #                            hjust = 0,
      #                            vjust = -0.5),
      axis.title.x = text_function(margin = margin(14, 0, 0, 0)),
      axis.title.y = text_function(margin = margin(0, 14, 0, 0), angle = 90),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_line(size = 0.75, colour = 'grey75'),
      axis.ticks.length = unit(2, "pt"),
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.key.size = unit(11 * text.factor, 'pt'), # blokjes en lijnen links van tekst in legenda
      legend.text = element_text(size = unit(9 * text.factor, 'pt'), # tekst zelf
                                 margin = margin(l = 1, r = 6, unit = "pt"), # ruimte links en rechts van tekst
                                 face = legend.italic),
      legend.position = legend.position,
      legend.title = text_function(face = 'bold', size = unit(10 * text.factor, 'pt')),
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(size = 0.375, colour = 'grey75'),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_line(size = 0.25, colour = 'grey85'),
      axis.line = element_line(size = 0.375, colour = 'grey75'),
      axis.line.y = element_blank(),
      plot.margin = unit(c(5,                                  # top
                           ifelse(horizontal == TRUE, 25, 5),  # right
                           5,                                  # bottom
                           5),                                 # left
                         units = "pt"),
      plot.background = element_blank(),
      plot.subtitle = text_function(size = unit(11 * text.factor, 'pt'),
                                    margin = margin(0, 0, ifelse(has_subtitle == TRUE, 15, 7), 0),
                                    hjust = 0.5,
                                    colour = subtitle.colour),
      plot.title = text_function(size = unit(13 * text.factor, 'pt'),
                                 margin = margin(0, 0, ifelse(has_subtitle == TRUE, 7, 15), 0),
                                 hjust = 0.5,
                                 colour = 'black'),
      plot.caption = text_function(colour = 'grey50',
                                   size = unit(10 * text.factor, 'pt'),
                                   hjust = 1),
      plot.tag = text_function(size = unit(14 * text.factor, 'pt'),
                               margin = margin(0, 0, 0, 0),
                               hjust = 0,
                               colour = 'black',
                               face = "bold"),
      # voor x.category (facet_wrap):
      strip.background = element_rect(fill = x.category.fill, colour = '#FFFFFF00'),
      strip.text = element_text(face = case_when(x.category.bold & x.category.italic ~ 'bold.italic',
                                                 x.category.bold ~ 'bold',
                                                 x.category.italic ~ 'italic',
                                                 TRUE ~ 'plain'),
                                size = unit(x.category.size * text.factor, 'pt'),
                                margin = margin(t = x.category.margin, b = x.category.margin / 2)),
      strip.switch.pad.wrap = unit(10 * text.factor, "pt"),
      strip.placement = 'outside',
      complete = TRUE)

  if (x.lbl.angle < 90 & x.lbl.angle > 10) {
    t <- t +
      theme(axis.text.x = element_text(margin = margin(-5, 0, 0, 0)))
  }

  if (horizontal == TRUE) {
    t <- t %+replace%
      theme(panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_line(size = 0.375, colour = 'grey75'),
            panel.grid.minor.x = element_line(size = 0.25, colour = 'grey85'),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_line(size = 0.75, colour = 'grey75'),
            # tekst op y-as (wat x-as was) rechts align en minder ruimte geven
            axis.text.y = element_text(hjust = 1.0, vjust = 0.3, margin = margin(0, 3, 0, 0)),
            axis.text.x = element_text(margin = margin(0, 0, 0, 0)),
            axis.line.y = element_line(size = 0.375, colour = 'grey75'),
            axis.line.x = element_blank())
  }

  if (length(list(...)) > 0) {
    t <- t %+replace% theme(...)
  }

  t
}

empty <- function(x) is.null(x) || isFALSE(x) || identical(x, "")

#' @importFrom forcats fct_inorder fct_reorder
sort_data <- function(var, sort_method, datapoints, summarise_function, horizontal) {
  if (is.null(sort_method) || tolower(sort_method) == "asis") {
    # don't sort
    return(var)
  }

  if (isTRUE(sort_method)) {
    if (is.factor(var)) {
      # don't sort at all
      return(var)
    } else {
      sort_method <- "asc"
    }
  }

  sort_method.bak <- sort_method
  sort_method <- tolower(sort_method[1L])
  sort_method <- gsub("asc[a-z]+", "asc", sort_method)
  sort_method <- gsub("desc[a-z]+", "desc", sort_method)

  if (is.numeric(var)) {
    values <- as.double(var)
  } else {
    values <- as.character(var)
  }

  if (grepl("freq$", sort_method)) {
    sort_method <- paste0(sort_method, "-desc")
  }

  if (isTRUE(horizontal)) {
    # reverse asc and desc
    sort_method <- gsub("asc", "asc2", sort_method)
    sort_method <- gsub("desc", "asc", sort_method)
    sort_method <- gsub("asc2", "desc", sort_method)
  }

  if (sort_method %in% c("alpha", "alpha-asc", "asc")) {
    # alphabetical, or ascending
    out <- factor(values, levels = sort(unique(values)))
  } else if (sort_method %in% c("alpha-desc", "desc")) {
    out <- factor(values, levels = rev(sort(unique(values))))
  } else if (sort_method %in% c("false", "order", "inorder")) {
    out <- fct_inorder(as.character(values))
  } else if (sort_method %in% c("freq-asc", "infreq-asc")) {
    out <- fct_reorder(.f = as.character(values),
                       .x = datapoints,
                       .fun = summarise_function,
                       .desc = FALSE)
  } else if (sort_method %in% c("freq-desc", "infreq-desc")) {
    out <- fct_reorder(.f = as.character(values),
                       .x = datapoints,
                       .fun = summarise_function,
                       .desc = TRUE)
  } else {
    stop("invalid sorting option: '", sort_method.bak, "'")
  }

  out
}

sort_data <- function(data, var, count_var, sorting, summarise_function, horizontal) {
  if (!is.null(sorting)) { # sorting = NULL niet sorteren
    if (var != "") {
      if (data %>% pull(var) %>% is.numeric()) {
        values <- data %>% pull(var) %>% as.double()
      } else {
        values <- data %>% pull(var) %>% as.character()
      }
      if (sorting == TRUE) {
        if ("factor" %in% (data %>% pull(var) %>% class())) {
          sorting <- "" # helemaal niet sorteren
        } else {
          sorting <- "asc"
        }
      } else if (sorting == FALSE) {
        sorting <- "inorder"
      }
      sorting <- gsub("[^a-z-]+", "", tolower(sorting))
      if (sorting %like% "freq$") {
        sorting <- paste0(sorting, "-desc")
      }
      if (horizontal == TRUE) {
        # asc en desc omdraaien
        sorting <- gsub("asc", "asc2", sorting)
        sorting <- gsub("desc", "asc", sorting)
        sorting <- gsub("asc2", "desc", sorting)
      }
      if (sorting %in% c("alpha", "asc")) {
        data[, var] <- base::factor(values,
                                    levels = values %>%
                                      unique() %>%
                                      str_sort(numeric = any(data %>% pull(var) %like% "[0-9]")))
      } else if (sorting == "desc") {
        data[, var] <- base::factor(values,
                                    levels = values %>%
                                      unique() %>%
                                      str_sort(numeric = any(data %>% pull(var) %like% "[0-9]"),
                                               decreasing = TRUE))
      } else if (sorting %in% c("inorder", "order")) {
        data[, var] <- forcats::fct_inorder(values %>% as.character())
      } else if (sorting %in% c("infreq-desc", "freq-desc")) {
        data[, var] <- forcats::fct_reorder(.f = values %>% as.character(),
                                            .x = data %>% pull(count_var),
                                            .fun = summarise_function,
                                            .desc = FALSE) # dit moet echt FALSE zijn
      } else if (sorting %in% c("infreq-asc", "freq-asc")) {
        data[, var] <- forcats::fct_reorder(.f = values %>% as.character(),
                                            .x = data %>% pull(count_var),
                                            .fun = summarise_function,
                                            .desc = TRUE) # dit moet echt TRUE zijn
      }
    }
  }
  data
}



add_datalabels <- function(grafiek,
                           data,
                           x,
                           y,
                           datalabels,
                           datalabels.fill,
                           datalabels.size,
                           has_category,
                           stacked,
                           stackedpercent,
                           horizontal,
                           reverse,
                           font.family,
                           text.factor,
                           width,
                           type) {

  width <- ifelse(!is.null(width), width, 0.5)

  datalabel_fill <- datalabels.fill
  if (stacked == FALSE & stackedpercent == FALSE) {
    datalabel_colour <- 'gray25'
    datalabel_alpha <- 1
  } else {
    datalabel_colour <- 'black'
    datalabel_alpha <- 0.25
  }

  if (any(grepl('%*%', datalabels, fixed = TRUE) == TRUE)) {
    datalabels <- as.expression(datalabels)
    as_formula <- TRUE
  } else {
    as_formula <- FALSE
  }

  h.label <- 0.5
  h.tekst <- 0.5
  v.label <- -0.1
  v.tekst <- -0.75
  if (horizontal == TRUE) {
    v.label <- 0.5
    v.tekst <- 0.5
    h.label <- -0.1
    h.tekst <- -0.25
  }

  textsize.txt <- text.factor * datalabels.size
  textsize.lbl <- (text.factor * 1.25) + textsize.txt
  if (text.factor == 1) {
    textsize.lbl <- textsize.txt * 0.75
  }

  if (has_category) {
    # meerdere series

    if (stackedpercent == TRUE) {
      lbls <- grafiek +
        # rechthoek achter tekst:
        geom_label(
          aes(label = lbls.nieuw),
          parse = as_formula,
          position = position_fill(reverse = reverse, vjust = 0.5),
          vjust = 0.5,
          hjust = 0.5,
          size = textsize.lbl,
          fill = datalabel_fill,
          alpha = datalabel_alpha,
          colour = NA,
          family = font.family
        ) +
        geom_text(
          aes(label = lbls.nieuw),
          parse = as_formula,
          position = position_fill(reverse = reverse, vjust = 0.5),
          vjust = 0.5,
          hjust = 0.5,
          size = textsize.txt,
          colour = datalabel_colour,
          family = font.family)

    } else if (stacked == TRUE) {
      lbls <- grafiek +
        # rechthoek achter tekst:
        geom_label(
          aes(label = lbls.nieuw),
          parse = as_formula,
          position = position_stack(reverse = reverse, vjust = 0.5),
          size = textsize.lbl,
          fill = datalabel_fill,
          alpha = datalabel_alpha,
          colour = NA,
          family = font.family,
          na.rm = TRUE
        ) +
        geom_text(
          aes(label = lbls.nieuw),
          parse = as_formula,
          position = position_stack(reverse = reverse, vjust = 0.5),
          size = textsize.txt,
          colour = datalabel_colour,
          family = font.family,
          na.rm = TRUE)
    } else {
      lbls <- grafiek +
        # rechthoek achter tekst:
        geom_label(
          aes(label = lbls.nieuw),
          parse = as_formula,
          position = position_dodge2(width = width, preserve = "single"),
          size = textsize.lbl,
          colour = NA,
          fill = datalabel_fill,
          alpha = datalabel_alpha,
          label.padding = unit(0.25, 'lines'),
          label.r = unit(0, 'lines'),
          vjust = v.label,
          hjust = h.label,
          family = font.family,
          na.rm = TRUE) +
        geom_text(
          aes(label = lbls.nieuw),
          parse = as_formula,
          position = position_dodge2(width = width, preserve = "single"),
          vjust = v.tekst,
          hjust = h.tekst,
          size = textsize.txt,
          colour = datalabel_colour,
          family = font.family,
          na.rm = TRUE)
    }

  } else {
    # enkele serie
    lbls <- grafiek +
      # rechthoek achter tekst:
      geom_label(
        aes(label = lbls.nieuw),
        parse = as_formula,
        size = textsize.lbl,
        colour = NA,
        fill = datalabel_fill,
        label.padding = unit(0.25, 'lines'),
        label.r = unit(0, 'lines'),
        vjust = v.label,
        hjust = h.label,
        family = font.family,
        na.rm = TRUE) +
      # tekst zelf:
      geom_text(
        aes(label = lbls.nieuw),
        parse = as_formula,
        size = textsize.txt,
        colour = datalabel_colour,
        vjust = v.tekst,
        hjust = h.tekst,
        family = font.family,
        na.rm = TRUE)
  }

  if (stacked == FALSE & stackedpercent == FALSE) {
    # laag label helemaal naar onderen verplaatsen, dat is de een na laatste;
    # daardoor valt het label (kleur achtergrond) nooit over een bar heen
    alle_lagen <- c(1:length(lbls$layers))
    label_laag <- length(alle_lagen) - 1
    rest_lagen <- alle_lagen[-label_laag]
    lbls$layers <- lbls$layers[c(label_laag, rest_lagen)]
  }

  lbls

}
