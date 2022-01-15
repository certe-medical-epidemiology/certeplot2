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

plotdata <- data.frame(x = seq_len(10) + 10,
                       x_char = letters[1:10],
                       x_date = Sys.Date() - seq_len(10),
                       n = seq_len(10),
                       stringsAsFactors = FALSE)

`%or%` <- function(a, b) if (is.null(a)) b else a

get_mapping <- function(plot) plot$mapping %>% sapply(deparse) %>% gsub("~", "", .)
get_layers <- function(plot) plot$layers
get_labels <- function(plot) unlist(plot$labels)
get_data <- function(plot) plot$data
get_range_x <- function(plot) {
  ggplot2::ggplot_build(plot)$layout$panel_scales_x[[1]]$limits %or% 
    ggplot2::ggplot_build(plot)$layout$panel_scales_x[[1]]$range$range
}
get_range_y <- function(plot) {
  ggplot2::ggplot_build(plot)$layout$panel_scales_y[[1]]$limits %or%
    ggplot2::ggplot_build(plot)$layout$panel_scales_y[[1]]$range$range
}

test_that("general types work", {
  expect_s3_class(plot2(rnorm(10, 10)), "gg")
  expect_s3_class(plot2(rnorm(10, 10), type = "l"), "gg")
  expect_s3_class(plot2(mtcars, mpg^2, hp^2), "gg")
  expect_s3_class(plot2(mtcars, mpg^2, hp^2, smooth = TRUE), "gg")
  expect_s3_class(iris %>% plot2(Species), "gg")
  expect_s3_class(iris %>% plot2(Species, type = "violin"), "gg")
  expect_s3_class(iris %>% plot2(Species, type = "violin"), "gg")
  expect_s3_class(iris %>% plot2(Species, type = "blank"), "gg")
  expect_s3_class(iris %>% plot2(Species, type = "area"), "gg")
  expect_warning(iris %>% plot2(Species, type = "dotplot"))
  # difftime coercion to double:
  expect_s3_class(data.frame(x = letters[1:10],
                             y = difftime(Sys.time(), Sys.time() - seq_len(10))) %>%
                    plot2(),
                  "gg")
  expect_s3_class(admitted_patients %>% plot2(hospital, n(), where(is.character)), "gg")
  expect_s3_class(admitted_patients %>% plot2(hospital, n(), c(gender, ward)), "gg")
})

test_that("na.rm works", {
  # as characters
  df <- data.frame(hp = mtcars$hp,
                   letters = letters[seq_len(nrow(mtcars))],
                   stringsAsFactors = FALSE)
  expect_lt(df %>% plot2(na.rm = TRUE) %>% get_data() %>% nrow(), nrow(df))
  expect_lt(df %>% plot2(na.rm = FALSE) %>% get_data() %>% nrow(), nrow(df))
  # as factors
  df <- data.frame(hp = mtcars$hp,
                   letters = letters[seq_len(nrow(mtcars))],
                   stringsAsFactors = TRUE)
  expect_lt(df %>% plot2(na.rm = TRUE) %>% get_data() %>% nrow(), nrow(df))
  expect_lt(df %>% plot2(na.rm = FALSE) %>% get_data() %>% nrow(), nrow(df))
})

test_that("S3 implementations work", {
  # lm
  expect_s3_class(lm(mpg ~ hp, mtcars) %>% plot2(), "gg")
  # freq
  expect_s3_class(cleaner::freq(admitted_patients$hospital) %>% plot2(), "gg")
  # sf
  expect_s3_class(netherlands %>% plot2(), "gg")
  # bug_drug_combinations
  expect_s3_class(AMR::example_isolates %>%
                    select(mo, CIP, AMC) %>%
                    AMR::bug_drug_combinations(FUN = AMR::mo_gramstain) %>%
                    plot2(),
                  "gg")
  # qc_test
  expect_s3_class(certestats::qc_test(rnorm(1000)) %>% plot2(), "gg")
  # type should become boxplot here
  expect_s3_class(admitted_patients %>% plot2(x = hospital, y = certestats::z_score(age)), "gg")
})

test_that("general mapping works", {
  expect_equal(plotdata %>% plot2() %>% .$mapping %>% names(),
               c("y", "x", "fill", "colour"))
})

test_that("adding types works", {
  
  expect_length(mtcars %>% plot2(mpg, hp, cyl) %>% get_layers(), 1)
  expect_length(mtcars %>% plot2(mpg, hp, cyl) %>% add_line() %>% get_layers(), 2)
  expect_length(mtcars %>% plot2(mpg, hp, cyl) %>% add_col() %>% get_layers(), 2)
  expect_error(mtcars %>% plot2(mpg, hp, cyl) %>% add_type(type = NULL))
  
  p <- data.frame(x = c(1:100),
                  y = rnorm(100, 100, 25)) %>% 
    plot2()
  expect_length(p %>% get_layers(), 1)
  expect_length(p %>% 
                  add_line(mean(y)) %>%
                  get_layers(), 2)
  expect_length(p %>%
                  add_line(mean(y)) %>%
                  add_col(y / 5,
                          colour = "black",
                          colour_fill = "yellow",
                          width = 0.25) %>%
                  get_layers(), 3)
  
  expect_length(p %>%
                  add_line(certestats::rr_ewma(y, 0.75),
                           colour = "certeroze",
                           size = 2,
                           linetype = 2,
                           alpha = 0.5) %>%
                  get_layers(), 2)
  
  expect_length(plot2(certegis::geo_provincies, datalabels = FALSE) %>%
                  add_sf(certegis::geocode("Martini Ziekenhuis"),
                         colour = "certeroze") %>%
                  get_layers(), 2)
  expect_length(plot2(certegis::geo_provincies, datalabels = FALSE) %>%
                  add_sf(certegis::geocode("Martini Ziekenhuis"),
                         colour = "certeroze",
                         datalabels = place) %>%
                  get_layers(), 3)
})

test_that("titles work", {
  expect_equal(mtcars %>%
                 plot2(caption = "caption",
                       tag = "tag",
                       subtitle = "subtitle",
                       title = "title",
                       y.title = "y.title",
                       x.title = "x.title") %>%
                 get_labels(),
               c(caption = "caption",
                 tag = "tag",
                 subtitle = "subtitle",
                 title = "title",
                 y = "y.title",
                 x = "x.title"))
})

test_that("max items and sorting work", {
  expect_equal(admitted_patients %>%
                 plot2(x = hospital,
                       y = n(),
                       x.sort = "asc") %>% 
                 get_range_x(),
               c("A", "B", "C", "D"))
  expect_equal(admitted_patients %>%
                 plot2(x = hospital,
                       y = n(),
                       x.sort = "desc") %>% 
                 get_range_x(),
               c("D", "C", "B", "A"))
  expect_equal(admitted_patients %>%
                 plot2(x = hospital,
                       y = n(),
                       x.sort = "freq-desc") %>% 
                 get_range_x(),
               c("B", "D", "C", "A"))
  expect_equal(admitted_patients %>%
                 plot2(x = hospital,
                       y = n(),
                       x.sort = "freq-asc") %>% 
                 get_range_x(),
               c("A", "C", "D", "B"))
  expect_equal(admitted_patients %>%
                 plot2(x = hospital,
                       y = n(),
                       x.sort = "asc") %>% 
                 get_range_x(),
               c("A", "B", "C", "D"))
  expect_equal(admitted_patients %>%
                 plot2(x = format(date, "%Y"),
                       y = n(),
                       x.sort = "freq-desc",
                       x.max_items = 5) %>%
                 get_range_x(),
               c("2008", "2004", "2009", "2015", "(rest, x 12)"))
  
})

test_that("x scale works", {
  expect_s3_class(plotdata %>% plot2(x = x_date), "gg")
  plotdata %>% plot2(x = x_date, x.limits = c(Sys.Date() - 13, Sys.Date() + 2))
  expect_s3_class(plotdata %>% plot2(x = x_char), "gg")
  expect_s3_class(plotdata %>% plot2(n, type = "hist"), "gg")
  expect_s3_class(plotdata %>% plot2(n, type = "density"), "gg")
  expect_s3_class(plotdata %>% plot2(n, type = "jitter"), "gg")
  expect_s3_class(plotdata %>% plot2(type = "line"), "gg")
  expect_s3_class(plotdata %>% plot2(type = "barpercent"), "gg")
  
  p <- plotdata %>%
    plot2(x = x_date,
          x.limits = c(Sys.Date() - 13,
                       Sys.Date() + 2))
  expect_equal(p %>% get_range_x() %>% as.Date(origin = "1970-01-01"),
               c(Sys.Date() - 13 - 1, Sys.Date() + 2 + 1))
})

test_that("category scale works", {
  # set as numeric
  expect_s3_class(plotdata %>% .[1:4, ] %>% plot2(x = x_char, y = 1, category = n), "gg")
  # 2-colour scale
  expect_true(all(c(fill = "Petal.Length", colour = "Petal.Length") %in%
                    (plot2(iris, Sepal.Length, Sepal.Width, Petal.Length,
                           colour = c("red", "blue")) %>%
                       get_mapping())))
  # 3-colour scale
  expect_true(all(c(fill = "Petal.Length", colour = "Petal.Length") %in%
                    (plot2(iris, Sepal.Length, Sepal.Width, Petal.Length,
                           colour = c("red", "blue", "green")) %>%
                       get_mapping())))
  # multi-colour scale
  expect_true(all(c(fill = "Petal.Length", colour = "Petal.Length") %in%
                    (plot2(iris, Sepal.Length, Sepal.Width, Petal.Length,
                           colour = c("red", "blue", "green", "yellow")) %>%
                       get_mapping())))
  expect_true(all(c(fill = "Petal.Length", colour = "Petal.Length") %in%
                    (plot2(iris, Sepal.Length, Sepal.Width, Petal.Length,
                           colour = "certe") %>%
                       get_mapping())))
  expect_s3_class(mtcars %>% plot2(mpg, hp, as.character(cyl), category.focus = 2), "gg")
  expect_s3_class(mtcars %>% plot2(mpg, hp, as.character(cyl), category.focus = "4"), "gg")
})

test_that("facet scale works", {
  expect_s3_class(iris %>% plot2(as.integer(Sepal.Length), Sepal.Width, Petal.Length,
                                 Species), "gg")
  expect_s3_class(iris %>% plot2(as.integer(Sepal.Length), Sepal.Width, Petal.Length,
                                 Species, facet.relative = TRUE), "gg")
  expect_s3_class(iris %>% plot2(as.integer(Sepal.Length), Sepal.Width, Petal.Length,
                                 Species, facet.relative = TRUE, facet.nrow = 2), "gg")
})

test_that("blank plot works", {
  expect_s3_class(plotdata %>% subset(n < 0) %>% plot2(), "gg")
  expect_s3_class(plotdata %>% plot2(type = "blank"), "gg")
})

test_that("misc elements works", {
  expect_s3_class(plotdata %>% plot2(x_char, taxonomy_italic = TRUE), "gg")
})

test_that("get title works", {
 p <- plot2(mtcars, title = "Plotting **mpg** vs. **cyl**!")
  expect_equal(get_plot_title(p), "plotting_mpg_vs_cyl")
  expect_equal(get_plot_title(p, valid_filename = FALSE), "Plotting **mpg** vs. **cyl**!")
  expect_equal(get_plot_title(plot2(mtcars)), NA_character_)
  expect_equal(get_plot_title(plot2(mtcars), default = "test"), "test")
})

test_that("type validation works", {
  library(dplyr, warn.conflicts = FALSE)
  df <- tibble(x = letters[1:10],
               y = c(1:10),
               z = LETTERS[1:10],
               `_var_x` = x,
               `_var_y` = y,
               `_var_category` = z)
  expect_equal(validate_type(NULL, df), "geom_col")
  expect_equal(validate_type(NULL, df %>% select(-x, -`_var_x`)), "geom_boxplot")
  expect_equal(validate_type("a", df), "geom_area")
  expect_equal(validate_type("b", df), "geom_boxplot")
  expect_equal(validate_type("c", df), "geom_col")
  expect_equal(validate_type("h", df), "geom_histogram")
  expect_equal(validate_type("j", df), "geom_jitter")
  expect_equal(validate_type("l", df), "geom_line")
  expect_equal(validate_type("p", df), "geom_point")
  expect_equal(validate_type("r", df), "geom_ribbon")
  expect_equal(validate_type("v", df), "geom_violin")
})

test_that("adding scales works", {
  library(ggplot2)
  expect_message(mtcars %>%
                   plot2(mpg, hp, cyl, colour = "viridis") + 
                   scale_color_certe_c())
  expect_message(mtcars %>%
                   plot2(mpg, hp, cyl, colour = "viridis") + 
                   scale_fill_certe_c())
  expect_message(mtcars %>%
                   plot2(mpg, hp, rownames(.), colour = "viridis") +
                   scale_color_certe_d())
  expect_message(mtcars %>%
                   plot2(mpg, hp, rownames(.), colour = "viridis") +
                   scale_fill_certe_d())
})

test_that("moving layer works", {
  expect_s3_class((mtcars %>%
                     plot2(mpg, hp, cyl) +
                     geom_line(colour = "grey75")) %>%
                    move_layer(-1), "gg")
})

test_that("messaging works", {
  expect_message(plot2_message("test", print = TRUE))
  expect_message(plot2_warning("test", print = TRUE))
})

test_that("date labels work", {
  expect_equal(determine_date_breaks_labels(c( Sys.Date(), Sys.Date() + 30 - 1)),
               list(breaks = "1 day", labels = "d mmm"))
  expect_equal(determine_date_breaks_labels(c( Sys.Date(), Sys.Date() + 92 - 1)),
               list(breaks = "4 days", labels = "d mmm"))
  expect_equal(determine_date_breaks_labels(c( Sys.Date(), Sys.Date() + 183 - 1)),
               list(breaks = "2 weeks", labels = "d mmm"))
  expect_equal(determine_date_breaks_labels(c( Sys.Date(), Sys.Date() + 365 - 1)),
               list(breaks = "1 month", labels = "mmmm yyyy"))
  expect_equal(determine_date_breaks_labels(c( Sys.Date(), Sys.Date() + 730 - 1)),
               list(breaks = "3 months", labels = "mmm yyyy"))
  expect_equal(determine_date_breaks_labels(c( Sys.Date(), Sys.Date() + 1095 - 1)),
               list(breaks = "6 months", labels = "mmm yyyy"))
  expect_equal(determine_date_breaks_labels(c( Sys.Date(), Sys.Date() + 2556 - 1)),
               list(breaks = "1 year", labels = "mmm yyyy"))
})
