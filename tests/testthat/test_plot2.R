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
get_data <- function(plot) plot$data
get_xrange <- function(plot) {
  ggplot2::ggplot_build(plot)$layout$panel_scales_x[[1]]$limits %or% 
    ggplot2::ggplot_build(plot)$layout$panel_scales_x[[1]]$range$range
}
get_yrange <- function(plot) {
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
  expect_warning(iris %>% plot2(Species, type = "area"))
  # difftime coercion to double:
  expect_s3_class(data.frame(x = letters[1:10],
                             y = difftime(Sys.time(), Sys.time() - seq_len(10))) %>%
                    plot2(),
                  "gg")
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
})

test_that("general mapping works", {
  expect_equal(plotdata %>% plot2() %>% .$mapping %>% names(),
               c("y", "x", "fill", "colour"))
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
  expect_equal(p %>% get_xrange() %>% as.Date(origin = "1970-01-01"),
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

test_that("adding elements works", {
  expect_length(mtcars %>% plot2(mpg, hp, cyl) %>% get_layers(), 1)
  expect_length(mtcars %>% plot2(mpg, hp, cyl) %>% add_line() %>% get_layers(), 2)
  expect_length(mtcars %>% plot2(mpg, hp, cyl) %>% add_col() %>% get_layers(), 2)
  expect_error(mtcars %>% plot2(mpg, hp, cyl) %>% add_type(type = NULL))
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
