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

plotdata <- data.frame(x = seq_len(8) + 10,
                       x_char = letters[1:8],
                       x_date = Sys.Date() - seq_len(8),
                       n = seq_len(8),
                       stringsAsFactors = FALSE)

test_that("S3 implementations work", {
  # bug_drug_combinations
  expect_s3_class(AMR::example_isolates |>
                    dplyr::select(mo, CIP, AMC) |>
                    AMR::bug_drug_combinations(FUN = AMR::mo_gramstain) |>
                    plot2(),
                  "gg")
  # sir_df
  expect_s3_class(AMR::example_isolates |>
                    dplyr::select(date, NIT, CIP, IPM, MEM) |>
                    dplyr::group_by(yr = format(date, "%Y")) |>
                    AMR::sir_df() |>
                    dplyr::filter(yr >= 2015) |>
                    plot2(x = yr),
                  "gg")
  # qc_test
  expect_s3_class(certestats::qc_test(rnorm(1000)) |> plot2(), "gg")
  # disease clusters
  expect_s3_class(data.frame(date = sample(seq(as.Date("2018-01-01"),
                                               as.Date("2022-12-31"),
                                               "1 day"),
                                           size = 300),
                             patient = sample(LETTERS, size = 300, replace = TRUE)) |> 
                    certestats::detect_disease_clusters(minimum_cases = 1, threshold_percentile = 0.75) |>
                    plot2(),
                  "gg")
  
  # type should become boxplot here
  expect_s3_class(plot2::admitted_patients |> plot2(x = hospital, y = certestats::z_score(age)), "gg")
  # this uses the format2_scientific function for the y axis
  expect_s3_class(plot2::admitted_patients |>
                    plot2(format(date, "%Y"),
                          certestats::z_score(age),
                          hospital,
                          y.scientific = TRUE),
                  "gg")
  expect_s3_class(certegis::geo_provincies |> plot2(markdown = TRUE), "gg")
  expect_s3_class(certegis::geo_provincies |> plot2(markdown = TRUE), "gg")
})

test_that("adding scales works", {
  library(ggplot2)
  expect_message(mtcars |>
                   plot2(mpg, hp, cyl, colour = "viridis") + 
                   scale_color_certe_c())
  expect_message(mtcars |>
                   plot2(mpg, hp, cyl, colour = "viridis") + 
                   scale_fill_certe_c())
  expect_message(mtcars |>
                   plot2(mpg, hp, rownames(mtcars), colour = "viridis") +
                   scale_color_certe_d())
  expect_message(mtcars |>
                   plot2(mpg, hp, rownames(mtcars), colour = "viridis") +
                   scale_fill_certe_d())
})

test_that("matrices works", {
  expect_s3_class(mtcars |>
                    stats::cor() |>
                    plot2(colour = c("certeblauw", "white", "certeroze"),
                          category.limits = c(-1, 1)), "gg")
})

test_that("errorbars work", {
  expect_s3_class(plotdata |>
                    dplyr::mutate(error1 = n * 0.9, error2 = n * 1.1) |> 
                    plot2(type = "c", colour = "certeroze4") |>
                    plot2::add_errorbar(error1, error2), "gg")
})
