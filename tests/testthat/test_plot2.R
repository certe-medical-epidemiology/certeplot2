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

test_that("general types work", {
  expect_s3_class(plot2(rnorm(10, 10)), "gg")
  expect_s3_class(plot2(rnorm(10, 10), type = "l"), "gg")
  expect_s3_class(plot2(mtcars, mpg^2, hp^2), "gg")
  expect_s3_class(plot2(mtcars, mpg^2, hp^2, smooth = TRUE), "gg")
  expect_s3_class(iris %>% plot2(Species), "gg")
  expect_s3_class(iris %>% plot2(Species, type = "violin"), "gg")
  expect_s3_class(lm(mpg ~ hp, mtcars) %>% plot2(), "gg")
  expect_s3_class(cleaner::freq(admitted_patients$hospital) %>% plot2(), "gg")
  expect_s3_class(netherlands %>% plot2(), "gg")
  expect_s3_class(AMR::example_isolates %>%
                    select(mo, CIP, AMC) %>%
                    AMR::bug_drug_combinations(FUN = AMR::mo_gramstain) %>%
                    plot2(),
                  "gg")
  expect_s3_class(certestats::qc_test(rnorm(100)) %>% plot2(), "gg")
})

test_that("general mapping works", {
  expect_equal(plotdata %>% plot2() %>% .$mapping %>% names(),
               c("y", "x", "fill", "colour"))
})

test_that("x scale works", {
  expect_s3_class(plotdata %>% plot2(x = x_date), "gg")
  expect_s3_class(plotdata %>% plot2(x = x_char), "gg")
  expect_s3_class(plotdata %>% plot2(n, type = "hist"), "gg")
  expect_s3_class(plotdata %>% plot2(n, type = "density"), "gg")
  expect_s3_class(plotdata %>% plot2(n, type = "jitter"), "gg")
  expect_s3_class(plotdata %>% plot2(type = "line"), "gg")
  expect_s3_class(plotdata %>% plot2(type = "barpercent"), "gg")
})

test_that("blank plot works", {
  expect_s3_class(plotdata %>% subset(n < 0) %>% plot2(), "gg")
  expect_s3_class(plotdata %>% plot2(type = "blank"), "gg")
})

test_that("misc elements works", {
  expect_s3_class(plotdata %>% plot2(x_char, taxonomy_italic = TRUE), "gg")
})
