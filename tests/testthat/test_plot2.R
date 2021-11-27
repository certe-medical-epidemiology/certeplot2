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

test_that("general mapping works", {
  expect_equal(plotdata %>% plot2() %>% get_mapping() %>% names(),
               c("y", "x", "fill", "colour"))
  expect_equal(plotdata %>% plot2() %>% get_data() %>% get_x_name(), "x")
  expect_equal(plotdata %>% plot2() %>% get_data() %>% get_y_name(), "n")
  expect_equal(plotdata %>% plot2() %>% get_data() %>% get_category_name(), "x_char")
  expect_equal(plotdata %>% plot2() %>% get_data() %>% get_facet_name(), NULL)
})

test_that("x scale works", {
  expect_s3_class(plotdata %>% plot2(x = x_date))
})
