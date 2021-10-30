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

library(dplyr)
library(AMR)
admitted_patients <- example_isolates %>%
  group_by(patient_id) %>%
  filter(n() == 1) %>%
  ungroup() %>%
  transmute(date,
            gender,
            age,
            age_group = age_groups(age),
            hospital = hospital_id,
            ward = ifelse(ward_icu, "ICU", "Non-ICU")) %>%
  slice_sample(n = 250) %>%
  arrange(date)
