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

# These functions are universal; they are internal functions in every Certe R package.
# This is to prevent dependency on one another.
# They are also exported in the 'certetoolbox' package.

like <- function (x, pattern) {
  x <- tolower(x)
  pattern <- tolower(pattern)
  if (length(pattern) == 1) {
    # only one pattern
    grepl(pattern, x, ignore.case = FALSE, fixed = FALSE,  perl = TRUE)
  } else {
    # multiple patterns
    if (length(x) == 1) {
      x <- rep(x, length(pattern))
    } else {
      stop("'x' and 'pattern' must be of equal length", call. = FALSE)
    }
    unlist(mapply(FUN = grepl, x = x, pattern = pattern, 
                  fixed = FALSE, perl = TRUE, MoreArgs = list(ignore.case = FALSE), 
                  SIMPLIFY = FALSE, USE.NAMES = FALSE))
  }
}
`%like%` <- function(x, pattern) {
  like(x = x, pattern = pattern)
}
`%unlike%` <- function(x, pattern) {
  !like(x = x, pattern = pattern)
}
concat <- function(...) {
  paste(c(...), collapse = "", sep = "")
}
#' @importFrom yaml read_yaml
read_secret <- function(property, file = Sys.getenv("secrets_file")) {
  if (file == "" && identical(file, Sys.getenv("secrets_file"))) {
    warning("In read_secret(): environmental variable 'secrets_file' not set", call. = FALSE)
    return("")
  }
  contents <- read_yaml(file)
  if (!property %in% names(contents)) {
    warning("In read_secret(): property '", property, "' not found", call. = FALSE)
    return("")
  }
  contents[[property]]
}
