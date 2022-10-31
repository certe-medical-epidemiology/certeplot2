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

#' Convert Markdown to Plotmath Expression
#' 
#' This function converts common markdown language to an \R [plotmath][grDevices::plotmath] expression. [plot2()] uses this function internally to convert plot titles and axis titles.
#' @param x text to convert, only the first value will be evaluated
#' @details This function only supports common markdown (italic, bold, bold-italic, subscript, superscript), but also supports some additional functionalities for more advanced expressions using \R [plotmath][grDevices::plotmath]. Please see *Examples*.
#' 
#' *Rationale*: we tried to use the `ggtext` package instead to support markdown using their `element_markdown()` function for `ggplot2` [themes][ggplot2::theme()], but it currently supports only very limited markdown and no [plotmath][grDevices::plotmath] at all.
#' @export
#' @return An [expression]
#' @examples
#' # use '*' for italics, not '_', to prevent conflicts with variable naming
#' md_to_expression("this is *italic* text, this is _not italic_ text")
#' 
#' md_to_expression("this is **bold** text")
#' 
#' md_to_expression("this is ***bold and italic*** text")
#' 
#' # subscript and superscript can be done in HTML or markdown with curly brackets:
#' md_to_expression("this is some<sub>subscripted text</sub>, this is more_{additional subscripted} text")
#' md_to_expression("this is some<sup>superscripted text</sup>, this is more^{additional superscripted} text")
#' 
#' # use $...$ to use any plotmath expression as-is (see ?plotmath):
#' md_to_expression("text $omega$ text, $a[x]$")
#' 
#' mtcars |>
#'   plot2(mpg, hp,
#'         title = "*These are* the **Greek** lower $omega$ and upper $Omega$",
#'         x.title = "x_{mpg}",
#'         y.title = "y_{hp}")
#'         
#' mtcars |> 
#'   plot2(mpg, hp,
#'         title = "$f[X](x)==frac(1, sigma*sqrt(2*pi))*plain(e)^{frac(-(x-mu)^2, 2*sigma^2)}$",
#'         subtitle = "Some insane $widehat(plotmath)$ title")
md_to_expression <- function(x) {
  out <- x[1L]
  
  if (out %like% "^[$].+[$]$") {
    # a full plotmath expression
    return(parse(text = gsub("^[$](.+)[$]$", "\\1", out, perl = TRUE)))
  }
  out <- paste0("'", out, "'")
  
  # remove backticks
  out <- gsub("`", "", out, fixed = TRUE)
  
  # translate ***bold-italic***
  while (out %like% "[*]{3}.+[*]{3}") {
    out <- gsub("[*]{3}(.+?)[*]{3}", "', bolditalic('\\1'), '", out, perl = TRUE)
  }
  
  # translate **bold**
  while (out %like% "[*]{2}.+[*]{2}") {
    out <- gsub("[*]{2}(.+?)[*]{2}", "', bold('\\1'), '", out, perl = TRUE)
  }
  
  # translate *italic*
  while (out %like% "[*].+[*]") {
    out <- gsub("[*](.+?)[*]", "', italic('\\1'), '", out, perl = TRUE)
  }
  
  # translate sub<sub>script</sub>
  while (grepl("\\S+<sub>.+</sub>", out, ignore.case = FALSE)) {
    out <- gsub("(\\S+?)<sub>(.+?)</sub>", "', \\1['\\2'], '", out, perl = TRUE)
  }
  
  # translate super<sup>script</sup>
  while (grepl("\\S+<sup>.+</sup>", out, ignore.case = FALSE)) {
    out <- gsub("(\\S+?)<sup>(.+?)</sup>", "', \\1^'\\2', '", out, perl = TRUE)
  }
  
  # translate sub_{script}
  while (grepl("\\S+_[{].+[}]", out, ignore.case = FALSE)) {
    out <- gsub("(\\S+?)_[{](.+?)[}]", "', \\1['\\2'], '", out, perl = TRUE)
  }
  
  # translate super^{script}
  out <- gsub("\\^([a-zA-Z0-9,._-]+)", "^{\\1}", out)
  while (grepl("\\S+\\^[{].+[}]", out, ignore.case = FALSE)) {
    out <- gsub("(\\S+?)\\^[{](.+?)[}]+?", "\\1'^'\\2', '", out, perl = TRUE)
  }
  
  # translate $plotmath$, such as $omega$
  while (out %like% "[$].+[$]") {
    out <- gsub("[$](.+?)[$]", "', \\1, '", out, perl = TRUE)
  }
  
  # clean up
  out <- gsub("^', '?'?", "", out)
  out <- gsub("^'', ", "", out)
  out <- gsub(", ''$", "", out)
  out <- gsub(", '$", "", out)
  out <- gsub("''", "'", out, fixed = TRUE)
  out <- gsub("), ', '^", ")^", out, fixed = TRUE)
  out <- gsub(", '^", "^", out, fixed = TRUE)
  out <- gsub("^, ", "", out)
  
  tryCatch(parse(text = paste0("paste(", out, ")")),
           error = function(e) {
             stop("This cannot be parsed by md_to_expression(): \"", out,
                  "\"\n\nFor more complex expressions, start and end with '$' to write in plotmath, or use parse(text = \"...\").",
                  call. = FALSE)
           })
}
