#' twinkle: Tools for working with [https://CRAN.R-project.org/package=stars] and [simple features](https://CRAN.R-project.org/package=sf).
#'
#'
#' @docType package
#' @name twinkle
#' @importFrom dplyr %>%
#' @importFrom rlang .data := !!
NULL

load_packages <- function(packages = c("rlang", "dplyr", "tidyselect", "sf", "stars")){
  for (p in packages){require(p, character.only = TRUE)}
}