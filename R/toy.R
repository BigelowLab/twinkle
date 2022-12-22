#' Create a polygon object suitable for use with the toy object
#' 
#' @export
#' @param nc integer, number of columns
#' @param nr integer, number of rows
#' @return sf POLYGON object
toy_polygon <- function(nc = 10, nr = 10){
  dx <- nc * 0.2
  xc <- nc/2
  dy <- nr * 0.2
  yc <- nr/2
  
  p <- rbind(c(xc, nr-dy), c(nc - dx, yc), c(xc, dy), c(xc - dx, yc - dy), c(xc, nr-dy))
  g <- sf::st_as_sfc(sf::st_as_text(sf::st_polygon(list(p))))
  sf::st_as_sf(dplyr::tibble(id = 1), geom = g)
}

#' Create a points object suitable for use with the toy object
#' 
#' @export
#' @param n numeric, the number of points
#' @param nc integer, number of columns in destination stars object
#' @param nr integer, number of rows in destination stars object
#' @param nb integer, number of bands in destination stars object
#' @return sf POINTS object
toy_points <- function(n = 10, nc = 10, nr = 10, nb = 5){
  
  x <- sample(nc-1, n, replace = TRUE) + runif(n, min = -1, max = 1)
  y <- sample(nr-1, n, replace = TRUE) + runif(n, min = -1, max = 1)
  band <- sample(nb, n, replace = TRUE)
  
  dplyr::tibble(id = seq_len(n), x, y, band = paste0("b", band)) |>
    sf::st_as_sf(coords =  c("x", "y"))
}

#' Create a single band toy \code{stars} object
#' 
#' @seealso [Example from introduction](https://r-spatial.github.io/stars/articles/stars4.html#regular-grids)
#' @export
#' @param nc integer, number of columns
#' @param nr integer, number of rows
#' @param mask logical, if TRUE mask the lower left portion
#' @return stars object with regions masked
toy_single <- function(nc = 10, nr = 10, mask = FALSE){
  m <- matrix(seq_len(nc*nr), ncol = nc, nrow = nr)
  if (mask) m[lower.tri(m)] <- NA
  dim(m) = c(x = nr, y = nc)
  stars::st_as_stars(m) |>
    rlang::set_names("values")
}

#' Create a lightweight toy \code{stars} object
#'
#' @export
#' @param nc integer, number of columns
#' @param nr integer, number of rows
#' @param nb integer, number of bands
#' @param ... besides \code{along}, other arguments for \code{\link[stars]{c.stars}} 
#' @return stars object with regions masked
toy_multi <- function(nc = 10, nr = 10, nb = 5, ...){
  lapply(seq_len(nb),
         function(i, n = 1, ...){
           toy_single(nc = nc, nr = nr, ...) |>
             dplyr::mutate(values = .data$values +  ((i-1) * n))
         }, n = nr * nc, ...) |>
    bind_bands()
}
