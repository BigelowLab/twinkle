#' Create a lightweight toy \code{stars} object
#'
#' @export
#' @param nc integer, number of columns
#' @param nr integer, number of rows
#' @return stars mask with NA values assigned to area to be masked
toy_mask <- function(nc = 10, nr = 10){
  m <- matrix(seq_len(nc*nr), ncol = nc, nrow = nr, byrow = TRUE)
  m[lower.tri(m)] <- NA
  m <- t(m)
  bb <- sf::st_bbox(c(xmin = 0, xmax = 10, ymin = 0, ymax = 10))
  x <- stars::st_as_stars(bb,
                          nx = 10,
                          ny = 10,
                          inside = FALSE,
                          values = m)
}

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

#' Create a lightweight toy \code{stars} object
#'
#' @export
#' @param nc integer, number of columns
#' @param nr integer, number of rows
#' @param nb integer, number of bands
#' @return stars object with regions masked
toy_multi <- function(nc = 10, nr = 10, nb = 5){
  m <- toy_mask(nc = nc, nr = nr)
  n <- nc * nr
  idx <- seq_len(nb)
  names(idx) <- paste0("b", idx)
  lapply(idx,
         function(i) {
           m + ((i-1) * n)
         }) |>
    bind_stars() |>
    set_names(names(idx)) |>
    merge(name = "band")
}