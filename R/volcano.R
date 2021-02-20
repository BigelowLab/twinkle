#' Create a lightweight toy stars object
#'
#' @export
#' @param nc integer, number of columns
#' @param nr integer, number of rows
#' @return stars mask with NA values assigned to area to be masked
make_toy_mask <- function(nc = 10, nr = 10){
  m <- matrix(seq_len(nc*nr), ncol = nc, nrow = nr, byrow = TRUE)
  m[lower.tri(m)] <- NA
  bb <- sf::st_bbox(c(xmin = 0, xmax = 10, ymin = 0, ymax = 10))
  x <- stars::st_as_stars(bb,
    nx = 10,
    ny = 10,
    inside = FALSE,
    values = t(m))
}

#' Create a polygon object suitable for use with the toy object
#' 
#' @export
#' @param nc integer, number of columns
#' @param nr integer, number of rows
#' @return sf POLYGON object
make_toy_polygon <- function(nc = 10, nr = 10){
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
make_toy_points <- function(n = 10, nc = 10, nr = 10, nb = 5){
  
  x <- sample(nc-1, n, replace = TRUE) + runif(n, min = -1, max = 1)
  y <- sample(nr-1, n, replace = TRUE) + runif(n, min = -1, max = 1)
  band <- sample(nb, n, replace = TRUE)
  
  dplyr::tibble(id = seq_len(n), x, y, band = paste0("b", band)) %>%
    sf::st_as_sf(coords =  c("x", "y"))
}

#' Create a lightweight toy \code{stars} object
#'
#' @export
#' @param nc integer, number of columns
#' @param nr integer, number of rows
#' @param nb integer, number of bands
#' @return stars object with regions masked
make_toy <- function(nc = 10, nr = 10, nb = 5){
  m <- make_toy_mask(nc = nc, nr = nr)
  n <- nc * nr
  lapply(seq_len(nb),
               function(i) {
                 m <- m + ((i-1) * n)
                 names(m) <- paste0("b", i)
                 m
               }) %>%
    bind_stars() %>%
    merge(name = "band")
}

#' Retrieve a stars object of Auckland’s Maunga Whau volcano topography.  Adapted from
#' USGS-R inlmisc package.
#'
#' @seealso \url{https://waterdata.usgs.gov/blog/inlmiscmaps/}
#' @seealso \url{https://CRAN.R-project.org/package=inlmisc}
#' @export
#' @param indexed logical, if TRUE then assign 1,2,3,... as cell values
#' @param name charcater, the name to apply to the attribute
#' @return single attribute stars
volcano_single <- function(indexed = FALSE, name = "values"){
  crs <- "epsg:27200"
  #m <- t(datasets::volcano)[61:1, ]
  m <- t(datasets::volcano)
  d <- dim(m)
  if (indexed) m[] <- seq_len(prod(d))
  dx <- dy <- 10
  x1 <- 6478705
  y1 <- 2667405
  x2 <- x1 + (d[2]) * dx
  y2 <- y1 + (d[1]) * dy
  
  bb <- sf::st_bbox(c(xmin = x1, xmax = x2, ymin = y1, ymax = y2),
                   crs = 27200)
  s <- stars::st_as_stars(bb,
              nx = d[2],
              ny = d[1],
              values = t(m)) %>%
    stars::st_flip(which = 2)
  names(s) <- name
  s
}

#' Retrieve a multi-attribute stars object of Auckland’s Maunga Whau volcano topography.
#' Attribute/bands 2 to \code{n} are slightly altered from the original as per
#' \code{\link{volcano_single}}
#'
#' @seealso \code{\link{volcano_single}}
#' @export
#' @param n numeric, the number attributes (or bands)
#' @param what character, if "attributes" (default) then yield a \code{n}-attribute object, but 
#'   if "bands" then yield a single attribute object with \code{n} bands
#' @param indexed logical, if TRUE then assign 1,2,3,... as cell values
#' @return stars class object
volcano_multi <- function(n = 3,
                          what = c("attributes", "bands")[1],
                          indexed = FALSE){
  v <- lapply(seq_len(n),
    function(i){
      nm <- sprintf("v%i", i)
      if (indexed){
        v <- volcano_single(indexed = TRUE, name = nm) + i
      } else {
        v <- volcano_single(indexed = FALSE, name = nm) * runif(1, min = 0.8, max = 1.2)
      }
      v
    }) %>%
    bind_stars() 
  
  if (tolower(what[1]) == "bands"){
    v <- merge(v, name = "volcano")
  }
  
  v
}

#' Generate a table of random points in a \code{stars} object
#'
#' @export
#' @param x stars object
#' @param ... further arguments for \link{random_points}
#' @return tibble of locations with values
volcano_points <- function(x = volcano_multi(what = "bands"),  ...){
  random_points(x, ...)
}


#' Generate a polygon for the volcano stars
#'
#' @export
#' @return simple feature geometry for a POLYGON
#' @examples
#' \dontrun{
#' library(dplyr)
#' x <- volcano_stack(indexed = TRUE)
#' p <- volcano_polygon()
#' pts <- randomPts(x, polygon = p)
#' par(mfrow = c(1,3))
#' for (i in seq_len(3)){
#'   plot(x[[i]], main = paste("Layer", i))
#'   plot(p, add = TRUE)
#'   with(pts %>% dplyr::filter(layer == i), points(x, y))
#' }
#' }
volcano_polygon <- function(){
  x <- 6478700 + c(301, 622, 622, 500, 500, 301, 301)
  y <- 2667400 + c(100, 100, 450, 450, 200, 200, 100)
  g <- sf::st_sfc(sf::st_polygon(list(cbind(x,y))),
             crs = 27200)
  sf::st_as_sf(dplyr::tibble(id = 1), geom = g)
}