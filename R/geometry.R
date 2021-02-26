#' Translate a simple feature 
#' 
#' @seealso https://r-spatial.github.io/sf/articles/sf3.html#affine-transformations-1
#' @param x sf object
#' @param d numeric, values to translate by in order of [x, y]
#' @return a translated version of the input \code{x}
#' @export
#' @examples
#' \dontrun{
#'   v <- volcano_multi(what = 'bands')
#'   x <-  volcano_polygon()
#'   x2 <- st_translate(x, c(-250, 120))
#'   plot(v[,,,1], reset = FALSE, axes = TRUE)
#'   plot(sf::st_geometry(x), add = TRUE, border = "purple", col = NA, lwd = 3)
#'   plot(sf::st_geometry(x2), add = TRUE, border = "orange", col = NA, lwd = 3)
#' }
st_translate <- function(x, d = c(0,0)){
  
  if (FALSE){
    v <- volcano_multi(what = 'bands')
    x <-  volcano_polygon()
    x2 <- st_translate(x, c(-250, 120))
    plot(v[,,,1], reset = FALSE, axes = TRUE)
    plot(sf::st_geometry(x), add = TRUE, border = "purple", col = NA, lwd = 3)
    plot(sf::st_geometry(x2), add = TRUE, border = "orange", col = NA, lwd = 3)
  }

  sf::st_set_geometry(x, sf::st_geometry(x) + d)
}


#' Rotate a simple feature 
#' 
#' @seealso https://r-spatial.github.io/sf/articles/sf3.html#affine-transformations-1
#' @param x sf object
#' @param r numeric, radians (or degrees) to rotate
#' @param degrees logical, if TRUE intepret \code{r} as degrees instead of radians
#' @return a rotated version of the input \code{x}
#' @export
#' @examples
#' \dontrun{
#'  v <- volcano_multi(what = 'bands')
#'  x <-  volcano_polygon()
#'  x2 <- st_rotate(x, -pi/8)
#'  plot(v[,,,1], reset = FALSE)
#'  plot(sf::st_geometry(x), add = TRUE, border = "purple", col = NA, lwd = 3)
#'  plot(sf::st_geometry(x2), add = TRUE, border = "orange", col = NA, lwd = 3)
#' }
st_rotate <- function(x, r = pi/2, degrees = FALSE){
  
  if (FALSE){
    v <- volcano_multi(what = 'bands')
    x <-  volcano_polygon()
    x2 <- st_rotate(x, -pi/8)
    plot(v[,,,1], reset = FALSE)
    plot(sf::st_geometry(x), add = TRUE, border = "purple", col = NA, lwd = 3)
    plot(sf::st_geometry(x2), add = TRUE, border = "orange", col = NA, lwd = 3)
  }
  
  if (degrees) r <- r * (pi/180)
  
  rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
  
  xg <- sf::st_geometry(x)
  xc <- sf::st_centroid(xg)
  x %>% 
    sf::st_set_geometry((xg - xc) * rot(r) + xc)
}

#' Convert sfc_POINTS to a mesh of polygons (delaunay triangulation)
#'
#' @seealso \code{\link[geometry]{delaunayn}}
#' @seealso \href{https://stackoverflow.com/questions/57677412/sf-how-to-get-back-to-multipolygon-from-geometrycollection}{stackoverflow}
#' @export
#' @param x sf object with geometry of type POINT
#' @param method charcater, 'geometry' (default) or 'sf'
#' @param varname character one or more variable names to transfer to the polygon
#'        Ignored if method is not 'geometry'
#' @param fun function to compute value the variables specifed by varname
#'        Ignored if method is not 'geometry'
#' @param ... other arguments for \code{fun}
#' @return sf POLYGON table is method is 'geometry' and sf GEOMETRYCOLLECTION otherwise
#' @examples
#' \dontrun{
#' pts <- sf::read_sf(system.file("datasets/penbay-points.gpkg", package = 'twinkle'))
#' mesh <- points_to_mesh(pts)
#' sst <- stars::read_stars(system.file("datasets/20140601-20140630-sst.tif", package = "twinkle"))
#' plot(sst[,,,1],
#'      main = "sst 2014-01-01",
#'      reset = FALSE,
#'      add.geom = list(sf::st_geometry(pts), col = 'orange', pch = 19))
#' plot(sf::st_geometry(mesh), add = TRUE, border = 'green')
#' }
points_to_mesh <- function(x, 
                           method = c("geometry", "sf")[1],
                           varname = NULL, fun = mean, ...){
  
  if (FALSE){
    pts <- sf::read_sf(system.file("datasets/penbay-points.gpkg", package = 'twinkle'))
    mesh <- points_to_mesh(pts)
    sst <- stars::read_stars(system.file("datasets/20140601-20140630-sst.tif", package = "twinkle"))
    plot(sst[,,,1],
      main = "sst 2014-01-01",
      reset = FALSE,
      add.geom = list(sf::st_geometry(pts), col = 'blue', pch = 19))
    plot(sf::st_geometry(mesh), add = TRUE, border = 'orange')
  }
  
  if (tolower(method[1]) == 'sf'){
    mesh <- sf::st_triangulate(sf::st_union(x), ...)
  } else {
    geomcol <- attr(x, "sf_column")
    xy <- sf::st_coordinates(x)   # n xy locations
    d <- geometry::delaunayn(xy)  # m polygons x 3 nodes
    d <- cbind(d, d[,1])          # wrap the first point
    g <- lapply(seq_len(nrow(d)),
                function(i, xy = NULL, del = NULL){
                  sf::st_polygon(list(xy[del[i,],]))
                }, xy = xy, del = d)
    mesh <- dplyr::tibble(p1 = d[,1], p2 = d[,2], p3 = d[,3]) %>%
      dplyr::mutate(!!geomcol := g) %>%
      sf::st_sf(sf_column_name = geomcol, crs = sf::st_crs(x))
    
    if (!is.null(varname)){
      stopifnot(all(varname %in% colnames(x)))
      d <- d[,1:3]  # revert to the original m polygons by 3 nodes
      len <- seq_len(nrow(mesh))  # compute this just once
      for (var in varname){
        vals <- x[[var]]
        v <- sapply(len,
                    function(i){
                      fun(vals[d[i,]], ...)
                    })
        mesh <- mesh %>%
          dplyr::mutate(!!var := v) %>%
          dplyr::relocate(!!var := v, .before = "p1")
      }
    }
  }
  mesh
}

#' Compute the convex hull of a set of points
#'
#' @seealso \href{https://stackoverflow.com/questions/51718839/in-r-how-do-i-run-st-convex-hull-function-on-point-sf-object}{stackoverflow}
#' @export
#' @param x sf object with geometry of type POINT
#' @return sf POLYGON table
#' @examples
#' \dontrun{
#' pts <- sf::read_sf(system.file("datasets/penbay-points.gpkg", package = 'twinkle'))
#' ch <- points_to_mesh(pts)
#' sst <- stars::read_stars(system.file("datasets/20140601-20140630-sst.tif", package = "twinkle"))
#' plot(sst[,,,1],
#'      main = "sst 2014-01-01",
#'      reset = FALSE,
#'      add.geom = list(sf::st_geometry(pts), col = 'blue', pch = 19))
#' plot(sf::st_geometry(ch), add = TRUE, border = 'orange', lwd = 2, col = NA)
#' }
points_to_convexhull <- function(x){
  
  if (FALSE){
    pts <- sf::read_sf(system.file("datasets/penbay-points.gpkg", package = 'twinkle'))
    chull <- points_to_convexhull(pts)
    sst <- stars::read_stars(system.file("datasets/20140601-20140630-sst.tif", package = "twinkle"))
    plot(sst[,,,1],
         main = "sst 2014-01-01",
         key.pos = NULL, 
         reset = FALSE,
         add.geom = list(sf::st_geometry(pts), col = 'blue', pch = 19))
    plot(sf::st_geometry(chull), add = TRUE, border = 'orange', lwd = 2, col = NA)
  }
  
  sf::st_convex_hull(sf::st_union(x))
}