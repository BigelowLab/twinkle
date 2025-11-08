#' Given a table of x and y coordinates data generate a sfc_POLYGON
#'
#' @export
#' @param x table or matrix of x and y column of coordinates
#' @param crs character or numeric coordinate ref system flag
#' @return sfc_POLYGON object
#' @examples
#' \dontrun{
#' # An example of interating over a number of sets of points in a grouped table
#'  x <- dplyr::tibble(
#'                     name = c('a', 'a', 'a', 'a', "b", "b", "b", "b"),
#'                     x = c(1,2,1,0, 3,4,4,5), 
#'                     y = c(0,1,2,1, 8,8,9,9))
#'  make_one_polygon <- function(x, y, ...){
#'    cat(str(x, "\n"))
#'    points_to_polygon(x, ...)
#'  } 
#'  p <- x |>
#'    dplyr::group_by(name) |>
#'    dplyr::group_map(make_one_polygon, crs = "WGS84") |>
#'    dplyr::bind_rows()
#' }
points_to_polygon <- function(x = dplyr::tibble(x = c(1,2,1,0), 
                                                y = c(0,1,2,1)), 
                              crs =  "WGS84"){
  m <- as.matrix(x)
  n <- nrow(m)
  if (!identical(m[1,], m[n,])) m <- rbind(m, m[1,]) # close of open
  sf::st_polygon(x = list(m)) |> 
    sf::st_sfc(crs = crs)
}


#' Generate a listing of matches between a set of points and a set of polygons.
#' 
#' @export
#' @param pts sf POINT table with one or more records (rows)
#' @param poly sf POLYGON or sf MULTIPOLYGON with one or more records (rows)
#' @param form character, identifies desired output format (default 'string')
#' \describe{
#' \item{string}{string representation where a point is matched to the row(s) in poly where it is contained}
#' \item{sparse}{list of vectors (some may be empty) with polygon indices}
#' \item{matrix}{logical matrix with nrow(pts) x nrow(poly) elements, where TRUE means the point is contained}
#' }
#' @return match listing as described by \code{form}
#' @examples
#' \dontrun{
#' v <- volcano_multi(what = "bands")
#' pts <- volcano_points(x = v)
#' y1 <- volcano_polygon()
#' y2 <- st_rotate(y1, pi/4) 
#' y3 <- st_translate(y1, c(-250, 120))
#' poly <- dplyr::bind_rows(y1, y2, y3)
#' 
#' m1 <- match_points_to_polygons(pts, poly, form = 'matrix')
#' head(m1)
#' m2 <- match_points_to_polygons(pts, poly, form = 'sparse')
#' m2
#' m3 <- match_points_to_polygons(pts, poly, form = 'string')
#' head(m3)
#' 
#' pts <- dplyr::mutate(pts, polygon = m3)
#' 
#' plot(v[,,,1], axes = TRUE, reset = FALSE)
#' plot(sf::st_geometry(pts), color = 'pink', 
#'      add = TRUE, pch = 19, cex = 0.5)
#' plot(y1, col = NA, border = 'purple', add = TRUE)
#' plot(sf::st_geometry(pts |> dplyr::filter(grepl("1", polygon))), 
#'      pch = 1, col = 'purple', add = TRUE)
#' plot(y2, col = NA, border = 'orange', add = TRUE)
#' plot(sf::st_geometry(pts |> dplyr::filter(grepl("2", polygon))), 
#'      pch = 2, col = 'orange', add = TRUE, cex = 1.5)
#' plot(y3, col = NA, border = 'cornflowerblue', add = TRUE)
#' plot(sf::st_geometry(pts |> dplyr::filter(grepl("3", polygon))), 
#'      pch = 0, col = 'cornflowerblue', add = TRUE, cex = 1.5)
#' }
match_points_to_polygons <- function(pts, poly, form = 'string'){
  
  if(FALSE){
    v <- volcano_multi(what = "bands")
    pts <- volcano_points(x = v)
    y1 <- volcano_polygon()
    y2 <- st_rotate(y1, pi/4) 
    y3 <- st_translate(y1, c(-250, 120))
    poly <- dplyr::bind_rows(y1, y2, y3)

    m1 <- match_points_to_polygons(pts, poly, form = 'matrix')
    head(m1)
    m2 <- match_points_to_polygons(pts, poly, form = 'sparse')
    m2
    m3 <- match_points_to_polygons(pts, poly, form = 'string')
    head(m3)
    
    pts <- dplyr::mutate(pts, polygon = m3)
    
    plot(v[,,,1], axes = TRUE, reset = FALSE)
    plot(sf::st_geometry(pts), color = 'pink', add = TRUE, pch = 19, cex = 0.5)
    plot(y1, col = NA, border = 'purple', add = TRUE)
    plot(sf::st_geometry(pts |> dplyr::filter(grepl("1", polygon))), pch = 1, col = 'purple', add = TRUE)
    plot(y2, col = NA, border = 'orange', add = TRUE)
    plot(sf::st_geometry(pts |> dplyr::filter(grepl("2", polygon))), pch = 2, col = 'orange', add = TRUE, cex = 1.5)
    plot(y3, col = NA, border = 'cornflowerblue', add = TRUE)
    plot(sf::st_geometry(pts |> dplyr::filter(grepl("3", polygon))), pch = 0, col = 'cornflowerblue', add = TRUE, cex = 1.5)
  }
  
  to_string <- function(pts, poly){
    index <- t(sf::st_contains(poly, pts, sparse = FALSE))
    do_paste <- function(x) { paste(which(x), collapse = ",")}
    p <- apply(index, 1, do_paste)
    ix <- nchar(p) == 0
    p[ix] <- NA_character_
    p
  }
  
  switch(tolower(form[1]),
         'sparse' = sf::st_contains(poly, pts),
         'matrix' = t(sf::st_contains(poly, pts, sparse = FALSE)),
          to_string(pts, poly))
  
}


#' Write to a geopackage file by writing to a temp file first and then
#' copying to the specified destination.
#' 
#' @seealso \href{https://github.com/r-spatial/sf/issues/628#issuecomment-381690663}{sf issue 628}
#' @export
#' @param obj object to write
#' @param dsn character, the file to write to
#' @param overwrite, logical, overwite existing file?
#' @param ... other arguments passed to \code{\link[sf]{write_sf}}
#' @return obj invisibly
write_gpkg <- function(obj, dsn, 
                       overwrite = FALSE,
                       ...){
  dsnpath <- dirname(dsn)
  if (!dir.exists(dsnpath)) stop("destination path doesn't exist: ", dsnpath)
  tmpfile <- tempfile(fileext = ".gpkg")
  obj <- try(sf::write_sf(obj, dsn=tmpfile, driver = "GPKG"))
  if (inherits(obj, "try-error")){
    print(obj)
  } else {
    ok <- file.copy(tmpfile, dsn, overwrite = overwrite)
  }
  invisible(obj)
}

#' Retrieve the attribute that names to sf column
#' 
#' @export
#' @param x sf object
#' @return character, the name of the sf column
sf_column <- function(x){
  attr(x, "sf_column")
}
