#' Test is an object inherits from \code{stars} class
#' 
#' @export
#' @param x object to test
#' @param class character, the class to test against, by default 'stars'
#' @return logical
is_stars <- function(x, class = 'stars') {
  inherits(x, class)
}

#' Set names of an object
#' 
#' @seealso \code{\link[stats]{setNames}}
#' @param x object with name-able elements
#' @param nm character vector of names, by default \code{v1, v2, v3, ...}
#' @return the input object with names
set_names <- function(x, nm = paste0("v", seq_len(length(x)))){
  stats::setNames(x, nm)
}

#' Assemble a vector of stars dimension or shape
#'
#' @export
#' @param x stars object
#' @return numeric vector of
#' \itemize{
#'   \item{ncol the number of columns}
#'   \item{nrow the number of rows}
#'   \item{ncell the number of cells (pixels) in each band (ncol * nrow)}
#'   \item{nband the number of bands}
#'   \item{nindex the number of indices (ncell*nband)}
#'   \item{natt the number of attributes}
#' }
#' @examples
#' \dontrun{
#' v <- volcano_multi(what = 'bands')
#' shape_stars(v)
#' v2 <- bind_stars(v, v)
#' shape_stars(v2)
#' }
shape_stars <- function(x){
  if (!is_stars(x)) stop("Input x must be a stars class")
  d <- unname(dim(x))
  ncol = d[1]
  nrow = d[2]
  ncell = ncol * nrow
  nband = ifelse(length(d) == 2, 1, d[3])
  c(ncol = ncol, 
    nrow = nrow, 
    ncell = ncell, 
    nband = nband, 
    nindex = ncell * nband,
    natt = length(x))
}


#' Bind a list of \code{stars} objects.  
#' 
#' @seealso \code{\link[stars]{c.stars}}
#' 
#' @export
#' @param x list of one or more \code{stars} objects
#' @param nms character, vector of names to apply to attributes
#' @param ... arguments for \code{\link[stars]{c.stars}}
#' @return \code{stars} objects
bind_stars <- function(x, nms = names(x), ...){
  y <- do.call(c, x)
  if (is.null(names)) names <- paste("b", seq_len(length(x)))
  names(y) <- nms
  y
}


#' Convert a index (1, 2, 3,..., ncells*nbands) into stars obeject to row, col, cell and
#'    band location coordinates
#'
#' @export
#' @param index vector of multi-band index coordinates
#' @param x stars object
#' @param form character, specifies output format as "table" (default) or "sf"
#' @return a tibble of index, cell, col, row, x, y, and band
#' \itemize{
#'  \item{index, 1-based 3d index into objects if as array}
#'  \item{cell, 1-based 2d index into each band as if a matrix}
#'  \item{col, 1-based column index}
#'  \item{row, 1-based row index}
#'  \item{x, coordinate of center of cell, missing if \code{form} is 'sf'}
#'  \item{y, coordinate of center of cell, missing if \code{form} is 'sf'}
#'  \item{band, 1-based integer index of band}
#' }
stars_index_to_loc <- function(index, x, form  = c("table", "sf")[1]){
  if (!is_stars(x)) stop("Input x must be a stars class")
  shape <- shape_stars(x)
  band <- ((index-1) %/% shape[['ncell']]) + 1
  cell  <- index - ((band - 1) * shape[['ncell']])
  col   <- ((cell-1) %% shape[['ncol']])  + 1
  row   <- floor((cell - 1) / shape[['ncol']]) + 1
  xv <- stars::st_get_dimension_values(x, which = 1)[col]
  yv <- stars::st_get_dimension_values(x, which = 2)[row]
  r <- dplyr::tibble(index, cell, col, row, x = xv, y = yv, band)
  if (tolower(form[1]) == 'sf'){
    r <- sf::st_as_sf(r, coords = c("x", "y"), crs = sf::st_crs(x))
  }
  r
}


#' Convert a sf POINTS object with a band variable into stars object to row, col, 
#' cell x, y, and band location coordinates (cell centers).
#'
#' @export
#' @param pts sf POINT object with one variable whose name matches the name of the band dimension
#'   in the \code{x}. 
#' @param x stars object.  Limited to [x,y] of [x,y, band] dimensions
#' @param form character, specifies output format as "table" (default) or "sf"
#' @return a tibble of index, cell, col, row, x, y, and layer
#' \itemize{
#'  \item{index, 1-based 3d index into objects if as array}
#'  \item{cell, 1-based 2d index into each band as if a matrix}
#'  \item{col, 1-based column index}
#'  \item{row, 1-based row index}
#'  \item{x, coordinate of center of cell (not original input x value), missing if \code{form} is 'sf'}
#'  \item{y, coordinate of center of cell (not original input y value), missing if \code{form} is 'sf'}
#'  \item{band, 1-based integer index of band (note original band value)}
#' }
stars_pts_to_loc <- function(pts, x, form  = c("table", "sf")[1]){
  
  if (!is_stars(x)) stop("Input x must be a stars class")
  bandname <- names(dim(x))[3]
  if(!(bandname %in% names(pts))) {
    stop("Input pts must have a variable that matches the name of the band dimension")
  }
  shape <- shape_stars(x)
  
  xp <- stars::st_xy2sfc(x[,,,1], as_points = FALSE, na.rm = FALSE)
  cell <- sf::st_intersects(sf::st_geometry(pts), sf::st_geometry(xp)) %>%
    unlist()
  bandvals <- stars::st_get_dimension_values(x, which = bandname)
  band <- match(pts[[bandname]], bandvals)
  step <- (band - 1) * shape[['ncell']]
  index <- cell + step
  col   <- ((cell-1) %% shape[['ncol']])  + 1
  row   <- floor((cell - 1) / shape[['ncol']]) + 1
  xv <- stars::st_get_dimension_values(x, which = 1)[col]
  yv <- stars::st_get_dimension_values(x, which = 2)[row]
  r <- dplyr::tibble(index, cell, col, row, x = xv, y = yv, band)
  if (tolower(form[1]) == 'sf'){
    r <- sf::st_as_sf(r, coords = c("x", "y"), crs = sf::st_crs(x))
  }
  r
}

#' Generate random points within a stars object.
#'
#' Providing a polygon to limit the search area can speed up the search
#' if the polygon is small relative to the spatial cover of the stars geometry.
#'
#' Note that it is possible
#' to filter the available pool of candidate cells to something
#' less than the requested sample size, n.  In such cases you might try 
#' adjusting the value of \code{m} higher. If you still have issues then
#' investigate the presence of NAs (if \code{na.rm = TRUE}), or the size 
#' of the \code{polygon} (if provided.)  
#'         
#' @export
#' @param x stars object
#' @param n numeric, the number of points to return
#' @param m multiplier to use when seeking to avoid missing values.
#'        Ignored if polygon is provided.
#' @param na.rm logical, if TRUE then avoid cells with missing values.
#' @param points a table of points to avoid. If na.rm is TRUE that is handled first,
#'        then point avoidance is handled.  Ignored if NULL.
#' @param polygon polygon (sf) that describes the region to select
#'        points from.  Ignored if NULL.
#' @param form character, specifies output format as "table" (default) or "sf"
#' @return a table or sf POINT object of locations with values.  
#' @examples
#' \dontrun{
#'  x <- make_toy() 
#'  pts <- make_toy_points()
#'  poly <- make_toy_polygon()
#'  
#'  p <- random_points(x, na.rm = TRUE, form = "sf") %>%
#'    dplyr::filter(band == 1)
#'  plot(x[,,,1], reset = FALSE, axes = TRUE, main = "avoiding NAs")
#'  plot(sf::st_geometry(dplyr::filter(p, band == 1)), 
#'       add = TRUE, pch = 19, col = "orange")
#'  
#'  p <- random_points(x, points = pts, form = "sf") %>%
#'    dplyr::filter(band == 1)
#'  plot(x[,,,1], reset = FALSE, axes = TRUE, main = "avoiding points")
#'  plot(sf::st_geometry(dplyr::filter(p, band == 1)), 
#'       add = TRUE, pch = 19, col = "orange")
#'  plot(sf::st_geometry(dplyr::filter(pts, band == "b1")), 
#'       add = TRUE, pch = 1, col = "green", cex = 2)
#'  
#'  p <- random_points(x, polygon = poly, form = "sf") %>%
#'    dplyr::filter(band == 1)
#'  plot(x[,,,1], reset = FALSE, axes = TRUE, main = "within a polygon")
#'  plot(sf::st_geometry(dplyr::filter(p, band == 1)), 
#'       add = TRUE, pch = 19, col = "orange")
#'  plot(sf::st_geometry(poly), add = TRUE, border = "green", col = NA)
#'  
#'  p <- random_points(x, polygon = poly, na.rm = TRUE, form = "sf") %>%
#'    dplyr::filter(band == 1)
#'  plot(x[,,,1], reset = FALSE, axes = TRUE, main = "within a polygon avoiding NAs")
#'  plot(sf::st_geometry(dplyr::filter(p, band == 1)), 
#'       add = TRUE, pch = 19, col = "orange")
#'  plot(sf::st_geometry(poly), 
#'       add = TRUE, border = "green", col = NA)
#' 
#' }
random_points <- function(x, 
                          n = 100,
                          m = 2,
                          na.rm = FALSE,
                          points = NULL,
                          polygon = NULL,
                          form = c("table", "sf")[1]){
  
  if (FALSE){
    x = volcano_multi(what = "bands") # toy_multi()
    n = 100
    m = 2
    na.rm = FALSE
    points = NULL # volcano_points(x)
    polygon = NULL #toy_polygon()
  }
  if (FALSE){
    n = 100
    m = 2
    na.rm = TRUE
    x <- list(
      stars::read_stars(system.file("datasets/20140601-20140630-sst.tif", 
                                    package = "twinkle"))[,,,1:2],
      stars::read_stars(system.file("datasets/20140601-20140630-sst_slope.tif", 
                                    package = "twinkle"))[,,,1:2],
      stars::read_stars(system.file("datasets/20140601-20140630-sst_cum.tif", 
                                    package = "twinkle"))[,,,1:2] ) %>%
      bind_stars(along = 1, nms = c("sst", "slope", "cum"))
    
    points <- sf::read_sf(system.file("datasets/penbay-points.gpkg", package = 'twinkle')) %>%
      dplyr::filter(layer %in% (1:2))
    
    polygon <- sf::read_sf(system.file("datasets/penbay-polygons.gpkg", package = 'twinkle'))
  }
  
  if (!is_stars(x)) stop("Input x must be a stars class")
  
  shape <- shape_stars(x)
  
  if (!is.null(polygon)){
    # we compute 2d cells numbers for first band
    # then we propagate those through the the other bands, ala steps
    # 0, ncell, ncell*2, ... 
    step <- shape[['ncell']] * (seq_len(shape[['nband']])-1)
    # convert x/y dims to geometry points
    xp <- stars::st_xy2sfc(x[,,,1], as_points = TRUE, na.rm = FALSE)
    # find which are within the polygon (note edges) as logical vector
    # convert to indices (2d cell numbers)
    cell <- sf::st_contains(polygon, xp, sparse = FALSE)[1,] %>%
      which()
    # propagate from cell numbers for first layer to cell numbers for all layers
    loc <- unlist(lapply(step, function(s) cell + s))
    if (length(loc) > n*m) {
      loc <- sample(loc, n*m, replace = FALSE) 
    } 
    loc <- stars_index_to_loc(loc, x, form = "sf")
  } else {
    loc <- sample(shape[["nindex"]], n*m, replace = FALSE) %>%
      stars_index_to_loc(x, form = "sf")
  }
  # now get the values
  vals <- stars::st_extract(x, loc) %>% 
    sf::st_as_sf() %>%
    dplyr::as_tibble() %>%
    dplyr::select(-.data$geometry)
  
  # remove NAs form consideration?
  if (na.rm){
    ix <- complete.cases(vals)
    loc <- loc %>%
      dplyr::filter(ix)
    vals <- vals %>%
      dplyr::filter(ix)
  }
  
  # points to avoid?
  if (!is.null(points)){
    ploc <- stars_pts_to_loc(points, x)
    ix <- !(loc$index %in% ploc$index)
    loc <- loc %>%
      dplyr::filter(ix)
    vals <- vals %>%
      dplyr::filter(ix)
  }
  
  reduce_to_band <- function(x, nms = names(x)){
    if (length(x) > 1){
      # match the column names (sst.V1, sst.V2, ...) to each name (sst)
      # for each group 
      #    m = build matrix of columns
      #    mutate original to add sst = m[,band] and drop (sst.V1, sst.V2, ...)
      onames <- names(x)
      for (name in nms){
        vnames <- colnames(x)
        ix <- grep(paste0("^",name), vnames)
        m <- as.matrix(x %>% dplyr::as_tibble() %>% dplyr::select(vnames[ix]))
        
        x <- x %>%
          dplyr::mutate(!!name := sapply(seq_len(nrow(m)), function(i) m[i,.data$band[i]] )) %>%
          dplyr::select(-!!vnames[ix])
      }
    }
    x
  }
  
  
  loc <- loc %>%
    dplyr::bind_cols(vals) %>%
    reduce_to_band(nms = names(x)) %>%
    dplyr::relocate(.data$geometry, .after = dplyr::last_col())
  if (nrow(loc) < n){
    warning("fewer than requested points sampled - try increasing value of m")
  } else {
    loc <- loc %>%
      dplyr::slice_sample(n = n, replace = FALSE)
  }
  
  if (tolower(form[1]) == 'sf'){
    loc <- sf::st_as_sf(loc, coords = c("x", "y"), crs = sf::st_crs(x))
  }
  
  loc
}