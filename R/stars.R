#' Mask a stars object with a polygon
#' 
#'  You can use st_crop(...) but it runs slowly for large rasters
#'  
#' @seealso https://github.com/r-spatial/stars/issues/622
#' 
#' @export
#' @param x stars object
#' @param y sf polygon object.  Pixels exterior of this as assigned NA
#' @param filename character or NA, if a filename write to the file
#' @param invert logical, if TRUE values inside the polygon are set to NA
#' @return a masked stars object, areas outside (or possibly inside) are set to NA
#'   depending upon the value of \code{invert}.  Default is outside is NA
#' @examples
#' \dontrun{
#'   library(stars)
#'   library(sf)
#'   x = read_stars(system.file("tif/L7_ETMs.tif", package = "stars")) 
#'   m = cbind(c(290000, 295000, 292500, 290000),
#'             c(9113000, 9115000, 9118000, 9113000))
#'   p = st_sfc( st_polygon(list(m)) ) |> 
#'     st_set_crs(31985)
#'   m = mask_stars(x, p)
#'   plot_p = function() plot(p, add = TRUE, border = 'orange')
#'   z = c(x,m, along = 'band')
#'   plot(z, hook = plot_p)
#' }
mask_stars = function(x, y, invert = FALSE, filename = NA){
  
  starsfile = tempfile(fileext = ".tif")
  polyfile = tempfile(fileext = ".gpkg")
  resultfile = if(is.na(filename[1])){
      tempfile(fileext = ".tif")
    } else {
      filename[1]
    }
  
  all_one = function(x) {
    x[[1]] = 1
    x
  }
    
  stars::write_stars(all_one(dplyr::select(x,1)), starsfile)
  sf::write_sf(sf::st_geometry(y), polyfile)
  
  ok = sf::gdal_utils(util = "warp", 
                      source = starsfile, 
                      destination = resultfile, 
                      options = c("-cutline", polyfile))
  if (!ok) return(NULL)
  result = stars::read_stars(resultfile)
  # not the subtle switch from result back to x
  ix <- result[[1]] > 0
  if (invert) ix = !ix
                    
  result = 0
  
  for (n in names(x)) x[[n]][!ix] <- NA_real_

  if (is.na(filename)) {
    ok = file.remove(resultfile)
  } else {
    stars::write_stars(x, resultfile)
  }
  
  ok = file.remove(starsfile, polyfile)
  
  x
}

#' Plot multiple attributes of a stars object
#' 
#' This is pretty limited, and legends (keys) are disabled.
#' 
#' @seealso https://github.com/r-spatial/stars/issues/374#issuecomment-761773542
#' @export
#' @param x stars object with one or more attributes
#' @param key.pos see \code{\link[stars]{plot}}
#' @param mfrow see \code{\link[graphics]{par}}
#' @param band see \code{\link[stars]{plot}}
#' @param ... other arguments for \code{\link[stars]{plot}}
#' @examples
#' \dontrun{
#' v = volcano_single(threshold = 120)
#' lut = make_raster_lut(v)
#' x = c(v, lut)
#' mplot(v)
#' }
mplot <- function(x = volcano_multi(threshold = 120), 
                  key.pos = NULL,
                  mfrow = c(1, length(x)),
                  band = 1,
                  ...){
  
  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))
  
  par(mfrow = mfrow)
  
  sh = shape_stars(x)
  for (a in seq_len(sh[['natt']])) {
    if (sh[["nband"]] > 1){
      plot(x[a,,,], band = band, key.pos = key.pos, reset = FALSE, ...)
    } else {
      plot(x[a,,], band = band, key.pos = key.pos, reset = FALSE, ...)
    }
  }
  invisible(NULL)
}


#' Determine the closest non-NA pixel given [lon,lat] and a lut raster
#'
#' @export
#' @param x sf POINT data, data.frame or with [lon,lat] coordinates in that order 
#'  and assumed to be aligned with the CRS of the \code{lut}
#' @param lut stars object, look-up with precomputed closest non-NA cells. 
#'   See \code{\link{make_raster_lut}}
#' @param verbose logical, if TRUE output messages
#' @return sf object with two attributes \code{index} (the index of the closets non-NA point)
#'   and \code{original} (the original index)
closest_available_cell <- function(x = volcano_points(), lut = make_raster_lut(),
                                   verbose = FALSE){
  
  if (!inherits(x, c("sf", "sfc"))){
    if (verbose) message("converting x to sf using lut's CRS")
    if (inherits(x, "matrix")){
      x <- as.data.frame(x[,1:2])
    }
    x <- sf::st_as_sf(x, coords = c(1,2), crs = sf::st_crs(lut))
  }
  
  index <- stars::st_extract(lut, at = x)
  index = dplyr::mutate(index, 
  											original = stars::st_cells(lut, x), .after = 1)
  index
  
}


#' Make a neighborhood LUT, where each cell value contains the 1-d address of 
#' the closest non-missing cell (which might be itself). 
#' 
#' @export
#' @param x stars object, only the first layer of the first attribute is used.
#' @param mask_value numeric, the value of masked areas, by default NA
#' @param nonreassigned_value either "cellnumber" which indicates cell number should be used
#'        or some numeric value like NA. 
#' @return stars of cell addresses.  Where the input, x, had non-mask values the
#'   cell addresses point to the input cell.  Where the input had mask-valued cells,
#'   the output cell addresses point to the nearest non-mask cells in the input.
#' @examples
#' \dontrun{
#' library(stars)
#' library(twinkle)
#' v = volcano_single(threshold = 120)
#' lut <- make_raster_lut(v)
#' z <- c(mask, lut)
#' names(z) <- c("mask", "lut")
#' mplot(z)
#' }
make_raster_lut <- function(x = volcano_single(threshold = 120), 
                            mask_value = NA, 
                            nonreassigned_value = "cellnumber"){
  
  shp <- shape_stars(x)
  
  # slice out a one-attribute one-band layer
  # add to it an index_ 
  lut <- if (shp[['nband']] > 1) {
    x[1,,,,1] 
  } else {
    x[1,,]
  } |>
  dplyr::mutate(index_ = seq_len(prod(shp[c("ncol", "nrow")])))  
  # convert to a data frame
  lutdf <- dplyr::as_tibble(lut)
  isna <- if (is.na(mask_value[1])){
      is.na(lutdf[[3]])
    } else {
      dplyr::near(lutdf[[3]], mask_value[1])
    }
  # if there are no cells that are flagged as 'masked' then we simply
  # return the native indexing
  if (!any(isna)){
    return(dplyr::select(lutdf, "index_") |> set_names("index"))
  }
  
  goodpts <- dplyr::filter(lutdf, !isna)
  ix <- RANN::nn2(goodpts[1:2], 
                  lutdf[1:2] |> dplyr::filter(isna),
                  k = 1)
  
  lutdf$index_[isna] <- goodpts$index_[ix$nn.idx]   
  
  if (!inherits(nonreassigned_value, "character")){
    lutdf$index_[!isna] <-  nonreassigned_value
  }
  
  dplyr::mutate(lut, index = lutdf$index_) |>
    dplyr::select(dplyr::all_of("index"))
  
} #make_raster_lut


#' Test is an object inherits from \code{stars} class
#'
#' @export
#' @param x object to test
#' @param class character, the class to test against, by default 'stars'
#' @return logical
is_stars <- function(x, class = 'stars') {
  inherits(x, class)
}

#   Set names of an object
#  
#   @seealso \code{\link[stats]{setNames}}
#   @param x object with name-able elements
#   @param nm character vector of names, by default \code{v1, v2, v3, ...}
#   @return the input object with names
#   set_names <- function(x, nm = paste0("v", seq_len(length(x)))){
#     stats::setNames(x, nm)
#   }

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
#' v2 <- bind_attrs(v, v)
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


#' Retrieve the deltas of stars dimensions
#'
#' @export
#' @param x stars object
#' @return named list of deltas
delta_stars <- function(x){
  if (!is_stars(x)) stop("Input x must be a stars class")
  dd <- attributes(x)[['dimensions']]
  if (length(dd) == 0) { return(list())}
  sapply(dd, '[[', "delta", simplify = FALSE)
}

#' Bind a list of \code{stars} objects by attribute.
#'
#' @seealso [stars issue 440](https://github.com/r-spatial/stars/issues/440#issuecomment-877627732)
#'
#' @export
#' @param x list of one or more \code{stars} objects.  Any NULL elements are
#'   silently removed first.
#' @param other arguments for c.stars()
#' @return \code{stars} objects
bind_attrs <- function(x, ...){
  # remove any null entries
  x = x[!sapply(x, is.null)]
  if (length(x) == 0) stop("input has zero length")
  do.call(c, append(x, list(along = NA_integer_, ...)))
}


#' Bind a list of \code{stars} objects by band
#'
#' @seealso [stars issue 440](https://github.com/r-spatial/stars/issues/440#issuecomment-877627732)
#'
#' @export
#' @param x list of one or more \code{stars} objects
#' @return \code{stars} objects
bind_bands <- function(x){
  do.call(c, append(x, list(along = "band")))
}

#' Convert a index (1, 2, 3,..., ncells*nbands) into stars obeject to row, col, cell and
#'    band location coordinates
#'
#' @export
#' @param index vector of multi-band index coordinates (1-d addresses into n-d space)
#' @param x stars object
#' @param form character, specifies output format as "table" (default) or "sf"
#' @return a tibble of index, cell, col, row, x, y, and band
#' \itemize{
#'  \item{index, 1-based 3d index into objects if as array}
#'  \item{cell, 1-based 2d index into each band as if a matrix}
#'  \item{col, 1-based column index}
#'  \item{row, 1-based row index}
#'  \item{x, coordinate of center of cell, part of \code{geometry} if \code{form} is 'sf'}
#'  \item{y, coordinate of center of cell, part of \code{geometry} if \code{form} is 'sf'}
#'  \item{band, 1-based integer index of band}
#' }
stars_index_to_loc <- function(index = c(12L, 14L, 19L, 21L, 23L, 37L, 46L, 56L, 58L, 59L), 
                               x = toy_multi(nb = 3, nc = 5, nr = 4, mask = FALSE), 
                               form  = c("table", "sf")[1]){
  if (!is_stars(x)) stop("Input x must be a stars class")
  shape = shape_stars(x)
  band = ((index-1) %/% shape[['ncell']]) + 1
  cell = index - ((band - 1) * shape[['ncell']])
  cr = colrow_from_cells(x, cell)
  xv = stars::st_get_dimension_values(x, which = 1)[cr[,1, drop = TRUE]]
  yv = stars::st_get_dimension_values(x, which = 2)[cr[,2, drop = TRUE]]
  r = dplyr::tibble(index = as.integer(index), 
                    cell = as.integer(cell), 
                    col = as.integer(cr[,1, drop = TRUE]), 
                    row = as.integer(cr[,2, drop = TRUE]), 
                    band = as.integer(band),
                    x = xv, 
                    y = yv)
  if (tolower(form[1]) == 'sf'){
    r = sf::st_as_sf(r, coords = c("x", "y"), crs = sf::st_crs(x))
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
stars_pts_to_loc <- function(pts = stars_index_to_loc(form = 'sf') |>
                               dplyr::select(dplyr::all_of("band")), 
                             x = toy_multi(nb = 3, nc = 5, nr = 4, mask = FALSE), 
                             form  = c("table", "sf")[1]){

  if (!is_stars(x)) stop("Input x must be a stars class")
  # In theory we don't know it is third, but we don't have access to st_upfront
  xbandname <- names(dim(x))[3]
  if(!(xbandname %in% names(pts))) {
    if ("band" %in% names(pts)) {
      pbandname = "band"
    } else {
      stop("Input pts must have a variable that matches 'band' or the name of the x band dimension:", xbandname)
    }
  } else {
    pbandname = xbandname
  }
  
  shape <- shape_stars(x)

  #xp <- stars::st_xy2sfc(x[,,,1], as_points = FALSE, na.rm = FALSE)
  #cell <- sf::st_intersects(sf::st_geometry(pts), sf::st_geometry(xp)) |>
  #  unlist()
  cell <- stars::st_cells(x, pts)
  bandvals <- stars::st_get_dimension_values(x, which = xbandname)
  
  band <- match(pts[[pbandname]], bandvals)
  step <- (band - 1) * shape[['ncell']]
  index <- cell + step
  colrow <- colrow_from_cells(x, cell)
  xv <- stars::st_get_dimension_values(x, which = 1)[colrow[,1, drop = TRUE]]
  yv <- stars::st_get_dimension_values(x, which = 2)[colrow[,2, drop = TRUE]]
  r <- dplyr::tibble(index = as.integer(index), 
                     cell = as.integer(cell), 
                     col = as.integer(colrow[,1, drop = TRUE]), 
                     row = as.integer(colrow[,2, drop = TRUE]), 
                     band = as.integer(band),
                     x = xv, 
                     y = yv)
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
#' # generate a multiband array, points and a polygon
#' set.seed(1)
#' x <- volcano_multi(what = 'bands', threshold = 120)
#' pts <- volcano_points(x, n = 30)
#' poly <- volcano_polygon()
#' 
#' # generate random points across layers that avoid where the array values are missing
#' p <- random_points(x, na.rm = TRUE, n = 30, form = "sf") |>
#'   dplyr::filter(band == 1)
#' plot(x[,,,1], reset = FALSE, axes = TRUE, main = "avoiding NAs")
#' plot(sf::st_geometry(dplyr::filter(p, band == 1)),
#'      add = TRUE, pch = 19, col = "orange")
#' 
#' # generate random points that avoid other known points
#' p <- random_points(x, points = pts, form = "sf", n = 30) |>
#'   dplyr::filter(band == 1)
#' plot(x[,,,1], reset = FALSE, axes = TRUE, main = "avoiding points")
#' plot(sf::st_geometry(dplyr::filter(p, band == 1)),
#'      add = TRUE, pch = 19, col = "orange")
#' plot(sf::st_geometry(dplyr::filter(pts, band == "1")),
#'      add = TRUE, pch = 15, col = "green")
#' 
#' # generate points within a polygon
#' set.seed(2)
#' p <- random_points(x, polygon = poly, form = "sf", n = 30) |>
#'   dplyr::filter(band == 1)
#' plot(x[,,,1], reset = FALSE, axes = TRUE, main = "within a polygon")
#' plot(sf::st_geometry(dplyr::filter(p, band == 1)),
#'      add = TRUE, pch = 19, col = "orange")
#' plot(sf::st_geometry(poly), add = TRUE, border = "green", col = NA)
#' 
#' # generate points within a polygon that also avoid NAs
#' # first, nudge the polygon to lower right
#' poly = twinkle::st_translate(poly, d=c(100, -100))
#' p <- random_points(x, polygon = poly, na.rm = TRUE, form = "sf") |>
#'   dplyr::filter(band == 1)
#' plot(x[,,,1], reset = FALSE, axes = TRUE, main = "within a polygon avoiding NAs")
#' plot(sf::st_geometry(dplyr::filter(p, band == 1)),
#'      add = TRUE, pch = 19, col = "orange")
#' plot(sf::st_geometry(poly),
#'      add = TRUE, border = "green", col = NA)
#'
#' }
random_points <- function(x = volcano_multi(what = "bands", threshold = 120),
                          n = 100,
                          m = 2,
                          na.rm = FALSE,
                          points = NULL,
                          polygon = NULL,
                          form = c("table", "sf")[1]){

  if (!is_stars(x)) stop("Input x must be a stars class")

  shape <- shape_stars(x)

  if (!is.null(polygon)){
    # we compute 2d cells numbers for first band
    # then we propagate those through the the other bands, ala steps
    # 0, ncell, ncell*2, ...
    step <- shape[['ncell']] * (seq_len(shape[['nband']])-1)
    # convert x/y dims to geometry points
    #xp <- stars::st_xy2sfc(x[,,,1], as_points = TRUE, na.rm = FALSE)
    # find which are within the polygon (note edges) as logical vector
    # convert to indices (2d cell numbers)
    # cell <- sf::st_contains(polygon, xp, sparse = FALSE)[1,] |>
    #  which()
    cell <- sf::st_intersects(x, polygon, as_points = TRUE, transpose = TRUE)
    cell <- do.call(c, cell)
    # propagate from cell numbers for first layer to cell numbers for all layers
    loc <- unlist(lapply(step, function(s) cell + s))
    if (length(loc) > n*m) {
      loc <- sample(loc, n*m, replace = FALSE)
    }
    loc <- stars_index_to_loc(loc, x, form = "sf")
  } else {
    loc <- sample(shape[["nindex"]], n*m, replace = FALSE) |>
      stars_index_to_loc(x, form = "sf")
  }
  # now get the values for each attribute
  for (name in names(x)) {
    loc <- loc |>
      dplyr::mutate(!!name := x[[name]][.data$index, drop = TRUE])
  }
  
  loc <- dplyr::relocate(loc, dplyr::all_of("geometry"), .after = dplyr::last_col())

  # remove NAs from consideration?
  if (na.rm){
    loc <- na.omit(loc)
  }

  # points to avoid?
  if (!is.null(points)){
    if (!inherits(points, "sf")) {
      points <- sf::st_as_sf(points, coords = c("x", "y"), crs = sf::st_crs(x))
    }
    ploc <- stars_pts_to_loc(points, x)
    ix <- !(loc$index %in% ploc$index)
    loc <- loc |>
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
        m <- as.matrix(x |> dplyr::as_tibble() |> dplyr::select(vnames[ix]))

        x <- x |>
          dplyr::mutate(!!name := sapply(seq_len(nrow(m)), function(i) m[i,.data$band[i]] )) |>
          dplyr::select(-!!vnames[ix])
      }
    }
    x
  }

  # we don't need to do this if there is only one attribute
  # but it is nice to get the value at each attribute
  if (length(x) > 1){
    loc <- loc |>
      #dplyr::bind_cols(vals) |>
      #reduce_to_band(nms = names(x)) |>
      dplyr::relocate(dplyr::all_of("geometry"), .after = dplyr::last_col())
  }
  
  if (nrow(loc) < n){
    warning("fewer than requested points sampled - try increasing value of m")
  } else {
    loc <- loc |>
      dplyr::slice_sample(n = n, replace = FALSE)
  }

  if (tolower(form[1]) == 'sf'){
    loc <- sf::st_as_sf(loc, coords = c("x", "y"), crs = sf::st_crs(x))
  }

  loc
}

#' Given cell indices, convert to col-row addresses
#' @export
#' @param x stars object
#' @param cells num, cell addresses 
#' @return matrix of [col, row]
colrow_from_cells <- function(x, cells){
  d = dim(x)
  if (any(cells > prod(d))) stop("cell indices must <= nrow(x) * ncol(x)")
  col = ((cells-1) %% d[1])  + 1
  row = floor((cells - 1) / d[1]) + 1
  cbind(col, row)
}

#' Given cell indices, retrieve xy coordinates for 
#' 
#' @export
#' @param x stars object
#' @param cells numeric 1-d address (indices) of pixels of interest
#' @param where character, see \code{\link[stars]{st_get_dimension_values}}
#' @return sf object
st_xy_from_cells <- function(x = toy_single(), 
                             cells = c(60L, 89L, 38L, 16L, 22L, 24L, 83L, 71L, 88L, 20L),
                             where = c("start", "center", "end")[2]){
  
  cr = colrow_from_cells(x, cells)
  xv = stars::st_get_dimension_values(x, which = 1, where = where[1])[cr[,1, drop = TRUE]]
  yv = stars::st_get_dimension_values(x, which = 2, where = where[1])[cr[,2, drop = TRUE]]
  dplyr::tibble(
    cell = cells,
    col = cr[,1, drop = TRUE],
    row = cr[,2, drop = TRUE],
    x = xv,
    y = yv) |>
  sf::st_as_sf(coords = c("x", "y"), crs = sf::st_crs(x))
}

#' A diagnostic tool for comparing the dimensionality of two stars objects 
#'
#' @export
#' @param x `stars` or `dimensions` object
#' @param y another `stars` or `dimensions` object
#' @param tolerance num, see [base::all.equal]
#' @param ignore_resolution logival, if TRUE ignore resolution
#' @return logical, TRUE if the objects are compatable with [stars::c.stars]
combine_ready = function(x, y, 
												 tolerance = 0, 
												 ignore_resolution = FALSE){
	if (inherits(x, "stars")) {
		x = stars::st_dimensions(x)
	} else if (!inherits(x, "dimensions")){
		stop("x must be `stars` or `dimensions` object")
	}
	if (inherits(y, "stars")) {
		y = stars::st_dimensions(y)
	} else if (!inherits(y, "dimensions")){
		stop("y must be `stars` or `dimensions` object")
	}
	
	compatico = TRUE
	if (length(x) != length(y)){
		message(sprintf("x has %i dims while y has %i", length(dimx), length(dimy)))
		return(!compatico)
	}
	
	if (!all(names(x) %in% names(y))){
		message(sprintf("x has [%s] dims while y has [%s]"),
						paste(names(x, collapse = ",")),
						paste(names(y, collapse = ",")))
		return(!compatico)
	}
	
	if (sf::st_crs(x) != sf::st_crs(y)){
		message("x and y have differing CRS values")
		return(!compatico)
	}
	
	sf::st_crs(x) = sf::st_crs(y) <- NA_crs_
	
	for (nm in names(x)){
		dx = x[[nm]]
		dy = y[[nm]]
		if (ignore_resolution){
			dx$delta <- dx$to <- NA_real_
			dy$delta <- dy$to <- NA_real_
		}
		test = all.equal(dx, dy, tolerance = tolerance, check_attributes = FALSE)
		if (!isTRUE(test)){
			message("x and y differ along dimension: ", nm)
			print(test)
			compatico = FALSE
		}
	}

	return(compatico)
}
