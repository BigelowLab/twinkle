#' day_length generic
#'
#' @param x vector of latitudes, sf or stars object 
#' @param ... Arguments passed to or from other methods
#' @export
day_length <- function(x, ...) {
	UseMethod("day_length")
}

#' day_length default method
#'
#' @export
#' @param x num latitude values
#' @param ... Arguments passed to or from other methods
#' @export
day_length.default <- function(x, ...) {
	geosphere::daylength(x, ...)
}

#' Function to compute daylength for a sfc object
#' 
#' @export
#' @param x sfc object
#' @param doy numeric or Date, day-of-year or Date, the date to compute day length
#' @return vector of day length values
day_length.sfc = function(x = read_buoys(),
												doy = as.Date("2014-01-01")){
	
	if (!requireNamespace("geosphere")){
		stop("please install the geosphere package first")
	}
	if (!sf::st_is_longlat(x)){
		stop("input must be in degrees longitude-latitude")
	}
	xy = sf::st_coordinates(x)
	geosphere::daylength(xy[,1], doy)
}

#' Function to compute daylength for a sf object
#' 
#' @export
#' @param x stars, template stars object on which to build
#' @param doy numeric or Date, day-of-year or Date, the date to compute day length
#' @return vector of day length values
day_length.sf = function(x = read_buoys(),
												doy = as.Date("2014-01-01")){
	
	if (!requireNamespace("geosphere")){
		stop("please install the geosphere package first")
	}
	if (!sf::st_is_longlat(x)){
		stop("input must be in degrees longitude-latitude")
	}
	xy = sf::st_coordinates(x)
	geosphere::daylength(xy[,1], doy)
}

#' Function to create a day length stars object
#' 
#' @export
#' @param x stars, template stars object on which to build
#' @param doy numeric or Date, day-of-year or Date, the date to compute day length
#' @param transfer_na logical, if TRUE transfer NAs from x to the output
#' @return stars object with day length in hours per cell
day_length.stars = function(x = stars::read_stars(system.file("datasets/20140601-20140630-sst.tif", 
																									     package = "twinkle")) |>
										 	dplyr::slice("band", 1),
										 doy = as.Date("2014-01-01"),
										 transfer_na = FALSE){
	
	if (!requireNamespace("geosphere")){
		stop("please install the geosphere package first")
	}
	if (!sf::st_is_longlat(x)){
		stop("input must be in degrees longitude-latitude")
	}
	dims = dim(x)
	x = x[1] |>
		rlang::set_names("daylength")
	
	# This is where we could split off and compute daylength for each time-related band
	# but we'll leave that for some other day
	if (length(dims) > 2) x = dplyr::slice(x, names(dims)[3], 1)
	
	lats = stars::st_get_dimension_values(x, names(dims)[2], where = "center")
	
	m = x[[1]] 
	if (transfer_na){
		isna = is.na(m) |> as.vector()
	}
	dm = dim(m)
	index = seq_len(prod(dm))
	dl = geosphere::daylength(lats, doy)
	tmp = outer(dl, rep(1, dims[1]), "*") |>
		t()
  x$daylength[index] = as.vector(tmp)
	
  if (transfer_na){
  	x$daylength[isna] <- NA
  }
	x
}
