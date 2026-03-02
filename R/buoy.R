#' Read a selection of buoy data
#' 
#' Data are a subset of the buoys available from NOAA-EDAB group's 
#' [buoydata](https://github.com/NOAA-EDAB/buoydata) R package.
#' 
#' @export
#' @return a sf table of buoy metadata
read_buoys = function(){
	system.file("datasets/buoy.gpkg", package = "twinkle") |>
		sf::read_sf()
}
