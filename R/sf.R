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
#'  p <- x %>%
#'    dplyr::group_by(name) %>%
#'    dplyr::group_map(make_one_polygon, crs = "WGS84") %>%
#'    dplyr::bind_rows()
#' }
points_to_polygon <- function(x = dplyr::tibble(x = c(1,2,1,0), 
                                                y = c(0,1,2,1)), 
                              crs =  "WGS84"){
  m <- as.matrix(x)
  n <- nrow(m)
  if (!identical(m[1,], m[n,])) m <- rbind(m, m[1,]) # close of open
  sf::st_polygon(x = list(m)) %>% 
    sf::st_sfc(crs = crs)
}


points_in_polygons <- function(x, y){
  if(FALSE){
    y1 <- volcano_polygon()
    y2 <- st_rotate(y1, pi/2)
      dplyr::bind_rows(st_rotate())
    x = volcano_points(x = volcano_multi(what = "bands"))
  }
  
  pid <- rep(NA_character_, nrow(x))
  index <- sf::st_contains(y, x)
  p <- sapply(index, 
                function(idx){
                  #cat(str(idx,"\n"))
                  pid[idx] <- ifelse(is.na(pid[idx]), idx, paste(pid[idx], idx, sep = ","))
                  pid
                })
  
  
}
