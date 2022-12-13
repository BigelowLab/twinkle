#' A wrapper around base::findInterval() that allows decreasing values in the
#' value of the vector within which we wish to place values of x.
#'
#' When \code{vec} is in ascending order we use \code{base::findInterval()}, but
#' when \code{vec} is in descending order we implement an adaptation of the
#' \code{locate()} function from Numerical Recipes for C \url{http://apps.nrbook.com/c/index.html}
#'
#' @export
#' @param x numeric values we wish to located within \code{vec}
#' @param vec numeric vector of sorted values (ascending or descending order)
#'    within which we wish to find the placement of \code{x}
#' @param rightmost.closed see \link{findInterval}
#' @param all.inside see \link{findInterval}
#' @return see \link{findInterval}
find_interval <- function(x, vec, rightmost.closed = FALSE, all.inside = FALSE){
  
  # locate one value of x within v
  # @param v ordered numeric vector
  # @param x one numeric lo locate within v
  # @return index into v
  locate_one <- function(v, x){
    n <- length(v)
    ascnd <- v[n] >= v[1]
    iL <- 1
    iU <- n
    while((iU-iL) > 1){
      iM <- bitwShiftR((iU+iL),1)
      if (ascnd){
        if (x >= v[iM]){
          iL <- iM
        } else {
          iU <- iM
        }
      } else {
        if (x <= v[iM]){
          iL <- iM
        } else {
          iU <- iM
        }
      }
    }
    
    if (ascnd) {
      if ( x < v[1]) {
        index <- 0
      } else if (x >= v[n]) {
        index <- n
      } else {
        index <- iL
      }
    } else {
      if ( x > v[1]) {
        index <- 0
      } else if (x <= vec[n]) {
        index <- n
      } else {
        index <- iL
      }
    }
    return(index)
  }  # locate_one
  
  ascending <- vec[length(vec)] >= vec[1]
  
  if (!ascending) {
    # here we do our own implementation (with a performance hit)
    j <- sapply(x, function(x, v=NULL) locate_one(v,x), v = vec)
    nv <- length(vec)
    if (all.inside){
      j[j < 1] <- 1
      j[j >= nv] <- nv - 1
    }
    if (rightmost.closed){
      j[x <= vec[nv]] <- nv - 1
    }
  } else {
    # this is plain vanilla stuff we pass to findInterval
    j <- base::findInterval(x, vec,
                            rightmost.closed = rightmost.closed, all.inside = all.inside)
  }
  j
}  # find_interval
