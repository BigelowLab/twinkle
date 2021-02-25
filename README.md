# twinkle

A small set of R tools to use with [sf](https://CRAN.R-project.org/package=sf) and [stars](https://CRAN.R-project.org/package=stars) in species distribution modeling.  See the [wiki](https://github.com/BigelowLab/twinkle/wiki) for usage example.

## Requirements
 
 + [R v4+](https://www.r-project.org/)
 
 + [rlang](https://CRAN.R-project.org/package=rlang)
  
 + [dplyr](https://CRAN.R-project.org/package=dplyr)
 
 + [sf](https://CRAN.R-project.org/package=sf)
 
 + [stars](https://CRAN.R-project.org/package=stars)

 + [geometry](https://CRAN.R-project.org/package=geometry)

## Resources

 + [RSeek.org](https://rseek.org/?q=stars+sf)
 
 + [rspatial.org](https://rspatial.org/)
 
 + [`sf` guide](http://r-spatial.github.io/sf/)
 
 + [`stars` guide](https://r-spatial.github.io/stars/)
 
 + [Geocomputation with R](https://geocompr.robinlovelace.net/) (*n.b.* [raster](https://CRAN.R-project.org/package=raster) centric)

## Intended usage

`stars` will handle a wide variety of data models, but this package is intended for use with regular grids with 3 dimensions such as [x, y, band], [x, y, time] or [x, y, z].  See about the  `stars` data model [here](https://r-spatial.github.io/stars/articles/stars4.html).  


## Wiki

The [wiki](https://github.com/BigelowLab/twinkle/wiki) provides a suite of examples.


