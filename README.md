# twinkle

A small set of R tools to use with [sf](https://CRAN.R-project.org/package=sf) and [stars](https://CRAN.R-project.org/package=stars) in species distribution modeling.

## Requirements
 
 + [R v4+](https://www.r-project.org/)
 
 + [rlang](https://CRAN.R-project.org/package=rlang)
  
 + [dplyr](https://CRAN.R-project.org/package=dplyr)
 
 + [sf](https://CRAN.R-project.org/package=sf)
 
 + [stars](https://CRAN.R-project.org/package=stars)


## Resources

 + [RSeek.org](https://rseek.org/?q=stars+sf)
 
 + [rspatial.org](https://rspatial.org/)
 
 + [https://r-spatial.github.io](https://r-spatial.github.io/stars/)


## Provided example data

### MUR Sea Surface Temperature

One month of daily MUR SST rasters (as one GeoTIFF) from 2014 covering Penobscot Bay, Maine. The bounding box `[west, east, south, north]` is `[-69.2, -68.49, 43.78, 44.5]`. Dates provided are 2014-06-01 through 2014-06-30.  Also provided are daily sst slope and daily cumulative sst (origin January 1).  These are named `20140601-20140630-sst.tif`, `20140601-20140630-sst_slope.tif` amd `20140601-20140630-sst_cum.tif`.

> JPL MUR MEaSUREs Project. 2015. GHRSST Level 4 MUR Global Foundation Sea Surface Temperature Analysis. Ver. 4.1. PO.DAAC, CA, USA. Dataset accessed [2021-02-08] at https://doi.org/10.5067/GHGMR-4FJ04

100 points within Penobscot Bay, selected at random, stored in [geopackage](https://www.geopackage.org/) format. Use `sf::read_sf(system.file("datasets/penbay-points.gpkg", package = 'twinkle))` to read them in.

A polygons within Penobscot Bay stored in [geopackage](https://www.geopackage.org/) format.  Use `sf::read_sf(system.file("datasets/penbay-polygons.gpkg", package = 'twinkle))` to them in.

```
library(sf)
library(stars)
library(dplyr)

sst <- stars::read_stars(system.file("datasets/20140601-20140630-sst.tif"), package = "twinkle")
pts <- sf::read_sf(system.file("datasets/penbay-points.gpkg", package = 'twinkle))
poly <- sf::read_sf(system.file("datasets/penbay-polygons.gpkg", package = 'twinkle))

plot(sst[,,,1], axes = TRUE, main = "MUR SST", reset = F)
plot(pts, add = TRUE, pch = 19, col = "orange")
plot(poly, add = TRUE, border = "green", col = NA)
```

### Volcano

A `stars` version of this [nice example](https://waterdata.usgs.gov/blog/inlmiscmaps/) of the built in `volcano` dataset. Points and polygons are also provided.

```
v_a <- volcano_multi(what = "attributes")
v_b <- volcano_multi(what = "bands")
```

### Tiny Toy

Multiband or multi-attribute dataset - with very small dimensions, with convenient polygons and points.

```
library(sf)
library(stars)
library(dplyr)

x <- toy() 
pts <- toy_points()
poly <- toy_polygon()

plot(x[,,,1], axes = TRUE, reset = FALSE)
plot(pts["id"], add = TRUE, pch = 19, col = "orange")
plot(poly, add = TRUE, border = "green", col = NA)
```

## Wiki

The [wiki](https://github.com/BigelowLab/twinkle/wiki) provides a suite of examples.


