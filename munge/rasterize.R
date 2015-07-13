#!/usr/bin/Rscript
require(docopt)

'Usage:
   rasterize.R [-d <data> -o <geo> -x <dim>]

Options:
   -d Geographic data in geojson format [default: data/spatial/Neighborhoods_Philadelphia.geojson]
   -o Output raster [default: cache/spatial/phl_raster.grd]
   -x Dimensions of raster grid [default: 50,50]
' -> doc

opts <- docopt(doc)


require(raster)
require(rgdal)
require(dplyr)
require(stringr)
require(pipeR)
require(readr)

## Unload dimensions
dims <- as.numeric(str_split(opts$x,",")[[1]])

## Load up philly shapefile
m <- readOGR(opts$d, "OGRGeoJSON")

## Get extent of map
ext <- extent(m)

## Create a raster for the shapefile
r <- raster(ncol = dims[1], nrow = dims[2])

## Set the extent of the raster to the extent of the shapefile
extent(r) <- extent(m)

## Rasterize at the neighborhood level
rr <- rasterize(m, r, 'mapname')

## Write raster to file
writeRaster(rr, opts$o, overwrite=TRUE)

