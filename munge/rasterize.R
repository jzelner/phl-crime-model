#!/usr/bin/Rscript
require(docopt)

'Usage:
   rasterize.R [-d <data> -o <geo> -x <dim>]

Options:
   -d Geographic data in geojson format [default: data/spatial/Neighborhoods_Philadelphia.geojson]
   -o Output raster [default: cache/spatial/phl_raster.grd]
   -a Area of raster grid cells (in KM) [default: 0.25]
   -z UTM zone [default: 17]
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
readOGR(opts$d, "OGRGeoJSON") %>>%

## Project to utm
spTransform(CRS=CRS(sprintf("+proj=utm +zone=%s",opts$z))) -> m

area <- as.numeric(opts$a)

## Get extent of map
ext <- extent(m)
ncol <- ceiling(((ext@xmax-ext@xmin)/1000)/sqrt(area))
nrow <- ceiling(((ext@ymax-ext@ymin)/1000)/sqrt(area))



## Create a raster for the shapefile
r <- raster(ncol = ncol, nrow = nrow)

## Set the extent of the raster to the extent of the shapefile
extent(r) <- extent(m)

## Rasterize at the neighborhood level
rr <- rasterize(m, r, 'mapname')

## Convert raster to spatial pixels data frame
raster_length_non_na <- sum(!is.na(values(rr)))

rr %>>%
{SpatialPoints(coordinates(.)[!is.na(values(.)),], proj4string = CRS(proj4string(.)))} %>>%
SpatialPixelsDataFrame(data = data.frame(id = 1:raster_length_non_na)) -> spdf

## Convert back to raster with IDs as cell values
rr <- raster(spdf)

writeRaster(rr, opts$o, overwrite=TRUE)


