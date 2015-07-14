#!/usr/bin/Rscript
require(docopt)

'Usage:
   stack.R [-d <data> -s <output> -r <raster>]

Options:
   -d Input crime data [default: cache/crime/police_inct_clean.csv]
   -r Input raster data [default: cache/spatial/phl_raster.grd]
   -s Output raster stack [default: cache/crime_stack.grd]
' -> doc

opts <- docopt(doc)

require(raster)
require(dplyr)
require(pipeR)
require(readr)

r <- raster(opts$r)
d <- read_csv(opts$d)

## Get counts by raster ID and crime type

d %>>%
group_by(TEXT_GENERAL_CODE, RASTER_ID) %>>%
summarize(N = length(TEXT_GENERAL_CODE)) -> cell_counts


rasters <- list("ID" = r)
for (c in unique(cell_counts$TEXT_GENERAL_CODE)) {

    assault_cell_counts <- rep(0, length(r))
    assault_counts <- filter(cell_counts, TEXT_GENERAL_CODE == c)
    assault_cell_counts[assault_counts$RASTER_ID] <- assault_counts$N
    assault_cell_counts[is.na(values(r))] <- NA

    assault_r  <- setValues(r, assault_cell_counts)
    rasters[c] <- assault_r
}

counts <- stack(rasters)

writeRaster(counts, opts$s, overwrite = TRUE)
