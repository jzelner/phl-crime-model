#!/Usr/bin/Rscript
require(docopt)

'Usage:
   clean_crime_data.R [-d <data> -o <output> -r <raster> -z <zone> -g <geojson>]

Options:
   -d Input data [default: data/crime/police_inct.csv]
   -o Output data [default: cache/crime/police_inct_clean.csv]
   -r Raster data [default: cache/spatial/phl_raster.grd]
   -g Geographic data in geojson format [default: data/spatial/Neighborhoods_Philadelphia.geojson]
   -z UTM Zone [default: 17]
' -> doc


opts <- docopt(doc)
p4string <- sprintf("+proj=utm +zone=%s",opts$z)

## Get path to file

require(pipeR)
require(dplyr)
require(stringr)
require(lubridate)
require(raster)
require(rgdal)
require(readr)
## load raw input
raw_d <- read_csv(opts$d)
readOGR(opts$g, "OGRGeoJSON") %>>%
## Project to utm
spTransform(CRS=CRS(p4string)) -> phila_map

## Convert dates to lubridate dates
raw_d$DISPATCH_DATE <- ymd(raw_d$DISPATCH_DATE)
raw_d$YEAR <- year(raw_d$DISPATCH_DATE)
raw_d$WEEK <- week(raw_d$DISPATCH_DATE)

## Select only dates from 1/1/2006 to 12/31/2014
raw_d %>>% filter(DISPATCH_DATE >= ymd("2006-01-01"),
                  DISPATCH_DATE <= ("2014-12-31")) -> f_d

## Make a list of weeks from the beginning to the end of the
## observation period, which we'll use to assign distinct weeks
## to each observation
seq(from=ymd("2006-01-01"),
    to=ymd("2014-12-31"), by = "weeks") %>>%

{data.frame(TOTAL_WEEK = 1:length(.),
            WEEK = week(.),
            YEAR = year(.))} -> all_weeks

f_d <- merge(f_d, all_weeks, by = c("WEEK", "YEAR"))

## Clean up crime categories by stripping out excess writepace
f_d %>>%
(TEXT_GENERAL_CODE) %>>%
str_trim -> f_d$TEXT_GENERAL_CODE

## Now get rid of anything with a missing coordinate
f_d %>>% dplyr::filter(!is.na(POINT_X), !is.na(POINT_Y)) -> f_d

## Now load up the raster
rr <- raster(opts$r)

## Make an intermediate spatialpoints dataframe we'll use
## to convert the long,lat locations of the input crimes
## to the projection of the raster
SpatialPoints(cbind(f_d$POINT_X, f_d$POINT_Y), proj4string=CRS("+proj=longlat")) %>>%
spTransform(CRS=CRS(proj4string(rr))) -> utm_points

utm_points %>>% (coords) -> utm_coords

## Extract raster ids
r_ids <- extract(rr, utm_coords, cellnumbers=TRUE)

## Store UTM coordinates for each point in output data frame
f_d$UTM_X <- utm_coords[,1]
f_d$UTM_Y <- utm_coords[,2]

## Add to data
f_d$RASTER_CELL <- r_ids[,2]
f_d$RASTER_ID <- r_ids[,1]

## Get rid of anything not in the shapefile area
f_d %>>% dplyr::filter(!is.na(RASTER_ID),
                       !is.na(RASTER_CELL)) -> f_d

crime_points <- SpatialPoints(cbind(f_d$UTM_X, f_d$UTM_Y), proj4string = CRS(p4string))
f_d$NEIGHBORHOOD <- over(crime_points, phila_map)$name

## Now query the neighborhood geojson to get neighborhood each crime
## occurred in
## crime_points <- SpatialPoints(cbind(f_d$UTM_X, d$UTM_Y), proj4string = CRS(p4string))
## d$NEIGHBORHOOD  <- over(crime_points, m)$name


## Make sure the output directory exists before writing to it
dir.create(dirname(opts$o), showWarnings = FALSE)

## Write file out
write.csv(f_d, opts$o, row.names=FALSE)
