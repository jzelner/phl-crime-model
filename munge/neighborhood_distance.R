#!/usr/bin/Rscript
require(docopt)

'Usage:
   neighborhoods.R [-g <geodata> -o <output> -z <zone> ]

Options:
   -g Geographic data in geojson format [default: data/spatial/Neighborhoods_Philadelphia.geojson]
   -o Neighborhood distance matrix [default: cache/neighborhood_distances.csv]
   -z UTM Zone [default: 17]
' -> doc

opts <- docopt(doc)

p4string <- sprintf("+proj=utm +zone=%s",opts$z)
require(raster)
require(rgdal)
require(dplyr)
require(stringr)
require(pipeR)
require(readr)

distances <- function(x1, x2, y1, y2) {
    return(sqrt((x1-x2)**2 + (y1-y2)**2))
}

## Load up philly shapefile
readOGR(opts$g, "OGRGeoJSON") %>>%

## Project to utm
spTransform(CRS=CRS(p4string)) -> m

## Get neighborhood coordinates
z <- coordinates(m)
data.frame(NEIGHBORHOOD = m$name,
           CENTROID_X = z[,1],
           CENTROID_Y = z[,2]) %>>%
arrange(NEIGHBORHOOD) %>>%
{mutate(.,NEIGHBORHOOD_ID = 1:nrow(.))}-> centroids


out_df <- data.frame()
N <- nrow(centroids)
for (i in 1:N) {
    dist <- distances(centroids$CENTROID_X[i], centroids$CENTROID_X, centroids$CENTROID_Y[i], centroids$CENTROID_Y)
    df <- data.frame(from = centroids$NEIGHBORHOOD[i],
                     to = centroids$NEIGHBORHOOD,
                     from_id = centroids$NEIGHBORHOOD_ID[i],
                     to_id = centroids$NEIGHBORHOOD_ID,
                     distance = dist/1000)
    out_df <- rbind(out_df, df)
}

write_csv(out_df, opts$o)
