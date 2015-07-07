#!/usr/bin/Rscript
require(docopt)
require(rgdal)
require(maptools)
require(readr)
require(pipeR)
require(lubridate)
require(deldir)
require(polyclip)

clipped_tiles <- function(voronoi, outline) {

    vtiles <- tile.list(voronoi)


    out_id <- c()
    out_x <- c()
    out_y <- c()
    area <- c()
    for (i in 1:length(vtiles)) {
        print(i)
        vt <- vtiles[[i]]

        ##Make a list for clipping with polyclip
        vt_a <- list(list(x = vt$x, y = vt$y))
        clipped_vt <- polyclip(outline, vt_a,op=c("intersection"))

        ## This is important in case a polygon essentially
        ## gets clipped out of existence
        if (length(clipped_vt) > 0) {
            clipped_vt <- clipped_vt[[1]]
            for (j in 1:length(clipped_vt$x)) {
                out_x <-  append(out_x,clipped_vt$x[j])
                out_y <- append(out_y, clipped_vt$y[j])
                out_id <- append(out_id,i)
                area <- append(area, voronoi$summary$dir.area[i])
                
            }
        }

        
    }
    plot_df <- data.frame(x = out_x,
                          y = out_y,
                          area = area,
                          density = 1/area,
                          id = out_id)
    return(plot_df)

}

## Load up crime data
read_csv("cache/crime/police_inct_clean.csv") %>>%
filter(TEXT_GENERAL_CODE  == "Homicide - Criminal") %>>%
filter(!is.na(POINT_X) == TRUE) %>>%
{
    .$DISPATCH_DATE <- ymd(.$DISPATCH_DATE)
    return(.)
} %>>%
arrange(DISPATCH_DATE) %>>%
select(POINT_X, POINT_Y, DISPATCH_DATE) -> crime_d

## Break the period from the beginning of the observations
## to the end into 1-week intervals

## Load up city map
phila_map<- readOGR("data/spatial/Neighborhoods_Philadelphia.geojson", "OGRGeoJSON")
phl_outline <- unionSpatialPolygons(phila_map, IDs=rep(1,length(phila_map)))
outline_coords <- phl_outline@polygons[[1]]@Polygons[[1]]@coords
outline <- list(list(x = outline_coords[,1], y = outline_coords[,2]))

point_file <- crime_d[1:100,]

voronoi <- deldir(point_file$POINT_X, point_file$POINT_Y)
vtiles <- tile.list(voronoi)

for (i in 1:2) {
    voronoi <- deldir(tile.centroids(vtiles))
    vtiles <- tile.list(voronoi)
}
clipped_voronoi <- clipped_tiles(voronoi, outline) 
