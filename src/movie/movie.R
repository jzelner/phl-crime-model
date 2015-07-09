#!/usr/bin/Rscript
require(docopt)
'Usage:
   movie.R [-d <data> -o <output>]

Options:
   -d Input data [default: cache/crime/police_inct_clean.csv]
   -o Output movie [default: output/movie/movie_2.mp4]
' -> doc

opts <- docopt(doc)

require(rgdal)
require(maptools)
require(readr)
require(pipeR)
require(lubridate)
require(deldir)
require(polyclip)
require(ggplot2)
require(dplyr)
require(animation)

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
d <- read_csv(opts$d)

d %>>% filter(TEXT_GENERAL_CODE %in% c("Homicide - Criminal", "Aggravated Assault Firearm")) %>>%
filter(!is.na(POINT_X) == TRUE) %>>%
{
    .$DISPATCH_DATE <- ymd(.$DISPATCH_DATE)
    return(.)
} %>>%
arrange(DISPATCH_DATE) %>>%
select(POINT_X, POINT_Y, DISPATCH_DATE, TEXT_GENERAL_CODE) %>>%
##Add week and year
mutate(week = week(DISPATCH_DATE), year = year(DISPATCH_DATE))  -> crime_d

## Break the period from the beginning of the observations
## to the end into 1-week intervals

## First make a data frame with every week in it
obs_weeks <- seq(from=min(crime_d$DISPATCH_DATE), to = max(crime_d$DISPATCH_DATE), by = "week")
odf <- data.frame(dates = obs_weeks,
                  week = week(obs_weeks),
                  year = year(obs_weeks),
                  total_week = 1:length(obs_weeks))

## Merge into data frame to get proper week index
crime_d <- merge(crime_d, odf, by = c("week","year"))
hom_d <- subset(crime_d, TEXT_GENERAL_CODE == "Homicide - Criminal")
min_x <- min(hom_d$POINT_X)
max_x <- max(hom_d$POINT_X)
min_y <- min(hom_d$POINT_Y)
max_y <- max(hom_d$POINT_Y)

step_size <- 4

i <- 1

## Load up city map
phila_map<- readOGR("data/spatial/Neighborhoods_Philadelphia.geojson", "OGRGeoJSON")
phl_outline <- unionSpatialPolygons(phila_map, IDs=rep(1,length(phila_map)))
outline_coords <- data.frame(phl_outline@polygons[[1]]@Polygons[[1]]@coords)
outline_coords$group <- 1
##outline <- list(list(x = outline_coords[,1], y = outline_coords[,2]))


data_window <- function(df, i, step_size, outline) {
    df  %>>%
    filter(total_week >= i, total_week < i+step_size) %>>%
    mutate(fade = (total_week-(i-1))/step_size) -> windowed_crime
    
    g <- ggplot()


    g <- g + geom_point(data=windowed_crime,
                        aes(x = POINT_X,
                            y = POINT_Y,
                            group = TEXT_GENERAL_CODE,
                            colour = TEXT_GENERAL_CODE,
                            alpha = fade)) 
    
    g <- g + coord_map(projection="mercator")
    g <- g + theme(axis.line = element_line(colour = "black"),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.border = element_blank(),
                   panel.background = element_blank())+
                     theme(axis.text.x = element_text(colour = "black"),
                           axis.text.y = element_text(colour = "black"),
                           plot.title = element_text(face = "bold"),
                           axis.ticks = element_line(colour="black"))


    g <- g + xlim(min_x, max_x) +
      ylim(min_y, max_y) +
      xlab("Long") +
      ylab("Lat") +
      ggtitle(i) +
      guides(colour = FALSE,
             alpha = FALSE) 

    print(g)
}

data_window <- function(df, i, step_size, outline) {
    df  %>>%
    filter(total_week >= i, total_week < i+step_size) %>>%
    mutate(fade = (total_week-(i-1))/step_size) -> windowed_crime
    
    g <- ggplot()
    g <- g + geom_density2d(data=windowed_crime,
                            aes(fill = ..level..,
                                colour = ..level..,
                                x = POINT_X,
                                y = POINT_Y,
                                alpha = 1))    
    g <- g + coord_map(projection="mercator")
    g <- g + theme(axis.line = element_line(colour = "black"),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.border = element_blank(),
                   panel.background = element_blank())+
                     theme(axis.text.x = element_text(colour = "black"),
                           axis.text.y = element_text(colour = "black"),
                           plot.title = element_text(face = "bold"),
                           axis.ticks = element_line(colour="black"))


    g <- g + xlim(min_x, max_x) +
      ylim(min_y, max_y) +
      xlab("Long") +
      ylab("Lat") +
      ggtitle(i) +
      guides(colour = FALSE,
             alpha = FALSE,
             fill = FALSE) 

    print(g)
}


##data_window(crime_d, 1, 4, outline_coords) #




saveVideo({
    for (i in 1:(nrow(odf)-(step_size-1))) {
        data_window(crime_d, i, 4)
    }},interval = 0.15,
          video.name=opts$o)



## point_file <- crime_d[1:100,]

## voronoi <- deldir(point_file$POINT_X, point_file$POINT_Y)
## vtiles <- tile.list(voronoi)

## for (i in 1:2) {
##     voronoi <- deldir(tile.centroids(vtiles))
##     vtile s <- tile.list(voronoi)
## }
## clipped_voronoi <- clipped_tiles(voronoi, outline) 
