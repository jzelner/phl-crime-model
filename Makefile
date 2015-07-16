all: data

data : cache/crime/police_inct_clean.csv

###################################################################################
## PROCESSING OF RAW DATA INTO CLEANED DATA

## Rasterize neighborhood-level data
cache/spatial/phl_raster.grd : munge/rasterize.R data/spatial/Neighborhoods_Philadelphia.geojson
	$(info **** CREATING CITY-LEVEL RASTER FOR ALL NEIGHBORHOODS ****)
	./$< -d $(word 2, $^) -o $@ -x 0.25

## Create a grid adjacency matrix in terms of 1:N id values (i.e. without NA cells from the original raster)
cache/spatial/raster_adjacency.csv : munge/adjacency.R cache/spatial/phl_raster.grd
	$(info **** MAKING RASTER ADJACENCY LIST ****)
	./$< -d $(word 2, $^) -o $@

## Get a clean set of crime categories and data only from 1/1/2006-12/31/2014
## and corresponding raster squares
cache/crime/police_inct_clean.csv : munge/clean_crime_data.R data/crime/police_inct.csv cache/spatial/phl_raster.grd data/spatial/Neighborhoods_Philadelphia.geojson
	$(info **** CLEANING RAW CRIME DATA ***)
	./$< -d $(word 2, $^) -o $@ -r $(word 3, $^) -g $(word 4, $^) -z 17

## Translate into pairwise distance matrix between neighborhood
## centroids

cache/neighborhood_distances.csv : munge/neighborhood_distance.R data/spatial/Neighborhoods_Philadelphia.geojson
	./$< -o $@ -g $(word 2, $^) -z 17

###################################################################################
## DESCRIPTIVES

## Make a raster stack with counts for every crime type by location
cache/crime_stack.grd : munge/stack.R cache/spatial/phl_raster.grd cache/crime/police_inct_clean.csv
	$(info **** MAKING RASTER STACK OF CELL-SPECIFIC COUNTS ****)
	XB./$< -s $@ -r $(word 2, $^) -d $(word 3, $^)


cache/crime/daily_crime_counts.csv : munge/generate_daily_crime_counts.R cache/crime/police_inct_clean.csv
	$(info **** PROCESSING RAW CRIME DATA INTO CITY-WIDE DAILY COUNTS ****)
	./$< -d $(word 2, $^) -o $@


###################################################################################
## MAKING FIGURES

graphs/counts/*.pdf : src/weekly_counts.R cache/crime/daily_crime_counts.csv
	./$<

################################################################################oop
## MAKING MOVIE
output/movie/movie.mp4 : src/movie/movie.R cache/crime/police_inct_clean.csv
	./$< -d $(word 2, $^) -o $@

################################################################################
## POINT PROCESS MODEL

## PROCESS DATA FOR INPUT TO MODEL
max_d := 2
max_t := 60
ppmdata: cache/input_pairs.csv cache/input_points.csv
cache/input_pairs%csv cache/input_points%csv : munge/point_data.R cache/crime/police_inct_clean.csv
	./$< -d $(word 2, $^) -p cache/input_pairs$*csv -o cache/input_points$*csv -t $(max_t) -x $(max_d)
