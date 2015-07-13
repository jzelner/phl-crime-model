all: data

data : cache/crime/police_inct_clean.csv 

###################################################################################
## PROCESSING OF RAW DATA INTO CLEANED AND AGGREGATED DATA

## Rasterize neighborhood-level data
cache/spatial/phl_raster.grd : munge/rasterize.R data/spatial/Neighborhoods_Philadelphia.geojson
	$(info **** CREATING CITY-LEVEL RASTER FOR ALL NEIGHBORHOODS ****)
	./$< -d $(word 2, $^) -o $@ -x 0.5

## Get a clean set of crime categories and data only from 1/1/2006-12/31/2014
## and corresponding raster squares 
cache/crime/police_inct_clean.csv : munge/clean_crime_data.R data/crime/police_inct.csv cache/spatial/phl_raster.grd
	$(info **** CLEANING RAW CRIME DATA ***)
	./$< -d $(word 2, $^) -o $@ -r $(word 3, $^)

cache/crime/daily_crime_counts.csv : munge/generate_daily_crime_counts.R cache/crime/police_inct_clean.csv
	$(info **** PROCESSING RAW CRIME DATA INTO CITY-WIDE DAILY COUNTS ****)
	./$< -d $(word 2, $^) -o $@

## Get raster IDs for each 
###################################################################################
## MAKING FIGURES
graphs/counts/*.pdf : src/weekly_counts.R cache/crime/daily_crime_counts.csv
	./$<

################################################################################
## MAKING MOVIE
output/movie/movie.mp4 : src/movie/movie.R cache/crime/police_inct_clean.csv
	./$< -d $(word 2, $^) -o $@

################################################################################
## Make input dataset for point-process model
max_d := 2
max_t := 60
cache/point_data.csv : munge/point_data.R cache/crime/police_inct_clean.csv
	./$< -d $(word 2, $^) -o $@ -t $(max_t) -x $(max_d)


