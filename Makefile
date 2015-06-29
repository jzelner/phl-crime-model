all: data

data : cache/crime/police_inct_clean.csv

###################################################################################
## PROCESSING OF RAW DATA INTO CLEANED AND AGGREGATED DATA

## Get a clean set of crime categories and data only from 1/1/2006-12/31/2014
cache/crime/police_inct_clean.csv : munge/clean_crime_data.R data/crime/police_inct.csv
	$(info **** CLEANING RAW CRIME DATA ***)
	./$< -d $(word 2, $^) -o $@

cache/crime/daily_crime_counts.csv : munge/generate_daily_crime_counts.R cache/crime/police_inct_clean.csv
	$(info **** PROCESSING RAW CRIME DATA INTO CITY-WIDE DAILY COUNTS ****)
	./$< -d $(word 2, $^) -o $@
