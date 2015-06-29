## Get a clean set of crime categories and data only from 1/1/2006-12/31/2014
cache/crime/police_inct_clean.csv : munge/clean_crime_data.R data/crime/police_inct.R
	$(info **** CLEANING RAW CRIME DATA ***)
	./$< -d $(word 2, $^) -o $@
