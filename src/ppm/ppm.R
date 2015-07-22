#!/usr/bin/Rscript

## Author:   Jon Zelner 
##           jlz2115@columbia.edu
## Date:     14 Jul 2014
require(docopt)

'Usage:
   ppm.R [-d <data> -a <adjacency> -m <model> -o <output> -n <niter>]

Options:
   -a Neighborhood adjacency [default: cache/neighborhood_distances.csv]
   -d Crime point data [default: cache/input_points.csv]
   -m Model file [default: src/ppm/model.stan]
   -o Model output [default: output/ppm/ppm.csv]
   -n Number of iterations [default: 1000]
' -> doc

opts <- docopt(doc)

## libraries
require(packrat)
packrat::disable()

require(rstan)
require(readr)
require(raster)
require(pipeR)
require(dplyr)
require(lubridate)

set_cppo('fast');

## Load up the crime data and filter out the assaults
d <- read_csv(opts$d)

##d <- subset(d, TEXT_GENERAL_CODE == "Homicide - Criminal")
## Load up the adjacency list
a <- read_csv(opts$a)
num_a <- max(a$from_id)
## Convert into an adjacency matrix
dist_mat <- matrix(0, num_a, num_a)
for (r in 1:nrow(a)) {
    i <- a$from_id[r]
    j <- a$to_id[r]
    dist_mat[i,j] <- a$distance[r]
}

## Get neighborhood IDs
a %>>%
group_by(from) %>>%
summarize(ID = from_id[1]) %>>%
select(NEIGHBORHOOD = from, NEIGHBORHOOD_ID = ID) -> neighborhood_ids
d <- merge(d, neighborhood_ids, by = "NEIGHBORHOOD")
moy <- month(seq(from = min(d$DISPATCH_DATE), to = max(d$DISPATCH_DATE), by = "month"))

## Load input into data
data_in <- list(A=num_a,
                T = max(d$TOTAL_MONTHS), ## Total number of months in observation period
                M = 12, ## Months in a year
                N = nrow(d),
                total_month = d$TOTAL_MONTHS,
                month = d$MONTH,
                area = d$NEIGHBORHOOD_ID,
                moy = moy,
                dmat=dist_mat)


## Run model
m <- stan(
    opts$m,
    data = data_in,
    sample_file = opts$o,
    chains = 1,
    iter = as.numeric(opts$n)
)


