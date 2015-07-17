#!/usr/bin/Rscript

## Author:   Jon Zelner (adapted from code by Kyle Foreman)
##           jlz2115@columbia.edu
## Date:     14 Jul 2014
require(docopt)

'Usage:
   ppm.R [-d <data> -a <adjacency> -m <model>]

Options:
   -a Neighborhood adjacency [default: cache/neighborhood_distances.csv]
   -d Crime point data [default: cache/input_points.csv]
   -m Model file [default: src/ppm/model.stan]
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
total_weeks <- seq(from = mdy("1/1/2006"), to = mdy("12/31/2014"),by = "week")

month_df <- data.frame(month = month(total_weeks), year = year(total_weeks))
month_df$month_year <-sprintf("%d,%d", month_df$month, month_df$year)

all_months <- data.frame(month_year = unique(month_df$month_year), total_months = 1:length(unique(month_df$month_year)))

month_df <- merge(month_df, all_months, by = "month_year")
month_df %>>%
arrange(month, year)-> month_df

## Load input into data
data_in <- list(A=num_a,
                T = max(d$TOTAL_MONTHS),
                N = nrow(d),
                W =  max(week(d$DISPATCH_DATE)),
                TW = length(total_weeks),
                month = d$TOTAL_MONTHS,
                area = d$NEIGHBORHOOD_ID,
                week = week(d$DISPATCH_DATE),
                week_month = week(total_weeks),
                month_week = sort(month_df$total_months),
                dmat=dist_mat)

## init <- list(
##     list(
##         log_intensity = rep(0, data_in$T),
##         beta1 = rep(0, data_in$A),
##         tau = 1.0,
##         p = 0.99,
##         week_alpha = 0.0,
##         week_beta = 1.0,
##         week_sigma = 1.0)
##     )



m <- stan(
    opts$m,
    data = data_in,
##    init = init,
    chains = 1,
    iter = 1000
)


