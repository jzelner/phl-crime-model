#!/usr/bin/Rscript
require(docopt)
'Usage:
   point_data.R [-d <data> -o <output>]

Options:
   -d Input data [default: cache/crime/police_inct_clean.csv]
   -o Output data [default: cache/point_data.csv]
   -t Maximum time between related cases (days) [default: 60]
   -x Maximum distance between related cases (km) [default: 2]
' -> doc

opts <- docopt(doc)

require(readr)
require(lubridate)
require(dplyr)
require(pipeR)

## Unpack options into numeric vars
max_t <- as.numeric(opts$t)
max_d <- as.numeric(opts$x)

## Load up point data and filter out any points with NAs
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
{mutate(.,week = week(DISPATCH_DATE),
        year = year(DISPATCH_DATE),
        id = 1:nrow(.))}  -> crime_d

##Now get days since beginning
crime_d$days <- rep(0, nrow(crime_d))

for (i in 1:nrow(crime_d)) {
    crime_d$days[i] <- as.numeric(crime_d$DISPATCH_DATE[i]-min(crime_d$DISPATCH_DATE)) +1
    
}
## For each crime, get the set of crimes with days less than this crime

## First, get unique days in dataset
days <- unique(crime_d$days)

##Get the first day outside the lag length
first_index <- which(days > max_t)[1]

out_df <- data.frame()
for (i in first_index:length(days)) {
    d <- days[i]
    ## First filter out the ones corresponding to this day
    current_d <- filter(crime_d, days == d)
    lag_d <- filter(crime_d, days < d, days > d-max_t)

    print(i)
    print(nrow(current_d))
    print(nrow(lag_d))

    ## Lag is uniform for all day/
    time_lag <- d - lag_d$days
    lag_string <- paste(time_lag, collapse=",")
    current_d$time_lags <- lag_string

    current_d$distance <- rep(0, nrow(current_d))
    for (j in 1:nrow(current_d)) {
        distance <- sqrt((current_d$POINT_X[j] - lag_d$POINT_X)**2 + (current_d$POINT_Y[j] - lag_d$POINT_Y)**2)
        print(j)
        current_d$distance[j] <- paste(distance[distance <= max_d],collapse=",")
    }
    out_df <- rbind(out_df, current_d)
}

write_csv(out_df, opts$o)
