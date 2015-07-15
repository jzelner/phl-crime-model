#!/Usr/bin/Rscript
require(docopt)
'Usage:
   point_data.R [-d <data> -o <output> -p <points> -x <distance> -t <time> ]

Options:
   -d Input data [default: cache/crime/police_inct_clean.csv]
   -p Output file for pairs [default: cache/input_pairs.csv]
   -o Output file for points [default: cache/input_points.csv]
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

d %>>% filter(TEXT_GENERAL_CODE %in% c("Homicide - Criminal")) %>>%
filter(!is.na(POINT_X) == TRUE) %>>%
{
    .$DISPATCH_DATE <- ymd(.$DISPATCH_DATE)
    return(.)
} %>>%
arrange(DISPATCH_DATE) %>>%
select(UTM_X, UTM_Y, DISPATCH_DATE, TEXT_GENERAL_CODE, RASTER_ID, RASTER_CELL) %>>%
##Add week and year
{mutate(.,week = week(DISPATCH_DATE),
        year = year(DISPATCH_DATE),
        id = 1:nrow(.))}  -> crime_d

##Now get days since beginning
crime_d$days <- rep(0, nrow(crime_d))

for (i in 1:nrow(crime_d)) {
    crime_d$days[i] <- as.numeric(crime_d$DISPATCH_DATE[i]-min(crime_d$DISPATCH_DATE)) +1   
}

## Make a sequence of weeks from the first to the last
all_weeks <- seq(from=min(crime_d$DISPATCH_DATE), to = max(crime_d$DISPATCH_DATE), by = "week")
week_df <- data.frame(total_weeks = 1:length(all_weeks),
                      week = week(all_weeks),
                      year = year(all_weeks))

## Merge into crime data
crime_d <- merge(crime_d, week_df, by = c("week", "year"))
## For each crime, get the set of crimes with days less than this crime
## First, get unique days in dataset
days <- unique(crime_d$days)

##Get the first day outside the lag length
### TODO: CHOP OFF FIRST X ROWS OF POINT DATA AND BE SURE
### TO RE-NORMALIZE DAYS/WEEKS SO THAT THE NORMALIZED DATA
### START AT ONE
first_index <- which(days > max_t)[1]

out_df <- data.frame()

from <- c()
to <- c()
lags <- c()
distance <- c()
for (i in first_index:length(days)) {
    cday <- days[i]
    ## First filter out the ones corresponding to this day
    current_d <- filter(crime_d, days == cday)
    lag_d <- filter(crime_d, days < cday, days > cday-max_t)

    ## Lag is uniform for all day/

    current_d$distance <- rep(0, nrow(current_d))
    for (j in 1:nrow(current_d)) {
        lag_d$distance <- sqrt(((current_d$UTM_X[j] - lag_d$UTM_X)**2) + ((current_d$UTM_Y[j] - lag_d$UTM_Y)**2))/1000

        close_lag_d <- filter(lag_d, distance <= max_d)
        time_lag <- cday - close_lag_d$days


        from <- append(from, close_lag_d$id)
        to <- append(to, rep(current_d$id[j], nrow(close_lag_d)))
        lags <- append(lags, time_lag)
        distance <- append(distance, close_lag_d$distance)
        #current_d$distance[j] <- paste(distance[distance <= max_d],collapse=",")
    }

}
    out_df <- data.frame(from = from,
                         to = to,
                         distance = distance,
                         lag = lags)

## Write out pairs
write_csv(out_df, opts$p)
names(crime_d) <- toupper(names(crime_d))

## Write out points
write_csv(crime_d, opts$o)
