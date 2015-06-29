#!/usr/bin/Rscript
require(docopt)

'Usage:
   generate_daily_crime_counts.R [-d <data> -o <output>]

Options:
   -d Input data [default: cache/crime/police_inct_clean.csv]
   -o Output data [default: cache/crime/daily_crime_counts.csv]
' -> doc

opts <- docopt(doc)

## Get path to file
require(pipeR)
require(dplyr)
require(lubridate)


## Open up the raw data
raw_d <- read.csv(opts$d)

## Get counts by crime type

raw_d %>>%
group_by(DISPATCH_DATE, TEXT_GENERAL_CODE) %>>%
summarize(N = length(TEXT_GENERAL_CODE)) -> day_counts


## Make a set of dummy counts by day so we're sure to
## have zeroes for days w/o any crimes of a given type

all_dates <- seq(from=ymd("2006-01-01"),
                 to=ymd("2014-12-31"), by = "days")

zero_frame <- data.frame(DISPATCH_DATE = all_dates,
                         N = 0)

crime_counts <- data.frame()
for (c in unique(day_counts$TEXT_GENERAL_CODE)) {
    day_counts %>>%
    filter(TEXT_GENERAL_CODE == c) %>>%
    select(DISPATCH_DATE, N) -> tmp_d

    tmp_d$DISPATCH_DATE <- ymd(tmp_d$DISPATCH_DATE)

    ## Gets dates in zero frame but not in crime
    ## specific frame
    missing_dates <- !(zero_frame$DISPATCH_DATE %in% tmp_d$DISPATCH_DATE)

    full_d <- rbind(tmp_d, zero_frame[missing_dates,])
    full_d$TEXT_GENERAL_CODE <- c

    crime_counts <- rbind(crime_counts, full_d)
}

## Add day/week/month fields
crime_counts$WEEK <- week(crime_counts$DISPATCH_DATE)
crime_counts$DAY <- day(crime_counts$DISPATCH_DATE)
crime_counts$YEAR <- year(crime_counts$DISPATCH_DATE)

## Save out daily crime counts
write.csv(crime_counts, opts$o, row.names=FALSE)
