#!/usr/bin/Rscript
require(docopt)

'Usage:
   clean_crime_data.R [-d <data> -o <output>]

Options:
   -d Input data [default: data/crime/police_inct.csv]
   -o Output data [default: cache/crime/police_inct_clean.csv]
' -> doc

opts <- docopt(doc)

## Get path to file

require(pipeR)
require(dplyr)
require(stringr)
require(lubridate)


raw_d <- read.csv(opts$d)

## Convert dates to lubridate dates
raw_d$DISPATCH_DATE <- ymd(raw_d$DISPATCH_DATE)

## Select only dates from 1/1/2006 to 12/31/2014
raw_d %>>% filter(DISPATCH_DATE >= ymd("2006-01-01"),
                  DISPATCH_DATE <= ("2014-12-31")) -> f_d

## Clean up crime categories by stripping out excess writepace
f_d %>>%
(TEXT_GENERAL_CODE) %>>%
str_trim -> f_d$TEXT_GENERAL_CODE

## Make sure the output directory exists before writing to it
dir.create(dirname(opts$o), showWarnings = FALSE)

## Write file out
write.csv(f_d, opts$o, row.names=FALSE)
