#!/usr/bin/Rscript
require(docopt)

'Usage:
   adjacency.R [-d <data> -o <geo>]

Options:
   -d Input raster [default: cache/spatial/phl_raster.grd]
   -o Output adjacency list [default: cache/spatial/raster_adjacency.csv]
' -> doc

opts <- docopt(doc)
require(raster)
require(dplyr)
require(pipeR)
r <- raster(opts$d)

## Get adjacency of the whole raster
full_A <- adjacent(r, 1:length(r), directions = 8)


## Now drop anything with ans( NA value


from_ids <- values(r)[full_A[,1]]
to_ids <- values(r)[full_A[,2]]

data.frame(from = from_ids,
           to = to_ids) %>>%
filter(!is.na(from), !is.na(to)) -> adjacency

write.csv(adjacency, opts$o, row.names=FALSE)
             
