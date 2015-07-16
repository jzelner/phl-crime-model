#!/usr/bin/Rscript

## Author:   Jon Zelner (adapted from code by Kyle Foreman)
##           jlz2115@columbia.edu
## Date:     14 Jul 2014
require(docopt)

'Usage:
   ppm.R [-d <data> -a <adjacency> -m <model>]

Options:
   -a Neighborhood adjacency [default: cache/spatial/raster_adjacency.csv]
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

##set_cppo('fast');

## Load up the crime data and filter out the assaults
d <- read_csv(opts$d)


## Load up the adjacency list
a <- read_csv(opts$a)

## Convert into an adjacency matrix
A <- matrix(0, max(a), max(a))
for (r in 1:nrow(a)) {
    i <- a$from[r]
    j <- a$to[r]
    A[i,j] <- 1
    A[j,i] <- 1
}

## N = number of observations
## O = observed cases
## A = adjacency matrix
## D = diagonal matrix
D <- diag(rowSums(A))

## create sparse versions of A & D
## D is just the diagonal itself
D_sparse <- diag(D)
## A is two vectors corresponding to the non-zero pairs
A_sparse <- which(A == 1, arr.ind=TRUE)
A_sparse <- A_sparse[A_sparse[,1] < A_sparse[,2],]  # remove duplicates (because matrix is symmetric)
A_N <- dim(A_sparse)[1]

A1 <- A_sparse[,1]
A2 <- A_sparse[,2]

## Number of areas
num_a <- max(a)

## so, about that determinant...
## the multivariate normal likelihood requires the determinant of Tau (Tau = D - p * A)
## it's quite slow to calculate
## there are some sparse matrix algorithms that could work, but they're a bit cumbersome
## since the determinant is a relatively small contributor to the likelihood we can live with an approximation
## so we'll just sample it at some values and then interpolate the rest
## determinant(Tau) is a function of just p since D & A are constants
## and p is constrained to [0,1], so we can just sample at 1000 values of p
p_samples <- 1e3

## Load input into data
data_in <- list(A=num_a,
                T = max(d$TOTAL_MONTHS),
                N = nrow(d),
                month = d$TOTAL_MONTHS,
                area = d$RASTER_CELL,
                A_N=A_N,
                A1=A1,
                A2=A2,
                D_sparse=D_sparse,
                p_samples=p_samples)


init <- list(
    list(
        log_intensity = rep(0, data_in$T),
        beta1 = rep(0, data_in$A),
        tau = 1.0,
        p = 0.99,
        week_alpha = 0.0,
        week_beta = 1.0,
        week_sigma = 1.0)
    )



m <- stan(
    opts$m,
    data = data_in,
##    init = init,
    chains = 1,
    iter = 1000
)


