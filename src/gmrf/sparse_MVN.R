## Author:   Kyle Foreman
##           kforeman@post.harvard.edu
## Date:     27 Nov 2013
## Purpose:  demonstrate sparse MVN methods for STAN
## Notes:    see orig## model CAR using multi_normal_prec()

## libraries
require(packrat)
packrat::disable()
require(rstan)
require(readr)
require(raster)
require(pipeR)
require(dplyr)

set_cppo('fast');

## load raster and create input adjacency list
r <- raster("cache/spatial/phl_raster.grd")
raster_vals <- getValues(r)
raster_vals[!is.na(raster_vals)] <- 0

A <- matrix(0, length(r), length(r))
d <- read_csv("cache/crime/police_inct_clean.csv")
A_sparse <- adjacent(r, 1:length(r), directions=8)
for (i in 1:nrow(A_sparse)) {
    A[A_sparse[i,1],A_sparse[i,2]] <- 1
    A[A_sparse[i,2],A_sparse[i,1]] <- 1
}

A <- A[!is.na(raster_vals),!is.na(raster_vals)]

## Get number of cases in each original cell
d %>>%
dplyr::filter(TEXT_GENERAL_CODE == "Aggravated Assault Firearm") %>>%
group_by(RASTER_CELL) %>>%
summarize(N = length(TEXT_GENERAL_CODE)) -> cell_counts

orig_raster_vals <- raster_vals
raster_vals[cell_counts$RASTER_CELL] <- cell_counts$N
raster_vals <- raster_vals[!is.na(orig_raster_vals)]
## N = number of observations
## O = observed cases
## A = adjacency matrix
## D = diagonal matrix


O <- raster_vals
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

## so, about that determinant...
## the multivariate normal likelihood requires the determinant of Tau (Tau = D - p * A)
## it's quite slow to calculate
## there are some sparse matrix algorithms that could work, but they're a bit cumbersome
## since the determinant is a relatively small contributor to the likelihood we can live with an approximation
## so we'll just sample it at some values and then interpolate the rest
## determinant(Tau) is a function of just p since D & A are constants
## and p is constrained to [0,1], so we can just sample at 1000 values of p
p_samples <- 1e3
N <- length(O)
sparse_init <- list(
    list(
        alpha0 = 0.0,
        beta1 = rep(0, N),
        tau = 1.0,
        p = 0.99
    )
)

sparse_data <- list(N=N, O=O, A_N=A_N, A1=A1, A2=A2, D_sparse=D_sparse, p_samples=p_samples)

sparse_fit <- stan(
    "src/gmrf/sparse_gmrf.stan",
    data = sparse_data,
    init = sparse_init,
    chains = 1,
    iter = 1000
)

## compare outputs to ensure we're getting the same result
print(sparse_fit)

