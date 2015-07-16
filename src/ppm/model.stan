data {

  #####################################
  ## COUNTERS
  int A;          # number of regions
  int N;          # Number of cases
  int T;          # Maximum number of months (for overall effects)

  ####################################
  ## CASE DATA
  ##real t[N];      # Times of observed cases
  int area[N];    # Raster cell the case is in
  int month[N];    # Week event occurs

  ##################################
  ## AREA DATA
  int A_N;        # number of adjacent region pairs
  int A1[A_N];    # first half of adjacency pairs
  int A2[A_N];    # second half of adjacency pairs
  vector[A] D_sparse; # diagonal (sum_cij)
  int p_samples;  # number of times to sample determinant
}

transformed data {

  #################################
  ## DECLARATIONS

  ## Approximation to determinant for grid cells
  vector[p_samples + 1] ldet_DpA;

  #################################
  ## TRANSFORMATIONS

  ## Calculate log(determinant(D - p * A)) at sampled values
  {
    matrix [A,A] DpA;   # D - p * A (e.g. Tau / tau)
    DpA <- diag_matrix(D_sparse);
    # calculate the off diagonal of D - p * A for each value of p
    for (i in 1:(p_samples + 1)) {
                for (a in 1:A_N)
                    DpA[A1[a], A2[a]] <- -1.0 * ((i - 1.0) / p_samples);
                # store the sampled log(determinant(D - p * A))
                ldet_DpA[i] <- log_determinant(DpA);
            }
        }
}
parameters {

  #################################
  ## DECLARATIONS

  ## Model parameters
  vector[A] beta1;           # area-specific random effect (CAR)
##  vector[T] log_intensity;   # weekly average log-intensity 

  ## Seasonal parameters
  // real week_alpha;           # intercept of AR1 process
  // real week_beta;            # strength of week-to-week correlation for AR1
  // real<lower=0> week_sigma;  # variance of random walk for AR1
  
  ## CAR Parameters
  real<lower=1e-5> tau;      # precision of CAR
  real<lower=0,upper=1> p;   # strength of spatial correlation
}

transformed parameters {

  ################################
  ## DECLARATIONS
  vector[A] beta_stz;    # Area-specific means constrained to sum to zero

  ################################
  ## TRANSFORMATION
  beta_stz <- beta1-mean(beta1);
}

model {
  ################################
  ## DECLARATIONS

  ## tmp variables for MVN likelihood
  row_vector[A] beta1t_D; # beta1' * D
  row_vector[A] beta1t_A; # beta1' * A
  real sigma_sq;
  # priors on model parameters
  tau ~ gamma(0.5, 0.0005);

  ## CAR model using sparse MVN
  ## (beta1' * Tau * beta1) can be calculated as (beta1' * D * beta1) - p * (beta1' * A * beta1)
  ## and each of those can benefit from a sparse representation

  ## sigma_sq
  sigma_sq <- 1 / tau;

  ## find beta1' * D
  beta1t_D <- (beta1 .* D_sparse)';

  ## find beta1' * A
  beta1t_A <- rep_vector(0.0, A)';    # initialize vector
  for (i in 1:A_N) {
    beta1t_A[A1[i]] <- beta1t_A[A1[i]] + beta1[A2[i]];
    beta1t_A[A2[i]] <- beta1t_A[A2[i]] + beta1[A1[i]];
  }

            ## incorporate log probability of MVN(0.0, Tau)
                increment_log_prob(-0.5 / sigma_sq * (beta1t_D * beta1 - p * (beta1t_A * beta1)));
                increment_log_prob(-0.5 * N * log(sigma_sq));
                ## hacky interpolated log(determinant(D - p * A))
                {
                    int idx;
                    real ldet_left;
                    real ldet_right;
                    idx <- 0;
                    while (floor(p * p_samples) >= idx) {
                        idx <- idx + 1;
                    }
                    ldet_left <- ldet_DpA[idx];
                    ldet_right <- ldet_DpA[idx + 1];
                    increment_log_prob(0.5 * (ldet_left + (ldet_right - ldet_left) * (p * p_samples - floor(p * p_samples))));
                }


  // ## Sample AR1 process for weekly intercepts
  // for (t in 2:T) {
  //   log_intensity[t] ~ normal(week_alpha + week_beta*log_intensity[t-1], week_sigma); 
  // }


 ## Sample individual event times

 ## First, log-likelihood for observed events
 for (i in 1:N) {
   increment_log_prob(beta_stz[area[i]]);
 }


## Now log-likelihood contribution for non-infection,
## which is just the sum of weekly contributions multiplied
## by the sum of the area-level random effects
increment_log_prob(-(sum(exp(beta_stz)*108)));
}








