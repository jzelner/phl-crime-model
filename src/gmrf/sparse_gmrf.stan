data {
  int N;          # number of regions
  int O[N];       # observed cases
  int A_N;        # number of adjacent region pairs
  int A1[A_N];    # first half of adjacency pairs
  int A2[A_N];    # second half of adjacency pairs
  vector[N] D_sparse; # diagonal (sum_cij)
  int p_samples;  # number of times to sample determinant
}

transformed data {
  # calculate log(determinant(D - p * A)) at sampled values
  vector[p_samples + 1] ldet_DpA;
  {
    matrix [N,N] DpA;   # D - p * A (e.g. Tau / tau)
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
  real alpha0;            # intercept
  vector[N] beta1;        # area-specific random effect (CAR)
  real<lower=1e-5> tau;   # precision of CAR
  real<lower=0,upper=1> p;# strength of spatial correlation
}

transformed parameters {
  vector[N] beta_stz <- beta1-mean(beta1);
}

model {
  ## tmp variables for MVN likelihood
  row_vector[N] beta1t_D; # beta1' * D
  row_vector[N] beta1t_A; # beta1' * A
  real sigma_sq;

  # other tmp variables
  vector[N] beta1_stz;    # beta1 with sum-to-zero constraint
  vector[N] log_mu;       # prediction of log(mu)

  # priors on model parameters
  # no explicit prior on alpha0 (improper flat prior)

  tau ~ gamma(0.5, 0.0005);

  # CAR model using sparse MVN
  # (beta1' * Tau * beta1) can be calculated as (beta1' * D * beta1) - p * (beta1' * A * beta1)
  # and each of those can benefit from a sparse representation

  # sigma_sq
  sigma_sq <- 1 / tau;

  # find beta1' * D
  beta1t_D <- (beta1 .* D_sparse)';

  # find beta1' * A
  beta1t_A <- rep_vector(0.0, N)';    # initialize vector
  for (i in 1:A_N) {
    beta1t_A[A1[i]] <- beta1t_A[A1[i]] + beta1[A2[i]];
    beta1t_A[A2[i]] <- beta1t_A[A2[i]] + beta1[A1[i]];
  }

            # incorporate log probability of MVN(0.0, Tau)
                increment_log_prob(-0.5 / sigma_sq * (beta1t_D * beta1 - p * (beta1t_A * beta1)));
                increment_log_prob(-0.5 * N * log(sigma_sq));
                # hacky interpolated log(determinant(D - p * A))
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


  # calculate prediction
  log_mu <-  alpha0 + beta1_stz;

  # data likelihood
  O ~ poisson_log(log_mu);
    }
    
