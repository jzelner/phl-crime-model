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
  matrix[A,A] dmat;
}

transformed data {

  vector[A] area_mu;
  area_mu <- rep_vector(0, A);
}
parameters {

  #################################
  ## DECLARATIONS

  ## Model parameters
  vector[A] alpha;           # area-specific random effect (CAR)
  vector[T] log_intensity;   # weekly average log-intensity 

  # Seasonal parameters
  real week_alpha;           # intercept of AR1 process
  real week_beta;            # strength of week-to-week correlation for AR1
  real<lower=0> week_sigma;  # variance of random walk for AR1
  
  ## GP
  real<lower=0> eta_sq;
  real<lower=0> rho_sq;
  real<lower=0> sigma_sq;

}
transformed parameters {

  ################################
  ## DECLARATIONS
  vector[A] alpha_stz;    # Area-specific means constrained to sum to zero

  ################################
  ## TRANSFORMATION
  alpha_stz <- alpha-mean(alpha);
}

model {
  ################################
  ## DECLARATIONS

  real sq_d;  
  ## Covariance matrices for GP components
  matrix[A,A] Sigma; //Spatially structured covariance

  ## Sample AR1 process for weekly intercepts
  for (t in 2:T) {
    log_intensity[t] ~ normal(week_alpha + week_beta*log_intensity[t-1], week_sigma); 
  }

  # Sample GP hyperparameters
  eta_sq ~ cauchy(0,5);
  rho_sq ~ cauchy(0,5);
  sigma_sq ~ cauchy(0,5);


  ########################################
  ## Construct the covariance matrix for the spatial GP
  for (i in 1:(A-1)) {
    for (j in (i+1):A) {
      sq_d <- pow(dmat[i,j],2);
        ## Covariance function for spatial LTBI intercepts
        Sigma[i,j] <- eta_sq*exp(-rho_sq*sq_d);
        Sigma[j,i] <- Sigma[i,j];
    }
  }


  for (i in 1:A) {
      Sigma[i,i] <- eta_sq + sigma_sq;
  }

alpha ~ multi_normal(area_mu, Sigma);

  ## Sample individual event times

  ## First, log-likelihood for observed events
 for (i in 1:N) {
   increment_log_prob(log_intensity[month[i]]+alpha_stz[area[i]]);
 }

## Now log-likelihood contribution for non-infection,
## which is just the sum of weekly contributions multiplied
## by the sum of the area-level random effects
increment_log_prob(-(sum(exp(log_intensity))*sum(exp(alpha_stz))));
}








