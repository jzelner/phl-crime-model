data {

  #####################################
  ## COUNTERS
  int A;          # number of regions
  int N;          # Number of cases
  int T;          # Maximum number of months (for overall effects)
  int W;          # Number of weeks in year
  int TW;         # Total weeks
  

  ####################################
  ## CASE DATA
  ##real t[N];       # Times of observed cases
  int area[N];       # Raster cell the case is in
  int month[N];      # total month event occurs
  int week[N];       # week of year in which event occurs
  int week_month[TW]; # week of year in which each input month occurs
  int month_week[TW]; # mapping back from total weeks to total month
  
  ##################################
  ## AREA DATA
  matrix[A,A] dmat;
}

transformed data {

  vector[A] area_mu;
  vector[W] week_mu;
  area_mu <- rep_vector(0, A);
  week_mu <- rep_vector(0, W);
}
parameters {

  #################################
  ## DECLARATIONS

  ## Model parameters
  vector[A] alpha;           # area-specific random effect (CAR)
  vector[T-1] log_intensity;   # weekly average log-intensity
  vector[W] log_week_intensity;

  # Seasonal parameters
  real<lower=0> month_sigma;  # variance of random walk for AR1
  
  ## GP
  real<lower=0> eta_sq[2];
  real<lower=0> rho_sq[2];
  real<lower=0> sigma_sq[2];

}
transformed parameters {

  ################################
  ## DECLARATIONS
  vector[A] alpha_stz;    # Area-specific means constrained to sum to zero
  vector[T] log_intensity_stz;
  ################################
  ## TRANSFORMATION
  alpha_stz <- alpha-mean(alpha);
  log_intensity_stz[1] <- 0;
  for (i in 2:T) {
    log_intensity_stz[i] <- log_intensity[i-1];
  }
 
}

model {
  ################################
  ## DECLARATIONS

  real sq_d;
  real week_d;
  ## Covariance matrices for GP components
  matrix[A,A] Sigma; //Spatially structured covariance
  matrix[W,W] Week_Sigma;
  vector[TW] week_intensity;
  ## Sample AR1 process for weekly intercepts

  log_intensity ~ normal(0, month_sigma);

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
        Sigma[i,j] <- eta_sq[1]*exp(-rho_sq[1]*sq_d);
        Sigma[j,i] <- Sigma[i,j];
    }
  }


  for (i in 1:A) {
      Sigma[i,i] <- eta_sq[1]+ sigma_sq[1];
  }

  ########################################
  ## Construct the covariance matrix for the spatial GP
  for (i in 1:(W-1)) {
    for (j in (i+1):W) {
      week_d <- abs(i-j);
      if (week_d > 26) {
        week_d <- fabs(week_d-53);
      }

        ## Covariance function for spatial LTBI intercepts
        Week_Sigma[i,j] <- eta_sq[2]*exp(-rho_sq[2]*pow(week_d,2));
        Week_Sigma[j,i] <- Week_Sigma[i,j];
    }
  }


  for (i in 1:W) {
      Week_Sigma[i,i] <- eta_sq[2] + sigma_sq[2];
  }

  alpha ~ multi_normal(area_mu, Sigma);
  log_week_intensity ~ multi_normal(week_mu, Week_Sigma);
  ## Sample individual event times

 
  ## First, log-likelihood for observed events
 for (i in 1:N) {
   increment_log_prob(log_week_intensity[week[i]]+log_intensity_stz[month[i]]+alpha_stz[area[i]]);
 }
for (i in 1:TW) {
  week_intensity[i] <- exp(log_intensity_stz[month_week[i]]+log_week_intensity[week_month[i]]);
}
## Now log-likelihood contribution for non-infection,
## which is just the sum of weekly contributions multiplied
## by the sum of the area-level random effects
// print(exp(log_week_intensity))
// print(sum(week_intensity)*sum(exp(alpha_stz)))
//print(sum(week_intensity)*sum(exp(alpha_stz)))
increment_log_prob(-(sum(week_intensity)*sum(exp(alpha_stz))));
}








