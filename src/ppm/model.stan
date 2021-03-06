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
  int month[N];      # Month of year when event occurs
  int total_month[N]; # Month out of total observation period when event occurs
  vector[T] moy;

  ##################################
  ## AREA DATA
  matrix[A,A] dmat;
}

transformed data {
  int M; # Number of months

  vector[A] area_mu;
  vector[T] month_mu;
  real ir;
  area_mu <- rep_vector(0, A);
  month_mu <- rep_vector(0, T);
  M <- 12;


}
parameters {

  #################################
  ## DECLARATIONS

  ## Model parameters
  vector[A] alpha;           # area-specific random effect (GP)
  vector[T] log_intensity;   # weekly average log-intensity
  // vector[11] month_beta_raw;

  # Seasonal parameters
  real month_alpha;           # intercept of AR1 process

  
  ## GP
  real<lower=0> eta_sq[2];
  real<lower=0> rho_sq[2]; 
  real<lower=0> sigma_sq[2]; # Noise terms only for the full vars

}
transformed parameters {

  ################################
  ## DECLARATIONS
  vector[A] alpha_stz;    # Area-specific means constrained to sum to zero
  vector[T] month_stz;
  vector[12] month_beta;

  ################################
  ## TRANSFORMATION
  alpha_stz <- alpha-mean(alpha);
  month_stz <- log_intensity-mean(log_intensity);
  // month_beta[1] <- 0;
  // for (i in 2:12) {
  //   month_beta[i] <- month_beta_raw[i-1];
  // }

}

model {
  ################################
  ## DECLARATIONS

  real sq_d;
  vector[T] total_intensity;
  ## Covariance matrices for GP components
  matrix[A,A] Sigma; # Spatially structured covariance
  matrix[T,T] Month_Sigma; # Covariance for month effects 


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
      Sigma[i,i] <- eta_sq[1] + sigma_sq[1];
  }

  ########################################
  ## Construct the covariance matrix for the spatial GP
  for (i in 1:(W-1)) {
    for (j in (i+1):W) {
      week_d <- abs(i-j);
      if (week_d > 26) {
        week_d <- fabs(week_d-53);
      }

  ########################################
  ## Covariance matrix for temporal GP
  ## Sample individual event times

  for (i in 1:(T-1)) {
    real sq_t; ## Squared difference in total months
    real m_d; ## Raw difference in months
    real sq_m; ## Squared difference in months
    for (j in (i+1):T) {
      sq_t <- square(i-j);
      m_d <- fabs(moy[i]-moy[j]);
      if (m_d > 6) {
        m_d <- fabs(m_d-12);
      }

      sq_m <- square(m_d);

      Month_Sigma[i,j] <- eta_sq[2]*exp(-rho_sq[2]*sq_t); ## Periodic annual component
      Month_Sigma[j,i] <- Month_Sigma[i,j];
    }
  }

for (i in 1:T) {
  Month_Sigma[i,i] <- eta_sq[2] + sigma_sq[2];
 }

## Sample monthly intensity
log_intensity ~ multi_normal(month_mu, Month_Sigma);
  
  ## First, log-likelihood for observed events
 for (i in 1:N) {
   increment_log_prob(month_alpha + month_stz[total_month[i]]+alpha_stz[area[i]]);
 }

     for (i in 1:T) {
       total_intensity[i] <- month_alpha + month_stz[i];# + month_beta[month[i]];
     }
## Now log-likelihood contribution for non-infection,
## which is just the sum of weekly contributions multiplied
## by the sum of the area-level random effects
increment_log_prob(-(sum(exp(total_intensity))*sum(exp(alpha_stz))));

}








