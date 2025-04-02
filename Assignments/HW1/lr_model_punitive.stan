// LINEAR REGRESSION MODEL

// DATA BLOCK
data {
  int<lower=1> N;                 // Number of observations
  int<lower=1> K;                 // Number of covariates (predictors)
  matrix[N, K] X;                 // Predictors - respectability, age, sex, intercept
  vector<lower=0, upper=1>[N] y;  // Outcome - sagging_pants, bounded because %
}

// PARAMETERS BLOCK
parameters {
  vector[K] beta;             // coefficients for X; must be 'vector', not 'real'!!!!
  real<lower=0> sigma;        // bounding sigma to half-normal distribution
}                             // no alpha because our intercept is included in cov matrix

// MODEL BLOCK
model {
  // PRIORS: parameters distributed normal and lognormal, centered around 0 with stdev 1
  beta ~ normal(0, 1);  // a normal prior for the coefficients
  sigma ~ lognormal(0, 1);  // a lognormal prior for the st. dev.
  
  // LIKELIHOOD: 
  // y follows a normal dist, and miu is replaced by X * beta
  y ~ normal(X * beta, sigma);  // again: no alpha (intercept) bc it's included in X 
                                // also: X goes before beta
                                // sigma error term (st dev)
}

