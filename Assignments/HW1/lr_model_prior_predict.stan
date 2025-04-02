 
// data block
data {
  int<lower=1> N; 
  int<lower=1> K; 
  matrix[N, K] X; // only X
}

// parameters block
parameters {
  vector[K] beta;
  real<lower=0> sigma; 
}
  
// model block
model {
  // priors only, no likelihood for a prior predictive check
beta ~ normal(0,1); // X coefficients normal
sigma ~ lognormal(0,1); // st dev lognormal
}

// generated quantities block!
generated quantities {
  vector[N] y_prior; // prior predictive draws
  for (n in 1:N) { // for loop that runs model iterations
  y_prior[n] = normal_rng(X[n] * beta, sigma); 
  } 
}

