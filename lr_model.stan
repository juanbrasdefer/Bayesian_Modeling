// LINEAR REGRESSION MODEL
data {
  int<lower=1> N;                 // Number of observations
  vector<lower=0, upper=1>[N] x;  // Predictor - majority
  vector<lower=0, upper=1>[N] y;  // Outcome - perchired
}

parameters {
  real alpha;
  real beta;
  real<lower=0> sigma;
}
// this <lower=0> sigma; is the way we achieve our half-normal dist for st.dev.

model {
  // priors: parameters distributed normal, centered around 0 with stdev 1
  alpha ~ normal(0, 1);
  beta ~ normal(0, 1);
  sigma ~ normal(0, 1);
  
  // likelihood: y follows a normal dist, and miu is replaced by alpha + beta * x
  y ~ normal(alpha + beta * x, sigma);
  
  // equivalent (unvectorized) form of the likelihood; 
  // this is not so important
  // stan is always looping
  // this is just an abstraction for understanding
  // for (n in 1:N) {
  //   y[n] ~ normal(alpha + beta * x[n], sigma);
  // }
}

