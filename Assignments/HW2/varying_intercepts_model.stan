
data {
  int<lower=1> N;                     // Number of observations
  int<lower=1> J;                     // Number of judges
  int<lower=1> nT;                    // Number of years (nT instead of T because R reads T as TRUE)
  int<lower=1> JT;                    // Number of judge year pairs
  array[N] int<lower=1, upper=J> jj;  // Judge ID
  array[N] int<lower=1, upper=nT> tt; // Year ID
  array[N] int<lower=1, upper=JT> jt; // Judge Year ID
  array[N] int<lower=0, upper=1> y;   // Outcome, sentence (0/1)
}

parameters {
  real mu_alpha;                      // Mean across judges (only judges get the mu!!)
  real<lower=0> sigma_alpha;          // Standard deviation across judges
  real<lower=0> sigma_gamma;          // Standard deviation across years
  real<lower=0> sigma_delta;          // Standard deviation across judge years

  vector[J] alpha;                    // Judge-level intercepts
  vector[nT] gamma;                   // Year-level intercepts
  vector[JT] delta;                   // Judge-Year-level intercepts
}

model {
  // Hyperpriors
  mu_alpha ~ student_t(3, 0, 1);                // only judges get mu
  sigma_alpha ~ student_t(3, 0, 1) T[0.01, ];   // our sigmas were creating errors
  sigma_gamma ~ student_t(3, 0, 1) T[0.01, ];   // 'Scale parameter is 0, but must be positive!'
  sigma_delta ~ student_t(3, 0, 1) T[0.01, ];   // so we add a tiny lower bound to stop that
  
  // Priors (for the actual intercepts)
  alpha ~ normal(mu_alpha, sigma_alpha); // distributed around its hyperparameters
  gamma ~ normal(0, sigma_gamma);        // distributed around 0 and its st dev
  delta ~ normal(0, sigma_delta);        // distributed around 0 and its st dev
  
  // Likelihood
  for (n in 1:N)
  y[n] ~ bernoulli_logit(alpha[jj[n]] + gamma[tt[n]] + delta[jt[n]]);
}

