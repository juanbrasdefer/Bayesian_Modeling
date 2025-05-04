
// Item Response model, 2PL IRT (2-Parameter Logistic Item Response Model)
 data {
    int<lower=1> N;                       // total observations 
    int<lower=1> I;                       // number of elections 
    int<lower=1> J;                       // number of voters 
    array[N] int<lower=1,upper=I> ii;     // election index 
    array[N] int<lower=1,upper=J> jj;     // voter index 
    array[N] int<lower=0,upper=1> y;      // turnout yn 
 }
 parameters {                            
   vector[I] alpha;                 // alpha i, intercept (difficulty) parameter 
   vector<lower=0>[I] beta;         // beta i, slope (discrimination) parameter 
   vector[J] eta;                   // eta j, latent trait (ability) parameter
   real mu_alpha;               // hyperparmeters
   real<lower=0> sigma_alpha;
   real<lower=0> sigma_beta;
 }
 model {
   alpha       ~ normal(mu_alpha, sigma_alpha); // alpha hyperprior
   beta        ~ lognormal(0, sigma_beta);      // beta, constrained to be positive
   eta         ~ normal(0, 1);                  // eta, centered at 0 and with unit variance
   mu_alpha    ~ student_t(3, 0, 1);
   sigma_alpha ~ student_t(3, 0, 1);
   sigma_beta  ~ student_t(3, 0, 1);

   y ~ bernoulli_logit(beta[ii] .* (eta[jj] - alpha[ii])); // our 2PL IRT likelihood setup
                    // note: the period star (.*) is the element-wise multiplication operator in Stan
                    // it means we're multiplying each element of beta[ii] 
                    // with the corresponding element of (eta[jj] - alpha[ii]) element by element
                    // rather than doing matrix or dot-product multiplication.
 }
 
