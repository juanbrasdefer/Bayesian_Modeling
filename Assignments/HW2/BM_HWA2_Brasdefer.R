
#### HOMEWORK ASSIGNMENT 2 ====================================================================
# BAYESIAN MODELING
# Homework Assignment 2
# Instructor: Sascha Göbel
# March 2025


# In this assignment, you will work with an extended version of 
# the sentencing data from session 6 and 7. 
# The dataset now includes information on sentences by 118 judges over 5 years 
# (sentencing_data2). You are going to rely on this dataset to investigate 
# the probability of incarceration/percent incarcerated across judges and years 
# using a Bayesian hierarchical regression model. 




# script prep --------------------------------------------------------------------------

# packages
pacman::p_load(here,
               tidyverse,
               ggplot2,
               cmdstanr,
               haven,
               vroom,
               install = TRUE,
               update = FALSE)

# working directory
here::i_am("Assignments/HW2/BM_HWA2_Brasdefer.R")
#setwd(here("Assignments/HW2/BM_HWA2_Brasdefer.R")) 
source(here("Lectures/Code-20250329/functions.r")) 








# TASK 1, NESTED OR NON-NESTED? --------------------------------------------------------------------------------

# Your outcome variable of interest yi is a judge’s sentence (sentence), which 
# takes the value 1 if a defendant was incarcerated and the value 0 if 
# a defendant was put on probation. 

# Given this outcome, you want to specify a varying-intercepts model 
# with intercepts varying across 
# (1) judges (j_id) and 
# (2) years (year). 
# Additionally, you want to add varying intercepts that capture 
# (3) the judge-year-specific probability of incarceration/percentage incarcerated. 
# Write this model down formally, fully specifying the likelihood and all priors. 

# Is this a nested or non-nested model? 
# Explain your decision. Upload the answers to this task in a .pdf file.




# this is a NON NESTED model, because clearly there is no strict hierarchy across levels
# although each sentence only occured in one year and was given by one judge
# judges give sentences in many different years
# meaning that we have CROSSING hierarchies (?)
# and thus our data is Non-Nested.








# TASK 2, IMPORT AND PROCESS --------------------------------------------------------------------------------

# In R, import the sentencing_data2 dataset. 
# Process the dataset in such a way that you can use if for 
# full Bayesian inference with Stan and assemble the data required 
# to fit the varying-intercepts model specified in Task 1. 

# How many varying intercept parameters are we estimating in total?


# import data 
sentencing_data2 <- readRDS(here("Assignments/HW2/sentencing_data2"))

sentencing_data2_processed <- sentencing_data2 %>%
  select()

# TASK 3, COMPILE AND SAMPLE --------------------------------------------------------------------------------

# In R, compile the data in the appropriate format for passing it to cmdstanr’s sample method 
# and specify the full Stan program that corresponds to your varying-intercepts model and
# the data you prepared. 

# Next, store the Stan file on your computer and compile your Stan program using the cmdstanr R package. 
# Given the Stan program and data you prepared, sample from the posterior. 
# Estimates should be directly stored on your computer. 
# Do not use convenience R packages, such as brms or rstanarm.


# prepare data for stan
data_stan <- list(
  N = nrow(sentencing_data),                      # Number of observations
  J = length(unique(sentencing_data$j_id)),       # Number of judges
  nT = length(unique(sentencing_data$year)),      # Number of years (renamed from T to nT)
  jj = as.integer(factor(sentencing_data$j_id)),  # Judge ID (converted to integer)
  tt = as.integer(factor(sentencing_data$year)),  # Year ID (converted to integer)
  y = sentencing_data$sentence,                   # Outcome - sentence (0 or 1)
  x = sentencing_data$d_black                     # Predictor - d_black
)




varying_intercepts_nn_model <- "
data {
  int<lower=1> N;                     // Number of observations
  int<lower=1> J;                     // Number of judges
  int<lower=1> nT;                    // Number of years (renamed from T to nT)
  array[N] int<lower=1, upper=J> jj;  // Judge ID
  array[N] int<lower=1, upper=nT> tt; // Year ID
  array[N] int<lower=0, upper=1> y;   // Outcome - sentence
  vector[N] x;                        // Predictor - d_black
}

parameters {
  real mu_alpha;
  real<lower=0> sigma_alpha;
  vector[J] alpha;    // Judge-specific intercepts

  real mu_gamma;
  real<lower=0> sigma_gamma;
  vector[nT] gamma;   // Year-specific intercepts

  real mu_delta;
  real<lower=0> sigma_delta;
  matrix[J, nT] delta; // Judge-year-specific intercepts

  real beta;          // Effect of predictor x (d_black)
}

transformed parameters {
  vector[N] pi;
  for (i in 1:N) {
    pi[i] = alpha[jj[i]] + gamma[tt[i]] + delta[jj[i], tt[i]] + beta * x[i];
  }
}

model {
  // Priors
  mu_alpha ~ student_t(3, 0, 1);
  sigma_alpha ~ student_t(3, 0, 1);
  alpha ~ normal(mu_alpha, sigma_alpha);
  
  mu_gamma ~ student_t(3, 0, 1);
  sigma_gamma ~ student_t(3, 0, 1);
  gamma ~ normal(mu_gamma, sigma_gamma);
  
  mu_delta ~ student_t(3, 0, 1);
  sigma_delta ~ student_t(3, 0, 1);
  to_vector(delta) ~ normal(mu_delta, sigma_delta);
  
  beta ~ normal(0, 1); // Prior for beta
  
  // Likelihood
  y ~ bernoulli_logit(pi);
}
"


# save as .stan file
varying_intercepts_nn_model %>%
  write(here("Assignments/HW2/varying_intercepts_nn_model.stan"))


# compile model
varying_intercepts_nn_model <- cmdstan_model(here("Assignments/HW2/varying_intercepts_nn_model.stan"))



# sample from the posterior
fit <- varying_intercepts_nn_model$sample(
  data = data_stan,
  parallel_chains = 4,
  chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  refresh = 50,
  output_dir = here("Assignments/HW2"),
  output_basename = "varying_intercepts_nn_model"
)



# TASK 4, COMPUTE P(INCARCERATION) FOR JUDGE-YEAR -----------------------------------------

# In R, choose 5 judges from the data for which we have observations 
# on all five years. Using the posterior estimates for varying intercept parameters, 
# compute the probability of 
# incarceration/
# percent incarcerated 
# for each of these 5 judges in every year. 
# Visualize your posterior predictions.



# OPTIONAL BONUS TASK -----------------------------------------------------------------

# Alter your Stan program to incorporate additional intercepts 
# varying across courts (c_id).



