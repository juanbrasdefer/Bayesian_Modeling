# ---------------------------------------------------------------------------------------
# BAYESIAN MODELING
# Session 5: Introducing Stan
# Instructor: Sascha Göbel
# March 2025
# ---------------------------------------------------------------------------------------

#### PREPARATIONS =======================================================================

# set working directory -----------------------------------------------------------------
setwd("YOUR WORKING DIRECTORY")
library(here)
here::i_am("Code-20250312/stan_intro.R")


# install and load packages -------------------------------------------------------------
source(here("Code-20250312/packages.R"))
source(here("Code-20250312/functions.R"))

# set seed ------------------------------------------------------------------------------
set.seed(2843)


#### PREPARE DATA =======================================================================
# paper: Hassan, Mai, Horacio Larreguy, and Stuart Russel. 2024. 'Who Gets Hired? 
#           Political Patronage and Bureaucratic Favoritism'. American Political Science
#           Review. https://doi.org/10.1017/S0003055423001338
# data source: https://doi.org/10.7910/DVN/MLZ39X

# import data ---------------------------------------------------------------------------
patronage_data <- read_dta(here("Code-20250312/localauthority_level.dta"))

# remove missings (no hires in a given year) --------------------------------------------
patronage_data <- patronage_data %>%
  filter(perchiredmiss != 1) # stan can't work with NA vals

# format data for stan ------------------------------------------------------------------
patronage_data <- patronage_data %>%
  mutate(perchired = as.numeric(perchired),
         majority = as.integer(majority))

# STEP 1 prepare data for stan -----------------------------------------------------------------
data_stan <- list(N = nrow(patronage_data), # our dataset was already with no factors, but we do anyway
                  y = patronage_data$perchired,
                  x = patronage_data$majority)
# note that here we are prepping in the nomenclature of Stan:
  # N is our number of rows
  # y is our dependent variable
  # x is our covariate
    # if we had many variables, we would transform the X data into a matrix
    # and keep it as one big X instead of x1, x2...

# so we have num obvs, dependent, independent


#### SPECIFY AND PROCESS STAN PROGRAM ===================================================

# STAN PROGRAM IS JUST A STRING
# SO WE WRITE OUR MODEL AS A STRING
# which some people do inside a .txt or .stan file that you import
# stan program --------------------------------------------------------------------------

# below, where we see the 
# vector<lower=0, upper=1>[N] x
# x is percentages
# we have a vector of values with minimum 0 and max 1, of length N

# vector<lower=0, upper=1>[N] y
# in the case of y, which is a binary variable
# we need to have it bounded this way because
# of the way that it gets multiplied through the model
# BUT I DONT UNDERSTAND IT BTW

lr_model <- "// LINEAR REGRESSION MODEL
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
"

# save as .stan file --------------------------------------------------------------------
write(lr_model, "lr_model.stan")

# compile model -------------------------------------------------------------------------
lr_model_compiled <- cmdstan_model("lr_model.stan")


#### RUN MCMC ===========================================================================

# now we have our compiled model, 
# and it has this attribute $sample that we can use
# so
# sample from the posterior -------------------------------------------------------------
fit <- lr_model_compiled$sample(
  data = data_stan,
  chains = 3, # self-explanatory
  iter_warmup = 500, # default 1000
  iter_sampling = 500, # default 1000
  refresh = 50, # how often you get an update of where the chains are
  output_dir = "YOUR OUTPUT DIRECTORY",
  output_basename = "lr_model"
)


# estimation has been done; now we inspect results
# basically a table with the classic regression diagnostics
# inspect results -----------------------------------------------------------------------
fit$summary() # is one way of looking at it
fit$cmdstan_summary() # can be a little nicer to look at
posterior <- fit$draws(format = "df") # extract the posterior; then you can format as a matrix or smthn
View(posterior)

# dont care too much about log probs
# we care about the alpha beta sigma columns
# which show us the parameter results of all the iterations (excluding warmups)
# you can take that and then create a histogram that shows you 
# confidence intervals simply just by using quantiles

# use custom function to obtain the posterior for beta from stored files ----------------
# aka just so we dont use it between session and closing R etc
posterior_files <- c("lr_model-1.csv", "lr_model-2.csv", "lr_model-3.csv")
beta_posterior <- posterior_files %>%
  purrr::map_dfr(importCmdstanPosterior, 
                 parameter = "beta")
^