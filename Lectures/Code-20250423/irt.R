# ---------------------------------------------------------------------------------------
# BAYESIAN MODELING
# Session 10: Item Response models
# Instructor: Sascha Göbel
# April 2025
# ---------------------------------------------------------------------------------------


#### PREPARATIONS =======================================================================

# set working directory -----------------------------------------------------------------
setwd("YOUR WORKING DIRECTORY")

# install and load packages -------------------------------------------------------------
source("packages.R")
source("functions.R")


#### PREPARE DATA =======================================================================

# paper: Carter, Jeff and Charles E. Smith Jr. 2020. 'A Framework for Measuring Leaders’ 
#           Willingness to Use Force'. American Political Science Review 
#           https://doi.org/10.1017/S0003055420000313
# data source: https://doi.org/10.7910/DVN/7WFX1K

# import data ---------------------------------------------------------------------------
force_data <- readRDS("force_data") 


#### 1PL ITEM RESPONSE MODEL ------------------------------------------------------------

# format data for stan ------------------------------------------------------------------
force_data <- force_data %>%
  filter(year %in% 1933) %>%
  select(leadername, milservice, noncombat, combat, warwin, warloss, militarycareer,
         miledu, rebel, rebelwin, rebelloss, irregular) %>%
  pivot_longer(milservice:irregular, names_to = "indicator", values_to = "value") %>%
  mutate(value = as.integer(value),
         leader_id = as.integer(factor(leadername)),
         indicator_id = as.integer(factor(indicator))) %>%
  drop_na()

# prepare data for stan -----------------------------------------------------------------
data_stan <- list(N = nrow(force_data),
                  J = length(unique(force_data$leader_id)),
                  I = length(unique(force_data$indicator_id)),
                  jj = force_data$leader_id,
                  ii = force_data$indicator_id,
                  y = force_data$value)

# stan program --------------------------------------------------------------------------
onepl_irt_model <- "// 1PL IRT MODEL
data {
  int<lower=1> N;                     // Number of observations
  int<lower=1> J;                     // Number of leaders
  int<lower=1> I;                     // Number of indicators
  array[N] int<lower=1, upper=J> jj;  // Leader ID for observation n
  array[N] int<lower=1, upper=I> ii;  // Indicator ID for observation n
  array[N] int<lower=0, upper=1> y;   // Value of indicator for observation n
}

parameters {
  vector[I] alpha;    // difficulty parameter
  vector[J] eta;      // latent trait/ability parameter
  real mu_alpha;
  real<lower=0> sigma_alpha;
  real<lower=0> sigma_eta;
}

model {
  // priors
  alpha ~ normal(mu_alpha, sigma_alpha);
  eta ~ normal(0, sigma_eta);
  sigma_alpha ~ student_t(3, 0, 1);
  sigma_eta ~ student_t(3, 0, 1);
  mu_alpha ~ student_t(3, 0, 1);
  
  // likelihood
  y ~ bernoulli_logit(eta[jj] - alpha[ii]);
}
"

# save as .stan file --------------------------------------------------------------------
write(onepl_irt_model, "onepl_irt_model.stan")

# compile model -------------------------------------------------------------------------
onepl_irt_model_compiled <- cmdstan_model("onepl_irt_model.stan")

# sample from the posterior -------------------------------------------------------------
fit <- onepl_irt_model_compiled$sample(
  data = data_stan,
  parallel_chains = 4,
  chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  refresh = 50,
  output_dir = "YOUR OUTPUT DIRECTORY",
  output_basename = "onepl_irt_model_compiled"
)

# import and inspect posterior ----------------------------------------------------------
posterior_files <- c("onepl_irt_model_compiled-1.csv", "onepl_irt_model_compiled-2.csv", 
                     "onepl_irt_model_compiled-3.csv", "onepl_irt_model_compiled-4.csv")

alpha_estimates <- posterior_files %>%
  purrr::map_dfr(importCmdstanPosterior, 
                 parameter = "alpha") 

alpha_estimates %>% colMeans()

eta_estimates <- posterior_files %>%
  purrr::map_dfr(importCmdstanPosterior, 
                 parameter = "eta") 

eta_estimates[1,] %>% unlist() %>% hist()
eta_estimates %>% colMeans() %>% hist()


#### 2PL ITEM RESPONSE MODEL ------------------------------------------------------------

# stan program --------------------------------------------------------------------------
twopl_irt_model <- "// 2PL IRT MODEL
data {
  int<lower=1> N;                     // Number of observations
  int<lower=1> J;                     // Number of leaders
  int<lower=1> I;                     // Number of indicators
  array[N] int<lower=1, upper=J> jj;  // Leader ID for observation n
  array[N] int<lower=1, upper=I> ii;  // Indicator ID for observation n
  array[N] int<lower=0, upper=1> y;   // Value of indicator for observation n
}

parameters {
  vector[I] alpha;    // difficulty parameter
  vector<lower=0>[I] beta;     // discrimination parameter
  vector[J] eta;      // latent trait/ability parameter
  real mu_alpha;
  real<lower=0> sigma_alpha;
  real<lower=0> sigma_beta;
}

model {
  // priors
  alpha ~ normal(mu_alpha, sigma_alpha);
  beta ~ lognormal(0, sigma_beta);
  eta ~ normal(0, 1);
  sigma_alpha ~ student_t(3, 0, 1);
  sigma_beta ~ student_t(3, 0, 1);
  mu_alpha ~ student_t(3, 0, 1);
  
  // likelihood
  y ~ bernoulli_logit(beta[ii] .* (eta[jj] - alpha[ii]));
}
"

# save as .stan file --------------------------------------------------------------------
write(twopl_irt_model, "twopl_irt_model.stan")

# compile model -------------------------------------------------------------------------
twopl_irt_model_compiled <- cmdstan_model("twopl_irt_model.stan")

# sample from the posterior -------------------------------------------------------------
fit <- twopl_irt_model_compiled$sample(
  data = data_stan,
  parallel_chains = 4,
  chains = 4,
  iter_warmup = 2000,
  iter_sampling = 1000,
  refresh = 50,
  output_dir = "YOUR OUTPUT DIRECTORY",
  output_basename = "twopl_irt_model_compiled"
)

# import and inspect posterior ----------------------------------------------------------
posterior_files <- c("twopl_irt_model_compiled-1.csv", "twopl_irt_model_compiled-2.csv", 
                     "twopl_irt_model_compiled-3.csv", "twopl_irt_model_compiled-4.csv")

alpha_estimates <- posterior_files %>%
  purrr::map_dfr(importCmdstanPosterior, 
                 parameter = "alpha") 

alpha_estimates %>% colMeans()

beta_estimates <- posterior_files %>%
  purrr::map_dfr(importCmdstanPosterior, 
                 parameter = "beta") 

beta_estimates %>% colMeans()

eta_estimates <- posterior_files %>%
  purrr::map_dfr(importCmdstanPosterior, 
                 parameter = "eta") 

eta_estimates[1,] %>% unlist() %>% hist()
eta_estimates %>% colMeans() %>% hist()
