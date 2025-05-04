# ---------------------------------------------------------------------------------------
# BAYESIAN MODELING
# Session 11: Quality checks
# Instructor: Sascha Göbel
# April 2024
# ---------------------------------------------------------------------------------------


#### PREPARATIONS =======================================================================

# set working directory -----------------------------------------------------------------
setwd("YOUR WORKING DIRECTORY")

# install and load packages -------------------------------------------------------------
source("packages.R")
source("functions.R")


#### PREPARE DATA =======================================================================

# paper: Harris, Allison P. 2023. 'Can Racial Diversity among Judges Affect Sentencing 
#           Outcomes?'. American Political Science Review.
#           https://doi.org/10.1017/S0003055423000552
# data source: https://doi.org/10.7910/DVN/YO5J2S


#### MONITORING PROGRESS ================================================================

# simple model --------------------------------------------------------------------------
sentencing_data <- readRDS("sentencing_data")
sentencing_data <- sentencing_data %>%
  mutate(sentence = as.integer(sentence),
         j_id = as.integer(as.factor(j_id))) %>%
  arrange(j_id)
data_stan <- list(N = nrow(sentencing_data),
                  J = length(unique(sentencing_data$j_id)),
                  jj = sentencing_data$j_id,
                  y = sentencing_data$sentence)

simple_model <- "// SIMPLE MODEL
data {
  int<lower=1> N;                     // Number of observations
  int<lower=1> J;                     // Number of judges
  array[N] int<lower=1, upper=J> jj;  // Judge ID
  array[N] int<lower=0, upper=1> y;   // Outcome - sentence
}

parameters {
  real mu_alpha;
  real<lower=0> sigma_alpha;
  vector[J] alpha;
}

model {
  // priors
  mu_alpha ~ cauchy(0, 1);
  sigma_alpha ~ cauchy(0, 1);
  alpha ~ normal(mu_alpha, sigma_alpha);
  
  // likelihood
  y ~ bernoulli_logit(alpha[jj]);
}

generated quantities {
// THIS IS HOW WE IMPLEMENT THE POSTERIOR PREDICTIVE DISTRIBUTION

  // Posterior predictive distribution
  array[N] int y_sim = bernoulli_logit_rng(alpha[jj]);
        // WE DECLARE THIS NEW VARIABLE, YSIM
        // THEN WE USE BERNOULLI (WHICH WAS THE DISTRIBUTION USED IN OUR LIKELIHOOD)
        // AND KIND OF JUST COPY OUR ORIGINAL LIKELIHOOD
    // NICE THING ABOUT STAN IS WE CAN DO IT ALL INSIDE THE STAN TXT, WE DONT NEED TO DO THIS LATER ON
    // THISIS 'WITHIN-SAMPLE' PREDICTION, SINCE WE ARE USING THE SAME X
    // USING THE SAME COVARIATES (X)
        // GIVES US WITHIN-SAMPLE POSTERIOR DISTRIBUTION, WHICH TELLS US MODEL FIT
      // SO IF WE INCLUDED NEW X VARIABLES (LIKE NEW DATA RESPONDENTS OR NEW JUDGES)
      // THEN WE WOULD BE ESTIMATING NEW THINGS
      // WHICH WOULD TELL US GENERALIZABILITY; HELPS US UNDERSTAND HOW IT PERFORMS IN THE REAL WORLD
}
"

write(simple_model, "simple_model.stan")
simple_model_compiled <- cmdstan_model("simple_model.stan")
fit <- simple_model_compiled$sample(
  data = data_stan,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  refresh = 50,
  output_dir = "./",
  output_basename = "simple_model",
  save_warmup = FALSE # SINCE WE WANT TO MONITOR PROGRESS, WE WANT TO SAVE WARMUP NOW
                          # BECAUSE WARMUP IS THE INTERESTING BIT
                          # WARMUP BEING OVER MEANS THE MODEL IS FIXED
)

# open monitor.R and execute corresponding part of the script

# complex model -------------------------------------------------------------------------
sentencing_data <- readRDS("sentencing_data")
sentencing_data <- sentencing_data %>%
  mutate(sentence = as.integer(sentence),
         d_black = as.integer(d_black),
         j_id = as.integer(as.factor(j_id)),
         c_id = as.integer(as.factor(c_id))) %>%
  arrange(j_id)
sentence_level_data <- sentencing_data %>%
  select(sentence, j_id, d_black) %>%
  arrange(j_id)
judge_level_data <- sentencing_data %>%
  distinct(j_id, .keep_all = TRUE) %>%
  select(j_id, j_black, c_id)
Z <- model.matrix(~ 1 + j_black, data = judge_level_data)
Z <- Z[,-1] %>% matrix()
data_stan <- list(N = nrow(sentence_level_data),
                  J = length(unique(sentence_level_data$j_id)),
                  L = length(unique(judge_level_data$c_id)),
                  jj = sentence_level_data$j_id,
                  ll = judge_level_data$c_id,
                  M = ncol(Z),
                  Z = Z,
                  x = sentencing_data$d_black,
                  y = sentence_level_data$sentence)

complex_model <- "// COMPLEX MODEL
data {
  int<lower=1> N;                     // Number of observations
  int<lower=1> J;                     // Number of judges
  int<lower=1> L;                     // Number of courts
  int<lower=1> M;                     // Number of group-level covariates
  array[N] int<lower=1, upper=J> jj;  // Judge ID
  array[J] int<lower=1, upper=L> ll;  // Court ID
  vector<lower=0, upper=1>[N] x;      // Predictor - d_black
  matrix[J, M] Z;                     // Group-level covariate matrix
  array[N] int<lower=0, upper=1> y;   // Outcome - sentence
}

parameters {
  vector[J] alpha;      // varying intercept for judges
  real beta;            // coefficient for d_black
  vector[M] gamma;      // coefficient vector for group-level predictors
  vector[L] zeta;       // varying intercept for courts
  real mu_zeta;      // average across courts/overall average
  real<lower=0> sigma_alpha;   // between-judge variation
  real<lower=0> sigma_zeta;   // between-court variation
}

model {
  // priors
  mu_zeta ~ cauchy(0, 1);
  sigma_zeta ~ student_t(3, 0, 1);
  zeta ~ student_t(3, mu_zeta, sigma_zeta);
  gamma ~ student_t(3, 0, 1);
  alpha ~ student_t(3, zeta[ll] + Z*gamma, sigma_alpha);
  beta ~ normal(0, 1);
  
  // likelihood
  y ~ bernoulli_logit(alpha[jj] + beta * x);
}

generated quantities {
  // Posterior predictive distribution
  array[N] int y_sim = bernoulli_logit_rng(alpha[jj] + beta * x);
}
"

write(complex_model, "complex_model.stan")
complex_model_compiled <- cmdstan_model("complex_model.stan")
fit <- complex_model_compiled$sample(
  data = data_stan,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 500,
  iter_sampling = 500,
  refresh = 50,
  output_dir = "YOUR OUTPUT DIRECTORY",
  output_basename = "complex_model",
  save_warmup = TRUE
)

# open monitor.R and execute corresponding part of the script

# broken model --------------------------------------------------------------------------
broken_model <- "// BROKEN MODEL
data {
  int<lower=1> N;                     // Number of observations
  int<lower=1> J;                     // Number of judges
  int<lower=1> L;                     // Number of courts
  int<lower=1> M;                     // Number of group-level covariates
  array[N] int<lower=1, upper=J> jj;  // Judge ID
  array[J] int<lower=1, upper=L> ll;  // Court ID
  vector<lower=0, upper=1>[N] x;      // Predictor - d_black
  matrix[J, M] Z;                     // Group-level covariate matrix
  array[N] int<lower=0, upper=1> y;   // Outcome - sentence
}

parameters {
  vector[J] alpha;      // varying intercept for judges
  real beta;            // coefficient for d_black
  vector[M] gamma;      // coefficient vector for group-level predictors
  vector[L] zeta;       // varying intercept for courts
  real mu;      // average across courts/overall average
  real mu_zeta;      // average across courts/overall average
  real<lower=0> sigma_alpha;   // between-judge variation
  real<lower=0> sigma_zeta;   // between-court variation
}

model {
  // priors
  mu ~ cauchy(0, 1);
  mu_zeta ~ cauchy(0, 1);
  sigma_zeta ~ student_t(3, 0, 1);
  zeta ~ student_t(3, mu_zeta, sigma_zeta);
  gamma ~ student_t(3, 0, 1);
  alpha ~ student_t(3, zeta[ll] + Z*gamma, sigma_alpha);
  beta ~ normal(0, 1);
  
  // likelihood
  y ~ bernoulli_logit(mu + alpha[jj] + beta * x);
}

generated quantities {
  // Posterior predictive distribution
  array[N] int y_sim = bernoulli_logit_rng(mu + alpha[jj] + beta * x);
}
"

write(broken_model, "broken_model.stan")
broken_model_compiled <- cmdstan_model("broken_model.stan")
fit <- broken_model_compiled$sample(
  data = data_stan,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 500,
  iter_sampling = 500,
  refresh = 50,
  output_dir = "YOUR OUTPUT DIRECTORY",
  output_basename = "broken_model",
  save_warmup = TRUE
)

# open monitor.R and execute corresponding part of the script


#### CONVERGE DIAGNOSTICS ===============================================================

# simple model --------------------------------------------------------------------------
posterior_files <- c("simple_model-1.csv", "simple_model-2.csv", 
                     "simple_model-3.csv", "simple_model-4.csv")
posterior <- posterior_files %>%
  purrr::map_dfr(importCmdstanPosterior, 
                 parameters = c("mu_alpha", "sigma_alpha", "alpha"), ) %>%
  as.matrix()
par_diagnostics <- data.frame(par = NA, rhat = NA, ess_bulk = NA, ess_tail = NA)
draws_per_chain <- 500

for (k in 1:ncol(posterior)) {
  cat(k, "\n")
  posterior_k <- cbind(posterior[1:draws_per_chain,k],
                       posterior[(draws_per_chain+1):(draws_per_chain*2),k],
                       posterior[(draws_per_chain*2+1):(draws_per_chain*3),k],
                       posterior[(draws_per_chain*3+1):(draws_per_chain*4),k])
  par_diagnostics$par <- colnames(posterior)[k]
  par_diagnostics$rhat <- posterior_k %>% posterior::rhat()
  par_diagnostics$ess_bulk <- posterior_k %>% posterior::ess_bulk()
  par_diagnostics$ess_tail <- posterior_k %>% posterior::ess_tail()
  if (k == 1) {
    par_diagnostics_all <- par_diagnostics
  } else {
    par_diagnostics_all <- rbind(par_diagnostics_all, par_diagnostics)
  }
}

par_diagnostics_all <- par_diagnostics_all %>%
  pivot_longer(-par, names_to = "diagnostic")

ggplot(data = par_diagnostics_all, aes(x = value)) +
  geom_histogram(bins = 70, color = "black", fill = "black") +
  facet_wrap(~diagnostic, scales = "free")

# complex model -------------------------------------------------------------------------
posterior_files <- c("complex_model-1.csv", "complex_model-2.csv", 
                     "complex_model-3.csv", "complex_model-4.csv")
posterior <- posterior_files %>%
  purrr::map_dfr(importCmdstanPosterior, 
                 parameters = c("mu_zeta", "sigma_alpha", "alpha", "beta", "gamma", 
                                "zeta", "sigma_zeta")) %>%
  as.matrix()
par_diagnostics <- data.frame(par = NA, rhat = NA, ess_bulk = NA, ess_tail = NA)
draws_per_chain <- 500

for (k in 1:ncol(posterior)) {
  cat(k, "\n")
  posterior_k <- cbind(posterior[1:draws_per_chain,k],
                       posterior[(draws_per_chain+1):(draws_per_chain*2),k],
                       posterior[(draws_per_chain*2+1):(draws_per_chain*3),k],
                       posterior[(draws_per_chain*3+1):(draws_per_chain*4),k])
  par_diagnostics$par <- colnames(posterior)[k]
  par_diagnostics$rhat <- posterior_k %>% posterior::rhat()
  par_diagnostics$ess_bulk <- posterior_k %>% posterior::ess_bulk()
  par_diagnostics$ess_tail <- posterior_k %>% posterior::ess_tail()
  if (k == 1) {
    par_diagnostics_all <- par_diagnostics
  } else {
    par_diagnostics_all <- rbind(par_diagnostics_all, par_diagnostics)
  }
}

par_diagnostics_all <- par_diagnostics_all %>%
  pivot_longer(-par, names_to = "diagnostic")

ggplot(data = par_diagnostics_all, aes(x = value)) +
  geom_histogram(bins = 70, color = "black", fill = "black") +
  facet_wrap(~diagnostic, scales = "free")

# broken model --------------------------------------------------------------------------
posterior_files <- c("broken_model-1.csv", "broken_model-2.csv", 
                     "broken_model-3.csv", "broken_model-4.csv")
posterior <- posterior_files %>%
  purrr::map_dfr(importCmdstanPosterior, 
                 parameters = c("mu_zeta", "sigma_alpha", "alpha", "beta", "gamma", 
                                "zeta", "sigma_zeta")) %>%
  as.matrix()
par_diagnostics <- data.frame(par = NA, rhat = NA, ess_bulk = NA, ess_tail = NA)
draws_per_chain <- 500

for (k in 1:ncol(posterior)) {
  cat(k, "\n")
  posterior_k <- cbind(posterior[1:draws_per_chain,k],
                       posterior[(draws_per_chain+1):(draws_per_chain*2),k],
                       posterior[(draws_per_chain*2+1):(draws_per_chain*3),k],
                       posterior[(draws_per_chain*3+1):(draws_per_chain*4),k])
  par_diagnostics$par <- colnames(posterior)[k]
  par_diagnostics$rhat <- posterior_k %>% posterior::rhat()
  par_diagnostics$ess_bulk <- posterior_k %>% posterior::ess_bulk()
  par_diagnostics$ess_tail <- posterior_k %>% posterior::ess_tail()
  if (k == 1) {
    par_diagnostics_all <- par_diagnostics
  } else {
    par_diagnostics_all <- rbind(par_diagnostics_all, par_diagnostics)
  }
}

par_diagnostics_all <- par_diagnostics_all %>%
  pivot_longer(-par, names_to = "diagnostic")

ggplot(data = par_diagnostics_all, aes(x = value)) +
  geom_histogram(bins = 70, color = "black", fill = "black") +
  facet_wrap(~diagnostic, scales = "free")

fit$cmdstan_diagnose()


#### POSTERIOR PREDICTIVE CHECKING ======================================================

# observed data -------------------------------------------------------------------------
y_obs <- sentencing_data$sentence
y_j_obs <- sentencing_data %>% 
  group_by(j_id) %>% 
  summarize(sentences = sum(sentence))

# random sample of simulated datasets ---------------------------------------------------
y_sample_idx <- sample(1:2000, 12)

# posterior predictive distribution for the simple model --------------------------------
posterior_files <- c("simple_model-1.csv", "simple_model-2.csv", 
                     "simple_model-3.csv", "simple_model-4.csv")
y_sim_sm <- posterior_files %>%
  purrr::map_dfr(importCmdstanPosterior, 
                 parameters = "y_sim") %>%
  t() %>%
  data.frame() %>%
  mutate(y_obs = y_obs,
         j_id = sentencing_data$j_id)

# comparing observed and predicted ------------------------------------------------------
y_sim_sm[,1:2000] %>% colMeans() %>% hist()
y_sim_sm$y_obs %>% mean()
f1 <- rep(NA, 2000)
for (i in 1:2000) {
  f1[i] <- table(y_sim_sm[,i], y_sim_sm$y_obs) %>%
    yardstick::f_meas() %>%
    pull(.estimate)
}
hist(f1)

y_j_sim_sm <- y_sim_sm %>%
  select(all_of(y_sample_idx), y_obs, j_id) %>%
  pivot_longer(-c(y_obs,j_id), names_to = "dataset", values_to = "y_sim") %>%
  group_by(dataset, j_id) %>%
  summarize(y_j_obs = mean(y_obs),
            y_j_sim = mean(y_sim), 
            .groups = "keep")

ggplot(data = y_j_sim_sm) +
  geom_histogram(aes(x = y_j_obs), fill = "red",alpha = 0.3) +
  geom_histogram(aes(x = y_j_sim), alpha = 0.3) +
  facet_wrap(~dataset)

# posterior predictive distribution for the complex model --------------------------------
posterior_files <- c("complex_model-1.csv", "complex_model-2.csv", 
                     "complex_model-3.csv", "complex_model-4.csv")
y_sim_cm <- posterior_files %>%
  purrr::map_dfr(importCmdstanPosterior, 
                 parameters = "y_sim") %>%
  t() %>%
  data.frame() %>%
  mutate(y_obs = y_obs,
         j_id = sentencing_data$j_id)

# comparing observed and predicted ------------------------------------------------------
y_sim_cm[,1:2000] %>% colMeans() %>% hist()
y_sim_cm$y_obs %>% mean()
f1 <- rep(NA, 2000)
for (i in 1:2000) {
  f1[i] <- table(y_sim_cm[,i], y_sim_cm$y_obs) %>%
    yardstick::f_meas() %>%
    pull(.estimate)
}
hist(f1)

y_j_sim_cm <- y_sim_cm %>%
  select(all_of(y_sample_idx), y_obs, j_id) %>%
  pivot_longer(-c(y_obs,j_id), names_to = "dataset", values_to = "y_sim") %>%
  group_by(dataset, j_id) %>%
  summarize(y_j_obs = mean(y_obs),
            y_j_sim = mean(y_sim), 
            .groups = "keep")

ggplot(data = y_j_sim_cm) +
  geom_histogram(aes(x = y_j_obs), fill = "red",alpha = 0.3) +
  geom_histogram(aes(x = y_j_sim), alpha = 0.3) +
  facet_wrap(~dataset)

# posterior predictive distribution for the complex model --------------------------------
posterior_files <- c("broken_model-1.csv", "broken_model-2.csv", 
                     "broken_model-3.csv", "broken_model-4.csv")
y_sim_bm <- posterior_files %>%
  purrr::map_dfr(importCmdstanPosterior, 
                 parameters = "y_sim") %>%
  t() %>%
  data.frame() %>%
  mutate(y_obs = y_obs,
         j_id = sentencing_data$j_id)

# comparing observed and predicted ------------------------------------------------------
y_sim_bm[,1:2000] %>% colMeans() %>% hist()
y_sim_bm$y_obs %>% mean()
f1 <- rep(NA, 2000)
for (i in 1:2000) {
  f1[i] <- table(y_sim_bm[,i], y_sim_bm$y_obs) %>%
    yardstick::f_meas() %>%
    pull(.estimate)
}
hist(f1)

y_j_sim_bm <- y_sim_bm %>%
  select(all_of(y_sample_idx), y_obs, j_id) %>%
  pivot_longer(-c(y_obs,j_id), names_to = "dataset", values_to = "y_sim") %>%
  group_by(dataset, j_id) %>%
  summarize(y_j_obs = mean(y_obs),
            y_j_sim = mean(y_sim), 
            .groups = "keep")

ggplot(data = y_j_sim_bm) +
  geom_histogram(aes(x = y_j_obs), fill = "red",alpha = 0.3) +
  geom_histogram(aes(x = y_j_sim), alpha = 0.3) +
  facet_wrap(~dataset)

