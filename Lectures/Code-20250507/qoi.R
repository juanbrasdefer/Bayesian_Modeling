# ---------------------------------------------------------------------------------------
# BAYESIAN MODELING
# Session 12: Computing and visualizing quantities of interest
# Instructor: Sascha Göbel
# May 2024
# ---------------------------------------------------------------------------------------


#### PREPARATIONS =======================================================================

# set working directory -----------------------------------------------------------------
setwd("YOUR WORKING DIRECTORY")

# install and load packages -------------------------------------------------------------
source("packages.R")
source("functions.R")


#### PREPARE DATA =======================================================================

# format data -----------------------------------------------------------------------
sentencing_data <- readRDS("sentencing_data") %>%
  mutate(sentence = as.integer(sentence),
         d_age = factor(case_when(d_age >= 12 & d_age < 18 ~ "under_18",
                                  d_age >= 18 & d_age <= 29 ~ "18_29",
                                  d_age >= 30 & d_age <= 44 ~ "30_44",
                                  d_age >= 45 & d_age <= 64 ~ "45_64",
                                  d_age >= 65 ~ "65_plus"),
                        levels = c("under_18", "18_29", "30_44", "45_64", "65_plus")),
         d_black = factor(case_when(d_black == 0 ~ "no",
                                    d_black == 1 ~ "yes"),
                          levels = c("no", "yes")),
         d_female = factor(case_when(d_female == 0 ~ "no",
                                     d_female == 1 ~ "yes"),
                           levels = c("no", "yes")),
         d_prevcharge = factor(case_when(d_prevcharge == 0 ~ "no",
                                         d_prevcharge == 1 ~ "yes"),
                               levels = c("no", "yes")),
         j_id = as.integer(as.factor(j_id)),
         j_black = factor(case_when(j_black == 0 ~ "no",
                                    j_black == 1 ~ "yes"),
                          levels = c("no", "yes")))
j_log_cases <- sentencing_data %>% distinct(j_id, j_casepyr) %>%
  group_by(j_id) %>%
  summarize(j_log_cases = log(sum(j_casepyr)))
sentencing_data <- sentencing_data %>%
  left_join(j_log_cases, by = "j_id") %>%
  select(sentence, d_age, d_black, d_female, d_prevcharge, 
         j_id, j_black, j_log_cases) %>%
  arrange(j_id)

# stan data -----------------------------------------------------------------------------
sentence_level_data <- sentencing_data %>%
  select(sentence, d_age, d_black, d_female, d_prevcharge, j_id) %>%
  arrange(j_id)
judge_level_data <- sentencing_data %>%
  distinct(j_id, .keep_all = TRUE) %>%
  select(j_id, j_black, j_log_cases) %>%
  arrange(j_id)
X <- model.matrix(~ 1 + d_age + d_black + d_female + d_prevcharge, data = sentence_level_data)
X <- X[,-1]
Z <- model.matrix(~ 1 + j_black + j_log_cases, data = judge_level_data)
Z <- Z[,-1]

data_stan <- list(N = nrow(sentence_level_data),
                  J = length(unique(sentence_level_data$j_id)),
                  jj = sentence_level_data$j_id,
                  K = ncol(X),
                  M = ncol(Z),
                  X = X,
                  Z = Z,
                  y = sentence_level_data$sentence)


#### MODELS =============================================================================

# simple model --------------------------------------------------------------------------
simple_model <- "// SIMPLE MODEL
data {
  int<lower=1> N;                     // Number of observations
  int<lower=1> J;                     // Number of judges
  int<lower=1> K;                     // Number of observation-level covariates
  array[N] int<lower=1, upper=J> jj;  // Judge ID
  matrix[N, K] X;                     // Observation-level covariate matrix
  array[N] int<lower=0, upper=1> y;   // Outcome - sentence
}

parameters {
  real mu_alpha;                      // average across judges/overall average
  real<lower=0> sigma_alpha;          // between-judge variation
  vector[J] alpha;                    // varying intercept for judges
  vector[K] beta;                     // coefficient vector for observation-level covariates
}

model {
  // priors
  mu_alpha ~ student_t(3, 0, 1);
  sigma_alpha ~ student_t(3, 0, 1);
  alpha ~ student_t(3, mu_alpha, sigma_alpha);
  beta ~ student_t(3, 0, 1);
  
  // likelihood
  y ~ bernoulli_logit(alpha[jj] + X*beta);
}
"

write(simple_model, "simple_model.stan")
simple_model_compiled <- cmdstan_model("simple_model.stan")
fit <- simple_model_compiled$sample(
  data = data_stan,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 250,
  refresh = 50,
  output_dir = "./",
  output_basename = "simple_model",
  save_warmup = FALSE
)

# complex model -------------------------------------------------------------------------
complex_model <- "// COMPLEX MODEL
data {
  int<lower=1> N;                     // Number of observations
  int<lower=1> J;                     // Number of judges
  int<lower=1> K;                     // Number of observation-level covariates
  int<lower=1> M;                     // Number of group-level covariates
  array[N] int<lower=1, upper=J> jj;  // Judge ID
  matrix[N, K] X;                     // Observation-level covariate matrix
  matrix[J, M] Z;                     // Group-level covariate matrix
  array[N] int<lower=0, upper=1> y;   // Outcome - sentence
}

parameters {
  real mu_alpha;                      // average across judges/overall average
  real<lower=0> sigma_alpha;          // between-judge variation
  vector[J] alpha;                    // varying intercept for judges
  vector[K] beta;                     // coefficient vector for observation-level covariates
  vector[M] gamma;                    // coefficient vector for group-level predictors
}

model {
  // priors
  mu_alpha ~ student_t(3, 0, 1);
  sigma_alpha ~ student_t(3, 0, 1);
  gamma ~ student_t(3, 0, 1);
  alpha ~ student_t(3, mu_alpha + Z*gamma, sigma_alpha);
  beta ~ student_t(3, 0, 1);
  
  // likelihood
  y ~ bernoulli_logit(alpha[jj] + X*beta);
}
"

write(complex_model, "complex_model.stan")
complex_model_compiled <- cmdstan_model("complex_model.stan")
fit <- complex_model_compiled$sample(
  data = data_stan,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 250,
  refresh = 50,
  output_dir = "./",
  output_basename = "complex_model",
  save_warmup = FALSE
)


#### POPULATION AVERAGED PREDICTIONS - SIMPLE MODEL =====================================

# import and transform posterior --------------------------------------------------------
posterior_files <- c("simple_model-1.csv", "simple_model-2.csv", 
                     "simple_model-3.csv", "simple_model-4.csv")
alpha_posterior <- posterior_files %>%
  purrr::map_dfr(importCmdstanPosterior,
                 parameters = c("alpha")) %>%
  pivot_longer(everything(), names_to = "parameter", values_to = "alpha") %>%
  mutate(j_id = as.integer(str_extract(parameter, "[[:digit:]]+"))) %>%
  group_by(parameter) %>%
  mutate(draw = 1:n()) %>%
  ungroup() %>%
  select(-parameter)
beta_posterior <- posterior_files %>%
  purrr::map_dfr(importCmdstanPosterior,
                 parameters = c("beta")) %>%
  mutate(draw = 1:n())
combined_posterior <- alpha_posterior %>%
  left_join(beta_posterior, by = "draw")

# create the linear predictor for each case in the data ---------------------------------
X_new <- data.frame(n = 1:nrow(sentence_level_data), j_id = sentencing_data$j_id, X) 
linpred_data <- expand_grid(n = 1:nrow(sentence_level_data),
                        draw = 1:1000) %>%
  left_join(X_new, by = "n") %>%
  left_join(combined_posterior, by = c("j_id", "draw"))

# predictions for different age groups by fixing levels of interest (classic way) -------
pred_age_under_18 <- linpred_data %>%
  mutate(d_age18_29 = 0,
         d_age30_44 = 0,
         d_age45_64 = 0,
         d_age65_plus = 0) %>%
  mutate(linpred = alpha + beta.1*d_age18_29 + beta.2*d_age30_44 + beta.3*d_age45_64 +
                beta.4*d_age65_plus + beta.5*d_blackyes + beta.6*d_femaleyes + beta.7*d_prevchargeyes) %>%
  group_by(draw) %>%
  summarize(avglinpred = mean(invlogit(linpred))) %>%
  ungroup() %>%
  mutate(group = "under_18")

pred_age_18_29 <- linpred_data %>%
  mutate(d_age18_29 = 1,
         d_age30_44 = 0,
         d_age45_64 = 0,
         d_age65_plus = 0) %>%
  mutate(linpred = alpha + beta.1*d_age18_29 + beta.2*d_age30_44 + beta.3*d_age45_64 +
           beta.4*d_age65_plus + beta.5*d_blackyes + beta.6*d_femaleyes + beta.7*d_prevchargeyes) %>%
  group_by(draw) %>%
  summarize(avglinpred = mean(invlogit(linpred))) %>%
  ungroup() %>%
  mutate(group = "18_29")

pred_age_30_44 <- linpred_data %>%
  mutate(d_age18_29 = 0,
         d_age30_44 = 1,
         d_age45_64 = 0,
         d_age65_plus = 0) %>%
  mutate(linpred = alpha + beta.1*d_age18_29 + beta.2*d_age30_44 + beta.3*d_age45_64 +
           beta.4*d_age65_plus + beta.5*d_blackyes + beta.6*d_femaleyes + beta.7*d_prevchargeyes) %>%
  group_by(draw) %>%
  summarize(avglinpred = mean(invlogit(linpred))) %>%
  ungroup() %>%
  mutate(group = "30_44")

pred_age_45_64 <- linpred_data %>%
  mutate(d_age18_29 = 0,
         d_age30_44 = 0,
         d_age45_64 = 1,
         d_age65_plus = 0) %>%
  mutate(linpred = alpha + beta.1*d_age18_29 + beta.2*d_age30_44 + beta.3*d_age45_64 +
           beta.4*d_age65_plus + beta.5*d_blackyes + beta.6*d_femaleyes + beta.7*d_prevchargeyes) %>%
  group_by(draw) %>%
  summarize(avglinpred = mean(invlogit(linpred))) %>%
  ungroup() %>%
  mutate(group = "45_64")

pred_age_65_plus <- linpred_data %>%
  mutate(d_age18_29 = 0,
         d_age30_44 = 0,
         d_age45_64 = 0,
         d_age65_plus = 1) %>%
  mutate(linpred = alpha + beta.1*d_age18_29 + beta.2*d_age30_44 + beta.3*d_age45_64 +
           beta.4*d_age65_plus + beta.5*d_blackyes + beta.6*d_femaleyes + beta.7*d_prevchargeyes) %>%
  group_by(draw) %>%
  summarize(avglinpred = mean(invlogit(linpred))) %>%
  ungroup() %>%
  mutate(group = "65_plus")
  
preds_age_fix <- rbind(pred_age_under_18, pred_age_18_29, pred_age_30_44, pred_age_45_64, pred_age_65_plus) %>%
  group_by(group) %>%
  summarize(lower = quantile(avglinpred, prob = 0.025),
            upper = quantile(avglinpred, prob = 0.975),
            fit = mean(avglinpred), .groups = "keep") %>%
  mutate(type = "fix")
preds_age_fix_spaghetti <- rbind(pred_age_under_18, pred_age_18_29, pred_age_30_44, pred_age_45_64, pred_age_65_plus) %>%
  filter(draw %in% sample(1:1000, 100)) %>%
  mutate(avglinpred = avglinpred) %>%
  mutate(type = "fix")

# predictions for different age groups by averaging over subsets ------------------------
pred_age_under_18 <- linpred_data %>%
  filter(d_age18_29 == 0 &
         d_age30_44 == 0 &
         d_age45_64 == 0 &
         d_age65_plus == 0) %>%
  mutate(linpred = alpha + beta.1*d_age18_29 + beta.2*d_age30_44 + beta.3*d_age45_64 +
           beta.4*d_age65_plus + beta.5*d_blackyes + beta.6*d_femaleyes + beta.7*d_prevchargeyes) %>%
  group_by(draw) %>%
  summarize(avglinpred = mean(invlogit(linpred))) %>%
  ungroup() %>%
  mutate(group = "under_18")

pred_age_18_29 <- linpred_data %>%
  filter(d_age18_29 == 1) %>%
  mutate(linpred = alpha + beta.1*d_age18_29 + beta.2*d_age30_44 + beta.3*d_age45_64 +
           beta.4*d_age65_plus + beta.5*d_blackyes + beta.6*d_femaleyes + beta.7*d_prevchargeyes) %>%
  group_by(draw) %>%
  summarize(avglinpred = mean(invlogit(linpred))) %>%
  ungroup() %>%
  mutate(group = "18_29")

pred_age_30_44 <- linpred_data %>%
  filter(d_age30_44 == 1) %>%
  mutate(linpred = alpha + beta.1*d_age18_29 + beta.2*d_age30_44 + beta.3*d_age45_64 +
           beta.4*d_age65_plus + beta.5*d_blackyes + beta.6*d_femaleyes + beta.7*d_prevchargeyes) %>%
  group_by(draw) %>%
  summarize(avglinpred = mean(invlogit(linpred))) %>%
  ungroup() %>%
  mutate(group = "30_44")

pred_age_45_64 <- linpred_data %>%
  filter(d_age45_64 == 1) %>%
  mutate(linpred = alpha + beta.1*d_age18_29 + beta.2*d_age30_44 + beta.3*d_age45_64 +
           beta.4*d_age65_plus + beta.5*d_blackyes + beta.6*d_femaleyes + beta.7*d_prevchargeyes) %>%
  group_by(draw) %>%
  summarize(avglinpred = mean(invlogit(linpred))) %>%
  ungroup() %>%
  mutate(group = "45_64")

pred_age_65_plus <- linpred_data %>%
  filter(d_age65_plus == 1) %>%
  mutate(linpred = alpha + beta.1*d_age18_29 + beta.2*d_age30_44 + beta.3*d_age45_64 +
           beta.4*d_age65_plus + beta.5*d_blackyes + beta.6*d_femaleyes + beta.7*d_prevchargeyes) %>%
  group_by(draw) %>%
  summarize(avglinpred = mean(invlogit(linpred))) %>%
  ungroup() %>%
  mutate(group = "65_plus")

preds_age_subset <- rbind(pred_age_under_18, pred_age_18_29, pred_age_30_44, pred_age_45_64, pred_age_65_plus) %>%
  group_by(group) %>%
  summarize(lower = quantile(avglinpred, prob = 0.025),
            upper = quantile(avglinpred, prob = 0.975),
            fit = mean(avglinpred), .groups = "keep") %>%
  mutate(type = "subset")
preds_age_subset_spaghetti <- rbind(pred_age_under_18, pred_age_18_29, pred_age_30_44, pred_age_45_64, pred_age_65_plus) %>%
  filter(draw %in% sample(1:1000, 100)) %>%
  mutate(avglinpred = avglinpred) %>%
  mutate(type = "subset")

# visualize
preds_age <- rbind(preds_age_fix, preds_age_subset) %>%
  mutate(group = factor(group,
                           levels = c("under_18", "18_29", "30_44", "45_64", "65_plus")))
preds_age_spaghetti <- rbind(preds_age_fix_spaghetti, preds_age_subset_spaghetti) %>%
  mutate(group = factor(group,
                           levels = c("under_18", "18_29", "30_44", "45_64", "65_plus")))

ggplot(data = preds_age_spaghetti) +
  geom_line(aes(x = group, y = avglinpred, group = draw), alpha = 0.3) +
  facet_wrap(~type)

ggplot(data = preds_age) +
  geom_pointrange(aes(x = group, y = fit, ymin = lower, ymax = upper)) +
  facet_wrap(~type)


#### POPULATION AVERAGED PREDICTIONS - COMPLEX MODEL ====================================

# import and transform posterior --------------------------------------------------------
posterior_files <- c("complex_model-1.csv", "complex_model-2.csv", 
                     "complex_model-3.csv", "complex_model-4.csv")
alpha_posterior <- posterior_files %>%
  purrr::map_dfr(importCmdstanPosterior,
                 parameters = c("alpha")) %>%
  pivot_longer(everything(), names_to = "parameter", values_to = "alpha") %>%
  mutate(j_id = as.integer(str_extract(parameter, "[[:digit:]]+"))) %>%
  group_by(parameter) %>%
  mutate(draw = 1:n()) %>%
  ungroup() %>%
  select(-parameter)
gamma_posterior <- posterior_files %>%
  purrr::map_dfr(importCmdstanPosterior,
                 parameters = c("gamma")) %>%
  mutate(draw = 1:n())
beta_posterior <- posterior_files %>%
  purrr::map_dfr(importCmdstanPosterior,
                 parameters = c("beta")) %>%
  mutate(draw = 1:n())
combined_posterior <- alpha_posterior %>%
  left_join(beta_posterior, by = "draw") %>%
  left_join(gamma_posterior, by = "draw")

# create the linear predictor for each case in the data ---------------------------------
X_new <- data.frame(n = 1:nrow(sentence_level_data), j_id = sentencing_data$j_id, X) 
Z_new <- data.frame(j_id = judge_level_data$j_id, Z) 
linpred_data <- expand_grid(n = 1:nrow(sentence_level_data),
                            draw = 1:1000) %>%
  left_join(X_new, by = "n") %>%
  left_join(Z_new, by = "j_id") %>%
  left_join(combined_posterior, by = c("j_id", "draw"))

# predictions for different judge race by averaging over subsets ------------------------
pred_judge_not_black <- linpred_data %>%
  filter(j_blackyes == 0) %>%
  mutate(linpred = alpha + beta.1*d_age18_29 + beta.2*d_age30_44 + beta.3*d_age45_64 +
           beta.4*d_age65_plus + beta.5*d_blackyes + beta.6*d_femaleyes + beta.7*d_prevchargeyes +
           gamma.1*j_blackyes + gamma.2*j_log_cases) %>%
  group_by(draw) %>%
  summarize(avglinpred = mean(invlogit(linpred))) %>%
  ungroup() %>%
  mutate(group = "not_black")

pred_judge_black <- linpred_data %>%
  filter(j_blackyes == 1) %>%
  mutate(linpred = alpha + beta.1*d_age18_29 + beta.2*d_age30_44 + beta.3*d_age45_64 +
           beta.4*d_age65_plus + beta.5*d_blackyes + beta.6*d_femaleyes + beta.7*d_prevchargeyes +
           gamma.1*j_blackyes + gamma.2*j_log_cases) %>%
  group_by(draw) %>%
  summarize(avglinpred = mean(invlogit(linpred))) %>%
  ungroup() %>%
  mutate(group = "black")

preds_j_race_subset <- rbind(pred_judge_not_black, pred_judge_black) %>%
  group_by(group) %>%
  summarize(lower = quantile(avglinpred, prob = 0.025),
            upper = quantile(avglinpred, prob = 0.975),
            fit = mean(avglinpred), .groups = "keep")
preds_j_race_subset_spaghetti <- rbind(pred_judge_not_black, pred_judge_black) %>%
  filter(draw %in% sample(1:1000, 100)) %>%
  mutate(avglinpred = avglinpred)

# visualize
preds_j_race <- preds_j_race_subset %>%
  mutate(group = factor(group,
                        levels = c("not_black", "black")))
preds_j_race_spaghetti <- preds_j_race_subset_spaghetti %>%
  mutate(group = factor(group,
                        levels = c("not_black", "black")))

ggplot(data = preds_j_race_spaghetti) +
  geom_line(aes(x = group, y = avglinpred, group = draw), alpha = 0.3) +
  coord_cartesian(ylim = c(0,1))

ggplot(data = preds_j_race) +
  geom_pointrange(aes(x = group, y = fit, ymin = lower, ymax = upper)) +
  coord_cartesian(ylim = c(0,1))


#### BAYESIAN R-Squared - SIMPLE MODEL ==================================================

# import and transform posterior --------------------------------------------------------
posterior_files <- c("simple_model-1.csv", "simple_model-2.csv", 
                     "simple_model-3.csv", "simple_model-4.csv")
alpha_posterior <- posterior_files %>%
  purrr::map_dfr(importCmdstanPosterior,
                 parameters = c("alpha")) %>%
  pivot_longer(everything(), names_to = "parameter", values_to = "alpha") %>%
  mutate(j_id = as.integer(str_extract(parameter, "[[:digit:]]+"))) %>%
  group_by(parameter) %>%
  mutate(draw = 1:n()) %>%
  ungroup() %>%
  select(-parameter)
beta_posterior <- posterior_files %>%
  purrr::map_dfr(importCmdstanPosterior,
                 parameters = c("beta")) %>%
  mutate(draw = 1:n())
combined_posterior <- alpha_posterior %>%
  left_join(beta_posterior, by = "draw")

# create the linear predictor for each case in the data ---------------------------------
X_new <- data.frame(n = 1:nrow(sentence_level_data), j_id = sentencing_data$j_id, X) 
linpred_data <- expand_grid(n = 1:nrow(sentence_level_data),
                            draw = 1:1000) %>%
  left_join(X_new, by = "n") %>%
  left_join(combined_posterior, by = c("j_id", "draw"))

# compute variance of predicted values
var_pred <- linpred_data %>%
  mutate(linpred = alpha + beta.1*d_age18_29 + beta.2*d_age30_44 + beta.3*d_age45_64 +
           beta.4*d_age65_plus + beta.5*d_blackyes + beta.6*d_femaleyes + beta.7*d_prevchargeyes) %>%
  group_by(draw) %>%
  summarize(var_pred = var(invlogit(linpred)))

# compute expected residual variance
var_resi <- linpred_data %>%
  mutate(linpred = alpha + beta.1*d_age18_29 + beta.2*d_age30_44 + beta.3*d_age45_64 +
                              beta.4*d_age65_plus + beta.5*d_blackyes + beta.6*d_femaleyes + beta.7*d_prevchargeyes) %>%
  group_by(draw) %>%
  summarize(var_resi = mean(invlogit(linpred)*(1-(invlogit(linpred)))))

# compute R-Squared
r2_simple <- var_pred %>%
  left_join(var_resi, by = "draw") %>% 
  mutate(r2 = var_pred/(var_pred+var_resi),
         model = "simple")


#### BAYESIAN R-Squared - COMPLEX MODEL =================================================

# import and transform posterior --------------------------------------------------------
posterior_files <- c("complex_model-1.csv", "complex_model-2.csv", 
                     "complex_model-3.csv", "complex_model-4.csv")
alpha_posterior <- posterior_files %>%
  purrr::map_dfr(importCmdstanPosterior,
                 parameters = c("alpha")) %>%
  pivot_longer(everything(), names_to = "parameter", values_to = "alpha") %>%
  mutate(j_id = as.integer(str_extract(parameter, "[[:digit:]]+"))) %>%
  group_by(parameter) %>%
  mutate(draw = 1:n()) %>%
  ungroup() %>%
  select(-parameter)
gamma_posterior <- posterior_files %>%
  purrr::map_dfr(importCmdstanPosterior,
                 parameters = c("gamma")) %>%
  mutate(draw = 1:n())
beta_posterior <- posterior_files %>%
  purrr::map_dfr(importCmdstanPosterior,
                 parameters = c("beta")) %>%
  mutate(draw = 1:n())
combined_posterior <- alpha_posterior %>%
  left_join(beta_posterior, by = "draw") %>%
  left_join(gamma_posterior, by = "draw")

# create the linear predictor for each case in the data ---------------------------------
X_new <- data.frame(n = 1:nrow(sentence_level_data), j_id = sentencing_data$j_id, X) 
Z_new <- data.frame(j_id = judge_level_data$j_id, Z) 
linpred_data <- expand_grid(n = 1:nrow(sentence_level_data),
                            draw = 1:1000) %>%
  left_join(X_new, by = "n") %>%
  left_join(Z_new, by = "j_id") %>%
  left_join(combined_posterior, by = c("j_id", "draw"))

# compute variance of predicted values --------------------------------------------------
var_pred <- linpred_data %>%
  mutate(linpred = alpha + beta.1*d_age18_29 + beta.2*d_age30_44 + beta.3*d_age45_64 +
                              beta.4*d_age65_plus + beta.5*d_blackyes + beta.6*d_femaleyes + beta.7*d_prevchargeyes +
                              gamma.1*j_blackyes + gamma.2*j_log_cases) %>%
  group_by(draw) %>%
  summarize(var_pred = var(invlogit(linpred)))

# compute expected residual variance ----------------------------------------------------
var_resi <- linpred_data %>%
  mutate(linpred = alpha + beta.1*d_age18_29 + beta.2*d_age30_44 + beta.3*d_age45_64 +
                              beta.4*d_age65_plus + beta.5*d_blackyes + beta.6*d_femaleyes + beta.7*d_prevchargeyes +
                              gamma.1*j_blackyes + gamma.2*j_log_cases) %>%
  group_by(draw) %>%
  summarize(var_resi = mean(invlogit(linpred)*(1-invlogit(linpred))))

# compute R-Squared ---------------------------------------------------------------------
r2_complex <- var_pred %>%
  left_join(var_resi, by = "draw") %>% 
  mutate(r2 = var_pred/(var_pred+var_resi),
         model = "complex")

# visualize -----------------------------------------------------------------------------
r2 <- rbind(r2_simple, r2_complex)
ggplot() +
  geom_histogram(data = r2, aes(x = r2)) +
  facet_wrap(~ model)
