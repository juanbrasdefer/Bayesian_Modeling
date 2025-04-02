# ---------------------------------------------------------------------------------------
# BAYESIAN MODELING
# Session 6: Varying intercepts and varying slopes
# Instructor: Sascha Göbel
# March 2025
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

# import data ---------------------------------------------------------------------------
sentencing_data <- readRDS("sentencing_data")

# format data for stan ------------------------------------------------------------------
sentencing_data <- sentencing_data %>%
  mutate(sentence = as.integer(sentence),
         d_black = as.integer(d_black),
         j_id = as.integer(as.factor(j_id))) %>%
  arrange(j_id)


#### COMPLETE POOLING ===================================================================

# prepare data for stan -----------------------------------------------------------------
data_stan <- list(N = nrow(sentencing_data),
                  y = sentencing_data$sentence,
                  x = sentencing_data$d_black)

# stan program --------------------------------------------------------------------------
complete_pooling_model <- "// COMPLETE POOLING MODEL
data {
  int<lower=1> N;                     // Number of observations
  vector<lower=0, upper=1>[N] x;      // Predictor - d_black
  array[N] int<lower=0, upper=1> y;   // Outcome - sentence
}

parameters {
  real alpha;
  real beta;
}

model {
  // priors
  alpha ~ cauchy(0, 1);
  beta ~ cauchy(0, 1);
  
  // likelihood
  y ~ bernoulli_logit(alpha + beta * x);
}
"

# save as .stan file --------------------------------------------------------------------
write(complete_pooling_model, "complete_pooling_model.stan")

# compile model -------------------------------------------------------------------------
complete_pooling_model_compiled <- cmdstan_model("complete_pooling_model.stan")

# sample from the posterior -------------------------------------------------------------
fit <- complete_pooling_model_compiled$sample(
  data = data_stan,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  refresh = 50,
  output_dir = "YOUR OUTPUT DIRECTORY",
  output_basename = "complete_pooling_model"
)

# inspect results -----------------------------------------------------------------------
fit$summary()
posterior_files <- c("complete_pooling_model-1.csv", "complete_pooling_model-2.csv", 
                     "complete_pooling_model-3.csv", "complete_pooling_model-4.csv")
alpha_posterior_mean <- posterior_files %>%
  purrr::map_dfr(importCmdstanPosterior, 
                 parameters = "alpha") %>%
  colMeans() %>%
  invlogit()
linpred_posterior_mean <- posterior_files %>%
  purrr::map_dfr(importCmdstanPosterior, 
                 parameters = c("alpha", "beta")) %>%
  rowSums() %>%
  mean() %>%
  invlogit()

# percent incarcerated when defendant not black (0)
alpha_posterior_mean 
sentencing_data %>% filter(d_black == 0) %>% pull(sentence) %>% table() %>% 
  prop.table() %>% magrittr::extract(2)
# percent incarcerated when defendant black (1)
linpred_posterior_mean
sentencing_data %>% filter(d_black == 1) %>% pull(sentence) %>% table() %>% 
  prop.table() %>% magrittr::extract(2)


#### NO POOLING =========================================================================

# no pooling is the equivalent of traditional statistics regressions
# fixed effects
# we assume that the between-group variance is infinite
# because we put a non informative, uniform prior on it by default

# this takes a very long time to run because we specify no priors
# prepare data for stan -----------------------------------------------------------------
X <- model.matrix( ~ -1 + factor(j_id) + d_black, data = sentencing_data)
# model matrix is basically the perfect function. takes care of intercept and factors
# the -1 has to do with intercept and 'reference category for judge ids'
# he said either you get rid of this number or use the -1 (to balance out the column of 1s)

# on the issue of turning a categorical variable with more than two categories
# (ie: not binary) into a usable format for stan,
# it looks like using this combination of model.matrix() and factor() 
# automatically changes the underlying variable to be one-hot encoded
  # which solves the issue that you ran into when you manually tried to prep the variable

# model.matrix is one way to create covariate matrix
data_stan <- list(N = nrow(sentencing_data),
                  K = ncol(X),
                  y = sentencing_data$sentence,
                  X = X)

# stan program --------------------------------------------------------------------------
no_pooling_model <- "// NO POOLING MODEL
data {
  int<lower=1> N;                     // Number of observations
  int<lower=1> K;                     // Number of covariates
  matrix[N, K] X;                     // Covariate matrix
  array[N] int<lower=0, upper=1> y;   // Outcome - sentence
}

parameters {
  vector[K] beta;                  // coefficient vector
}

model {
  // note that we don't put any priors in the model section
  // because we are doing no pooling AKA fixed effects AKA no priors!
  // likelihood
  y ~ bernoulli_logit(X * beta);
}
"

# save as .stan file --------------------------------------------------------------------
write(no_pooling_model, "no_pooling_model.stan")

# compile model -------------------------------------------------------------------------
no_pooling_model_compiled <- cmdstan_model("no_pooling_model.stan")

# sample from the posterior -------------------------------------------------------------
fit <- no_pooling_model_compiled$sample(
  data = data_stan,
  chains = 4, 
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  refresh = 50,
  output_dir = "YOUR OUTPUT DIRECTORY",
  output_basename = "no_pooling_model"
)

# assemble and inspect results ----------------------------------------------------------
posterior_files <- c("no_pooling_model-1.csv", "no_pooling_model-2.csv", 
                     "no_pooling_model-3.csv", "no_pooling_model-4.csv")
j_posterior_np <- posterior_files %>%
  purrr::map_dfr(importCmdstanPosterior, 
                 parameters = "beta") %>%
  select(-c("beta.82")) %>%
  pivot_longer(cols = everything(), names_to = "beta", values_to = "estimate") %>%
  group_by(beta) %>%
  summarize(lower = invlogit(quantile(estimate, prob = 0.025)),
            upper = invlogit(quantile(estimate, prob = 0.975)),
            fit = invlogit(mean(estimate))) %>%
  mutate(j_id = as.integer(str_extract(beta, "[[:digit:]]+")),
         model = "no_pooling") %>%
  select(-beta) %>%
  arrange(j_id)
j_posterior_np$fit %>% hist()


#### PARTIAL POOLING ====================================================================

# prepare data for stan -----------------------------------------------------------------
data_stan <- list(N = nrow(sentencing_data),
                  J = length(unique(sentencing_data$j_id)),
                  jj = sentencing_data$j_id, # i believe this is indices
                  y = sentencing_data$sentence,
                  x = sentencing_data$d_black)

# stan program --------------------------------------------------------------------------
partial_pooling_model <- "// PARTIAL POOLING MODEL
// note that we don't need an intercept because we allow our intercept to vary
data {
  int<lower=1> N;                     // Number of observations
  int<lower=1> J;                     // Number of judges
  array[N] int<lower=1, upper=J> jj;  // Judge ID
  vector<lower=0, upper=1>[N] x;      // Predictor - d_black
  array[N] int<lower=0, upper=1> y;   // Outcome - sentence
                                        // because our outcome variable is binary,
                                        // we have to use array, not vector
                                        // vector can only be 'reals', not 'int'
}

parameters {
  real mu_alpha;              // average across the data
  real<lower=0> sigma_alpha;  // between-group variance
                                // hyperparamter for the prior of alpha
  vector[J] alpha;            // percentage incarceration when defendant not black (intercept)
                                // but alpha is now not a 'real', it is a vector of params
                                // because we allow it to vary across judges
                                // for every judge we have a different intercept
  real beta;                  // effect of being black or not
                                // if we wanted beta to vary, we would say vector [J] beta
}

model {
  // priors
  mu_alpha ~ cauchy(0, 1);          // very weak prior
  sigma_alpha ~ cauchy(0, 1);       // very weak prior (because we know we have a lot of data)
  alpha ~ normal(mu_alpha, sigma_alpha);  // prior for our varying intercept alpha
                                          // which takes the mean and between group var as params
  beta ~ normal(0, 1);
  
  // likelihood
  y ~ bernoulli_logit(alpha[jj] + beta * x);  // need to note: alpha is now a vector
                                              // when we declare in this vectorized form, 
                                              // we need to index alpha
}
"

# save as .stan file --------------------------------------------------------------------
write(partial_pooling_model, "partial_pooling_model.stan")

# compile model -------------------------------------------------------------------------
partial_pooling_model_compiled <- cmdstan_model("partial_pooling_model.stan")

# sample from the posterior -------------------------------------------------------------
fit <- partial_pooling_model_compiled$sample(
  data = data_stan,
  parallel_chains = 4,
  chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  refresh = 50,
  output_dir = "YOUR OUTPUT DIRECTORY",
  output_basename = "partial_pooling_model"
)

# assemble and inspect results ----------------------------------------------------------
posterior_files <- c("partial_pooling_model-1.csv", "partial_pooling_model-2.csv", 
                     "partial_pooling_model-3.csv", "partial_pooling_model-4.csv")
j_posterior_pp <- posterior_files %>%
  purrr::map_dfr(importCmdstanPosterior, 
                 parameter = "alpha") %>%
  pivot_longer(cols = everything(), names_to = "alpha", values_to = "estimate") %>%
  group_by(alpha) %>%
  summarize(lower = invlogit(quantile(estimate, prob = 0.025)),
            upper = invlogit(quantile(estimate, prob = 0.975)),
            fit = invlogit(mean(estimate))) %>%
  mutate(j_id = as.integer(str_extract(alpha, "[[:digit:]]+")),
         model = "partial_pooling") %>%
  select(-alpha) %>%
  arrange(j_id)
j_posterior_pp$fit %>% hist()


#### COMPARISON OF COMPLETE, NO, AND PARTIAL POOLING ====================================

# combine posterior summaries -----------------------------------------------------------
comparison <- j_posterior_np %>%
  rbind(j_posterior_pp)

# add information on sample size per judge ----------------------------------------------
comparison <- sentencing_data %>%
  group_by(j_id) %>%
  summarize(n = n()) %>%
  left_join(comparison, by = "j_id") %>%
  mutate(model = case_when(model == "no_pooling" ~ "No pooling",
                           model == "partial_pooling" ~ "Partial pooling"))

# visualize comparison ------------------------------------------------------------------
comparison_a <- ggplot(data = comparison) +
  geom_hline(yintercept = alpha_posterior_mean) +
  geom_pointrange(aes(x = n, y = fit, ymin = lower, ymax = upper), 
                  alpha = 0.3, shape = 20, size = 0.3,
                  position=position_jitter(width=0.5)) +
  facet_wrap(~ model) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        panel.border = element_rect(fill=NA, colour = "black", size=.5),
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.2, "cm"),  
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(margin = unit(c(0, 0, 0, 0), "mm"), size = 16, color = "black", vjust = -1.5),
        strip.background = element_blank(),
        strip.text.x = element_text(size=16, face = "bold"),
        strip.text.y = element_text(size=16, face = "bold"),
        strip.placement = "outside") +
  ylab("Percent incarcerated when defendant not black") +
  xlab("Decisions per judge") +
  scale_x_continuous(expand = c(0.01, 0.01), breaks = seq(100, 700, 100)) +
  scale_y_continuous(expand = c(0.001, 0.001), breaks = seq(0.2, 0.8, 0.2)) +
  annotate("text", x = 480, y = alpha_posterior_mean+0.02, label = "Complete pooling", color = "black", size = 4)

comparison_b <- ggplot(data = comparison) +
  coord_cartesian(xlim = c(0, 50)) +
  geom_hline(yintercept = alpha_posterior_mean) +
  geom_pointrange(aes(x = n, y = fit, ymin = lower, ymax = upper), 
                  alpha = 0.3, shape = 20, size = 0.3,
                  position=position_jitter(width=1)) +
  facet_wrap(~ model) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        panel.border = element_rect(fill=NA, colour = "black", size=.5),
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.2, "cm"),  
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(margin = unit(c(0, 0, 0, 0), "mm"), size = 16, color = "black", vjust = -1.5),
        strip.background = element_blank(),
        strip.text.x = element_text(size=16, face = "bold"),
        strip.text.y = element_text(size=16, face = "bold"),
        strip.placement = "outside") +
  ylab("Percent incarcerated when defendant not black") +
  xlab("Decisions per judge") +
  scale_x_continuous(expand = c(0.01, 0.01), breaks = seq(10, 40, 10)) +
  scale_y_continuous(expand = c(0.001, 0.001), breaks = seq(0.2, 0.8, 0.2)) +
  annotate("text", x = 28, y = alpha_posterior_mean+0.02, label = "Complete pooling", color = "black", size = 4)

comparison_c <- ggplot(data = comparison) +
  coord_cartesian(xlim = c(0, 50)) +
  geom_hline(yintercept = alpha_posterior_mean) +
  geom_point(aes(x = n, y = fit), 
             alpha = 0.3, size = 1,
             position=position_jitter(width=1)) +
  facet_wrap(~ model) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        panel.border = element_rect(fill=NA, colour = "black", size=.5),
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.2, "cm"),  
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(margin = unit(c(0, 0, 0, 0), "mm"), size = 16, color = "black", vjust = -1.5),
        strip.background = element_blank(),
        strip.text.x = element_text(size=16, face = "bold"),
        strip.text.y = element_text(size=16, face = "bold"),
        strip.placement = "outside") +
  ylab("Percent incarcerated when defendant not black") +
  xlab("Decisions per judge") +
  scale_x_continuous(expand = c(0.01, 0.01), breaks = seq(10, 40, 10)) +
  scale_y_continuous(expand = c(0.001, 0.001), breaks = seq(0.2, 0.8, 0.2)) +
  annotate("text", x = 28, y = alpha_posterior_mean+0.02, label = "Complete pooling", color = "black", size = 4)


#### VARYING INTERCEPT AND VARYING SLOPE ================================================

# stan program --------------------------------------------------------------------------
var_int_var_slope_model <- "// VARYING INTERCEPT AND VARYING MODEL
data {
                         // same data block as before
  int<lower=1> N;                     // Number of observations
  int<lower=1> J;                     // Number of judges
  array[N] int<lower=1, upper=J> jj;  // Judge ID
  vector<lower=0, upper=1>[N] x;      // Predictor - d_black
  array[N] int<lower=0, upper=1> y;   // Outcome - sentence
}

parameters {
  real mu_alpha;
  real mu_beta;                 // beta gets its own priors
  real<lower=0> sigma_alpha;
  real<lower=0> sigma_beta;     // beta gets its own priors
  vector[J] alpha;
  vector[J] beta;               // now we allow for beta to vary across judges
}

transformed parameters {
  vector[N] pi;           // btw: pi is not being sampled here
                          // it's being assigned below, but not sampled
                          // everything with a sampling statement '~' goes into the model block
                          // but since pi uses '=', we don't specify it as parameter

  // 'everything that gets a sampling statement goes into parameters
  // everything that gets an assignment statement and is data goes into the data block
  // and everything that gets an assignment statement but doesnt belong to data 
    // and is not a parameter
    // or rather is not a sampled parameter, goes into the transformed parameters block
    // BUT WE ONLY NEED A TRANSFORMED PARAMETERS BLOCK IF WE USE THE PI IN THE LIKELIHOOD BLOCK
    // AS OPPOSED TO THE ALPHA[jj] ETC;
    // IF YOU USE THAT ALPHA VERSION, YOU CAN DELETE THIS ENTIRE TRANSFORMED PARAMS BLOCK
  
  for (i in 1 : N) {
    pi[i] = alpha[jj[i]] + beta[jj[i]] * x[i];
  }
}

model {
  // priors
  mu_alpha ~ student_t(3, 0, 1);
  sigma_alpha ~ student_t(3, 0, 1);
  mu_beta ~ student_t(3, 0, 1);
  sigma_beta ~ student_t(3, 0, 1);
  alpha ~ normal(mu_alpha, sigma_alpha);
  beta ~ normal(mu_beta, sigma_beta);
  
  // likelihood
  y ~ bernoulli_logit(pi);    // this is equivalent to:
  // y ~ bernoulli_logit(alpha[jj] + beta[jj] * x); 

}
"

# save as .stan file --------------------------------------------------------------------
write(var_int_var_slope_model, "var_int_var_slope_model.stan")

# compile model -------------------------------------------------------------------------
var_int_var_slope_model_compiled <- cmdstan_model("var_int_var_slope_model.stan")

# sample from the posterior -------------------------------------------------------------
fit <- var_int_var_slope_model_compiled$sample(
  data = data_stan,
  parallel_chains = 4,
  chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  refresh = 50,
  output_dir = "YOUR OUTPUT DIRECTORY",
  output_basename = "var_int_var_slope_model"
)

# selection of judges -------------------------------------------------------------------
n_sample <- 6
j_sample <- c(37,2,22, 56, 25, 8)

# assemble results ----------------------------------------------------------------------
posterior_files <- c("partial_pooling_model-1.csv", "partial_pooling_model-2.csv", 
                     "partial_pooling_model-3.csv", "partial_pooling_model-4.csv")
vi_alpha <- posterior_files %>%
  purrr::map_dfr(importCmdstanPosterior, 
                 parameter = "alpha") %>%
  select(all_of(j_sample)) %>%
  as.matrix()
vi_beta <- posterior_files %>%
  purrr::map_dfr(importCmdstanPosterior, 
                 parameter = "beta") %>%
  pull(beta)
posterior_files <- c("var_int_var_slope_model-1.csv", "var_int_var_slope_model-2.csv", 
                     "var_int_var_slope_model-3.csv", "var_int_var_slope_model-4.csv")
vivs_alpha <- posterior_files %>%
  purrr::map_dfr(importCmdstanPosterior, 
                 parameter = "alpha") %>%
  select(all_of(j_sample)) %>%
  as.matrix()
vivs_beta <- posterior_files %>%
  purrr::map_dfr(importCmdstanPosterior, 
                 parameter = "beta") %>%
  select(all_of(j_sample)) %>%
  as.matrix()
vivs_comparison <- data.frame(x = c(rep(0, n_sample*2), rep(1, n_sample*2)),
                              y = invlogit(c(colMeans(vi_alpha),
                                             colMeans(vivs_alpha),
                                             colMeans(vi_beta + vi_alpha),
                                             colMeans(vivs_beta + vivs_alpha))),
                              model = rep(rep(c("Varying intercepts", "Varying intercepts and slopes"), each = n_sample), 2),
                              j_id = rep(j_sample, 4)) 

# visualize comparison ------------------------------------------------------------------
vivs_plot <- ggplot(data = vivs_comparison) +
  geom_line(aes(x = x, y = y, group = factor(j_id))) +
  facet_wrap(~model)+ 
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        panel.border = element_rect(fill=NA, colour = "black", size=.5),
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.2, "cm"),  
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(margin = unit(c(0, 0, 0, 0), "mm"), size = 16, color = "black", vjust = -1.5),
        strip.background = element_blank(),
        strip.text.x = element_text(size=16, face = "bold"),
        strip.text.y = element_text(size=16, face = "bold"),
        strip.placement = "outside") +
  ylab("Percent incarcerated") +
  xlab("Defendant not Black vs. Black") +
  scale_x_continuous(expand = c(0.01, 0.01), breaks = seq(0, 1, 1))
