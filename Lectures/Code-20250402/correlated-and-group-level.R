# ---------------------------------------------------------------------------------------
# BAYESIAN MODELING
# Session 8: Correlated and group-level parameters
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

# paper: Harris, Allison P. 2023. 'Can Racial Diversity among Judges Affect Sentencing 
#           Outcomes?'. American Political Science Review.
#           https://doi.org/10.1017/S0003055423000552
# data source: https://doi.org/10.7910/DVN/YO5J2S

# import data ---------------------------------------------------------------------------
sentencing_data <- readRDS("sentencing_data")


#### GROUP-LEVEL PREDICTORS =============================================================

# format data for stan ------------------------------------------------------------------
sentencing_data <- sentencing_data %>%
  mutate(sentence = as.integer(sentence),
         j_id = as.integer(as.factor(j_id))) %>%
  arrange(j_id)

# prepare data for stan -----------------------------------------------------------------
sentence_level_data <- sentencing_data %>%
  select(sentence, j_id) %>%
  arrange(j_id)
judge_level_data <- sentencing_data %>%
  group_by(j_id, j_black) %>%
  summarize(n = n(), .groups = "keep")
Z <- model.matrix(~ 1 + j_black, data = judge_level_data) # covariate matrix at judge level
data_stan <- list(N = nrow(sentence_level_data),
                  J = length(unique(sentence_level_data$j_id)),
                  jj = sentence_level_data$j_id,
                  M = ncol(Z),
                  Z = Z,
                  y = sentence_level_data$sentence)

# stan program --------------------------------------------------------------------------
group_level_model <- "// PARTIAL POOLING MODEL WITH GROUP-LEVEL COVARIATES
data {
  int<lower=1> N;                     // Number of observations
  int<lower=1> J;                     // Number of judges
  int<lower=1> M;                     // Number of group-level covariates
  array[N] int<lower=1, upper=J> jj;  // Judge ID
  matrix[J, M] Z;                     // Group-level covariate matrix // M is number of group-level covariates
  array[N] int<lower=0, upper=1> y;   // Outcome - sentence
}

parameters {
  vector[J] alpha;      // varying intercept for judges
  vector[M] gamma;      // coefficient vector for group-level predictors
  real<lower=0> sigma_alpha;   // between-judge variation
}

model {
  // priors
  gamma ~ student_t(3, 0, 1);
  sigma_alpha ~ student_t(3, 0, 1);
  alpha ~ student_t(3, Z*gamma, sigma_alpha); // 'we have just moved the covariate matrix up to judges
                                              // where before it was a level down; at the sentence level'
  
  // likelihood
  y ~ bernoulli_logit(alpha[jj]);
}
"

# save as .stan file --------------------------------------------------------------------
write(group_level_model, "group_level_model.stan")

# compile model -------------------------------------------------------------------------
group_level_model_compiled <- cmdstan_model("group_level_model.stan")

# sample from the posterior -------------------------------------------------------------
fit <- group_level_model_compiled$sample(
  data = data_stan,
  parallel_chains = 4,
  chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  refresh = 50,
  output_dir = "YOUR OUTPUT DIRECTORY",
  output_basename = "group_level_model"
)

# assign posterior files ----------------------------------------------------------------
posterior_files <- c("group_level_model-1.csv", "group_level_model-2.csv", 
                     "group_level_model-3.csv", "group_level_model-4.csv")

# interpreting the group-level regression/averaging over judges -------------------------
group_level_estimates <- posterior_files %>%
  purrr::map_dfr(importCmdstanPosterior, 
                 parameter = "gamma") %>% # extract parameter for when judge black not black (i think)
  mutate(j_non_black = gamma.1,
         j_black = gamma.1 + gamma.2) %>% # seems like it ye
  select(j_non_black, j_black) %>%
  pivot_longer(cols = everything(), names_to = "estimand", values_to = "estimate") %>%
  group_by(estimand) %>%
  summarize(lower = invlogit(quantile(estimate, prob = 0.025)),
            upper = invlogit(quantile(estimate, prob = 0.975)),
            fit = invlogit(mean(estimate)))
plot_a <- ggplot(data = group_level_estimates) +
  geom_pointrange(aes(x = estimand, y = fit, ymin = lower, ymax = upper)) +
  coord_cartesian(ylim = c(0,1))
ggsave("plot_a.pdf", plot_a, width = 9, height = 6, 
       dpi = 1200, device = cairo_pdf)


#### CORRELATED PARAMETERS ==============================================================

# import data ---------------------------------------------------------------------------
sentencing_data <- readRDS("sentencing_data")

# format data for stan ------------------------------------------------------------------
sentencing_data <- sentencing_data %>%
  mutate(sentence = as.integer(sentence),
         d_black = as.integer(d_black),
         j_id = as.integer(as.factor(j_id))) %>%
  arrange(j_id)
X <- model.matrix(~ 1 + d_black, data = sentencing_data)
# we include our intercept here in the covariate matrix 
# individual level
# but why?
# because we are correlating the paramters for intercepts and slopes
# so we need to have one parameter for both
# and so we need to have them in one thing

# before, the intercept was an independent thing on its own
# but now we have to plug it into where we also want the slope to vary
# so we hard-code this in the data to be able to do this

# prepare data for stan -----------------------------------------------------------------
data_stan <- list(N = nrow(sentencing_data),
                  J = length(unique(sentencing_data$j_id)),
                  K = ncol(X),
                  jj = sentencing_data$j_id,
                  X = X,
                  y = sentencing_data$sentence)
# this model is simple; it's the one we've been using 


# repeat with multi-normal and check results for both

# stan program --------------------------------------------------------------------------
correlated_model <- "// PARTIAL POOLING MODEL WITH CORRELATED PARAMETERS
data {
  int<lower=1> N;                     // Number of observations
  int<lower=1> J;                     // Number of judges
  int<lower=1> K;                     // Number of covariates (including an intercept)
  array[N] int<lower=1, upper=J> jj;  // Judge ID
  matrix[N, K] X;                     // Covariate matrix
  array[N] int<lower=0, upper=1> y;   // Outcome - sentence
}

parameters {
  array[J] vector[K] beta;     // varying intercepts and slopes for judges
                                  // beta is a collection of varying parameters
                                  // each beta is a vector with two items (maybe?)
  vector[K] mu_beta;           // average across judges
  corr_matrix[K] Omega;        // prior correlation
  vector<lower=0>[K] tau;      // prior scale
                                  // previously, tau was just sigma
                                  // one vector, one number for each parameter
  real<lower=0> nu;             // degrees of freedom for students T dist.
}

model {
  vector[N] pi;

  // priors
  nu ~ gamma(2, 0.1);
  tau ~  student_t(3, 0, 1);
  Omega ~ lkj_corr(2);
  beta ~ multi_student_t(nu, mu_beta, quad_form_diag(Omega, tau));
                                        // construct covariance matrix
  
  // likelihood
  // YOU DONT NEED TO USE THE LOOP; YOU CAN JUST JUST X * BJJ
  for (i in 1:N) {
    pi[i] = X[i] * beta[jj[i]];
  }
  
  y ~ bernoulli_logit(pi);
}
"

# save as .stan file --------------------------------------------------------------------
write(correlated_model, "correlated_model.stan")

# compile model -------------------------------------------------------------------------
correlated_model_compiled <- cmdstan_model("correlated_model.stan")

# sample from the posterior -------------------------------------------------------------
fit <- correlated_model_compiled$sample(
  data = data_stan,
  parallel_chains = 4,
  chains = 4,
  iter_warmup = 2000,
  iter_sampling = 1000,
  refresh = 50,
  output_dir = "YOUR OUTPUT DIRECTORY",
  output_basename = "correlated_model"
)

# selection of judges -------------------------------------------------------------------
n_sample <- 6
j_sample <- c(37,2,22, 56, 25, 8)

# assemble results ----------------------------------------------------------------------
posterior_files <- c("correlated_model-1.csv", "correlated_model-2.csv",
                     "correlated_model-3.csv", "correlated_model-4.csv")
vi_alpha <- posterior_files %>%
  purrr::map_dfr(importCmdstanPosterior, 
                 parameter = paste0("beta.", 1:81, ".1")) %>%
  select(all_of(paste0("beta.", j_sample, ".1"))) %>%
  pivot_longer(everything(), values_to = "alpha") %>%
  mutate(iter = 1:n()) %>%
  mutate(name = str_remove(name, ".1$"))
vs_beta <- posterior_files %>%
  purrr::map_dfr(importCmdstanPosterior, 
                 parameter = paste0("beta.", 1:81, ".2"))  %>%
  select(all_of(paste0("beta.", j_sample, ".2"))) %>% # beta 1 and 2 are intercept and slope
  pivot_longer(everything(), values_to = "beta") %>%
  mutate(iter = 1:n()) %>%
  mutate(name = str_remove(name, ".2$"))
vivs <- vi_alpha %>%
  left_join(vs_beta, by = c("name", "iter")) %>%
  group_by(name) %>%
  summarize(d_non_black = invlogit(mean(alpha)),
            d_black = invlogit(mean(alpha + beta))) %>%
  rename(judge = name) %>%
  mutate(judge = str_extract(judge, "[[:digit:]]+")) %>%
  pivot_longer(-judge, names_to = "x", values_to = "y") %>%
  mutate(x = factor(x, levels = c("d_non_black", "d_black")))

# visualize -----------------------------------------------------------------------------
plot_c <- ggplot(data = vivs) +
  geom_line(aes(x = x, y = y, group = factor(judge))) +
  theme_bw()
ggsave("plot_c.pdf", plot_c, width = 9, height = 6, 
       dpi = 1200, device = cairo_pdf)

# assess correlation between varying intercepts and slopes ------------------------------
correlation <- posterior_files %>%
  purrr::map_dfr(importCmdstanPosterior, 
                 parameter = paste0("Omega.2.1")) 

plot_d <- ggplot(data = correlation) +
  geom_histogram(aes(x = Omega.2.1), color = "white", binwidth = 0.05) +
  theme_bw()
ggsave("plot_d.pdf", plot_d, width = 9, height = 6, 
       dpi = 1200, device = cairo_pdf)
