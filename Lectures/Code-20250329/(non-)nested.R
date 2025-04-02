# ---------------------------------------------------------------------------------------
# BAYESIAN MODELING
# Session 7: Non-nested and nested models
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


#### NESTED STRUCTURE ===================================================================

# format data for stan ------------------------------------------------------------------
sentencing_data <- sentencing_data %>%
  mutate(sentence = as.integer(sentence),
         d_black = as.integer(d_black),
         j_id = as.integer(as.factor(j_id)), # maps back to 1 automatically; index; it's not a predictor!
         c_id = as.integer(as.factor(c_id))) %>%
  arrange(c_id, j_id)

# prepare data for stan -----------------------------------------------------------------
sentence_level_data <- sentencing_data %>%
  select(sentence, d_black, j_id) %>%
  arrange(j_id)
judge_level_data <- sentencing_data %>%
  distinct(j_id, .keep_all = TRUE) %>%
  select(j_id, c_id) %>%
  arrange(j_id)
data_stan <- list(N = nrow(sentence_level_data),
                  J = length(unique(sentence_level_data$j_id)),
                  L = length(unique(judge_level_data$c_id)), # note L
                  jj = sentence_level_data$j_id,
                  ll = judge_level_data$c_id,
                  y = sentence_level_data$sentence,
                  x = sentence_level_data$d_black)

# stan program --------------------------------------------------------------------------
nested_model_pp <- "// NESTED MODEL WITH PARTIAL POOLING MODEL TO OVERALL MEAN
// data block is same idea as before
// we just add an extra bit for the court indices
data {
  int<lower=1> N;                     // Number of observations
  int<lower=1> J;                     // Number of judges
  int<lower=1> L;                     // Number of courts
  array[N] int<lower=1, upper=J> jj;  // Judge ID
  array[J] int<lower=1, upper=L> ll;  // Court ID
  vector<lower=0, upper=1>[N] x;      // Predictor - d_black
  array[N] int<lower=0, upper=1> y;   // Outcome - sentence
}

parameters {
// similarly, we add vector for gamma
// beta is still just a slope without varying slopes
  vector[J] alpha;    // varying intercept for judges
  vector[L] gamma;    // varying intercept for courts
  real beta;          // slope for predictor variable - d_black
  real mu_gamma;      // average across courts/overall average
  real<lower=0> sigma_alpha;   // between-judge variation
  real<lower=0> sigma_gamma;   // between-court variation
}

// PRIORS IS WHERE THINGS GET NEW
// REMEMBER YOU SHOULD READ FROM BOTTOM UP
model {
  // priors
  mu_gamma ~ student_t(3, 0, 1);
  sigma_alpha ~ student_t(3, 0, 1);
  sigma_gamma ~ student_t(3, 0, 1);
  gamma ~ student_t(3, mu_gamma, sigma_gamma);
  // NOTE THAT GAMMA DOES GET A PRIOR HERE; THIS IS THE SOLID LINE IN THE GRAPH
  alpha ~ student_t(3, gamma[ll], sigma_alpha); // difference happens here
                        // for our parameter alpha, we now add gamma as its miu
                        // in this vector notation, we have to index it with ll
  // implies
  //   for (n in 1:N) {
  //     alpha[jj[n]] ~ student_t(3, gamma[ll[jj[n]]], sigma_alpha); // this is an uglier way to write it
                                                                    // but shows u what's going on
  //  }
  beta ~ student_t(3, 0, 1);
  
  // likelihood
  y ~ bernoulli_logit(alpha[jj] + beta * x);
  // NESTED MODEL, SO WE DONT ADD VARYING INTERCEPT (GAMMA) AT THIS POINT
  // Remember in the non-nested we did (alpha[j] + gamma[l])
}
"

nested_model_no_pp <- "// NESTED MODEL WITHOUT PARTIAL POOLING TO OVERALL MEAN
data {
  int<lower=1> N;                     // Number of observations
  int<lower=1> J;                     // Number of judges
  int<lower=1> L;                     // Number of courts
  array[N] int<lower=1, upper=J> jj;  // Judge ID
  array[J] int<lower=1, upper=L> ll;  // Court ID
  vector<lower=0, upper=1>[N] x;      // Predictor - d_black
  array[N] int<lower=0, upper=1> y;   // Outcome - sentence
}

parameters {
  vector[J] alpha;    // varying intercept for judges
  vector[L] gamma;    // varying intercept for courts
  real beta;          // slope for predictor variable - d_black
  real<lower=0> sigma_alpha;  // between-judge variation
  real<lower=0> sigma_gamma;   // between-court variation
}

model {
  // priors
  // NOTE THAT GAMMA DOESNT GET A PRIOR HERE
  // THIS IS THE DOTTED LINE SCENARIO
  sigma_alpha ~ student_t(3, 0, 1);
  sigma_gamma ~ student_t(3, 0, 1);
  alpha ~ student_t(3, gamma[ll], sigma_alpha);
  beta ~ student_t(3, 0, 1);
  
  // likelihood
  y ~ bernoulli_logit(alpha[jj] + beta * x);
}
"

# save as .stan file --------------------------------------------------------------------
write(nested_model_pp, "nested_model_pp.stan")
write(nested_model_no_pp, "nested_model_no_pp.stan")

# compile model -------------------------------------------------------------------------
nested_model_pp_compiled <- cmdstan_model("nested_model_pp.stan")
nested_model_no_pp_compiled <- cmdstan_model("nested_model_no_pp.stan")

# sample from the posterior -------------------------------------------------------------
fit_pp <- nested_model_pp_compiled$sample(
  data = data_stan,
  parallel_chains = 4,
  chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  refresh = 50,
  output_dir = "YOUR OUTPUT DIRECTORY",
  output_basename = "nested_model_pp"
)

fit_no_pp <- nested_model_no_pp_compiled$sample(
  data = data_stan,
  parallel_chains = 4,
  chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  refresh = 50,
  output_dir = "YOUR OUTPUT DIRECTORY",
  output_basename = "nested_model_no_pp"
)

# collect sample size per judge ---------------------------------------------------------
sample_size_per_judge <- sentence_level_data %>% 
  group_by(j_id) %>% 
  summarize(n = n())

# assemble mean posterior estimates -----------------------------------------------------
posterior_files <- c("nested_model_pp-1.csv", "nested_model_pp-2.csv", 
                     "nested_model_pp-3.csv", "nested_model_pp-4.csv")
mu_gamma_posterior_pp <- posterior_files %>%
  purrr::map_dfr(importCmdstanPosterior, 
                 parameter = "mu_gamma") %>%
  summarize(mu_gamma_mean = invlogit(mean(mu_gamma))) 
gamma_posterior_pp <- posterior_files %>%
  purrr::map_dfr(importCmdstanPosterior, 
                 parameter = "gamma") %>%
  pivot_longer(everything(), values_to = "gamma") %>%
  mutate(c_id = as.integer(str_extract(name, "[[:digit:]]+"))) %>%
  select(gamma, c_id) %>%
  group_by(c_id) %>%
  summarize(gamma_mean = invlogit(mean(gamma)), .groups = "keep") %>%
  mutate(court = factor(case_when(c_id == 1 ~ "Bridgeview",
                                  c_id == 2 ~ "Chicago",
                                  c_id == 3 ~ "Skokie")))
alpha_posterior_pp <- posterior_files %>%
  purrr::map_dfr(importCmdstanPosterior, 
                 parameter = "alpha") %>%
  pivot_longer(everything(), values_to = "alpha") %>%
  mutate(j_id = as.integer(str_extract(name, "[[:digit:]]+"))) %>%
  select(alpha, j_id) %>%
  group_by(j_id) %>%
  summarize(alpha_mean = invlogit(mean(alpha)), .groups = "keep") %>%
  left_join(judge_level_data, by = "j_id") %>%
  left_join(sample_size_per_judge, by = "j_id") %>%
  left_join(gamma_posterior_pp, by = "c_id")
posterior_files <- c("nested_model_no_pp-1.csv", "nested_model_no_pp-2.csv", 
                     "nested_model_no_pp-3.csv", "nested_model_no_pp-4.csv")
gamma_posterior_no_pp <- posterior_files %>%
  purrr::map_dfr(importCmdstanPosterior, 
                 parameter = "gamma") %>%
  pivot_longer(everything(), values_to = "gamma") %>%
  mutate(c_id = as.integer(str_extract(name, "[[:digit:]]+"))) %>%
  select(gamma, c_id) %>%
  group_by(c_id) %>%
  summarize(gamma_mean_no_pp = invlogit(mean(gamma)), .groups = "keep") %>%
  mutate(court = factor(case_when(c_id == 1 ~ "Bridgeview",
                                  c_id == 2 ~ "Chicago",
                                  c_id == 3 ~ "Skokie")))
annotations <- data.frame(court = factor(c("Bridgeview", "Bridgeview", "Bridgeview")),
                          label = c("Overall mean", "Unpooled court-level mean", "Pooled court-level mean"),
                          x = c(300, 300, 300),
                          y = c(0.38, 0.525, 0.475))

# visualize -----------------------------------------------------------------------------
ggplot() +
  geom_point(data = alpha_posterior_pp, aes(x = n, y = alpha_mean), 
                  alpha = 0.3, shape = 20, size = 0.7,
                  position=position_jitter(width=0.5)) +
  geom_hline(data = gamma_posterior_pp, aes(yintercept = gamma_mean)) +
  geom_hline(data = gamma_posterior_no_pp, aes(yintercept = gamma_mean_no_pp), linetype = "dashed") +
  geom_hline(yintercept = mu_gamma_posterior_pp$mu_gamma_mean, color = "red") +
  geom_text(data = annotations, aes(x = x, y =y, label = label)) +
   facet_wrap(~ court) +
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
  scale_y_continuous(expand = c(0.01, 0.01), breaks = seq(0.2, 0.8, 0.2)) 


#### NON-NESTED STRUCTURE ===============================================================

# import data ---------------------------------------------------------------------------
sentencing_data <- readRDS("sentencing_data")

# NOW WE OMIT COURTS (so our data is non-nested)
# AND WE TAKE INSTEAD AGE
# format data for stan ------------------------------------------------------------------
# and turn them into our group index
sentencing_data <- sentencing_data %>%
  mutate(sentence = as.integer(sentence),
         age_id = as.integer(factor(case_when(d_age <= 29 ~ "<=_29",
                                              d_age >= 30 & d_age <= 44 ~ "30_44",
                                              d_age >= 45 & d_age <= 64 ~ "45_64",
                                              d_age >= 65 ~ "65_plus"),
                         levels = c("<=_29", "30_44", "45_64", "65_plus"))),
         j_id = as.integer(as.factor(j_id))) %>%
  arrange(c_id, j_id)

# prepare data for stan -----------------------------------------------------------------
data_stan <- list(N = nrow(sentencing_data),
                  J = length(unique(sentencing_data$j_id)),
                  L = length(unique(sentencing_data$age_id)),
                  jj = sentencing_data$j_id,
                  ll = sentencing_data$age_id, #ll is no longer court; now age id
                  y = sentencing_data$sentence)

# stan program --------------------------------------------------------------------------
non_nested_model <- "// NON-NESTED MODEL WITH PARTIAL POOLING MODEL TO OVERALL MEAN
// data: no real difference compared to nested model
// just change court for age
data {
  int<lower=1> N;                     // Number of observations
  int<lower=1> J;                     // Number of judges
  int<lower=1> L;                     // Number of age groups
  array[N] int<lower=1, upper=J> jj;  // Judge ID
  array[N] int<lower=1, upper=L> ll;  // Age group
  array[N] int<lower=0, upper=1> y;   // Outcome - sentence
}

// Again, no real difference vs. non-nested
parameters {
  vector[J] alpha;    // varying intercept for judges
  vector[L] gamma;    // varying intercept for age groups (gamma no maps to age, not courts)
  real mu_alpha;      // average across judges (aka average over all data)
  real<lower=0> sigma_alpha;  // between-judge variation
  real<lower=0> sigma_gamma;   // between-age variation
  // note that beta is omitted; just to simplify tings because we are looking at x = 0 cases
  // actually even simplifies interpretation bc it's not dependant on x= 1 or 0
}

// priors are same as in notation
model {
  // priors
  mu_alpha ~ student_t(3, 0, 1);  
  sigma_alpha ~ student_t(3, 0, 1);
  sigma_gamma ~ student_t(3, 0, 1);
  gamma ~ student_t(3, 0, sigma_gamma);
  alpha ~ student_t(3, mu_alpha, sigma_alpha); // here we see that we gave alpha the miu instead of to gamma
                                                // aka the average across data is given to alpha not to gamma
  
  // likelihood
  y ~ bernoulli_logit(alpha[jj] + gamma[ll]);
}
"

# save as .stan file --------------------------------------------------------------------
write(non_nested_model, "non_nested_model.stan")

# compile model -------------------------------------------------------------------------
non_nested_model_compiled <- cmdstan_model("non_nested_model.stan")

# sample from the posterior -------------------------------------------------------------
fit <- non_nested_model_compiled$sample(
  data = data_stan,
  parallel_chains = 4,
  chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  refresh = 50,
  output_dir = "YOUR OUTPUT DIRECTORY",
  output_basename = "non_nested_model"
)

# assemble and combine posterior estimates ----------------------------------------------
# REMEMBER THAT WITH A NON NESTED MODEL WE CANT JUST PLUCK OUT PARAMETERS AND INTERPRET
posterior_files <- c("non_nested_model-1.csv", "non_nested_model-2.csv", 
                     "non_nested_model-3.csv", "non_nested_model-4.csv")
gamma_posterior <- posterior_files %>% # THIS GIVES US THE GAMMA POSTERIOR
                                        # AKA THE PARAMETERS FOR THE AGE GROUPS
  purrr::map_dfr(importCmdstanPosterior, 
                 parameter = "gamma") %>%
  pivot_longer(everything(), values_to = "gamma") %>%
  mutate(age_id = as.integer(str_extract(name, "[[:digit:]]+"))) %>%
  group_by(age_id) %>%
  mutate(iter = 1:n()) %>%
  ungroup()
alpha_posterior <- posterior_files %>% # THIS GIVES PARAMETERS FOR JUDGES
  purrr::map_dfr(importCmdstanPosterior, 
                 parameter = "alpha") %>%
  pivot_longer(everything(), values_to = "alpha") %>%
  mutate(j_id = as.integer(str_extract(name, "[[:digit:]]+"))) %>%
  group_by(j_id) %>%
  mutate(iter = 1:n()) %>%
  ungroup() %>%
  select(alpha, j_id, iter)
combined_posterior <- alpha_posterior %>% # WE COMBINE THEM!
  left_join(gamma_posterior, by ="iter", relationship = "many-to-many") %>% # NOT ALWAYS MANY TO MANY; 
                                                                      # IF INTERACTION, ITS NOT MANY TO MANY
  arrange(iter, j_id, age_id)
# JUST FOR US TO SEE THAT WE DID NOT INTERACT THEM
# AKA FOR EACH DRAW, FOR EACH JUDGE, YOU WILL HAVE THE SAME PARAMETER ACROSS AGE GROUPS


# compute group-specific predictions by marginalizing over other parameters -------------
# THIS IS HOW YOU ACTUALLY INTERPRET ONE OF THE NON-INTERACTED BUT DEPENDANT PARAMS
preds_age <- combined_posterior %>%
  group_by(iter, age_id) %>% # WE ARE INTERESTED IN AGE, SO WE GROUP BY AGE
  summarize(age_marginal = mean(alpha + gamma), .groups = "keep") %>% # THEN WE EVALUATE AT EACH AGE GROUP
                                                  # MEAN OVER ALL JUDGES
  group_by(age_id) %>% # RUNNING UP TO HERE GETS US THE 'MARGINAL' FOR EVERY AGE GROUP
                        # AND, AGAIN, ITS AVERAGED OVER ALL JUDGES
                  # WHILE THIS GETS US CLOSE TO OUR ANSWER (VARIATION BY AGE)
                  # IT GIVES US A DIFFERENT VALUE FOR EACH DRAW
                  # BUT WE WANT MORE SUMMARIZED ANSWER, SO
  summarize(age_fit = invlogit(mean(age_marginal)),
            age_lower = invlogit(quantile(age_marginal, probs = 0.025)), # SO WE DO SOME SUMMARIZATION AND THEN INVLOGIT TO TRANSFORM IT INTO A POSTERIOR
            age_upper = invlogit(quantile(age_marginal, probs = 0.975))) %>%
  mutate(age_id = fct_reorder(factor(age_id), age_fit)) # RUNNING UP TO HERE GETS YOU
                            # PREDICTIONS FOR EVERY AGE BASED ON YOUR MODEL
                            # TELLS PERCENT INCARCERATED FOR EACH AGE GROUP
                                  # WOULD BE EASY FOR US TO ESTIMATE WITHOUT ALL THIS FANCY STUFF
                                  # BUT IN CASES WHERE LITTLE DATA OR INTERACTIONS, YOU NEED ALL THIS

# AND IF WE WANT TO DO THE SAME FOR JUDGES, WE ONLY HAVE TO CHANGE THE GROUPING VAR
preds_judges <- combined_posterior %>%
  group_by(iter, j_id) %>% 
  summarize(judge_marginal = mean(alpha + gamma), .groups = "keep") %>%
  group_by(j_id) %>%
  summarize(judge_fit = invlogit(mean(judge_marginal)),
            judge_lower = invlogit(quantile(judge_marginal, probs = 0.025)),
            judge_upper = invlogit(quantile(judge_marginal, probs = 0.975))) %>%
  mutate(j_id = fct_reorder(factor(j_id), judge_fit))

# visualize -----------------------------------------------------------------------------
ggplot(data = preds_judges) +
  geom_pointrange(aes(x = judge_fit, y = factor(j_id), xmin = judge_lower, xmax = judge_upper)) +
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
  ylab("Judges") +
  xlab("Percent incarcerated") +
  scale_x_continuous(expand = c(0.0, 0.0), breaks = seq(.2, .8, .2))

ggplot(data = preds_age) +
  geom_pointrange(aes(x = age_fit, y = factor(age_id), xmin = age_lower, xmax = age_upper)) +
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
  ylab("Age groups") +
  xlab("Percent incarcerated") +
  scale_x_continuous(expand = c(0.0, 0.0), breaks = seq(.2, .8, .2))
