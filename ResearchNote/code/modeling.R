# actual Stan code

# packages
library(tidyverse)
library(here)
library(brms)
library(tidybayes)
library(patchwork)
library(bayesplot)



# 1 - Import data and Process for Stan --------------------------------------------------------------------------------

# import and clean data
source(here("ResearchNote/code/cleaning_ANES.R"))

# process for Stan
survey_processed <- survey_clean %>% 
  select(repvote_yn,
         income_level,
         leftright_self,
         therm_dems,
         therm_reps,
         attention_pol) %>%
  mutate(repvote_yn = as.integer(repvote_yn), # Outcome, binary
         income_level = as.integer(as.factor(income_level)),  # Grouping variable: 1-11 ordinal
         leftright_self = as.integer(leftright_self), # Covariate one: 0-10  ordinal
         therm_dems = as.numeric(therm_dems),         # Covariate two: 0-100  continuous
         therm_reps = as.numeric(therm_reps),         # Covariate three: 0-100  continuous
         attention_pol = as.integer(attention_pol)    # Group-level covariate: 1-5 ordinal (avg attention to politics per income group)
  )




# 2 - BRMS Model Specifications  -----------------------------------------------------------


# base model formula (no correlation, no group-level regressor)
model_formula <- bf(
  repvote_yn ~ 1 + leftright_self + therm_dems + therm_reps +
    (1 + leftright_self + therm_dems + therm_reps || income_level)
)


# define priors
priors <- c(
# covariates 
  prior(normal(0.05, 0.05), class = "b", coef = "leftright_self"),
  prior(normal(-0.03, 0.05), class = "b", coef = "therm_dems"),
  prior(normal(0.05, 0.05), class = "b", coef = "therm_reps"),
# global intercept
  #prior(student_t(3, 0, 2.5), class = "Intercept"), # Gelman recommended, BUT BAD
                          # PRIOR PRED SHOWS: this prior allows the global intercept (log-odds) to vary widely, and in logistic regression, that quickly saturates to π ≈ 0 or 1.
  prior(normal(0, 1), class = "Intercept"), # this one is better

# varying intercepts
  prior(exponential(1), class = "sd")   # for all group-level standard deviations
# varying slopes
  #prior(exponential(1), class = "sd", coef = "leftright_self"), # implied
  #prior(exponential(1), class = "sd", coef = "therm_dems"), # implied
  #prior(exponential(1), class = "sd", coef = "therm_reps") # implied
)

# group-level regression on varying intercepts (attention_pol as group-level covariate)
group_level_data <- survey_processed %>%
  group_by(income_level) %>%
  summarise(attention_pol_mean = mean(attention_pol, na.rm = TRUE))

# merge back group-level covariate to the main data
survey_prepped <- left_join(survey_processed, group_level_data, by = "income_level")




# Optional: if you include correlation later
# prior(lkj(2), class = "cor")

# 3 - Prior Predictive Checks --------------------------------------------
prior_check_model <- brm(
  formula = bf(
    repvote_yn ~ 1 + leftright_self + therm_dems + therm_reps +
      (1 + leftright_self + therm_dems + therm_reps || income_level)
  ),
  data = survey_prepped,
  family = bernoulli(link = "logit"),
  prior = priors,
  sample_prior = "only",  # <--- KEY LINE
  chains = 4, iter = 2000, seed = 42
)

# sanity check
# Simulate prior draws
set.seed(123)
n_draws <- 10000
# b1 <- rnorm(n_draws, 0.02, 0.03)
# b2 <- rnorm(n_draws, -0.01, 0.03)
# b3 <- rnorm(n_draws, 0.02, 0.05)
# intercept <- rnorm(n_draws, 0, 0.7)
b1 <- rnorm(n_draws, 0.05, 0.05)
b2 <- rnorm(n_draws, -0.03, 0.05)
b3 <- rnorm(n_draws, 0.05, 0.05)
intercept <- rnorm(n_draws, 0, 1)

# Assume predictor values = ±5 (after scaling up)
eta <- intercept + b1*5 + b2*5 + b3*5
hist(plogis(eta), breaks = 50)

# 3.1 Plot Prior Predictive Checks ---------------------------------------
# Posterior predictive checks using *prior* draws
pp_check(prior_check_model, ndraws = 100)

plot(prior_check_model)  # View implied priors on parameters
conditional_effects(prior_check_model)  # See how covariates behave under priors



# 4 - Model Fit ----------------------------------------------------------
fit_repvote <- brm(
  formula = model_formula,
  data = survey_prepped,
  family = bernoulli(),
  prior = priors,
  chains = 4,
  cores = 4,
  warmup = 1000,
  iter = 3000,
  refresh = 50,
  control = list(adapt_delta = 0.95, max_treedepth = 15),
  seed = 42)

saveRDS(fit_repvote, file = here("ResearchNote/data/fit_repvote_model.rds"))

# took 7 mins... crazy fast actually
 
# 5 - Chain Diagnostics ---------------------------------------------------
fit_repvote <- readRDS(here("ResearchNote/data/fit_repvote_model.rds"))

### 5.1 - TracePlots----------------------------------------------------------
# Traceplots show the sampled values of a parameter over iterations for each MCMC chain

# we are looking for
# A) Well-mixed chains: All chains should appear as overlapping "fuzzy caterpillars" 
  # meaning they’ve explored the posterior space fully and evenly.
# B) No obvious trends or drift: 
  # we don’t want chains to be stuck in one region or slowly drifting.
# C) No divergence between chains: 
# If different chains stay in different regions, it signals poor mixing or non-convergence.

# steps:
# 1 - extract posterior draws
posterior_samples <- as_draws_array(fit_repvote)

# 2 - traceplot for specific parameters, covariates
mcmc_trace(posterior_samples, pars = c("b_leftright_self", 
                                       "b_therm_dems", 
                                       "b_therm_reps")) +
  labs(title = "Traceplot for Chain Convergence",
       subtitle = "Diagnostics of Variance, Convergence, Trends for Fixed Effects") 
ggsave(here("ResearchNote/outputs/diagnostics/traceplot_fixed_effects.png"), width = 14, height = 6, dpi = 300)

#trace plots looks fantastic; we have:
# A) Well-mixed chains (overlapping chains, "fuzzy caterpillars")
      # meaning they’ve explored the posterior space fully and evenly.
# B) No obvious trends or drift: 
      # our chains are not stuck in one region or slowly drifting.
# C) No divergence between chains: 
      # are chains are not going to different regions, so we have good mixing AND convergence.



# 5.2 - RHat --------------------------------------------------------------
# R-Hat compares within-chain and between-chain variability
# When all chains have converged to the same posterior distribution, R-hat ≈ 1.00.

# R-hat ≈ 1.00 is Good.
# R-hat > 1.01  needs attention, suggests  non-convergence or poor mixing
# R-hat > 1.1 is definitely problematic

summary(fit_repvote)
# note that this is the group-level (random effects) summary
# so sd is standard deviations of the varying effects across income levels
# and cor tells correlations among those random effects

# Multilevel Hyperparameters:
#   ~income_level (Number of levels: 11) 
#                       Estimate Est.Error l-95% CI u-95%CI Rhat 
# sd(Intercept)          0.29      0.17     0.02     0.70   1.00     
# sd(leftright_self)     0.27      0.12     0.06     0.55   1.00
# sd(therm_dems)         0.86      0.23     0.53     1.40   1.00
# sd(therm_reps)         0.79      0.21     0.48     1.30   1.00

# Regression Coefficients:
#                Estimate   Est.Error l-95% CI u-95% CI     Rhat 
# Intercept         -0.67      0.14    -0.96    -0.39       1.00     
# leftright_self     0.12      0.05     0.01     0.22       1.00     
# therm_dems        -0.06      0.05    -0.16     0.03       1.00     
# therm_reps         0.08      0.05    -0.01     0.19       1.00

# looks like all our Rhats are == 1 
# which is great
# it means all of our chains have converged to the same posterior distribution


# 5.3 - ESS (Effective Sample Size) ---------------------------------------
# ESS tells us how many independent draws we effectively have from the posterior, 
    # accounting for autocorrelation in chains.
    # When using MCMC, samples are not independent due to autocorrelation. 
    # So while you might have drawn 4,000 samples per chain, 
    # the actual number of "effective" independent samples is lower.
# in other words: 
  # "how much usable information are we actually getting from the posterior samples"


# There are two types:
    # Bulk ESS: Related to the main body (bulk) of the posterior.
    # Tail ESS: Related to the tails of the posterior distribution 
        # which is important for credible intervals
# Rules of thumb:
    # ESS > 400 for each parameter is typically acceptable.
    # Low ESS = high autocorrelation or poor mixing 
    #         = unreliable estimates
    # <100 is concerning, especially if concentrated in important parameters
    # Tail ESS < 100 means posterior intervals may be unstable

# Multilevel Hyperparameters:
#   ~income_level (Number of levels: 11) 
#                   Bulk_ESS  Tail_ESS
# sd(Intercept)        2197     2333
# sd(leftright_self)   1848     2057
# sd(therm_dems)       1357     2789
# sd(therm_reps)       1394     3194

# Regression Coefficients:
#                     Bulk_ESS  Tail_ESS
# Intercept              4760     5050
# leftright_self         3245     4094
# therm_dems             7750     5984
# therm_reps             7536     6106

# looks like all of our estimated parameters have both
# BULK and TAIL ESS greater than 400
# means we have a healthy amount of independent draws 




# 6 - Posterior Summaries ---------------------------------------------------------
# 6.1 - simple estimate check -------------------------------------------------
summary(fit_repvote)

# Regression Coefficients:
#                Estimate   Est.Error l-95% CI u-95% CI     Rhat 
# Intercept         -0.67      0.14    -0.96    -0.39       1.00     
# leftright_self     0.12      0.05     0.01     0.22       1.00     
# therm_dems        -0.06      0.05    -0.16     0.03       1.00     
# therm_reps         0.08      0.05    -0.01     0.19       1.00 
# we see that 
# global intercept is -0.67, meaning that people are not
    # very likely to vote for republicans
    # that's fine; makes sense with the fact that the dataset includes independent voting too
    # and that it was crazy skewed towards democrats for some reason (bad surveying?)
# leftright self has a small effect, likely because its dist is so normal?
    # maybe also because of how many centrists there are
# therm dems has the expected negative prediciton ability
    # interestingly, the confidence interval includes 0, which is not fantastic
    # means that there's a chance that coefficient on dems is actually positive...
# therm reps has similar concerns as the dem therm, being that it includes 0 in its CI
# OVERALL: the fact that these coefficients are super small is
    # potentially explained by how much larger they are by income group
    # ie: we were correct to make varying intercepts and varying slopes
    # since it seems that actually groups vary a lot in both directions

# Multilevel Hyperparameters:
#   ~income_level (Number of levels: 11) 
#                       Estimate Est.Error l-95% CI u-95%CI Rhat 
# sd(Intercept)          0.29      0.17     0.02     0.70   1.00     
# sd(leftright_self)     0.27      0.12     0.06     0.55   1.00
# sd(therm_dems)         0.86      0.23     0.53     1.40   1.00
# sd(therm_reps)         0.79      0.21     0.48     1.30   1.00
# we see that intercept has deviation of about half of its global effect size
# left right has more than double the deviation of its effect size
# dem and rep have unbelievably large deviation compared to the pop average coeff
      # tells that varying intercepts was an important decision for these variables



# 6.2 - Visualization of estimates -------------------------------------------------
# extract the posterior draws for deeper inspection:
post <- as_draws_df(fit_repvote)

# visualize central estimates and intervals:
# all three covs
mcmc_areas(
  posterior::subset_draws(post, variable = c("b_leftright_self", "b_therm_dems", "b_therm_reps")),
  pars = c("b_leftright_self", "b_therm_dems", "b_therm_reps"),
  prob = 0.95) +
  labs(title = "Posterior Draws - Covariate Population Estimates",
       subtitle = "Global Estimates and Intervals for Covariates") 
ggsave(here("ResearchNote/outputs/posterior/posteriorsummaries_covariates.png"), width = 14, height = 6, dpi = 300)



# visualize sd for each covaritate
mcmc_areas(
  posterior::subset_draws(post, variable = c("sd_income_level__Intercept", "sd_income_level__leftright_self", 
                                             "sd_income_level__therm_dems", "sd_income_level__therm_reps")),
  pars = c("sd_income_level__Intercept", "sd_income_level__leftright_self", 
           "sd_income_level__therm_dems", "sd_income_level__therm_reps"),
  prob = 0.95) +
  labs(title = "Posterior Draws - Standard Deviations",
       subtitle = "S.D. Across Group-Level (Income) Estimates") 
ggsave(here("ResearchNote/outputs/posterior/posteriorsummaries_stdevs.png"), width = 14, height = 6, dpi = 300)


# POTENTIALLY THE GRAPH TO SHOW
# income_level parameters - old
fit_draws <- fit_repvote %>%
  spread_draws(r_income_level[income_level, term]) %>%
  filter(income_level %in% c(1:11),  # pick income groups to focus on
         term %in% c("Intercept", "leftright_self", "therm_dems", "therm_reps"))





# Inference: Marginalized Prediction -------------------------------------
posterior_summary <- apply(preds, 2, function(x) {
  c(mean = mean(x), 
    lower = quantile(x, 0.25), 
    upper = quantile(x, 0.75))
}) %>% 
  t() %>% 
  as.data.frame() %>% 
  bind_cols(survey_prepped) %>%
  rename(lower25 = "lower.25%",
         upper75 = "upper.75%")


posterior_summary %>%
  ggplot(aes(x = factor(income_level), y = mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower25, ymax = upper75 ), width = 0.2) +
  labs(title = "P(Republican Vote) by Income Group",
       x = "Income Group", y = "Posterior Mean Probability") +
  theme_minimal()







# Inference: Conditional Prediction -------------------------------------

# New data example: 5 income levels, constant values for predictors
new_data <- expand.grid(
  income_level =  c(1:11),
  leftright_self = 5,
  therm_dems = 0.2,
  therm_reps = 0.8,
  attention_pol_mean = 3
)

# Generate posterior predicted probabilities
preds <- fitted(fit_repvote, newdata = new_data, scale = "response", summary = FALSE)

# Summarize posteriors per row (e.g. each income group)
posterior_summary <- apply(preds, 2, function(x) {
  c(mean = mean(x), 
    lower = quantile(x, 0.25), 
    upper = quantile(x, 0.75))
}) %>% 
  t() %>% 
  as.data.frame() %>% 
  bind_cols(new_data) %>%
  rename(lower25 = "lower.25%",
         upper75 = "upper.75%")







# POL ATTENTION Explanatory Regressor --------------------------------------------------

# dont know if this works
# straight from gpt
# change var names for sure
ranefs <- ranef(fit)$income_level[, , "Intercept"]
plot_data <- data.frame(
  income_level = rownames(ranefs),
  intercept_estimate = ranefs[, "Estimate"],
  attention_pol_mean = group_level_data$attention_pol_mean
)

# Plot estimated intercepts against group-level attention
ggplot(plot_data, aes(x = attention_pol_mean, y = intercept_estimate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Average Political Attention (Group)", y = "Estimated Intercept (Voting Republican)")



# CHECK - Estimates vs simple filtering summary stats ------------------------------------
# Get posterior predictive probabilities for all rows
post_probs <- posterior_epred(fit_repvote, newdata = survey_prepped)

# Now summarize by income_level
avg_preds_by_income <- survey_prepped %>%
  mutate(mean_prob = colMeans(post_probs)) %>%
  group_by(income_level) %>%
  summarise(mean = mean(mean_prob),
            lower25 = quantile(mean_prob, 0.25),
            upper75 = quantile(mean_prob, 0.75))

temp <- survey_prepped %>%
  select(income_level,
         repvote_yn) %>%
  group_by(income_level) %>%
  summarize(vote_pct = mean(repvote_yn))

temp2 <- temp %>%
  left_join(avg_preds_by_income, by = "income_level")





# CHECK - Distribution of LeftRight for Republican voters, by Income ------------------------------------
temp3 <- survey_prepped %>%
  filter(repvote_yn == 1)

plots <- lapply(1:11, function(i) {
  ggplot(temp3 %>% filter(income_level == i), aes(x = leftright_self)) +
    geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
    ggtitle(paste("Income Level", i)) +
    theme_minimal()
})

wrap_plots(plots, ncol = 3)
ggsave(here("ResearchNote/outputs/pre_leftrigh_byincome.png"), width = 14, height = 10, dpi = 300)




# quick plots for priors ---------------------------------------------------------------------
# leftright
survey_prepped %>%
  ggplot( aes(x = leftright_self))+
  geom_histogram(binwidth = 1, fill = "darkgreen", color = "white") +
  ggtitle("LeftRight Dist. Whole Dataset") +
  theme_minimal()
ggsave(here("ResearchNote/outputs/pre_leftright.png"))

# dem
survey_prepped %>%
  ggplot(aes(x = therm_dems, fill = "steelblue")) +
  geom_density(alpha = 0.4, adjust = 0.5, fill = "steelblue", color = "steelblue") +
  ggtitle("Dem Therm Dist. Whole Dataset") +
  theme_minimal()
ggsave(here("ResearchNote/outputs/pre_therm_dem.png"))


# rep
survey_prepped %>%
  ggplot(aes(x = therm_reps, fill = "darkred")) +
  geom_density(alpha = 0.4, adjust = 0.5, fill =  "darkred", color =  "darkred") +
  ggtitle("Rep Therm Dist. Whole Dataset") +
  theme_minimal()
ggsave(here("ResearchNote/outputs/pre_therm_rep.png"))




# Reshape to long format
survey_long <- survey_prepped %>%
  pivot_longer(cols = c(therm_dems, therm_reps), names_to = "group", values_to = "therm")

# Overlay density plots
ggplot(survey_long, aes(x = therm, fill = group, color = group)) +
  geom_density(alpha = 0.1, adjust = 0.5) +
  scale_fill_manual(values = c(therm_dems = "steelblue", therm_reps = "darkred")) +
  scale_color_manual(values = c(therm_dems = "steelblue", therm_reps = "darkred")) +
  ggtitle("Therm Dist.: Dem and Rep") +
  theme_minimal()
ggsave(here("ResearchNote/outputs/pre_therm_dem_and_rep.png"))





