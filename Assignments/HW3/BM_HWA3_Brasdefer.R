
#### HOMEWORK ASSIGNMENT 3 ====================================================================
# BAYESIAN MODELING
# Homework Assignment 3
# Instructor: Sascha Göbel
# May 2025


# In this assignment, you will work with data from the Florida voter record (voter_data). 
# For each voter, identified by the voter ID (voter), the dataset includes some demographic 
# information (sex, age_group, race, county), the voter’s party preference (party), and whether 
# the voter turned out to vote at a specific election, i.e., cast a vote in the election (1) 
# or abstained (0, NA means not eligible to vote). 
# Indicators for turnout are available from 2006 to 2018 for 
# primary (voted_pri_2006 to voted_pri_2018) and 
# general elections (voted_gen_2006 to voted_gen_2018) 

# You are going to rely on this dataset, specifically 
# the observed individual participation in several elections of different types and years, 
# to measure voters underlying propensity to exercise their voting rights, 
# i.e., the individual voting propensity using a Bayesian hierarchical regression model. 

# script prep --------------------------------------------------------------------------

# directory package
library(here)
library(tibble)


# working directory
here::i_am("Assignments/HW3/BM_HWA3_Brasdefer.R")

# bayesian packages
source(here("Lectures/Code-20250423/packages.r")) 

# custom functions
source(here("Lectures/Code-20250423/functions.r")) 



# TASK 1, Model Choice --------------------------------------------------------------------------------

# Given the (voter_data) and your goal to estimate individual voting propensities, 
# decide on the appropriate reflective measurement model. 
# Explain your choice of measurement model. 
# Write the model down formally, fully specifying the likelihood and all priors. 
# Interpret and explain the meaning of the core parameters of your model, 
# i.e., those that make up the linear predictor, in the context of your measurement goal. 
# Upload the answers to this task in a .pdf file.

 
# TASK 2, Import, Process, Specify, Compile, Sample --------------------------------------------------------------------------------

# In R, import the voter_data dataset. Reshape the data to align with 
# the requirements of the model you specified in task 1, then process 
# the dataset in such a way that you can use it for full Bayesian inference with Stan. 
# Next, specify the full Stan program that corresponds to your measurement model, 
# store the Stan file on your computer and compile your Stan program using the cmdstanr R package. 

# Given the Stan program and data you prepared, sample from the posterior. 
# Estimates should be directly stored on your computer.


# load data
voter_data <- readRDS(here("Assignments/HW3/voter_data"))

# massage data
voter_long <- voter_data %>%
  select(-sex, # drop cols that are not elections and voters
         -age_group,  # aka get rid of indicators we won't be using
         -race,
         -party,
         -county) %>% 
  pivot_longer(cols = -voter, # make df long instead of wide
               names_to  = "election",
               values_to = "y") %>% 
  mutate(y  = as.integer(y), # make sure our columns are integers
         election_id = as.integer(factor(election)),  # elections i:I
         voter_id = as.integer(factor(voter))) %>%  # voters j:J
  filter(!is.na(y), # we know that NAs are people who were not eligible to vote
         !is.na(election_id))


data_stan <- list(  
  N  = nrow(voter_long),                           # total observations
  I  = length(unique(voter_long$election_id)),     # number of elections
  J  = length(unique(voter_long$voter_id)),        # number of voters
  ii = voter_long$election_id,                     # index for each indicator (elections)
  jj = voter_long$voter_id,                        # index for each subject (voter
  y  = voter_long$y                                # voter turned out to vote yn (0/1)
) 

# stan program 
twopl_irt_model <- "
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
 "



# save as .stan file
twopl_irt_model %>%
  write(file = here("Assignments/HW3/twopl_irt_model.stan"))

# compile model
twopl_irt_model_compiled <- cmdstan_model(here("Assignments/HW3/twopl_irt_model.stan"))


# sample from the posterior
fit <- twopl_irt_model_compiled$sample(
  data = data_stan,
  seed = 42, # the answer to everything
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 2000,
  iter_sampling = 1000,
  refresh = 50,
  output_dir = here("Assignments/HW3"),
  output_basename = "twopl_irt_model_compiled"
)

fit %>%
  saveRDS(here("Assignments/HW3/fit_twopl_irt.rds"))






# TASK 3, Visualize Predicted Values, Propensity --------------------------------------------------------------------------------

# Based on the posterior estimates for the parameters of your model, 
# visualize the predicted probability of turnout/casting a vote along the
# full scale of potential values of voting propensities. 
# Prepare the predictions for each indicator. 

# You do not need to include estimation uncertainty in this visualization. 
# Posterior means or medians suffice.



# import and inspect posterior estimates of parameters
posterior_files <- c(paste0(here("Assignments/HW3/twopl_irt_model_compiled-1.csv")), 
                     paste0(here("Assignments/HW3/twopl_irt_model_compiled-2.csv")), 
                     paste0(here("Assignments/HW3/twopl_irt_model_compiled-3.csv")),
                     paste0(here("Assignments/HW3/twopl_irt_model_compiled-4.csv")))

# look at alpha, our intercepts across indicators
alpha_estimates <- posterior_files %>%
  purrr::map_dfr(importCmdstanPosterior, # custom function
                 parameter = "alpha") 
# each column is the estimates for alpha for each indicator (election)

# calculate mean estimate for each indicator and create new df to hold them
alpha_means <- alpha_estimates %>%
  colMeans() %>% # up to here, returns a Named Num object, which we then turn to transposed df
  enframe(name = "indicator", # enframe from tibble package
          value = "alpha_mean") %>%
  mutate(item = as.integer(str_extract(indicator, "\\d+"))) %>% # grab indicator number for future reference
  select(item, alpha_mean)

# look at beta, our slopes across indicators
beta_estimates <- posterior_files %>%
  purrr::map_dfr(importCmdstanPosterior, # custom function
                 parameter = "beta") 

# calculate mean estimate for each indicator and create new df to hold them
beta_means <- beta_estimates %>%
  colMeans() %>% # up to here, returns a Named Num object, which we then turn to transposed df
  enframe(name = "indicator", # enframe from tibble package
          value = "beta_mean") %>%
  mutate(item = as.integer(str_extract(indicator, "\\d+"))) %>% # grab indicator number for future reference
  select(item, beta_mean)


# look at eta, our propensities across voters
eta_estimates <- posterior_files %>%
  purrr::map_dfr(importCmdstanPosterior, 
                 parameter = "eta") 

# some quick histogram viz
#eta_estimates[1,] %>% unlist() %>% hist()
#eta_estimates %>% colMeans() %>% hist()



# build a range of η values using subject estimates
eta_values <- eta_estimates %>% unlist()
eta_range <- range(eta_values)
eta_seq <- seq(eta_range[1], eta_range[2], length.out = 200)

# construct item–response curves
item_response_curves <- tidyr::crossing(item = alpha_means$item, eta = eta_seq) %>%
  left_join(alpha_means, by = "item") %>%
  left_join(beta_means,  by = "item") %>%
  mutate(prob = plogis(beta_mean * (eta - alpha_mean)))

# plot IRT curves
ggplot(item_response_curves, aes(x = eta, y = prob, color = factor(item))) +
  geom_line(size = 1, linewidth = 0.3) +
  labs(
    x      = expression("Latent Trait, Voting Propensity (" * eta * ")"),
    y      = "Predicted P(Vote = 1)",
    color  = "Election ID",
    title  = "2PL Item Response Curves"
  ) +
  theme_minimal()
ggsave(here("Assignments/HW3/twopl_irt_curves.png"))



# TASK 4, Graphical Depiction of Reflective Relationships -----------------------------------------

# You want to extend your measurement model by incorporating all the other variables 
# that are available in voter_data, i.e., the voter demographics and party preferences. 
# Prepare a graphical depiction of your measurement model that shows the relationship 
# between the latent trait, the indicators, and the additional data that enters the model. 
# A simple sketch on paper, scanned and uploaded as a .pdf file, suffices.








