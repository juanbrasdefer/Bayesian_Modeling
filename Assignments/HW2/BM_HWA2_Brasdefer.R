
#### HOMEWORK ASSIGNMENT 2 ====================================================================
# BAYESIAN MODELING
# Homework Assignment 2
# Instructor: Sascha Göbel
# April 2025


# In this assignment, you will work with an extended version of 
# the sentencing data from session 6 and 7. 
# The dataset now includes information on sentences by 118 judges over 5 years 
# (sentencing_data2). You are going to rely on this dataset to investigate 
# the probability of incarceration/percent incarcerated across judges and years 
# using a Bayesian hierarchical regression model. 

# script prep --------------------------------------------------------------------------

# directory package
library(here)
library(posterior) # for posterior predictions later on


# working directory
here::i_am("Assignments/HW2/BM_HWA2_Brasdefer.R")

# bayesian packages
source(here("Lectures/Code-20250329/packages.r")) 

# custom functions
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




# TASK 2, IMPORT AND PROCESS --------------------------------------------------------------------------------

# In R, import the sentencing_data2 dataset. 
# Process the dataset in such a way that you can use if for 
# full Bayesian inference with Stan and assemble the data required 
# to fit the varying-intercepts model specified in Task 1. 

# How many varying intercept parameters are we estimating in total?


# import data 
sentencing_data2 <- readRDS(here("Assignments/HW2/sentencing_data2")) # load our dataframe

# format dataframe for stan
sentencing_data2_processed <- sentencing_data2 %>%
  mutate(sentence = as.integer(sentence), # binary sentence variable; encode as integer for stan
         j_id = as.integer(as.factor(j_id)), # categorical variable, so we use as.factor() to assign 
                                             # them to equally spaced levels for correct indexing
         year = as.integer(as.factor(year))) %>% # same process for year
  mutate(judge_year = as.integer(as.factor(paste0(j_id, "_", year)))) # create our new interaction variable



# let's count the number of unique intercept parameters: 
n_j_id    <- length(unique(sentencing_data2_processed$j_id)) # number of judges
n_year     <- length(unique(sentencing_data2_processed$year)) # number of years
n_judge_year <- length(unique(sentencing_data2_processed$judge_year)) # number of judge-year combinations

# interesting thing to note: it seems not every judge appears in every year,
# as 118 judges across 5 years would be 590, while our judge_year variable has only 364

# total number of varying intercept parameters that we will be estimating
sum_intercepts <- n_j_id + n_year + n_judge_year 
print(sum_intercepts) 

# gives us a total of 487 intercepts








# TASK 3, COMPILE AND SAMPLE --------------------------------------------------------------------------------

# In R, compile the data in the appropriate format for passing it to cmdstanr’s sample method 
# and specify the full Stan program that corresponds to your varying-intercepts model and
# the data you prepared. 

# Next, store the Stan file on your computer and compile your Stan program using the cmdstanr R package. 
# Given the Stan program and data you prepared, sample from the posterior. 
# Estimates should be directly stored on your computer. 
# Do not use convenience R packages, such as brms or rstanarm.


# prepare data for stan
  # note: i know we already turned our relevant columns in the df to
  # as.integer(factor()), but just in case,
  # i do it again :-))
data_stan <- list(
  N = nrow(sentencing_data2_processed),                      # number of observations
  J = length(unique(sentencing_data2_processed$j_id)),       # number of judges
  nT = length(unique(sentencing_data2_processed$year)),      # number of years (nT instead of T because R reads T as TRUE)
  JT = length(unique(sentencing_data2_processed$judge_year)),# number of unique judge-year pairs
  jj = as.integer(factor(sentencing_data2_processed$j_id)),  # judge ID (converted to integer)
  tt = as.integer(factor(sentencing_data2_processed$year)),  # year ID (converted to integer)         
  jt = as.integer(factor(sentencing_data2_processed$year)),  # judge year ID (converted to integer) 
  y = sentencing_data2_processed$sentence                    # Outcome variable: sentence (0 or 1)
)


# now we define our stan model 
# varying intercepts model! the best!
varying_intercepts_model <- "
data {
  int<lower=1> N;                     // Number of observations
  int<lower=1> J;                     // Number of judges
  int<lower=1> nT;                    // Number of years (nT instead of T because R reads T as TRUE)
  int<lower=1> JT;                    // Number of judge year pairs
  array[N] int<lower=1, upper=J> jj;  // Judge ID
  array[N] int<lower=1, upper=nT> tt; // Year ID
  array[N] int<lower=1, upper=JT> jt; // Judge Year ID
  array[N] int<lower=0, upper=1> y;   // Outcome, sentence (0/1)
}

parameters {
  real mu_alpha;                      // Mean across judges (only judges get the mu!!)
  real<lower=0> sigma_alpha;          // Standard deviation across judges
  real<lower=0> sigma_gamma;          // Standard deviation across years
  real<lower=0> sigma_delta;          // Standard deviation across judge years

  vector[J] alpha;                    // Judge-level intercepts
  vector[nT] gamma;                   // Year-level intercepts
  vector[JT] delta;                   // Judge-Year-level intercepts
}

model {
  // Hyperpriors
  mu_alpha ~ student_t(3, 0, 1);                // only judges get mu
  sigma_alpha ~ student_t(3, 0, 1) T[0.01, ];   // our sigmas were creating errors
  sigma_gamma ~ student_t(3, 0, 1) T[0.01, ];   // 'Scale parameter is 0, but must be positive!'
  sigma_delta ~ student_t(3, 0, 1) T[0.01, ];   // so we add a tiny lower bound to stop that
  
  // Priors (for the actual intercepts)
  alpha ~ normal(mu_alpha, sigma_alpha); // distributed around its hyperparameters
  gamma ~ normal(0, sigma_gamma);        // distributed around 0 and its st dev
  delta ~ normal(0, sigma_delta);        // distributed around 0 and its st dev
  
  // Likelihood
  for (n in 1:N)
  y[n] ~ bernoulli_logit(alpha[jj[n]] + gamma[tt[n]] + delta[jt[n]]);
}
"


# save as .stan file
varying_intercepts_model %>%
  write(file = here("Assignments/HW2/varying_intercepts_model.stan"))

# compile model
varying_intercepts_model <- cmdstan_model(here("Assignments/HW2/varying_intercepts_model.stan"))


# sample from the posterior
fit <- varying_intercepts_model$sample(
  data = data_stan,
  seed = 42, # the answer to everything
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  refresh = 50,
  output_dir = here("Assignments/HW2"),
  output_basename = "varying_intercepts_model"
)



# TASK 4, COMPUTE P(INCARCERATION) FOR JUDGE-YEAR -----------------------------------------

# In R, choose 
# 5 judges from the data for which we have 
# observations on all five years. 

# Using the posterior estimates for varying intercept parameters, 
# compute the probability of 
# incarceration/ percent incarcerated 
# for each of these 5 judges in every year. 
# Visualize your posterior predictions.

# choose 5 judges with obs in each of five years
five_fiveyear_judges <- sentencing_data2_processed %>%
  group_by(j_id) %>%                        # reduce to judge-level granularity
  summarise(n_years = n_distinct(year)) %>% # sum number of years appearing
  filter(n_years == 5) %>%                  # get rid of any without 5 years of appearances
  slice_sample(n = 5) %>%                   # select random rows
  pull(j_id)                                # take column as vector


# lookup table for judge-year combinations
# that we will use in the loop
lookup <- sentencing_data2_processed %>%
  distinct(j_id, year, judge_year)

# extract posterior draws into their own df
# each column represents a different parameter (alpha, gamma, delta)
# for each judge
draws_df <- as_draws_df(fit$draws())

# empty data frame to store predictions
predictions <- data.frame()

# loop structure to extract desired predictions for judge/years
# loop over five judges 
  # loop over five years
for (j in five_fiveyear_judges) {
  for (t in sort(unique(sentencing_data2_processed$year))) {
    
    # judge-year index for judge j in year t from the lookup
    jyear_index <- lookup %>% 
      filter(j_id == j, year == t) %>% 
      pull(judge_year)
    
    # extracting posterior draws for the parameters 
    alpha_draws <- draws_df[[paste0("alpha[", j, "]")]] # a little paste0 magic to fetch the correct columns
    gamma_draws <- draws_df[[paste0("gamma[", t, "]")]]
    delta_draws <- draws_df[[paste0("delta[", jyear_index, "]")]]
    
    # compute linear predictor for each draw
    linpred <- alpha_draws + gamma_draws + delta_draws # simple linear sum of paramters
    
    # convert linear predictor to prob.
    prob_draws <- plogis(linpred)
    
    # compute mean
    # and some range for a prettier graph
    mean_prob  <- mean(prob_draws)
    lower_prob <- quantile(prob_draws, 0.25)
    upper_prob <- quantile(prob_draws, 0.75)
    
    # append result to the running 'predictions' data frame
    predictions <- rbind(predictions, data.frame(
      judge = j,
      year = t,
      mean_prob = mean_prob,
      lower_prob = lower_prob,
      upper_prob = upper_prob
    ))
  }
}


# plot our judges
predictions %>%
ggplot(aes(x = year, 
           y = mean_prob, 
           color = as.factor(judge), 
           group = as.factor(judge))) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = lower_prob, ymax = upper_prob), width = 0.2) +
  labs(title = "Probability of Incarceration Sentence",
       subtitle = "Posterior Predictions for 5 Judges",
       x = "Year of Sentencing", 
       y = "Prob. of Incarceration", 
       color = "Judge") +
  scale_color_brewer(palette = "PuBuGn") +  
  theme_minimal()

# we notice that they have the same curves!!
# because we only did varying intercepts, not varying slopes!

ggsave(here("Assignments/HW2/fivejudge_posteriorpreds.png"))










# OPTIONAL BONUS TASK -----------------------------------------------------------------

# Alter your Stan program to incorporate additional intercepts 
# varying across courts (c_id).

# first i did a quick check to see if any judges belonged to more than one court
# and it looks like there WAS one judge, judge #49 
# who participated in two courts...
# not sure what that does for our discussion of nested/ strict hierarchy 
# but it sounds like this would no longer be a strict hierarchy :-((
court_check <- sentencing_data2_processed %>%
  group_by(j_id) %>% # reduce to judge-level granularity
  summarise(n_courts = n_distinct(c_id)) # find if any have more than one court

# but anyway, we return to the question:
# first, we turn our var to as.factor 
# (even though we wont actually run this model)
# (and even though we repeat this again below with data_stan oops)
sentencing_data2_processed <- sentencing_data2_processed %>%
  mutate(c_id = as.integer(as.factor(c_id)))   # just good habit-forming!

# need to boil down to judge level for the list
sentencing_d2p_judgelevel <- sentencing_data2_processed %>%
  group_by(j_id) %>%
  summarize(c_id = first(c_id)) %>% # need to do because of the one judge with 2 courts
  ungroup() 


# prepare data again for stan
data_stan <- list(
  N = nrow(sentencing_data2_processed),                      # number of observations
  J = length(unique(sentencing_data2_processed$j_id)),       # number of judges
  nT = length(unique(sentencing_data2_processed$year)),      # number of years (nT instead of T because R reads T as TRUE)
  JT = length(unique(sentencing_data2_processed$judge_year)),# number of unique judge-year pairs
  C = length(unique(sentencing_data2_processed$c_id)),       # number of courts
  jj = as.integer(factor(sentencing_data2_processed$j_id)),  # judge ID (converted to integer)
  tt = as.integer(factor(sentencing_data2_processed$year)),  # year ID (converted to integer)         
  jt = as.integer(factor(sentencing_data2_processed$year)),  # judge year ID (converted to integer) 
  c = as.integer(factor(sentencing_data2_processed$c_id)),   # court ID (converted to integer) 
  jc = as.integer(factor(sentencing_d2p_judgelevel$c_id)),   # mapping courts to judges at the judge level
  y = sentencing_data2_processed$sentence                    # Outcome variable: sentence (0 or 1)
)


# now we define our stan model 
# we add our new parameter, court id
varying_intercepts_model_c <- "
data {
  int<lower=1> N;                     // Number of observations
  int<lower=1> J;                     // Number of judges
  int<lower=1> nT;                    // Number of years
  int<lower=1> JT;                    // Number of judge year pairs
  int<lower=1> C;                     // Number of courts
  array[N] int<lower=1, upper=J> jj;  // Judge ID
  array[N] int<lower=1, upper=nT> tt; // Year ID
  array[N] int<lower=1, upper=JT> jt; // Judge Year ID
  array[N] int<lower=1, upper=C> c;   // Court ID
  array[N] int<lower=0, upper=1> y;   // Outcome, sentence (0/1)
  array[J] int<lower=1, upper=C> jc;  // Court judge mix

}

parameters {
  real mu_alpha;                      // Mean across judges (only judges get the mu!!)
  real<lower=0> sigma_alpha;          // Standard deviation across judges
  real<lower=0> sigma_gamma;          // Standard deviation across years
  real<lower=0> sigma_delta;          // Standard deviation across judge years
  real<lower=0> sigma_epsilon;        // Standard deviation across courts

  vector[J] alpha;                    // Judge-level intercepts
  vector[nT] gamma;                   // Year-level intercepts
  vector[JT] delta;                   // Judge-Year-level intercepts
  vector[C] epsilon;                  // Court-level intercepts

}


model {
  // Hyperpriors
  mu_epsilon ~ student_t(3, 0, 1);              // NOW: only courts get mu 
  sigma_alpha ~ student_t(3, 0, 1) T[0.01, ];   // our sigmas were creating errors
  sigma_gamma ~ student_t(3, 0, 1) T[0.01, ];   // 'Scale parameter is 0, but must be positive!'
  sigma_delta ~ student_t(3, 0, 1) T[0.01, ];   // so we add a tiny lower bound to stop that
  sigma_epsilon ~ student_t(3, 0, 1) T[0.01, ]; // new st dev for courts

  
  // Priors (for the actual intercepts)
  alpha ~ normal(epsilon[jc], sigma_alpha);       // distributed according to courts because of nesting
  gamma ~ normal(0, sigma_gamma);                 // distributed around 0 and its st dev
  delta ~ normal(0, sigma_delta);                 // distributed around 0 and its st dev
  epsilon ~ normal(mu_epsilon, sigma_epsilon);    // now courts is the one that gets the overall mu

  
  // Likelihood
  for (n in 1:N)
                // QUICK NOTE ON THIS LIKELIHOOD
                // normally, when adding a court-level varying intercept
                // we WOULDNT include the court intercept in the likelihood 
                // because of the fact that judges are nested inside courts
                // HOWEVER
                // as explored from line 315-322 of this script,
                // one of our judges actually belongs to TWO courts
                // meaning that there is a breach of the strict hierarchy
                // and thus we can no longer treat judge -> court as 'nested'
                // so: we include epsilon in our likelihood
                //y[n] ~ bernoulli_logit(alpha[jj[n]] + gamma[tt[n]] + delta[jt[n]] + epsilon[c[n]]);
          // NEW NOTE: apparently professor Göbel noted that we should  
          // disregard the fact that one of these judges appears in two courts
          // and just treat it like a hierarchy,
          // so in the end we WILL NOT include the court intercept in our likelihood
  
  y[n] ~ bernoulli_logit(alpha[jj[n]] + gamma[tt[n]] + delta[jt[n]]);
}
"





