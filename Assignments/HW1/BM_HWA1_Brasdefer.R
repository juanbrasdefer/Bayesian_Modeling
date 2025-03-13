
#### HOMEWORK ASSIGNMENT 1 ====================================================================
# BAYESIAN MODELING
# Homework Assignment 1: The Politics of Respectability and Black Americans’ Punitive Attitudes
# Instructor: Sascha Göbel
# March 2025


# You are going to rely on Jefferson’s data (punitive_attitudes.RData) 
# to investigate how support for sagging pants ordinances among Black Americans 
# differs along survey respondents’ RSP score, sex, and age group. 
# Sagging pants ordinances reflect a particular, discriminatory type of local policy 
# aimed at regulating the public presentation and dress of residents. 
# For this purpose, you will specify and estimate a Bayesian 
# linear regression model of the following form:

# yi ∼ Normal(Xiβ, σ) 
# σ ∼ Log-Normal(0, 1) 
# β ∼ Normal(0, 1)

# where 
# the outcome yi is the support for sagging pants policies (sagging_pants), 
# Xi represents the covariate matrix including 
  # an intercept column of 1s, 
  # respondents’ RSP score (respectability), 
  # sex (sex), 
  # and age group (age). 
# β is the corresponding coefficient vector and 
# σ is the noise term.



# script prep --------------------------------------------------------------------------

# packages
pacman::p_load(here,
               ggplot2,
               cmdstanr,
               dplyr,
               haven,
               magrittr,
               purrr,
               vroom,
               install = TRUE,
               update = FALSE)

# import custom function
source(here("Lectures/Code-20250312/functions.r")) 

# working directory
here::i_am("Assignments/HW1/BM_HWA1_Brasdefer.R")
setwd(here())





# TASK 1, DATA PREP --------------------------------------------------------------------------------

# In R, import the punitive_attitudes.RData dataset, 
# process the dataset in such a way that you can use it 
# for full Bayesian inference with Stan, 
# and compile the data in the appropriate format for passing it 
# to cmdstanr’s sample method. 
# Use whatever R functions and packages you deem appropriate for this task.


# import data 
load(here("Assignments/HW1/punitive_attitudes.RData"))

# check missing values across cols
punitive_attitudes_data %>%
  summarize(across(everything(), ~ sum(is.na(.)))) 
  # looks like we have missing values in two of our necessary columns, sex and age
  # so let's drop rows with missing values in those columns

# clean dataframe according to our needs
punitive_attitudes_clean <- punitive_attitudes_data %>% # re-assign as 'clean'
  mutate(intercept = 1) %>% # adding an intercept column of 1s
  select(sagging_pants, # select our relevant columns
         intercept,
         respectability,
         sex,
         age) %>% 
  filter(!is.na(sex),#  drop rows that have missing values 
         !is.na(age)) 

# check our age categories
punitive_attitudes_clean %>%
  group_by(age) %>%
  summarize()
#1 18_24  
#2 25_34  
#3 35_44  
#4 45_54  
#5 55_64  
#6 65_plus


# now let's change some data types for stan
punitive_attitudes_stan <- punitive_attitudes_clean %>%
  mutate(sex_binary = ifelse(sex == 'Female', 1, 0)) %>% # convert sex into num binary
  mutate(age_integer = ifelse(age == '18_24', 1, # convert age categories into int categories
                              ifelse(age == '25_34', 2,
                                     ifelse(age == '35_44', 3,
                                            ifelse(age == '45_54', 4,
                                                   ifelse(age == '55_64', 5, 6)))))) %>%
  select(-sex, # remove string columns
         -age) %>%
  mutate(sagging_pants = as.numeric(sagging_pants), # format data types for stan
         intercept = as.integer(intercept),
         respectability = as.numeric(respectability),
         sex_binary = as.integer(sex_binary),
         age_integer = as.integer(age_integer))



# given that we have more than one covariate (more than one x),
# we need to transform those dataframe columns into a matrix
# (so our good friend stan can read it)
X_matrix <- as.matrix(punitive_attitudes_stan[, c("intercept",      # cov 1 (intercept)
                                                  "respectability", # cov 2
                                                  "sex_binary",     # cov 3
                                                  "age_integer")])  # cov 4

# and finally set all our df values as objects in the stan nomenclature
data_stan <- list(N = nrow(punitive_attitudes_stan), # N is our number of rows
                  y = punitive_attitudes_stan$sagging_pants, # y is our dependent variable
                  X = X_matrix, # X is our covariance matrix of 3 covariates
                  K = ncol(X_matrix)) # K is the number of covariates (3)






# TASK 2, SPECIFY STAN PROGRAM -----------------------------------------------------------

# In R, specify the full Stan program that corresponds 
# to the above Bayesian linear regression model and the data you prepared.
# yi ∼ Normal(Xiβ, σ) 
# σ ∼ Log-Normal(0, 1) 
# β ∼ Normal(0, 1)
# Following the above notation, specify the program using matrix notation. 
# Store the Stan file on your computer. 
# Do not use convenience R packages, such as brms.



# specify stan program (as string!!)


lr_model <- "// LINEAR REGRESSION MODEL

// DATA BLOCK
data {
  int<lower=1> N;                 // Number of observations
  int<lower=1> K;                 // Number of covariates (predictors)
  matrix[N, K] X;                 // Predictors - respectability, age, sex, intercept
  vector<lower=0, upper=1>[N] y;  // Outcome - sagging_pants, bounded because %
}

// PARAMETERS BLOCK
parameters {
  vector[K] beta;             // coefficients for X; must be 'vector', not 'real'!!!!
  real<lower=0> sigma;        // bounding sigma to half-normal distribution
}                             // no alpha because our intercept is included in cov matrix

// MODEL BLOCK
model {
  // PRIORS: parameters distributed normal, centered around 0 with stdev 1
  beta ~ normal(0, 1);
  sigma ~ normal(0, 1);
  
  // LIKELIHOOD: 
  // y follows a normal dist, and miu is replaced by X * beta
  y ~ normal(X * beta, sigma);  // again: no alpha (intercept) bc it's included in X 
                                // also: X goes before beta
                                // sigma error term (st dev)
}
"

# save as .stan file 
write(lr_model, here("Assignments/HW1/lr_model_punitive.stan"))



# TASK 3 --------------------------------------------------------------------------------

# In R, compile your Stan program using the cmdstanr R package. 
# Next, given the Stan program and data you prepared, 
# sample from the posterior. In doing so, 
# specify the seed 1457L, 
# use 4 chains, 
# run in parallel, 
# with 1000 warmup and 500 sampling iterations. 
# Estimates should be directly stored on your computer. 
# Do not use convenience packages, such as brms.


# compile model 
lr_model_compiled <- cmdstan_model(here("Assignments/HW1/lr_model_punitive.stan"))

# NEED TO FIGURE OUT PARALLELLIZATION
# seems to be page 346 ish of users guide
# looks like you define parallellization in the FUNCTIONS block

# our model has attribute $sample that we can use, so:
# we sample from the posterior
fit <- lr_model_compiled$sample(
  data = data_stan, # our list of data and objects
  seed = 1457L, # set our seed
  chains = 4, # run 4 distinct chains
  parallel_chains = 4,  # run chains in parallel
  iter_warmup = 1000, # default 1000
  iter_sampling = 500, # default 1000
  refresh = 50, # how often you get an update of where the chains are
  output_dir = here("Assignments/HW1/lr_model_sampling_outputs"), # save to
  output_basename = "lr_model_punitive_run" # name
)






# TASK 4 --------------------------------------------------------------------------------

# In R, print a summary of the posterior samples. 
# Then, make use of the custom function to 
# import only the posterior for the coefficient β related to the RSP score. 
# Visualize the full posterior distribution of the β coefficient. 
# In addition to the custom function for importing posterior files, you can use 
# whatever R functions and packages you deem appropriate for this task.



# estimation has been done; now we inspect results
# basically a table with the classic regression diagnostics
#fit$summary() # is one way of looking at our results
# # can be a little nicer to look at
fit$cmdstan_summary()

# extract the posterior; then you can format as a matrix
posterior <- fit$draws(format = "df") 
#View(posterior)


# we dont care too much about log probability (lp) column
# we care about the beta and sigma columns
# which show us the parameter results of all the iterations (excluding warmups)
# you can take that and then create a histogram that shows you confidence intervals 
# simply by using quantiles

# set path to files of posteriors
posterior_files <- c(here("Assignments/HW1/lr_model_sampling_outputs/lr_model_punitive_run-1.csv"), 
                     here("Assignments/HW1/lr_model_sampling_outputs/lr_model_punitive_run-2.csv"),
                     here("Assignments/HW1/lr_model_sampling_outputs/lr_model_punitive_run-3.csv"),
                     here("Assignments/HW1/lr_model_sampling_outputs/lr_model_punitive_run-4.csv"))

# use custom function to obtain the posterior for beta from stored files

# NOTE: I had to edit the importCmdstanPosterior function
# in the functions.R file
# because the (warmup) check in the function could not handle NAs
# it seems the \\d extraction from meta[[1]][9] returned a NA, 
# which made the function explode (because it expected a digit)
# with the new alteration, the below now runs

beta_posterior_RSP <- posterior_files %>%
  purrr::map_dfr(importCmdstanPosterior, 
                 parameter = "beta",
                 incl_warmup = FALSE) %>%
  rename(beta_2_RSP = "beta.2") %>%
  select(beta_2_RSP)


# Visualize the full posterior distribution of the β coefficient. 

ggplot(beta_posterior_RSP, 
       aes(x = beta_2_RSP)) +
  geom_histogram(bins = 100, 
                 fill = "#9e59e3",
                 color = "white", 
                 alpha = 0.9) + 
  theme_minimal(base_size = 14) +  # Clean and modern theme
  labs(title = "Posterior Distribution of β 'RSP'",
       subtitle = "Beta Coefficient for 'Responsibility' Metric",
       x = "Parameter Value",
       y = "Frequency") +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.text = element_text(size = 12),
        axis.title = element_text(face = "bold"),
        panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
        panel.grid.minor = element_blank())  # Clean up minor grid lines



# OPTIONAL BONUS TASK --------------------------------------------------------------------------------

# Alter your Stan program to use the generated quantities block 
# in order to conduct a prior predictive check 
# with priors specified as above. 
# Having run the prior predictive check, 
# are the original priors for β and σ sensible in this context?





















