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

# WE HAVE A NEW CUSTOM FUNCTION IN THE FUNCTIONS FILE
# CONJURESUP A PART OF THE META AND GIVES US NEW METRICS



#### MONITOR PROGRESS ===================================================================

# simple model --------------------------------------------------------------------------
posterior_files <- c("simple_model-1.csv", "simple_model-2.csv", 
                     "simple_model-3.csv", "simple_model-4.csv")
alpha_sample <- paste0("alpha.", sample(1:81, 4))
posterior <- posterior_files %>%
  purrr::imap_dfr(~{
    posterior <- monitorChains(.x, parameters = alpha_sample) %>%
      mutate(chain = .y)
  }) %>%
  group_by(chain) %>% 
  mutate(iter = 1:n()) %>% 
  select(chain, iter, everything()) %>%
  pivot_longer(-c(chain, iter), names_to = "parameter") 

ggplot(data = posterior) +
  geom_line(aes(x = iter, y = value, color = as.factor(chain))) +
  facet_wrap(~parameter, scales = "free")


metrics <- posterior_files %>%
  purrr::imap_dfr(~{
    posterior <- monitorChains(.x, parameters = starts_with(c("lp", "accept_stat_", "stepsize", "treedepth", 
                                                  "n_leapfrog", "divergent", "energy", alpha_sample))) %>%
      mutate(chain = .y)
  })

# complex model -------------------------------------------------------------------------
posterior_files <- c("complex_model-1.csv", "complex_model-2.csv", 
                     "complex_model-3.csv", "complex_model-4.csv")
alpha_sample <- paste0("alpha.", sample(1:81, 4))
posterior <- posterior_files %>%
  purrr::imap_dfr(~{
    posterior <- monitorChains(.x, parameters = alpha_sample) %>%
      mutate(chain = .y)
  }) %>%
  group_by(chain) %>% 
  mutate(iter = 1:n()) %>% 
  select(chain, iter, everything()) %>%
  pivot_longer(-c(chain, iter), names_to = "parameter") 

ggplot(data = posterior) +
  geom_line(aes(x = iter, y = value, color = as.factor(chain))) +
  facet_wrap(~parameter, scales = "free")


metrics <- posterior_files %>%
  purrr::imap_dfr(~{
    posterior <- monitorChains(.x, parameters = starts_with(c("lp", "accept_stat_", "stepsize", "treedepth", 
                                                              "n_leapfrog", "divergent", "energy", alpha_sample))) %>%
      mutate(chain = .y)
  })



# LOOKING AT THE LP (LOG PROB), WE SHOULD SEE NEGATIVE NUMBERS THAT GET LARGER AND THEN STABILIZE
# ACCEPT STAT IS THE ACCEPTANCE RATE OF THE SAMPLER. WE DONT RLY LOOK AT THIS
# STEP SIZE SHOULDNT BE 1
#TREEDEPTH IS ABOUT LEAPFORG INTEGRATION, BUT ISNT INTERESTING FOR OUR PURPOSES
#TREEDPETH PARAMTERE IN THE SAMPLE COMMAND; DEFAULT IS 9, NEVER INCREASE ABOUT 12. SOMETIMES PUTTING IT AT 10 FIXES ISSUES
# BASICALLY JUST GIVES THE MODEL 'MORE TIME' TO EXPLORE THINGS


# broken model --------------------------------------------------------------------------

posterior_files <- c("broken_model-1.csv", "broken_model-2.csv", 
                     "broken_model-3.csv", "broken_model-4.csv")
alpha_sample <- paste0("alpha.", sample(1:81, 4))
posterior <- posterior_files %>%
  purrr::imap_dfr(~{
    posterior <- monitorChains(.x, parameters = "mu") %>%
      mutate(chain = .y)
  }) %>%
  group_by(chain) %>% 
  mutate(iter = 1:n()) %>% 
  select(chain, iter, everything()) %>%
  pivot_longer(-c(chain, iter), names_to = "parameter") 

ggplot(data = posterior) +
  geom_line(aes(x = iter, y = value, color = as.factor(chain))) +
  facet_wrap(~parameter, scales = "free")


metrics <- posterior_files %>%
  purrr::imap_dfr(~{
    posterior <- monitorChains(.x, parameters = starts_with(c("lp", "accept_stat_", "stepsize", "treedepth", 
                                                              "n_leapfrog", "divergent", "energy", alpha_sample))) %>%
      mutate(chain = .y)
  })




