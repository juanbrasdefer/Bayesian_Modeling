# ---------------------------------------------------------------------------------------
# BAYESIAN MODELING
# Session 9: The common factor model
# Instructor: Sascha Göbel
# April 2025
# ---------------------------------------------------------------------------------------


#### PREPARATIONS =======================================================================

# set working directory -----------------------------------------------------------------
setwd("YOUR WORKING DIRECTORY")

# install and load packages -------------------------------------------------------------
source("packages.R")


#### PREPARE DATA =======================================================================

# paper: Hanson, Jonathan K. and Rachel Sigman 2023. 'Leviathan’s Latent Dimensions. 
#           Measuring State Capacity for Comparative Political Research'. Journal of 
#           Politics https://doi.org/10.1086/715066
# data source: https://doi.org/10.7910/DVN/IFZXQX

# import data ---------------------------------------------------------------------------
state_capacity_data <- readRDS("state_capacity")

# import reformatted data exposing its hierarchical nature ------------------------------
state_capacity_data_hierarchy <- readRDS("state_capacity_hierarchy")


#### CONFIRMATORY FACTOR ANALYSIS WITH ONE DIMENSION ------------------------------------

# Capacity: Coercive
# Capacity: Extractive
# Capacity: Administrative

# model ---------------------------------------------------------------------------------
# latent, 'capacity'
cfa_1d_model <- 'capacity  =~ taxrev_gdp + tax_trade_tax + tax_inc_tax + irai_erm +
                              v2stfisccap + milexpercap + milpercap + policecap +
                              bti_mo + v2terr + StateHist50s + irai_qbfm + irai_qpa +
                              censusfreq + infcap + wbstat'
cfa_1d_fit <- bcfa(cfa_1d_model, 
                   n.chains = 4, 
                   burnin = 1000, # burn-in is warmup
                   sample = 1000, 
                   data = state_capacity_data, # in wide format
                   bcontrol = list(cores = 4, refresh = 25),
                   save.lvs = TRUE)

# summary -------------------------------------------------------------------------------
summary(cfa_1d_fit)
# column 'estimate' tells us the strength of the specific variable in explaining our latent var
# there is no objective strength to variables so you have to relativize across them
# aka 'which one is high, which one is low'
# 'estimate' is lambda

# also they fix here their best variable at 1
# they said 'the indicator taxrev_gdp is our best indicator of latent, so we place it first
# in the model and fix it at 1'

# intercepts is 'beta'

# posterior of latent variables ---------------------------------------------------------
posterior <- blavInspect(cfa_1d_fit, what = "lvs") %>%
  purrr::map_dfr(as.data.frame)

# visualize -----------------------------------------------------------------------------
posterior_formatted <- posterior %>%
  set_names(state_capacity_data$country) %>%
  pivot_longer(everything(), names_to = "country", values_to = "capacity") %>%
  group_by(country) %>%
  summarize(lower = quantile(capacity, prob = 0.025),
            upper = quantile(capacity, prob = 0.975),
            fit = mean(capacity)) %>%
  mutate(country = fct_reorder(factor(country), fit))

country_sample <- sample(state_capacity_data$country, 20)
posterior_formatted <- posterior_formatted %>%
  filter(country %in% country_sample)

ggplot(data = posterior_formatted) +
  geom_pointrange(aes(x = fit, y = country, xmin = lower, xmax = upper)) +
  theme_bw()

# even in the graph, dont believe the fact that it's centered around zero
# it doesnt mean that some countries have 0 state capacity or negative
# it is just a relative graph (countries being relative to each other)



#### CONFIRMATORY FACTOR ANALYSIS WITH MULTIPLE DIMENSIONS ------------------------------

# model ---------------------------------------------------------------------------------
cfa_3d_model <- 'extractive  =~ taxrev_gdp + tax_trade_tax + tax_inc_tax + irai_erm +
                                v2stfisccap
                 coercive =~ milexpercap + milpercap + policecap + bti_mo + v2terr + 
                             StateHist50s
                 administrative =~ irai_qbfm + irai_qpa + censusfreq + infcap + wbstat'
cfa_3d_fit <- bcfa(cfa_3d_model, n.chains = 4, burnin = 2000, sample = 1000, 
                   data = state_capacity_data, bcontrol = list(cores = 4, refresh = 25),
                   save.lvs = TRUE)

# summary -------------------------------------------------------------------------------
summary(cfa_3d_fit)

# posterior of latent variables ---------------------------------------------------------
posterior <- blavInspect(cfa_3d_fit, what = "lvs") %>%
  purrr::map_dfr(as.data.frame)
posterior_extractive <- posterior[, 1:178]
posterior_coercive <- posterior[, 179:356]
posterior_administrative <- posterior [, 357:534]

# visualize -----------------------------------------------------------------------------
posterior_extractive_formatted <- posterior_extractive %>%
  set_names(state_capacity_data$country) %>%
  pivot_longer(everything(), names_to = "country", values_to = "capacity") %>%
  group_by(country) %>%
  summarize(lower = quantile(capacity, prob = 0.025),
            upper = quantile(capacity, prob = 0.975),
            fit = mean(capacity)) %>%
  mutate(country = fct_reorder(factor(country), fit),
         dim = "extractive")
posterior_coercive_formatted <- posterior_coercive %>%
  set_names(state_capacity_data$country) %>%
  pivot_longer(everything(), names_to = "country", values_to = "capacity") %>%
  group_by(country) %>%
  summarize(lower = quantile(capacity, prob = 0.025),
            upper = quantile(capacity, prob = 0.975),
            fit = mean(capacity)) %>%
  mutate(dim = "coercive")
posterior_administrative_formatted <- posterior_administrative %>%
  set_names(state_capacity_data$country) %>%
  pivot_longer(everything(), names_to = "country", values_to = "capacity") %>%
  group_by(country) %>%
  summarize(lower = quantile(capacity, prob = 0.025),
            upper = quantile(capacity, prob = 0.975),
            fit = mean(capacity)) %>%
  mutate(dim = "administrative")
posterior_formatted <- rbind(posterior_extractive_formatted,
                             posterior_coercive_formatted,
                             posterior_administrative_formatted)

posterior_formatted <- posterior_formatted %>%
  filter(country %in% country_sample)

ggplot(data = posterior_formatted) +
  geom_pointrange(aes(x = fit, y = country, xmin = lower, xmax = upper)) +
  facet_wrap(~dim, scales = "free") +
  theme_bw()



