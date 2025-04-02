# ---------------------------------------------------------------------------------------
# BAYESIAN MODELING
# Session 7: Non-nested and nested models
# Instructor: Sascha Göbel
# March 2025
# ---------------------------------------------------------------------------------------


#### INSTALL AND LOAD PACKAGES ==========================================================

# install pacman package if not installed -----------------------------------------------
suppressWarnings(if (!require("pacman")) install.packages("pacman"))

# load packages and install if not installed --------------------------------------------
pacman::p_load(cmdstanr,
               dplyr,
               forcats,
               ggplot2,
               magrittr,
               purrr,
               stringr,
               tidyr,
               vroom,
               install = TRUE,
               update = FALSE)


# show loaded packages ------------------------------------------------------------------
cat("loaded packages\n")
print(pacman::p_loaded())
