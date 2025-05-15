# ---------------------------------------------------------------------------------------
# BAYESIAN MODELING
# Session 12: Computing and visualizing quantities of interest
# Instructor: Sascha Göbel
# May 2024
# ---------------------------------------------------------------------------------------


#### INSTALL AND LOAD PACKAGES ==========================================================

# install pacman package if not installed -----------------------------------------------
suppressWarnings(if (!require("pacman")) install.packages("pacman"))

# load packages and install if not installed --------------------------------------------
pacman::p_load(blavaan,
               cmdstanr,
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
