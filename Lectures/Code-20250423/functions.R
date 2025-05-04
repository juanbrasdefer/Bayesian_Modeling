# ---------------------------------------------------------------------------------------
# BAYESIAN MODELING
# Session 10: Item Response Models
# Instructor: Sascha Göbel
# April 2025
# ---------------------------------------------------------------------------------------


#### importCmdstanPosterior =============================================================

# function arguments --------------------------------------------------------------------
# posterior_file: path to the output posterior CSV file
# parameters: parameters to import posterior for
# incl_warmup: whether to include warmup draws (requires warmup to be stored in output CSV)
importCmdstanPosterior <- function(posterior_file, parameters, incl_warmup = FALSE) {
  # collect meta information
  meta <- vroom::vroom(file = posterior_file,
                       delim = ",",
                       n_max = 44)
  warmup_saved <- ifelse(as.integer(stringr::str_extract(meta[[1]][9], "[[:digit:]]+")) == 1L, TRUE, FALSE)
  if (is.na(warmup_saved)) {
    warmup_saved <- ifelse(stringr::str_extract(meta[[1]][9], "false") != "false", TRUE, FALSE)
  }
  if (warmup_saved) {
    warmup_draws <- stringr::str_extract(meta[[1]][8], "[[:digit:]]+") %>%
      as.integer()
  }
  sampling_draws <- stringr::str_extract(meta[[1]][7], "[[:digit:]]+") %>%
    as.integer()
  # collect posterior with warmup draws
  if (incl_warmup == TRUE) {
    if (warmup_saved == FALSE) {
      stop("Warmup draws not available.")
    }
    posterior <- vroom::vroom(posterior_file,
                              delim = ",",
                              comment = "# ",
                              n_max = warmup_draws+sampling_draws,
                              col_select = starts_with(parameters))
    gc()
  } else {
    # in the presence of warmup draws, collect posterior without warmup draws
    if (warmup_saved == TRUE) {
      posterior <- vroom::vroom(posterior_file,
                                delim = ",",
                                comment = "# ",
                                n_max = warmup_draws+sampling_draws,
                                col_select = starts_with(parameters)) %>%
        slice(warmup_draws+1:(warmup_draws+sampling_draws))
      gc()
      # absent warmup draws, collect posterior without warmup draws      
    } else {
      posterior <- vroom::vroom(posterior_file,
                                delim = ",",
                                comment = "# ",
                                n_max = sampling_draws,
                                col_select = starts_with(parameters))  
      gc()
    }
  }
  return(posterior)
}


#### invlogit ===========================================================================

# function ------------------------------------------------------------------------------
invlogit <- plogis
