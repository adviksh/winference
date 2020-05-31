# Seed --------------------------------------------------------------------
set.seed(1)


# Timing ------------------------------------------------------------------
tictoc::tic()


# Libraries ---------------------------------------------------------------
library(here)
library(tidyverse)
devtools::load_all()


# Helpers -----------------------------------------------------------------
is_covered <- function(tmt_effect, best_obs_tmt, ci) {

  if (length(ci) == 0) return(NA)

  true_value <- ifelse(best_obs_tmt == 1, tmt_effect, 0)
  between(true_value, ci[1], ci[2])
}


# Setup -------------------------------------------------------------------
ci_files <- list.files(path = ".",
                       pattern = "confidence_intervals")
ci_tb_nested <- map_df(ci_files, read_rds)


# Reshape Data ------------------------------------------------------------
message("Reshaping data...")
ci_tb <- unnest(ci_tb_nested, ci)


# Coverage ----------------------------------------------------------------
message("Calculating coverage...")
coverage_tb <- mutate(ci_tb,
                      covered = pmap_lgl(list(tmt_effect, best_obs_tmt, ci),
                                         is_covered))

# Save --------------------------------------------------------------------
message("Saving...")
write_rds(coverage_tb, "coverage.rds")


# Done --------------------------------------------------------------------
message("Done.")
tictoc::toc()
