# Seed --------------------------------------------------------------------
set.seed(1)


# Timing ------------------------------------------------------------------
tictoc::tic()


# Config ------------------------------------------------------------------
N_SIMS <- 1e4


# Libraries ---------------------------------------------------------------
library(here)
library(tidyverse)
devtools::load_all()


# Setup -------------------------------------------------------------------
experiment_config <- crossing(n_tmt      = c(2, 10, 50),
                              tmt_effect = seq(1, 3, by = 1),
                              tmt_se     = 1)

sim_config <- transmute(experiment_config,
                        true_means = map2(tmt_effect, n_tmt,
                                          ~c(.x, rep(0, .y - 1))),
                        st_errs   = map2(tmt_se, n_tmt, rep))

# Simulate Experiment -----------------------------------------------------
message("Simulating experiments...")
experiment_tb <- mutate(experiment_config,
                        experiments = pmap(sim_config,
                                           simulate_experiments,
                                           n_experiments = N_SIMS))


# Save --------------------------------------------------------------------
message("Saving...")
write_rds(experiment_tb,
          "simulated_experiments.rds")


# Done --------------------------------------------------------------------
message("Done.")
tictoc::toc()
