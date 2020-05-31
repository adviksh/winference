# Seed --------------------------------------------------------------------
set.seed(1)


# Timing ------------------------------------------------------------------
tictoc::tic()


# Config ------------------------------------------------------------------
N_SIMS <- 1e1


# Libraries ---------------------------------------------------------------
library(here)
library(tidyverse)
library(furrr)
library(optparse)
library(glue)
devtools::load_all()


# Helpers -----------------------------------------------------------------
map_ci <- function(experiment, conf_levels, ci_fun) {
  tibble(conf_level = conf_levels,
         ci         = map(conf_levels, ci_fun,
                          estimates = experiment$estimates,
                          st_errs   = experiment$st_errs_hat))
}


# Command Line Args -------------------------------------------------------
arg_list <- list(make_option(opt  = c("--ci_type"),
                             type = "character"))

arg_parser <- OptionParser(option_list = arg_list)
arg        <- parse_args(arg_parser)

# Setup -------------------------------------------------------------------
experiment_tb <- read_rds("simulated_experiments.rds")
conf_levels   <- seq(0.05, 0.95, by = 0.3)


ci_fun <- switch(arg$ci_type,
                 standard      = ci_standard,
                 conditional   = ci_conditional_equal_tailed,
                 unconditional = ci_unconditional,
                 hybrid        = ci_hybrid_equal_tailed,
                 rlang::abort("unrecognized ci_fun"))

# Set up parallelization for hybrid intervals
if (arg$ci_type == "hybrid") plan(multiprocess, workers = 4)

# Intervals ---------------------------------------------------------------
message("Calculating intervals...")
ci_tb <- experiment_tb %>%
  unnest(experiments) %>%
  rename(experiment = experiments) %>%
  transmute(n_tmt,
            tmt_effect,
            tmt_se,
            best_obs_tmt  = map_int(experiment, ~which.max(.x$estimates)),
            ci_type       = arg$ci_type,
            ci            = future_map(experiment, map_ci,
                                       ci_fun      = ci_fun,
                                       conf_levels = conf_levels,
                                       .options = future_options(packages = "winference")))

# Save --------------------------------------------------------------------
message("Saving...")
write_rds(ci_tb,
          glue("confidence_intervals_{arg$ci_type}.rds"))


# Done --------------------------------------------------------------------
message("Done.")
tictoc::toc()
