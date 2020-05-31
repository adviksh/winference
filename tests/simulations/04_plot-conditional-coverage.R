# Libraries ---------------------------------------------------------------
library(tidyverse)
library(ggthemes)
library(glue)

# Helpers -----------------------------------------------------------------
binom_ci <- function(y, n, side) {
  ci <- binom.test(x = y, n = n)$conf.int
  switch(side,
         left  = ci[1],
         right = ci[2])
}


# Load Data ---------------------------------------------------------------
cover_tb_raw <- read_rds("coverage.rds")


# Calculate Coverage ------------------------------------------------------
conditional_coverage <- cover_tb_raw %>%
  group_by(n_tmt, tmt_effect, ci_type,
           best_obs_tmt, conf_level) %>%
  summarize(n_cover    = sum(covered, na.rm = TRUE),
            n_instance = sum(!is.na(covered))) %>%
  ungroup() %>%
  mutate(coverage = n_cover / n_instance,
         coverage_lo = map2_dbl(n_cover, n_instance,
                                binom_ci,
                                side = "left"),
         coverage_hi = map2_dbl(n_cover, n_instance,
                                binom_ci,
                                side = "right"),
         tmt_effect = glue("+{tmt_effect}"),
         n_tmt = glue("{str_pad(n_tmt, 2, pad = '0')} Treatments"),
         ci_type = glue("{ci_type} interval"))


# Plot --------------------------------------------------------------------
conditional_plot <- ggplot(conditional_coverage,
                           aes(x = conf_level,
                               y = coverage - conf_level,
                               ymin = coverage_lo - conf_level,
                               ymax = coverage_hi - conf_level,
                               color = factor(tmt_effect))) +
  facet_grid(n_tmt ~ ci_type) +
  labs(x = "Confidence Level",
       y = "Deviation from Nominal Coverage\n(Coverage - Confidence Level)",
       color = "Treatment Effect") +
  scale_x_continuous(breaks = unique(conditional_coverage$conf_level),
                     minor_breaks = NULL) +
  scale_color_viridis_d(end = 0.95) +
  theme_bw() +
  geom_smooth(alpha = 0.5,
              method = "lm",
              se = FALSE) +
  geom_pointrange(size = 0.2,
                  position = position_dodge(width = 0.1))


# Save --------------------------------------------------------------------
ggsave("conditional_coverage.png", conditional_plot,
       width = 10, height = 5, units = "in")

