test_that("type 1 error", {

  alphas = c(0.05, 0.1, 0.25, 0.5)

  run_tests_at_alpha <- function(alpha, ys, mean, sigma, trunc_lo,
                                 trunc_hi) {

    purrr::map_lgl(ys, hybrid_equal_tailed_test_rejects,
                   n_estimates = 10,
                   mu    = mean,
                   sigma = sigma,
                   alpha = alpha,
                   trunc_lo = trunc_lo,
                   trunc_hi = trunc_hi)
  }

  simulate_tests <- function(n_sims, alphas, mean, sd, trunc_lo,
                             trunc_hi) {

    ys <- r_truncnorm(n    = n_sims,
                      mean = mean,
                      sd   = sd,
                      trunc_lo = trunc_lo,
                      trunc_hi = trunc_hi)

    purrr::map(alphas,
               run_tests_at_alpha,
               ys       = ys,
               mean     = mean,
               sigma    = sd,
               trunc_lo = trunc_lo,
               trunc_hi = trunc_hi)


  }

  tests <- simulate_tests(n_sims = 5000,
                          alphas = alphas,
                          mean = 0,
                          sd = 1,
                          trunc_lo = -0.5,
                          trunc_hi = Inf)

  # Hybrid tests have an upper bound T1 error rate of alpha
  t1_error_rate <- purrr::map(tests, mean)
  all_is_well   <- purrr::map2_lgl(t1_error_rate, alphas, `<`)

  expect_identical(all_is_well, rep_len(TRUE, length(all_is_well)))

})
