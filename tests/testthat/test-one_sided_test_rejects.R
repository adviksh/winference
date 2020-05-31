test_that("type 1 error", {

  alphas = c(0.05, 0.1, 0.25, 0.5)

  run_tests_at_alpha <- function(alpha, side, ys, mean, sigma, trunc_lo,
                                 trunc_hi) {

    purrr::map_lgl(ys, one_sided_test_rejects,
                   mu    = mean,
                   side  = side,
                   sigma = sigma,
                   alpha = alpha,
                   trunc_lo = trunc_lo,
                   trunc_hi = trunc_hi)
  }

  simulate_tests <- function(n_sims, side, alphas, mean, sd, trunc_lo,
                             trunc_hi) {

    ys <- r_truncnorm(n    = n_sims,
                      mean = mean,
                      sd   = sd,
                      trunc_lo = trunc_lo,
                      trunc_hi = trunc_hi)

    purrr::map(alphas,
               run_tests_at_alpha,
               side     = side,
               ys       = ys,
               mean     = mean,
               sigma    = sd,
               trunc_lo = trunc_lo,
               trunc_hi = trunc_hi)


  }

  for (side in c("left", "right")) {

    tests <- simulate_tests(n_sims = 1000,
                            side   = side,
                            alphas = alphas,
                            mean = 0,
                            sd = 1,
                            trunc_lo = -0.5,
                            trunc_hi = Inf)

    t1_error_prob <- purrr::map(tests, ci_proportion)
    all_is_well   <- purrr::map2_lgl(alphas, t1_error_prob, is_contained)

    expect_identical(all_is_well, rep_len(TRUE, length(all_is_well)))

  }

})
