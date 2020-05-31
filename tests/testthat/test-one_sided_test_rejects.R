test_that("type 1 error", {

  alphas = c(0.05, 0.1, 0.25, 0.5)

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

  set.seed(83219)
  for (side in c("left", "right")) {

    tests <- simulate_tests(n_sims = 5000,
                            side   = side,
                            alphas = alphas,
                            mean = 0,
                            sd = 1,
                            trunc_lo = -0.5,
                            trunc_hi = Inf)

    t1_error_prob  <- purrr::map(tests, ci_proportion)
    each_contained <- purrr::map2_lgl(alphas, t1_error_prob, is_contained)

    expect_identical(each_contained,
                     rep_len(TRUE, length(each_contained)))

  }

})
