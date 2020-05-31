test_that("symmetry", {
  expect_equal(e_truncnorm_times_indicator(c_z      = c(-Inf, Inf),
                                           mu       = 0,
                                           sigma    = 1,
                                           trunc_lo = -Inf,
                                           trunc_hi = Inf),
               0)

  expect_equal(e_truncnorm_times_indicator(c_z      = c(-1, 1),
                                           mu       = 0,
                                           sigma    = 1,
                                           trunc_lo = -Inf,
                                           trunc_hi = Inf),
               0)
})
