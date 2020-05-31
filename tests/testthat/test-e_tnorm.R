test_that("symmetry", {
  expect_equal(e_truncnorm(), 0)
  expect_equal(e_truncnorm(trunc_lo = -1, trunc_hi = 1), 0)
  expect_equal(e_truncnorm(trunc_lo = -5, trunc_hi = 5), 0)
  expect_equal(e_truncnorm(mean = 10, trunc_lo = 9, trunc_hi = 11), 10)
  expect_equal(e_truncnorm(trunc_lo = -1), -1 * e_truncnorm(trunc_hi = 1))
})

test_that("skew", {
  expect_gt(e_truncnorm(trunc_lo = -1), 0)
  expect_lt(e_truncnorm(trunc_hi = 1), 0)
})
