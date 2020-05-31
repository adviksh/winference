test_that("breaks on bad input", {
  expect_error(ci_standard(estimates = NA,
                           st_errs   = 1,
                           conf_level = 0.5))

  expect_error(ci_standard(estimates = 1,
                           st_errs   = -1,
                           conf_level = 0.5))

  expect_error(ci_standard(estimates = 1,
                           st_errs   = 1,
                           conf_level = 1.5))
})

test_that("basic interval", {
  ci <- round(ci_standard(0, 1), 2)
  expect_equal(ci, c(-1.96, 1.96))
})
