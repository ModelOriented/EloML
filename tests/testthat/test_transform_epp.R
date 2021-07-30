context("Testing function to transform epp")

test_that("Check difference of epp",{
  expect_equal(calculate_probability(1.9, 1), 0.7109495, tolerance=1e-7)
})
