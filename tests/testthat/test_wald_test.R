context("Testing Wald statistical test")

source("objects_for_tests.R")


test_that("Values of test", {
  expect_equal(test_players_diff(epp_glm, "gbm_10", "gbm_1")$pvalue,
               0.9999917, tolerance=1e-7)
})

test_that("Printing test", {
  expect_equal(print(test_players_diff(epp_glm, "gbm_10", "gbm_1")),
               NULL)
})




test_that("Check error", {
  expect_error(test_players_diff(epp_glmnet, "gbm_10", "gbm_1"))
})
