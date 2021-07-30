context("Testing functions calculating scores")

source("objects_for_tests.R")

test_that("Is epp calculated?", {
  expect_is(calculate_epp(auc_scores_short, estimation = "glmnet",
                          keep_columns = TRUE, keep_model = TRUE,
                          keep_data  = FALSE, reference = "gbm_1"),
            "epp_results")
  expect_is(calculate_epp(auc_scores_short, estimation = "glm",
                          compare_in_round = FALSE, reference = "gbm_1"),
            "epp_results")
})

test_that("Is epp_results printed?", {
  expect_equal(print(epp_glmnet), NULL)
})


test_that("Are actual wins calculated?", {
  expect_is(calculate_actual_wins(auc_scores_short, aggregate = FALSE,
                                  compare_in_round = FALSE, decreasing = FALSE),
            "data.frame")

  expect_is(calculate_actual_wins(auc_scores_short, aggregate = FALSE,
                                  compare_in_round = TRUE, decreasing = TRUE),
            "data.frame")
})
