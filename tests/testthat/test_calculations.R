context("Testing functions calculating scores")

data("auc_scores")

test_that("Is epp calculated?", {
  expect_is(calculate_epp(auc_scores, decreasing_metric = TRUE, compare_in_split = TRUE), "data.frame")
  expect_is(calculate_epp(auc_scores, decreasing_metric = FALSE, compare_in_split = FALSE, keep_data = FALSE), "data.frame")
})
