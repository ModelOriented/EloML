context("Testing functions calculating scores")

data("auc_scores")
auc_scores_short <- auc_scores[1:100,]
test_that("Is epp calculated?", {
  expect_is(calculate_elo(auc_scores_short, decreasing_metric = TRUE, compare_in_split = TRUE), "elo_results")
  expect_is(calculate_elo(auc_scores_short, decreasing_metric = FALSE, compare_in_split = FALSE, keep_data = FALSE), "elo_results")
})
