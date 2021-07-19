context("Testing functions calculating scores")

data("auc_scores")
auc_scores_short <- auc_scores[1:100,]
test_that("Is epp calculated?", {
  expect_is(calculate_epp(auc_scores_short, decreasing_metric = TRUE, compare_in_split = TRUE),
            "epp_results")
  expect_is(calculate_epp(auc_scores_short, decreasing_metric = FALSE, compare_in_split = FALSE, keep_data = FALSE),
            "epp_results")
})
