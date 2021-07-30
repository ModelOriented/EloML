context("Testing functions ploting epp ranking")

source("objects_for_tests.R")

test_that("Error arguments", {
  expect_error(plot_epp_ranking(epp_glm_wo_data_columns))
  expect_error(plot_epp_ranking(epp_glmnet, confidence_intervals = TRUE))
  expect_error(plot_epp_ranking(epp_glm_w_columns, confidence_intervals = TRUE))
})




test_that('Plot epp ranking', {
  vdiffr::expect_doppelganger("plot ranking vs. mean",
                              plot_epp_ranking(epp_glm_w_model_columns, aggregation = mean))
  vdiffr::expect_doppelganger("plot ranking vs. median",
                              plot_epp_ranking(epp_glm_w_model_columns, aggregation = median))
  vdiffr::expect_doppelganger("plot ranking with conf intervals",
                              plot_epp_ranking(epp_glm_w_model_columns, aggregation = median, confidence_intervals = TRUE))
  vdiffr::expect_doppelganger("plot ranking with players names",
                              plot_epp_ranking(epp_glm_w_model_columns, aggregation = mean,  show_player_names = TRUE))
})


