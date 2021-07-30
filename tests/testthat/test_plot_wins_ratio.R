context("Testing functions ploting wins ratio")

source("objects_for_tests.R")


test_that("Error arguments", {
  expect_error(plot_wins_ratio(epp_glm, random_sample = 1.2))
  expect_error(plot_wins_ratio(epp_glm, random_sample = 0.9, random_state = 'a'))
  expect_error(plot_wins_ratio(epp_glm_wo_data_columnss))
  })


test_that('Check sample size', {
  expect_equal(nrow(p_epp_win_ratio$data), 20)
  expect_equal(nrow(p_epp_win_ratio_sample$data), 12)
  })


test_that('Plot wins ratio', {
  vdiffr::expect_doppelganger("plot all players",
                              p_epp_win_ratio)
  vdiffr::expect_doppelganger("plot sample players",
                              p_epp_win_ratio_sample)
})
