data("auc_scores")

auc_scores_short <- auc_scores[1:100,]

epp_glm <- expect_is(calculate_epp(auc_scores_short, estimation = "glm"),
                 "epp_results")
epp_glmnet <- expect_is(calculate_epp(auc_scores_short, estimation = "glmnet", keep_columns = TRUE),
                 "epp_results")
epp_glm_w_model_columns <- expect_is(calculate_epp(auc_scores_short, estimation = "glm", keep_model = TRUE, keep_columns = TRUE),
                     "epp_results")
epp_glm_wo_data_columns <- expect_is(calculate_epp(auc_scores_short, estimation = "glm", keep_data = FALSE, keep_columns = FALSE),
                     "epp_results")
epp_glm_w_columns <- expect_is(calculate_epp(auc_scores_short, estimation = "glm", keep_data = FALSE, keep_columns = TRUE),
                                     "epp_results")


p_epp_win_ratio <- expect_is(plot_wins_ratio(epp_glm), 'ggplot')
p_epp_win_ratio_sample <- expect_is(plot_wins_ratio(epp_glm, random_sample = 0.8, random_state = 123), 'ggplot')
