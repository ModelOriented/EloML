data("auc_scores")

auc_scores_short <- auc_scores[1:100,]

epp_glm <- expect_is(calculate_epp(auc_scores_short, estimation = "glm"),
                 "epp_results")
epp_glmnet <- expect_is(calculate_epp(auc_scores_short, estimation = "glmnet"),
                 "epp_results")
