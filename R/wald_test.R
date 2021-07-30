#' @title Test for the Differences Between Two EPP Meta-Scores
#'
#' @description The test is based on the Wald test for logistic regression used to estimate EPP.
#'
#' @param epp epp_results. The result of a function \code{\link{calculate_epp}}.
#' @param player1 Character. Name of the first Player to test.
#' @param player2 Character. Name of the second Player to test.
#'
#' @details Naming convention, such as Player, Rounds, etc. comes from [Gosiewska et al. (2020)](https://arxiv.org/abs/2006.02293).
#'
#' @return epp_test
#'
#' @importFrom stats pchisq
#' @examples
#' data(auc_scores)
#' epp <- calculate_epp(auc_scores[1:400,], keep_model = TRUE, estimation = "glm")
#' test_players_diff(epp, "gbm_10", "gbm_2")
#'
#' @export
test_players_diff <- function(epp, player1, player2){
  if(epp$estimation != "glm") stop("Test requires estimated covariance matrix. Use `estimation='glm'` in function `caluclate_epp()`.")
  coefs <- epp$epp_summary$epp
  players <- epp$epp_summary$player
  covar <- epp$covariance_epp
  n <- length(coefs)


  C <- rep(0, n)
  C[players == player1] <- 1
  C[players == player2] <- -1

  C <- t(matrix(C))
  cb <- C %*% coefs
  cvc <- C %*% covar %*% t(C)
  Q <- t(cb) %*% solve(cvc) %*% cb

  structure(
    .Data = list(player1, player2, Q, 1 - pchisq(Q[1,1], df = 2)),
    .Names = c("player1", "player2", "statistic", "pvalue"),
    class = "epp_test"
  )
}

#' @title Printing Summary of the EPP Test
#'
#' @param x epp_test. The result of a function \code{\link{test_players_diff}}.
#' @param ... other parameters
#'
#' @return No return value, prints the structure of the object
#'
#' @export
print.epp_test <- function(x, ...) {
  cat("Wald-based test for the difference between ", x$player1, " and ", x$player2, ".\n", sep = "")
  cat("Test statistic: ", x$statistic, "\n")
  cat("p-value: ", x$pval, "\n")
}
