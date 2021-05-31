#' @title Test the differences between two Elo Scores
#'
#' @description The test is based on the Wald test.
#'
#' @param elo object of class elo_results.
#' @param player1 Name of the first player to test
#' @param player2 Name of the second player to test
#'
#' @ImportFrom stats pchisq
#' @examples
#' data(auc_scores)
#' elo <- calculate_elo(auc_scores[1:400,], keep_model = TRUE, estimation = "glm")
#' test_players_diff(elo, "gbm_10", "gbm_2")
#'
#' @export
test_players_diff <- function(elo, player1, player2){
  if(elo$estimation != "glm") stop("Test requires estimated covariance matrix. Use `estimation='glm'` in function `caluclate_elo()`.")
  coefs <- elo$epp_summary$epp
  players <- elo$epp_summary$model
  covar <- elo$covariance_epp
  n <- length(coefs)


  C <- rep(0, n)
  C[players == player1] <- 1
  C[players == player2] <- -1

  C <- t(matrix(C))
  cb <- C %*% coefs
  cvc <- C %*% covar %*% t(C)
  Q <- t(cb) %*% solve(cvc) %*% cb

  structure(
    .Data = list(player1, player2, Q, 1 - pchisq(Q, df = 2)),
    .Names = c("player1", "player2", "statistic", "pvalue"),
    class = "elo_test"
  )
}

#' @title Printing Summary of the Elo Test
#'
#' @param x elo_test object containing information about the test results.
#' @param ... other parameters
#' is not specified then transformations for all variables are printed
#'
#' @return No return value, prints the structure of the object
#'
#' @export
print.elo_test <- function(x, ...) {
  cat("Wald-based test for the difference between ", x$player1, " and ", x$player1, ".\n", sep = "")
  cat("Test statistic: ", x$statistic, "\n")
  cat("p-value: ", x$pval, "\n")
}
