#' @title Plot True Ratio vs EPP Probability
#'
#' @description Plot of an empirical probability of winning versus probability of winning
#' estimated from EPP values.
#'
#' @param epp epp_results. The result of a function \code{\link{calculate_epp}}.
#'
#' @return gg
#'
#' @details Naming convention, such as Player, Rounds, etc. comes from [Gosiewska et al. (2020)](https://arxiv.org/abs/2006.02293).
#'
#' @examples
#' library(EloML)
#' data(auc_scores)
#' epp <- calculate_epp(
#'   auc_scores[1:100, ],
#'   keep_columns=TRUE,
#'   keep_model = TRUE,
#'   estimation ="glm"
#'   )
#' plot_wins_ratio(epp)
#'
#' @import ggplot2
#' @export
plot_wins_ratio <- function(epp){
  ratio <- pred_ratio <- NULL
  if(!("actual_score" %in% names(epp))) stop("Use `keep_data == TRUE` parameter in function `calculate_epp()`")
  actual_score <- epp$actual_score
  epp_score <- epp$epp

  actual_score[["ratio"]] <- actual_score[["wins"]] / actual_score[["match"]]
  actual_score <- merge(actual_score, epp_score, by.x ="winner", by.y = "model")
  names(actual_score)[names(actual_score)=='epp'] <- "epp_winner"
  actual_score <- merge(actual_score, epp_score, by.x ="loser", by.y = "model")
  names(actual_score)[names(actual_score)=='epp'] <- "epp_loser"

  actual_score[['pred_ratio']] <- exp(actual_score[["epp_winner"]] - actual_score[['epp_loser']])/(1+exp(actual_score[["epp_winner"]] - actual_score[['epp_loser']]))

  ggplot(actual_score, aes(x=ratio, y = pred_ratio))+
    geom_point()+
    geom_abline(slope=1)


  }
