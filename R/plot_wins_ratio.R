#' @title Plot true ratio vs predicted probability
#'
#' @param elo object of class elo_results.
#'
#' @import ggplot2
#' @export
plot_wins_ratio <- function(elo){

  if(!("actual_score" %in% names(elo))) stop("Use `keep_data == TRUE` parameter in function `calculate_epp()`")
  actual_score <- elo$actual_score
  elo_score <- elo$elo

  actual_score[["ratio"]] <- actual_score[["wins"]] / actual_score[["match"]]
  actual_score <- merge(actual_score, elo_score, by.x ="winner", by.y = "model")
  names(actual_score)[names(actual_score)=='epp'] <- "epp_winner"
  actual_score <- merge(actual_score, elo_score, by.x ="loser", by.y = "model")
  names(actual_score)[names(actual_score)=='epp'] <- "epp_loser"

  actual_score[['pred_ratio']] <- exp(actual_score[["epp_winner"]] - actual_score[['epp_loser']])/(1+exp(actual_score[["epp_winner"]] - actual_score[['epp_loser']]))

  ggplot(actual_score, aes(x=ratio, y = pred_ratio))+
    geom_point()+
    geom_abline(slope=1)


  }
