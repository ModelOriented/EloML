#' @title Plot true ratio vs predicted probability
#'
#' @param epp object of class epp_results.
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
