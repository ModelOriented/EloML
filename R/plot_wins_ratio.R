#' @title Plot True Ratio vs EPP Probability
#'
#' @description Plot of an empirical probability of winning versus probability of winning
#' estimated from EPP values.
#'
#' @param epp epp_results. The result of a function \code{\link{calculate_epp}}.
#' @param random_sample Numeric. If not NULL `random sample` fraction of Players is sampled from the set of all Players.
#' @param random_state Numeric. Seed for sampling Players. Used only when `random_sample` is not NULL.
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
plot_wins_ratio <- function(epp, random_sample = NULL, random_state = NULL){
  ratio <- pred_ratio <- NULL
  if(is.null(epp[["actual_score"]])) stop("Use `keep_data == TRUE` parameter in function `calculate_epp()`")
  actual_score <- epp$actual_score
  epp_score <- epp$epp


  if(!is.null(random_sample)){

    if(!is.numeric(random_sample)) stop("Use `random_sample` as numeric value in (0,1].")
    if(random_sample>1 | random_sample <= 0) stop("Use `random_sample` as numeric value in (0,1].")

    if(!is.null(random_state)){
      if(!is.numeric(random_state)) stop("Use `seed` as numeric value.")
      set.seed(random_state)
    }

    sample_player <- sample(epp_score$player, size = floor(random_sample * length(epp_score$player)))
    actual_score <- actual_score[actual_score$winner %in% sample_player & actual_score$loser %in% sample_player,]
    epp_score <- epp_score[epp_score$player %in% sample_player, ]
  }

  actual_score[["ratio"]] <- actual_score[["wins"]] / actual_score[["match"]]
  actual_score <- merge(actual_score, epp_score, by.x ="winner", by.y = "player")
  names(actual_score)[names(actual_score)=='epp'] <- "epp_winner"
  actual_score <- merge(actual_score, epp_score, by.x ="loser", by.y = "player")
  names(actual_score)[names(actual_score)=='epp'] <- "epp_loser"

  actual_score[['pred_ratio']] <- exp(actual_score[["epp_winner"]] - actual_score[['epp_loser']])/(1+exp(actual_score[["epp_winner"]] - actual_score[['epp_loser']]))

  ggplot(actual_score, aes(x=ratio, y = pred_ratio))+
    geom_point()+
    geom_abline(slope=1)


  }
