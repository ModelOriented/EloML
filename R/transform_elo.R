#' @title Calculate the Probability of Winning
#'
#' @description Probability of winning of a Player is the logit of the difference between EPP score
#' of this Player and an Opponent.
#'
#' @param epp_1 Numeric. EPP score of Player.
#' @param epp_2 Numeric. EPP score of Opponent.
#'
#' @details Naming convention, such as Player, Rounds, etc. comes from [Gosiewska et al. (2020)](https://arxiv.org/abs/2006.02293).
#'
#' @return numeric
#'
#' @examples
#' library(EloML)
#' calculate_probability(10, 8)
#'
#' @export
calculate_probability <- function(epp_1, epp_2){
  diff <- epp_1 - epp_2
  exp(diff) / (1 + exp(diff))
}
