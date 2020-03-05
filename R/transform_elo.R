#' @title Calculate probability of winning
#'
#' @description Probability of winning of one model 1 is the logit of the difference between EPP score of this model and opponent model 2.
#'
#' @param epp_1 EPP score of model 1.
#' @param epp_2 EPP score of model 2.
#'
#' @export
calculate_probability <- function(epp_1, epp_2){
  diff <- epp_1 - epp_2
  exp(diff) / (1 + exp(diff))
}
