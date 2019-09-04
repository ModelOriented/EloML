
#' @importFrom magrittr "%>%"
#' @importFrom dplyr filter left_join mutate rename group_by summarise
#' @importFrom data.table setDT dcast
#' @importFrom stringr str_match
NULL

calculate_wins_all_model <- function(results, list_models, compare_in_split, compare_function){

  list_models <- data.frame(compare_with = list_models)

  tmp <- base::merge(results,  list_models, by=NULL) %>%
    filter(model_param_index  != compare_with)


  if(compare_in_split){
    tmp_joined <- tmp %>%
      left_join(results, by = c("compare_with" = "model_param_index", "row_index"="row_index"))
  }else{
    tmp_joined <- tmp %>%
      left_join(results, by = c("compare_with" = "model_param_index"))
  }


  tmp_joined %>%
    mutate(WIN = compare_function(auc.x, auc.y)) %>%
    rename(WINNER = model_param_index, LOSER = compare_with) %>%
    group_by(WINNER, LOSER ) %>%
    summarise(MATCH = n(),
              WINS = sum(WIN))


}


create_summary_model <- function(model){

  vector_coeff_model <- coefficients(model)
  all_levels <- c(paste0("WINNER", as.vector(unique(model$data$WINNER))),
                  paste0("LOSER", as.vector(unique(model$data$WINNER))))



  base_level <- setdiff(all_levels, names(vector_coeff_model))

  vector_coeff_model[base_level] <- 0
  data.frame(SIDE = str_match(names(vector_coeff_model), "[A-Z]+"),
             MODEL = str_match(names(vector_coeff_model), "[a-z]+.*") ,
             COEFF = round(vector_coeff_model, 3)) %>%
    setDT() %>%
    dcast(MODEL~SIDE , value.var = "COEFF") %>%
    mutate(EPP = (WINNER - LOSER)/2)

}


#' Calulate actual results for every pair of models
#'
#'
#' @param results raw results
#' @param decreasing_metric if used metric is decreasing
#' @param compare_in_split if compare models and parameters only in fhe same fold
#'
#' @export


calculate_actual_wins <- function(results, decreasing_metric = TRUE, compare_in_split){
  ### define comparison of metrics
  if(decreasing_metric){
    is_metric1_better <- function(metric1, metric2){
      as.numeric(metric1>metric2)

    }
  }else{

    is_metric1_better <- function(metric1, metric2){
      as.numeric(metric1<metric2)

    }

  }


  results <- results %>%
    mutate(model_param_index = paste0(model,"_", param_index))



  unique_model_param <- unique(results$model_param_index)


  summary_results <-  calculate_wins_all_model(results = results, list_models = unique_model_param, compare_in_split = compare_in_split, compare_function = is_metric1_better)


  summary_results
}

#' Calulate EPP score for all models
#'
#'
#' @param results data frame with results for one dataset
#' @param decreasing_metric if used metric is decreasing
#' @param compare_in_split if compare models and parameters only in the same fold
#'
#' @export
#' @importFrom stats glm
calculate_epp_score <- function(results, decreasing_metric = TRUE, compare_in_split){

  actual_score <- calculate_actual_wins(results = results, decreasing_metric = decreasing_metric, compare_in_split=compare_in_split)

  model_epp <- glm(cbind(WINS, MATCH - WINS) ~0+ WINNER + LOSER, data = actual_score, family = "binomial")

  create_summary_model(model_epp)


}
