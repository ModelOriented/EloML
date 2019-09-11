
#' @importFrom dplyr filter left_join mutate rename group_by summarise
#' @importFrom data.table setDT dcast
#' @importFrom stringr str_match
NULL

calculate_wins_all_model <- function(results, list_models, compare_in_split, compare_function){

  list_models <- data.frame(compare_with = list_models)

  tmp <- base::merge(results,  list_models, by=NULL)
  tmp <- filter(tmp, model  != compare_with)


  if(compare_in_split){
    tmp_joined <- left_join(tmp, results, by = c("compare_with" = "model", "split"="split"))
  }else{
    tmp_joined <- left_join(tmp, results, by = c("compare_with" = "model"))
  }


  tmp_joined <- mutate(tmp_joined, WIN = compare_function(score.x, score.y))
  tmp_joined <- rename(tmp_joined, WINNER = model, LOSER = compare_with)
  tmp_joined <- group_by(tmp_joined, WINNER, LOSER )
  tmp_joined <- summarise(tmp_joined, MATCH = n(), WINS = sum(WIN))
  tmp_joined

}


create_summary_model <- function(model){

  vector_coeff_model <- coefficients(model)
  all_levels <- c(paste0("WINNER", as.vector(unique(model$data$WINNER))),
                  paste0("LOSER", as.vector(unique(model$data$WINNER))))



  base_level <- setdiff(all_levels, names(vector_coeff_model))

  vector_coeff_model[base_level] <- 0
  result <- data.frame(SIDE = str_match(names(vector_coeff_model), "[A-Z]+"),
             MODEL = str_match(names(vector_coeff_model), "[a-z]+.*") ,
             COEFF = round(vector_coeff_model, 3))

  result <- setDT(result)
  result <- dcast(result, MODEL~SIDE , value.var = "COEFF")
  result <- mutate(result,EPP = (WINNER - LOSER)/2)
  result
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


  unique_model <- unique(results$model)

  summary_results <-  calculate_wins_all_model(results = results,
                                               list_models = unique_model,
                                               compare_in_split = compare_in_split,
                                               compare_function = is_metric1_better)

  summary_results
}

#' Calulate EPP score for all models
#'
#'
#' @param results data frame with results for one dataset
#' @param decreasing_metric if used metric is decreasing. If TRUE Model with higher score value is better.
#' @param compare_in_split if compare models and parameters only in the same fold
#'
#' @export
#' @importFrom stats glm
calculate_epp <- function(results, decreasing_metric = TRUE, compare_in_split = TRUE){
  # some cleaning to make unified naming
  colnames(results) <- c("model", "split", "score")
  results[,"model"] <- factor(results[,"model"])

  actual_score <- calculate_actual_wins(results = results, decreasing_metric = decreasing_metric, compare_in_split=compare_in_split)

  model_epp <- glm(cbind(WINS, MATCH - WINS) ~0+ WINNER + LOSER, data = actual_score, family = "binomial")

  create_summary_model(model_epp)

}
